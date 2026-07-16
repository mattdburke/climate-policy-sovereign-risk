fred_oas_raw <- read.csv("rawdata/fredgraph.csv")

colnames(fred_oas_raw) <- c(
    "date",
    "ccc",
    "bbb",
    "bb",
    "a",
    "b",
    "aa",
    "aaa"
)

#' This will natural produce a number of error messages. 
#' The data from FRED uses a single period for each date 
#' where there is no data. This function will replace each
#' of these with an NA. This is an intended output.
fred_oas_raw <- as.data.frame(fred_oas_raw %>%
    transmute(
        date = as.Date(date, format = "%Y-%m-%d"),
        aaa = as.numeric(as.character(aaa)),
        aa = as.numeric(as.character(aa)),
        a = as.numeric(as.character(a)),
        bbb = as.numeric(as.character(bbb)),
        bb = as.numeric(as.character(bb)),
        b = as.numeric(as.character(b)),
        ccc = as.numeric(as.character(ccc))
        )) 

fred_oas_raw <- as.data.frame(fred_oas_raw %>%
    dplyr::filter(
        date >= as.Date("2020-01-01", format = "%Y-%m-%d")
    ))

#' This section will compute the median spread from the last
#' two years of data. This probably gives the most timely, 
#' accurate representation of the cost of debt. Rather than
#' influencing the outcome with data which is overly
#' historic. I also divide them through by 100 to retrieve the
#' decimal placed percentage.
median_spread_rating <- data.frame(
    rating = colnames((fred_oas_raw)[,c(2:length(fred_oas_raw))]),
    rating_numeric = c(20, 18, 15, 12, 9, 6, 3),
    spread = c(
        median(fred_oas_raw[,2], na.rm = TRUE)/100,
        median(fred_oas_raw[,3], na.rm = TRUE)/100,
        median(fred_oas_raw[,4], na.rm = TRUE)/100,
        median(fred_oas_raw[,5], na.rm = TRUE)/100,
        median(fred_oas_raw[,6], na.rm = TRUE)/100,
        median(fred_oas_raw[,7], na.rm = TRUE)/100,
        median(fred_oas_raw[,8], na.rm = TRUE)/100
        ))

fit <- lm(spread ~ poly(rating_numeric, 3, raw=TRUE), data = median_spread_rating)

equa <- function(A, x1,x2,x3,x4){
  #' Calculates the output from a predetermined 3rd order polynomial. 
  #' A: This should be the series requiring 3rd order estimation.
  #' x1: 1st coefficient from the fitted model
  #' x2: 2nd coefficient from the fitted model
  #' x3: 3rd coefficient from the fitted model
  #' x4: 4th coefficient from the fitted model
	x1 + A*x2 + (A^2)*x3 + (A^3)*x4
	}

calculate_spreads <- function(series){
  #' Provides implementation of 3rd order polynomial.
  #' Series: The vector as input for the polynomial.
  spread <- equa(series,
    fit$coefficients[1],
	fit$coefficients[2],
    fit$coefficients[3],
    fit$coefficients[4])
  return (spread*100)
}

PD_data <- read.csv("rawdata/PD_ratings.csv", header = FALSE)

rating <- PD_data$V1
default <- PD_data$V2
pr_of_default_fit <- lm(default ~ poly(rating, 3, raw = TRUE))

equa <- function(a, x1, x2, x3, x4){
    x1 + a * x2 + (a^2) * x3 + (a^3) * x4
    }

implement_PD_equation <- function(rating_estimates){
    PD_vector <- equa(rating_estimates,
    pr_of_default_fit$coefficients[1],
    pr_of_default_fit$coefficients[2],
    pr_of_default_fit$coefficients[3],
    pr_of_default_fit$coefficients[4])
    PD_vector <- ifelse(
        PD_vector < 0, 0, PD_vector
    )
    PD_vector <- ifelse(
        PD_vector > 100, 100, PD_vector
    )
    return (PD_vector)}

baseline <- read.csv("cleandata/baseline_data_clean.csv", header=TRUE)

set.seed(77)
model.forest <- ranger(scale20 ~
	ln_S_GDPpercapitaUS +
	S_RealGDPgrowth +
	S_NetGGdebtGDP +
	S_GGbalanceGDP +
	S_NarrownetextdebtCARs +
	S_CurrentaccountbalanceGDP
	,
	data=baseline,
	num.trees=2000,
	importance='permutation',
	write.forest = TRUE,
	keep.inbag=TRUE)


produce_baseline_estimate <- function(model, df){
    pred <- predict(model, df, type="se")
	est <- pred$predictions
    return (est)}

produce_adjusted_ratings <- function(model, df){
	pred <- predict(model, df, type="se")
	est <- pred$predictions
	se <- pred$se
	actual <- df$scale20
	T <- pred$predictions / se
	P = exp(-0.717*T -0.416*(T^2))
	n = length(df$CountryName)
	DF = n - 3
	crit = tinv(.05, DF)
	est_lower = est + crit*se
	est_upper = est - crit*se
	country <- df$CountryName
	ISO2 <- df$ISO2
	m1 <- cbind(country, ISO2, actual, est, est_lower, est_upper)
	m1 <- do.call(rbind, Map(data.frame, country=country,
		ISO2 = ISO2,
		actual=actual,
		est=est,
		est_lower=est_lower,
		est_upper=est_upper
		))
	m1.sum <- as.data.frame(m1 %>% group_by(country) %>%
		summarise(ISO2 = first(ISO2),
				actual = mean(actual),
				est = mean(est), 
				est_lower = mean(est_lower),
				est_upper = mean(est_upper))) %>% ungroup() %>%
        mutate(
            cd_actual = calculate_spreads(actual),
            pd_actual = implement_PD_equation(actual),
            cd_est = calculate_spreads(est),
            pd_est = implement_PD_equation(est),
            cd_est_95 = calculate_spreads(est_lower),
            pd_est_95 = implement_PD_equation(est_lower),
            rating_delta = est - actual,
            cd_delta = cd_est - cd_actual,
            pd_delta = pd_est - pd_actual,
            rating_delta_95 = est_lower - actual,
            cd_delta_95 = cd_est_95 - cd_actual,
            pd_delta_95 = pd_est_95 - pd_actual
        )
    m1.sum <- m1.sum[!(m1.sum$ISO2 %in% "AR"),]
	return (m1.sum)
}

# for all the clean data
for (i in dir("cleandata/")){
        name <- substr(i,1,nchar(i)-4)
        # write a new csv file with adjusted ratings
        write.csv(produce_adjusted_ratings(
            # based on the rf model
            model.forest,
            # using the qualifying data files
            read.csv(paste0("cleandata/", i))
        ),
        # write them to the output folder under the same name
        file = paste0(
            "output/", name, "_ratings.csv"
        ))}

# aggegrate results into one master frame
y <- 1
dataframes <- c()
for (i in dir("output/")){
    if(grepl("0", c(i))){
        name <- substr(i,1,nchar(i)-4)
        assign(name, read.csv(paste0("output/", i)))
        dataframes[y] <- name
        y <- y + 1
    }
}

master_frame <- get(dataframes[1])
master_frame['scenario'] <- dataframes[1]

for (i in 2:length(dataframes)){
    frame <- get(dataframes[i])
    frame['scenario'] <- dataframes[i]
    master_frame <- rbind(master_frame, frame)
}

master_frame <- master_frame %>% dplyr::mutate(
        model = sub("^[^_]*_([^_]*)_.*$", "\\1", scenario),
        scenario_ = sub("^[^_]*_[^_]*_([^_]*)_.*$", "\\1", scenario),
        year = sub("^[^_]*_[^_]*_[^_]*_([^_]*)_.*$", "\\1", scenario)#,
        ) %>% dplyr::select(
            -c(scenario)
        )

write.csv(master_frame, "output/all_scenarios_ratings_results.csv", row.names=FALSE)

## Analysis of `fallen angels' for Page 12.

# How many countries are investment grade in the final year of the sample

baseline %>% 
    filter(Year==2019 & scale20>10 & CountryName %in% unique(master_frame$country)) %>% 
    nrow()

baseline %>% 
    filter(Year==2019 & scale20<11 & CountryName %in% unique(master_frame$country)) %>% 
    distinct(CountryName)


# Lost investment grade count by scenario

master_frame %>%
  group_by(country, scenario_) %>%
  summarise(
    actual = first(actual),  # assuming actual is constant
    est_mean = mean(est, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(actual >= 11, est_mean < 11) %>% as.data.frame() %>%
  group_by(scenario_) %>%
  summarise(
    n_countries = n_distinct(country),
    .groups = "drop"
  )

# Countries which lose investment grade status in all scenarios

n_scen <- n_distinct(master_frame$scenario_)

countries_below_11_all_scenarios <- master_frame %>%
  group_by(country, scenario_) %>%
  summarise(
    actual = first(actual),
    est_mean = mean(est, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(actual >= 11, est_mean < 11) %>%
  group_by(country) %>%
  summarise(
    n_scenarios = n_distinct(scenario_),
    .groups = "drop"
  ) %>%
  filter(n_scenarios == n_scen) %>%
  select(country)

countries_below_11_all_scenarios