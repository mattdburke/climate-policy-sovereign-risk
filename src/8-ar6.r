#' Notes:
#' 
#' In this paper we want to study the fiscal consequences of climate policies
#' 
#' We begin filtering by splitting the available scenarios from AR6 into 
#' their relevant warming level categories. 
#' 
#' Because we want to study policy, we restrict our scenarios to those that
#' seek to limit warming to 4 degrees C. This restricts the study of SSP5
#' 
#' We also group by policy category to ensure aggregation at a later stage 
#' follows a sensible approach
#' 
#' Because overall - ratings would look better in scenarios which maximise income
#' we study different policy scenarios within temperature categories
#' 
#' We focus on scenarios up to C4 and identify the trade-off in 
#' ratings terms between different policy approaches
#' 
#' We'll always be richer
#' 
#' P0 is reference
#' P1 is no action
#' P2 is coordinated
#' P3 is delayed
#' P4 is optimised


# df1_a<-read.csv("1668008131197-AR6_Scenarios_Database_ISO3_v1.1.csv/AR6_Scenarios_Database_ISO3_v1.1.csv")
# df_meta <- read.csv("rawdata/scenario_groupings.csv")
# df1<-inner_join(df1_a,df_meta,by=c("Model","Scenario"))
df1 <- read.csv("rawdata/AR6_data.csv")

# df1 <- df1 %>% 
#   filter(Variable %in% c("GDP|PPP"))

df1 <- df1 %>%
  mutate(Policy_category = substr(Policy_category, 1, 2))

df1 <- df1 %>% group_by(
    Region, Category, Policy_category 
) %>% summarise(
    X2030 = median(X2030),    
    X2040 = median(X2040),
    X2050 = median(X2050)
) %>% ungroup()

df1 <- df1 %>% filter(
    Category %in% c("C2", "C3", "C4", "C6", "C7")
)


df1 <- df1 %>% filter(
    Policy_category != "P0"
)

df_max_c6c7 <- df1 %>%
  filter(Category %in% c("C6", "C7")) %>%
  group_by(Region, Category) %>%
  slice_max(X2050, with_ties = FALSE) %>%
  ungroup()

df1 <- df1 %>%
  filter(!Category %in% c("C6", "C7"))

# Combine back
df1 <- bind_rows(df1, df_max_c6c7)

df1 <- df1 %>% arrange(desc(Region))

df1 <- df1 %>% 
    group_by(Region) %>% 
    mutate(Max_X2050_flag = if_else(X2050 == max(X2050, na.rm = TRUE), 1, 0)) %>%
    ungroup()

# Identify columns that start with X
x_cols <- grep("^X", names(df1), value = TRUE)

df1_logdiff <- df1 %>%
  group_by(Region) %>%
  # Create a reference row for Max_X2050_flag == 1
  mutate(across(all_of(x_cols), 
                .fns = ~ log(. / .[Max_X2050_flag == 1][1]), 
                .names = "logdiff_{.col}")) %>%
  ungroup()

df1_logdiff <- df1_logdiff %>% select(
    Region,
    Category,
    Policy_category,
    Max_X2050_flag,
    logdiff_X2030,
    logdiff_X2040,
    logdiff_X2050
) %>% pivot_longer(
    cols=starts_with("log"),
    names_to = "year",
    values_to = "pc_change"
)

df1_logdiff <- df1_logdiff %>% filter(
    year=="logdiff_X2050"
) %>% select(
    Region,
    T = Category,
    P = Policy_category,
    Flag = Max_X2050_flag,
    IAM_Year = year,
    GDP = pc_change
)

### FROM HERE


df1 <- read.csv("rawdata/economic.csv", header=TRUE)

y <- 1
list_of_dataframes <- c()
list_of_frames_year_losses <- c()

df1 <- as.data.frame(df1 %>% group_by(CountryName) %>%
	mutate(S_RealGDPgrowth = Delt(S_GDPpercapitaUS)))
df1 <- as.data.frame(df1 %>% dplyr::select(
	CountryName,
	Year,
	scale20,
	S_GDPpercapitaUS,
	S_RealGDPgrowth,
	S_NetGGdebtGDP,
	S_GGbalanceGDP,
	S_NarrownetextdebtCARs,
	S_CurrentaccountbalanceGDP, 
	))
df1$ISO2 <- parse_country(df1$CountryName, to="iso2c")

baseline <- as.data.frame(df1 %>%
	dplyr::select(
		CountryName,
		ISO2,
		Year,
		scale20,
		S_GDPpercapitaUS,
		S_RealGDPgrowth,
		S_NetGGdebtGDP,
		S_GGbalanceGDP,
		S_NarrownetextdebtCARs,
		S_CurrentaccountbalanceGDP) %>%
	dplyr::mutate(
		ln_S_GDPpercapitaUS = log(S_GDPpercapitaUS)) %>%
	dplyr::group_by(Year) %>%
	dplyr::filter(Year > 2014))
baseline <- baseline[complete.cases(baseline),]


df1_logdiff <- df1_logdiff %>%
  group_by(Region) %>%
  filter(!all(GDP == 0)) %>%
  ungroup()

df1_logdiff$ISO2 <- countrycode(df1_logdiff$Region, "iso3c", "iso2c")
df1 <- inner_join(
    df1_logdiff,
    baseline,
    by=c("ISO2")
)

fitted_gpi_values <- function(model, loss_vector){
    vector <- c()
    vector <- equa(loss_vector,
    model[['coefficients']][1],
    model[['coefficients']][2],
    model[['coefficients']][3],
    model[['coefficients']][4])
    vector <- exp(vector)
    return (vector)
}

df1$nggd <- fitted_gpi_values(fit_NGGD, df1$GDP)
df1$ggb <- fitted_gpi_values(fit_GGB, df1$GDP)
df1$nned <- fitted_gpi_values(fit_NNED, df1$GDP)
df1$cab <- fitted_gpi_values(fit_CAB, df1$GDP)

df1$ggb <- df1$ggb * -1
df1$cab <- df1$cab * -1

df1$S_NetGGdebtGDP <- df1$S_NetGGdebtGDP + df1$nggd
df1$S_GGbalanceGDP <- df1$S_GGbalanceGDP + df1$ggb
df1$S_NarrownetextdebtCARs <- df1$S_NarrownetextdebtCARs + df1$nned
df1$S_CurrentaccountbalanceGDP <- df1$S_CurrentaccountbalanceGDP + df1$cab

df1$S_GDPpercapitaUS <- df1$S_GDPpercapitaUS * (1+df1$GDP)

baseline <- df1 %>% filter(Flag==1)
alt <- df1 %>% filter(Flag!=1)

set.seed(77)
model.forest <- ranger(scale20 ~
	ln_S_GDPpercapitaUS +
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






produce_adjusted_ratings <- function(model, df){
	pred <- predict(model, df, type="se")
	est <- pred$predictions
	se <- pred$se
	actual <- df$scale20
	T <- pred$predictions / se
	P = exp(-0.717*T -0.416*(T^2))
	n = length(df$ISO2)
    temp <- df$T
    pol <- df$P
	DF = n - 3
	crit = tinv(.05, DF)
	est_lower = est + crit*se
	est_upper = est - crit*se
	country <- df$CountryName
	ISO2 <- df$ISO2
    Y <- df$Year
	m1 <- cbind(country, ISO2, Y, temp, pol, actual, est, est_lower, est_upper)
	m1 <- do.call(rbind, Map(data.frame, country=country,
		ISO2 = ISO2,
        Y = Y,
        temp = temp,
        pol = pol,
		actual=actual,
		est=est,
		est_lower=est_lower,
		est_upper=est_upper
		))

	return (m1)
}

x <- produce_adjusted_ratings(model.forest, alt)

x <- x %>% 
    group_by(country, temp, pol) %>%
    summarise(
        actual = mean(actual),
        est = mean(est)
        )

y <- produce_adjusted_ratings(model.forest, baseline)

y <- y %>% 
    group_by(country) %>%
    summarise(
        os = mean(est)
        )

df <- inner_join(
    x,
    y,
    by=c("country")
)


library(RColorBrewer)

df <- df %>%
  group_by(country) %>%
  mutate(
    os_spread = calculate_spreads(os),
    est_spread = calculate_spreads(est))


# Compute both metrics per country
df_summary <- df %>%
  group_by(country) %>%
  summarise(
    error_a = mean(abs(est_spread - os_spread), na.rm = TRUE),
    error_b = mean(abs(est - os), na.rm = TRUE),
    .groups = "drop"
  )

# Ordering
order_a <- df_summary %>%
  arrange(error_a) %>%
  pull(country)

df_summary <- df_summary %>%
  mutate(country = factor(country, levels = order_a))

# Plot
a <- ggplot(df_summary,
  aes(
    x = country,
    y = error_a,
    fill = error_b
  )
) +
  geom_col() +
  # 👇 labels at end of bars
  geom_text(
    aes(
      label = sprintf("%.1f", error_b)
    ),
    hjust = -0.1,   # 👈 move inside the bar
    color = "black",  # 👈 improve contrast
    size = 4
  ) +
  coord_flip() +
  scale_fill_gradientn(
    colours = rev(colorRampPalette(brewer.pal(11, "RdYlBu"))(100))
  ) +
  labs(
    x = "",
    y = "Transition Risk Spread (%)"
  ) +
  theme_classic(base_size = TEXT) +
  theme(
    legend.position = "none"   # 👈 remove legend
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

# Save
ggsave("plots/figure8.png", dpi=300, width=12, height=10, units="in")


