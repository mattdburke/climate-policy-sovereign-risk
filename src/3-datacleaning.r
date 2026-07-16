sp_data <- read.csv("rawdata/T3.csv", header=TRUE)

# This small section of straight code defines the relationships
# between the various government performance variables and GDP losses.
# Using data from S&P to do accomplish it.
equa <- function(A, x1,x2,x3,x4){
	x1 + A*x2 + (A^2)*x3 + (A^3)*x4
	}
gdp_losses <- sp_data$GDP_per_capita/100
NGGD <- log(sp_data$NGGD)
fit_NGGD <- lm(NGGD ~ poly(gdp_losses, 3, raw=TRUE))
GGB <- sp_data$GGB
sub_data <- data.frame(gdp_losses, GGB)
sub_data <- dplyr::filter(sub_data, GGB<0)
GGB <- sub_data$GGB
gdp_losses_GGB <- sub_data$gdp_losses
GGB <- log((GGB)*-1)
fit_GGB <- lm(GGB ~ poly(gdp_losses_GGB, 3, raw=TRUE))
NNED <- sp_data$NNED
sub_data <- data.frame(gdp_losses, NNED)
sub_data <- dplyr::filter(sub_data, NNED>0)
NNED <- sub_data$NNED
gdp_losses_NNED <- sub_data$gdp_losses
NNED <- log(NNED)
fit_NNED <- lm(NNED ~ poly(gdp_losses_NNED, 3, raw=TRUE))
CAB <- log(sp_data$CAB*-1)
fit_CAB <- lm(CAB ~ poly(gdp_losses, 3, raw=TRUE))

clim_data_management <- function(clim_df1){
	# Sort all years, placeholder x is positioned to produce string
	years <- paste0("X", c(
        2045,
        2046,
        2047,
        2048,
        2049,
        2050
        ))
	years <- c(years, "Country")
	# replace iso3 with iso2
    clim_df1 <- dplyr::filter(
        clim_df1,
        )

    clim_df1 <- clim_df1 %>% mutate(
        nchar_Region = nchar(Region)
    ) %>% dplyr::filter(
        nchar_Region==3
    ) %>% dplyr::select(-c(
        nchar_Region
    ))

	clim_df1$Country <- countrycode(clim_df1$Region, origin = "iso3c", destination = "iso2c")
	# remove original iso and country column
	clim_df1 <- dplyr::select(clim_df1, !(c(Region)))
	# replace column names with years rather than n period
    colnames(clim_df1) <- years
	clim_df1 <- clim_df1 %>% gather(Year, GDPpercapita, X2045, X2046, X2047, X2048, X2049, X2050)
	Year <- clim_df1$Year
	Year <- as.vector(Year)
	Year <- sub('.', '', Year)
	clim_df1$Year <- Year
	clim_df1_wide <- (tidyr::spread(clim_df1, Year, GDPpercapita))
	colnames(clim_df1_wide)[colnames(clim_df1_wide)
		== "Country"] <- "CountryName"
	return (clim_df1_wide)
}

addLossSeries<-function(dataframe, climateframe, years_added, RCP){
	# This function looks at the year we want for the simulation. i.e.
	# 2030. It then, looks at the year for the given country in the 
	# economic dataframe and adds the scenario year onto this number. 
	# For example, Albania in 2009. The function will then look up 
	# 2009 + 10 years = 2019 in the climate data and retrieve the
	# corresponding loss. The same would be true for the final year
	# in the economic data (2020), retrieving the 80 year loss would 
	# correspond to 2100.
	test <- inner_join(dataframe, climateframe, by=c("ISO2"="CountryName"))
	test <- as.data.frame(test)
	test$Year <- as.vector(test$Year)
	test$Year_F <- as.numeric(test$Year) + years_added
	test$Year_F <- as.character(test$Year_F)
	RCP <- as.character(RCP)
	test <- data.table(test)
	# Key look up function. The rest is restructuring the data and then 
	# putting it back together again. This could probably be sped up.
	# Reminder to raise a pull request.
	test <- (test[,test[,Climate_F_GDP := .SD[[.BY[[1]]]], by=Year_F], by=CountryName])
	test$CountryName <- NULL
	test <- unique(test)
	test <- as.data.frame(test %>% dplyr::select(
		CountryName, 
		ISO2,
		Year,
		Climate_F_GDP))
	new_col <- paste(as.character(years_added), "_", RCP, sep="")
	colnames(test) <- c("CountryName", "ISO2", "Year", new_col)
	return(test)
}

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

produce_estimation_files <- function(frame, list_of_gpi_models){
    t_df1 <- inner_join(df1, get(frame), by = c("CountryName", "ISO2", "Year"))
    loss_series_vector <- t_df1[, ncol(t_df1)]

    storage_vector_nggd <- fitted_gpi_values(fit_NGGD, loss_series_vector)
    storage_vector_ggb <- fitted_gpi_values(fit_GGB, loss_series_vector)
    storage_vector_nned <- fitted_gpi_values(fit_NNED, loss_series_vector)
    storage_vector_cab <- fitted_gpi_values(fit_CAB, loss_series_vector)

    storage_vector_ggb <- storage_vector_ggb*-1
    storage_vector_cab <- storage_vector_cab*-1
    
    t_df1 <- t_df1 %>% dplyr::mutate(
        S_NetGGdebtGDP = S_NetGGdebtGDP + storage_vector_nggd,
        S_GGbalanceGDP = S_GGbalanceGDP + storage_vector_ggb,
        S_NarrownetextdebtCARs = S_NarrownetextdebtCARs + storage_vector_nned,
        S_CurrentaccountbalanceGDP = S_CurrentaccountbalanceGDP + storage_vector_cab,
        S_RealGDPgrowth = S_RealGDPgrowth*(1+(t_df1[,ncol(t_df1)]/10)),
        # We used to calculate the growth rate based on the difference in the loss between 
        # the counterfactual and the scenario. In this setting, perhaps slightly more realistic
        # we reduce the historical growth rate by the same amount as we reduce the GDP level.
        # Economies will still grow, but by this much less on this given year.
        S_GDPpercapitaUS = S_GDPpercapitaUS * (1 + t_df1[,ncol(t_df1)]),
        ln_S_GDPpercapitaUS = log(S_GDPpercapitaUS * (1 + t_df1[,ncol(t_df1)]))
    ) %>% dplyr::select(
        # removed an ungroup arguument here from legacy code, because of the way we now
        # reconstruct the growth rate
        CountryName = CountryName,
        Year = Year,
        scale20 = scale20,
        ln_S_GDPpercapitaUS = ln_S_GDPpercapitaUS,
        S_RealGDPgrowth = S_RealGDPgrowth,
        S_GGbalanceGDP = S_GGbalanceGDP,
        S_NetGGdebtGDP = S_NetGGdebtGDP,
        S_CurrentaccountbalanceGDP = S_CurrentaccountbalanceGDP,
        S_NarrownetextdebtCARs = S_NarrownetextdebtCARs,
        ISO2 = ISO2
    ) %>% dplyr::filter(
        Year > 2014 & Year < 2020
    )
    t_df1 <- t_df1[complete.cases(t_df1),]
    return (t_df1)
}


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
	dplyr::filter(Year > 2014 & Year < 2020))
baseline <- baseline[complete.cases(baseline),]

for (i in dir("scenario_data/")){
        name <- substr(i,1,nchar(i)-4)
        assign(name, read.csv(paste0("scenario_data/", i)))
        list_of_dataframes[y] <- name
        y <- y + 1
}
y <- 1

for (i in list_of_dataframes){
    assign(i, clim_data_management(get(i)))
}


for (j in list_of_dataframes){
    for (i in c(30)){
        assign(
            paste0(j, "_", as.character(i)), addLossSeries(df1, get(j), i, j)
        )
        list_of_frames_year_losses[y] <- paste0(j, "_", as.character(i))
        y <- y + 1
    }
}


for (i in list_of_frames_year_losses){
    write.csv(
        produce_estimation_files(i),
        file = paste0("cleandata/", i , ".csv")
    )
}
write.csv(baseline, file = "cleandata/baseline_data_clean.csv")



