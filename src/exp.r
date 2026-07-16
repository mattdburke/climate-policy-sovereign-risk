x <- read.csv("rawdata/economic.csv")

x <- x[complete.cases(x),]

filtered_data <- x %>%
  group_by(CountryName) %>%  # Group by country
  mutate(abs_diff = max(scale20)-min(scale20)) %>%
  filter(abs_diff >1) %>%
  ungroup()

avg_gdp_growth <- filtered_data %>%
  group_by(CountryName) %>%  # Group by country
  summarize(Average_GDP_Growth = mean(S_RealGDPgrowth, na.rm = TRUE))  # Compute mean, ignoring NAs


avg_gdp_growth %>% as.data.frame() %>% arrange(desc(Average_GDP_Growth))

# 108 out of 133 countries only change in their rating by one 
# notch in the sample estimation period. Out of those countries,
# Some grow by as much as 8% on average, i.e. China.
# With unequal average growth rates, it means