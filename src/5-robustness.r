
pcd <- read.csv("rawdata/KAM_RCP85_spain.csv")

pcd <- pcd %>%
  rowwise() %>%
  mutate(upper_quartile = (quantile(c_across(starts_with("X")), 0.75, na.rm = TRUE))*-1) %>%
  ungroup() %>%
  select(iso, country, upper_quartile)


df1 <- read.csv("output/all_scenarios_ratings_results.csv")

scenarios <- unique(df1$scenario_)
scenarios[4] <- "National"

slm_estimate <- c()
slm_t_value <- c()
robust_estimate <- c()
robust_t_value <- c()
slm_model_fits <- c()
robust_model_fits <- c()

df2 <- read.csv("rawdata/per-capita-ghg-emissions.csv")
df3 <- read.csv("rawdata/total-ghg-emissions.csv")
df4 <- read.csv("rawdata/carbon-intensity-electricity.csv")
df2 <- inner_join(df2, df3, by=c("Entity", "Year", "Code"))
df2 <- inner_join(df2, df4, by=c("Entity", "Year", "Code"))
df2 <- df2 %>%
  dplyr::filter(Year > 2020) %>%
  dplyr::group_by(Entity, Code) %>%
  dplyr::summarize(
    log_emission_pc = log(median(Per.capita.greenhouse.gas.emissions.in.CO..equivalents, na.rm = TRUE)),
    log_emissions_total = log(median(Annual.greenhouse.gas.emissions.in.CO..equivalents, na.rm = TRUE)),
    log_emissions_intensity = log(median(Carbon.intensity.of.electricity...gCO2.kWh, na.rm = TRUE)),
    .groups = "drop"
  )
df2 <- inner_join(df2, pcd, by=c("Code"="iso"))


# initialize storage
slm_estimate <- slm_t_value <- robust_estimate <- robust_t_value <- slm_model_fits <- robust_model_fits <- rep(NA, length(scenarios))
slm_estimate_m2 <- slm_t_value_m2 <- robust_estimate_m2 <- robust_t_value_m2 <- slm_model_fits_m2 <- rep(NA, length(scenarios))
slm_estimate_m2_pcd <- slm_t_value_m2_pcd <- robust_estimate_m2_pcd <- robust_t_value_m2_pcd  <- rep(NA, length(scenarios))

for (i in 1:length(scenarios)){
    
    temp <- df1 %>% 
        dplyr::filter(
            str_detect(scenario_, scenarios[i]) &
            str_detect(year, "30")
        ) %>% 
        group_by(country) %>% 
        summarise(
            rating_delta = median(rating_delta),
            actual = median(actual),
            est = mean(est)
        )
    
    df_estimate <- inner_join(temp, df2, by = c("country" = "Entity"))
    
    dt_model1 <- lm(rating_delta ~ scale(log_emissions_total), data = df_estimate)
    dt_robust_model1 <- coeftest(dt_model1, vcov = vcovHC(dt_model1, type = "HC0"))
    sum_model1 <- summary(dt_model1)
    
    slm_estimate[i] <- sum_model1$coefficients[2]
    slm_t_value[i] <- sum_model1$coefficients[6]
    robust_estimate[i] <- dt_robust_model1[2]
    robust_t_value[i] <- dt_robust_model1[6]
    slm_model_fits[i] <- sum_model1$adj.r.squared
    robust_model_fits[i] <- sum_model1$adj.r.squared
    
    dt_model2 <- lm(
        rating_delta ~ scale(log_emissions_total) + scale(upper_quartile),
        data = df_estimate
    )
    
    dt_robust_model2 <- coeftest(dt_model2, vcov = vcovHC(dt_model2, type = "HC0"))
    sum_model2 <- summary(dt_model2)
    
    slm_estimate_m2[i] <- sum_model2$coefficients[2]
    slm_t_value_m2[i] <- sum_model2$coefficients[8]
    robust_estimate_m2[i] <- dt_robust_model2[2]
    robust_t_value_m2[i] <- dt_robust_model2[8]
    
    slm_estimate_m2_pcd[i] <- sum_model2$coefficients[3]
    slm_t_value_m2_pcd[i] <- sum_model2$coefficients[9]
    robust_estimate_m2_pcd[i] <- dt_robust_model2[3]
    robust_t_value_m2_pcd[i] <- dt_robust_model2[9]

    slm_model_fits_m2[i] <- sum_model2$adj.r.squared
}

results <- data.frame(
    scenario = scenarios,
    
    # Model 1
    slm_estimate = slm_estimate,
    slm_t_value = slm_t_value,
    robust_estimate = robust_estimate,
    robust_t_value = robust_t_value,
    slm_model_fits = slm_model_fits,
    
    # Model 2
    slm_estimate_m2 = slm_estimate_m2,
    slm_t_value_m2 = slm_t_value_m2,
    robust_estimate_m2 = robust_estimate_m2,
    robust_t_value_m2 = robust_t_value_m2,

    slm_estimate_m2_pcd = slm_estimate_m2_pcd,
    slm_t_value_m2_pcd = slm_t_value_m2_pcd,
    robust_estimate_m2_pcd = robust_estimate_m2_pcd,
    robust_t_value_m2_pcd = robust_t_value_m2_pcd,

    slm_model_fits_m2 = slm_model_fits_m2
)


pretty_n <- function(n){
    return (format(round(as.numeric(n), 2), nsmall = 2))
}


sink("tables/table5.tex", append=FALSE, split=FALSE)
cat("
\\begin{table}[tb!]
\\footnotesize
\\center
\\caption{Regression of Simulated Downgrades on Emissions}
\\label{tab:tab4}
\\begin{tabularx}{\\textwidth}{p{3cm} X X X X X}
\\\\
\\hline
 & Below 2$^\\circ$C & Delayed Transition & Fragmented World & NDCs & Net Zero 2050 \\\\
\\hline
\\multicolumn{6}{l}{Panel A: Emissions only}\\\\
\\hline
Emissions  & ",pretty_n(results[1,2])," & ",pretty_n(results[2,2])," & ",pretty_n(results[3,2])," & ",pretty_n(results[4,2])," & ",pretty_n(results[5,2]),"\\\\
 & (",pretty_n(results[1,5]),") & (",pretty_n(results[2,5]),") & (",pretty_n(results[3,5]),") & (",pretty_n(results[4,5]),") & (",pretty_n(results[5,5]),")\\\\[0.1cm]
$R^{2}$  & ",pretty_n(results[1,6])," & ",pretty_n(results[2,6])," & ",pretty_n(results[3,6])," & ",pretty_n(results[4,6])," & ",pretty_n(results[5,6]),"\\\\[0.1cm]
\\hline
\\multicolumn{6}{l}{Panel B: Emissions and Projected Damages}\\\\
\\hline
Emissions  & ",pretty_n(results[1,7])," & ",pretty_n(results[2,7])," & ",pretty_n(results[3,7])," & ",pretty_n(results[4,7])," & ",pretty_n(results[5,7]),"\\\\
 & (",pretty_n(results[1,10]),") & (",pretty_n(results[2,10]),") & (",pretty_n(results[3,10]),") & (",pretty_n(results[4,10]),") & (",pretty_n(results[5,10]),")\\\\[0.1cm]
Projected Damages  & ",pretty_n(results[1,12])," & ",pretty_n(results[2,12])," & ",pretty_n(results[3,12])," & ",pretty_n(results[4,12])," & ",pretty_n(results[5,12]),"\\\\
  & (",pretty_n(results[1,14]),") & (",pretty_n(results[2,14]),") & (",pretty_n(results[3,14]),") & (",pretty_n(results[4,14]),") & (",pretty_n(results[5,14]),")\\\\[0.1cm]
$R^{2}$  & ",pretty_n(results[1,15])," & ",pretty_n(results[2,15])," & ",pretty_n(results[3,15])," & ",pretty_n(results[4,15])," & ",pretty_n(results[5,15]),"\\\\[0.1cm]
\\hline
\\multicolumn{6}{p{\\textwidth}}{\\begin{footnotesize}Notes: This table shows the results of our regression of simulated downgrades on standardized log total emissions ($\\Delta Rating_{i} = \\beta lnEmissions_{i} + \\mu_{i}$) for 2020. We do this for each of the five scenarios. Column 2 reveals the $\\beta$ estimate for the regression, Columns 3 and 4 show the t-value and robust t-value respectively, and Column 5 shows the model fit.
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")

sink()


