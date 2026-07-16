c1 <- wes_palette("Zissou1", 5, type="discrete")[1]
c2 <- wes_palette("Zissou1", 5, type="discrete")[2]
c3 <- wes_palette("Zissou1", 5, type="discrete")[3]
c4 <- wes_palette("Zissou1", 5, type="discrete")[4]
c5 <- wes_palette("Zissou1", 5, type="discrete")[5]
ALPHA <- 0.9
TEXT <- 20

# Step 1: Reverse the rating_dict
rating_dict <- c(
  CC = 1, `CCC-` = 2, CCC = 3, `CCC+` = 4,
  `B-` = 5, B = 6, `B+` = 7,
  `BB-` = 8, BB = 9, `BB+` = 10,
  `BBB-` = 11, BBB = 12, `BBB+` = 13,
  `A-` = 14, A = 15, `A+` = 16,
  `AA-` = 17, AA = 18, `AA+` = 19,
  AAA = 20
)

# Reverse the dict for labels by number
rating_labels <- names(rating_dict)
names(rating_labels) <- rating_dict


baseline <- read.csv("cleandata/baseline_data_clean.csv", header=TRUE)
set.seed(77)
model.forest <- ranger(scale20 ~
	ln_S_GDPpercapitaUS +
	S_RealGDPgrowth +
	S_NetGGdebtGDP +
	S_GGbalanceGDP +
	S_NarrownetextdebtCARs +
	S_CurrentaccountbalanceGDP,
	data=baseline,
	num.trees=2000,
	importance='permutation',
	write.forest = TRUE,
	keep.inbag=TRUE)
fig <- data.frame(
    variables = names(model.forest$variable.importance),
    values = as.vector(model.forest$variable.importance)
)

custom_names <- c(
  "ln_S_GDPpercapitaUS" = "Log GDP per capita",
  "S_RealGDPgrowth"           = "Real GDP Growth",
  "S_NetGGdebtGDP"         = "Net GG Debt to GDP",
  "S_GGbalanceGDP"         = "GG Balance to GDP",
  "S_NarrownetextdebtCARs"       = "Narrow Net Ext. Debt to CARs",
  "S_CurrentaccountbalanceGDP"        = "CAB to GDP"
)

fig$variable <- custom_names[fig$variable]
fig <- fig %>% arrange(values) %>% mutate(variable = factor(variable, levels = variable))
fig <- fig %>%
  dplyr::mutate(variable = reorder(variable, values))
fig <- ggplot(fig, aes(x = variable, y = sqrt(values))) +
  geom_col(
    # aes(label = round(sqrt(values), 2)),
    fill = c1, 
    alpha = ALPHA,
    width = 0.7
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Increase in RMSE (Rating Notches)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = TEXT)
  )

ggsave(
  "plots/figure1.png",
  plot = fig,
  width = 11,
  height = 7,
  units = "in",
  dpi = 300   # better export quality
)






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
# Figure 1
baseline <- read.csv("cleandata/baseline_data_clean.csv", header=TRUE)
set.seed(5)
sample = sample.split(baseline$CountryName, SplitRatio = .8)
train = subset(baseline, sample == TRUE)
test  = subset(baseline, sample == FALSE)
set.seed(77)
model.forest <- ranger(scale20 ~
	ln_S_GDPpercapitaUS +
	S_RealGDPgrowth +
	S_NetGGdebtGDP +
	S_GGbalanceGDP +
	S_NarrownetextdebtCARs +
	S_CurrentaccountbalanceGDP,
	data=train,
	num.trees=2000,
	importance='permutation',
	write.forest = TRUE,
	keep.inbag=TRUE)
pred <- predict(model.forest, test, type="se")
est <- pred$predictions
se <- pred$se
actual <- test$scale20
T <- pred$predictions / se
P = exp(-0.717*T -0.416*(T^2))
n = length(test$CountryName)
DF = n - 3
crit = tinv(.05, DF)
est_lower = est + crit*se
est_upper = est - crit*se
country <- test$CountryName
ISO2 <- test$ISO2
m1 <- cbind(country, ISO2, actual, est, est_lower, est_upper)
m1 <- do.call(rbind, Map(data.frame, country=country,
	ISO2=ISO2,
	actual=actual,
	est=est,
	est_lower=est_lower,
	est_upper=est_upper
	))
m1$est_l <- m1$est - m1$est_lower
m1$est_u <- m1$est_upper - m1$est
m1.sum <- as.data.frame(m1 %>% group_by(country) %>%
	summarise(ISO2 = ISO2,
			actual = mean(actual),
			est = mean(est),
			est_l = mean(est_l),
			est_u = mean(est_u),
			est_lower=mean(est_lower),
			est_upper=mean(est_upper)))
m1.sum$notch <- round(abs(m1.sum$est - m1.sum$actual))
write.csv(m1.sum, "output/country_level_accuracy.csv")
countries <- unique(read.csv("output/all_scenarios_ratings_results.csv")$country)
# df <- read.csv("output/all_scenarios_ratings_results.csv")
fig <- m1.sum %>% 
    dplyr::filter(country %in% countries) %>%
    rowwise() %>% 
    mutate( mymean = mean(c(actual, est) )) %>% 
    arrange(mymean) %>% 
    mutate(country=factor(country, country))
ggplot(fig, aes(country)) +
    geom_segment(aes(x=country, xend=country, y=1, yend=est_upper), color="grey", lineend="round") +
    geom_errorbar(aes(ymin = est_lower, ymax = est_upper), width = 0.3) +
    geom_point( aes(x=country, y=actual, color=factor(2)), size=3 ) +
    geom_point( aes(x=country, y=est, color=factor(3)), size=3 ) +
    # coord_cartesian(ylim = c(1, 20)) +
    coord_flip() +
    scale_y_continuous(
    limits = c(1, max(fig$est_upper)),
    breaks = rating_dict,
    labels = rating_labels    # corresponding labels
  ) +
    theme_classic() +
    theme(
        legend.position = "inside",         
        legend.position.inside = c(0.85,0.2),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(angle=90,vjust=0.5,hjust=1,color="#989898")
    ) +
    scale_color_manual(
        name = "Rating",
        values=c(c4, c1),
        labels = c("Actual","Simulated")
    ) +
    # coord_cartesian(ylim = c(1, 20)) +
    theme(text=element_text(size=TEXT)) +
    xlab("") +
    geom_hline(yintercept = 11, linetype = "dashed") +

    # scale_y_continuous(limits = c(1, 20)) +
    ylab("Credit Rating (20-point scale)")
ggsave("plots/figure2.png", dpi=300, width=12, height=10, units="in")





# Figure 2
PD_data <- read.csv("rawdata/10_year_default_rate.csv", header = FALSE)
ggplot(PD_data, aes(x=V2, y=V3))+
geom_point(size=2, shape=23) +
theme_classic() +
theme(legend.position="none")+
stat_smooth(
    method="lm", 
    se=FALSE, 
    formula=y ~ poly(x,5), 
    colour=c1) +
labs(
    y = "Probability of Default (%)", 
    x="20 point numerical rating (20=AAA)") + 
theme(
    # legend.position = "none",
    text = element_text(size = TEXT)
  )
ggsave("plots/figure3.png",dpi=300,width=12, height=8, units="in")







df2 <- read.csv("output/all_scenarios_gdp_results.csv")
df2 <- dplyr::select(
    df2,
    c(
        Model,
        Scenario,
        Region,
        X2050
    )
)
df2 <- df2[complete.cases(df2),]
fig <- df2 %>% dplyr::mutate(
        country = countrycode(
            Region,
            origin = "iso3c",
            destination = "country.name"
        )
    ) %>%
    dplyr::mutate(
        country = if_else(country == "Hong Kong SAR China", "Hong Kong", country)
    ) %>% group_by(
        Scenario,
        country
    ) %>% summarize(
        gdp = mean(X2050) * 100,
        .groups = "drop"
    )
fig$Scenario <- factor(fig$Scenario, 
    levels=c(
        'Net Zero 2050', 
        'Below 2', 
        'Nationally Determined Contributions (NDCs)', 
        'Delayed transition', 
        'Fragmented World'))
fig$country = with(fig, reorder(country, gdp, mean))
    fig %>% 
        ggplot(aes(x=gdp,y=country)) +
        geom_line(aes(group=country), color="#E7E7E7", linewidth=3.5) + 
        geom_point(aes(color=Scenario), size=3) +
        theme_classic() +
        theme(
            legend.position = "inside", 
            legend.position.inside = c(0.88,0.10),
            axis.text.y = element_text(color="black"),
            axis.text.x = element_text(color="#989898")
        ) +
        scale_color_manual(
            name = "Scenario",
            labels = c("Net Zero 2050","Below 2", "NDCs", "Delayed Transition", "Fragmented World"),
            values=c(c1, c2, c3, c4, c5),
        ) +
        ylab("") +
        theme(text=element_text(size=TEXT)) +
        geom_vline(xintercept = 0, linetype="dashed") +
        xlab("GDP compared to baseline (%)")
ggsave("plots/figure4.png", dpi=300, width=12, height=10, units="in")



# Figure 4
df1 <- read.csv("output/all_scenarios_ratings_results.csv")
temp <- df1 %>% dplyr::filter(
    year==30
    ) %>% group_by(country, scenario_) %>%
    mutate(
        # cd_delta = median(cd_delta),
        # pd_delta = median(pd_delta),
        rating_delta = median(rating_delta)
    ) %>% dplyr::select(
        country,
        scenario_,
        # pd_delta,
        # cd_delta,
        rating_delta
    )
a <- ggplot(temp, aes(rating_delta, fill = scenario_)) +
    geom_boxplot(alpha=0.8) + 
    scale_fill_manual(
        breaks = c("Net Zero 2050", "Below 2", "Nationally Determined Contributions (NDCs)", "Delayed transition", "Fragmented World"),
        values = c(c1, c2, c3,c4,c5)
        ) + 
        xlim(-10,1.5) +
        labs(x = "",
        y = "", fill = "Scenario") +
        theme_classic() +
        theme(text=element_text(size=TEXT),
        legend.position = "none") +
        theme(axis.title.x = element_blank(),
               axis.text.x  = element_blank(),
               axis.ticks.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y  = element_blank(),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank())
b <- ggplot(temp, aes(rating_delta, fill = scenario_)) +
    geom_density(alpha=0.8) + 
    scale_fill_manual(
        breaks = c("Net Zero 2050", "Below 2", "Nationally Determined Contributions (NDCs)", "Delayed transition", "Fragmented World"),
        labels = c("Net Zero 2050", "Below 2", "NDCs", "Delayed transition", "Fragmented World"),
        values = c(c1, c2, c3,c4,c5)
        ) + 
        xlim(-10,1.5) +
        labs(x = "Sovereign credit downgrade (20-notch scale)",
        y = "", fill = "Scenario") +
        theme_classic() +
        theme(text=element_text(size=TEXT))
(a / b) +
  plot_layout(guides = "collect") +
  theme(legend.position = "right")
ggsave("plots/figure5.png", dpi=300, width=12, height=8, units="in")






































































# Figure 5
df1 <- read.csv("output/all_scenarios_ratings_results.csv")
df1 <- dplyr::filter(
    df1,
    year==30
)

# x_1 is a frame to get the current rating into the right format to be appended to a long dataframe later.
x_1 <- df1 %>%
    select(
        country,
        est = actual,
    ) %>% group_by(country) %>%
    mutate(
        scenario_ = "Actual"
    ) %>% distinct()

# x_2 gives the whiskers for the prediction in the plot
x_2 <-  produce_adjusted_ratings(model.forest, baseline)

x_2 <- x_2 %>% select(
    country, est_lower, est_upper
)

fig <- df1 %>% 
    select(
        country,
        est,
        model,
        scenario_
    ) %>% group_by(
        country, scenario_
    ) %>% summarise(
        est = mean(est),
        .groups = "drop"
    ) %>% select(
        country,est,scenario_
    )

fig <- rbind(fig,x_1)
fig <- inner_join(
    fig,
    x_2,
    by=c("country")
)

df_fw_means <- fig %>%
    group_by(country) %>%
    summarize(mean_fw = mean(est))          
fig$scenario_ <- factor(fig$scenario_, 
    levels=c(
        'Actual',
        'Net Zero 2050', 
        'Below 2', 
        'Nationally Determined Contributions (NDCs)', 
        'Delayed transition', 
        'Fragmented World'))
    fig <- fig %>%
    mutate(country = factor(country, levels = df_fw_means$country[order(df_fw_means$mean_fw)]))
    fig %>% 
        ggplot(aes(x=est,y=country)) +
        geom_line(aes(group=country), color="#E7E7E7", linewidth=3.5) + 
        geom_point(aes(color=scenario_), size=3) +
        geom_errorbar(aes(xmin = est_lower, xmax = est_upper), width = 0.3) +
            scale_x_continuous(
    limits = c(1, max(fig$est_upper)),
    breaks = rating_dict,
    labels = rating_labels    # corresponding labels
  ) +
        theme_classic() +
        theme(
            legend.position = "inside", 
            legend.position.inside = c(0.85,0.2),
            axis.text.y = element_text(color="black"),
            axis.text.x = element_text(angle=90,vjust=0.5,hjust=1,color="#989898")
            ) +
        scale_color_manual(
            name = "Scenario",
            labels = c("Actual", "Net Zero 2050","Below 2", "NDCs", "Delayed Transition", "Fragmented World"),
            values=c("grey", c1, c2, c3, c4, c5)) +
        ylab("") +
        geom_vline(xintercept = 11, linetype = "dashed") +
        theme(text=element_text(size=TEXT)) +
        xlab("Credit Rating (20-point scale)")
ggsave("plots/figure6.png", dpi=300, width=12, height=10, units="in")





# Figure 5a
df1 <- read.csv("output/all_scenarios_ratings_results.csv")
df1 <- dplyr::filter(
    df1,
    year==30
)

# x_1 is a frame to get the current rating into the right format to be appended to a long dataframe later.
x_1 <- df1 %>%
    select(
        country,
        cd = cd_actual,
    ) %>% 
    mutate(
        scenario_ = "Actual"
    ) %>% distinct()
    


fig <- df1 %>% 
    select(
        country,
        cd_est,
        model,
        scenario_
    ) %>% group_by(
        country, scenario_
    ) %>% summarise(
        cd = median(cd_est),
        .groups = "drop"
    ) %>% select(
        country,cd,scenario_
    )

fig <- rbind(fig,x_1)


df_fw_means <- fig %>%
    group_by(country) %>%
    summarize(mean_fw = mean(cd))          
fig$scenario_ <- factor(fig$scenario_, 
    levels=c(
        'Actual',
        'Net Zero 2050', 
        'Below 2', 
        'Nationally Determined Contributions (NDCs)', 
        'Delayed transition', 
        'Fragmented World'))
    fig <- fig %>%
    mutate(country = factor(country, levels = df_fw_means$country[order(df_fw_means$mean_fw)]))
    fig %>% 
        ggplot(aes(x=cd,y=country)) +
        geom_line(aes(group=country), color="#E7E7E7", linewidth=3.5) + 
        geom_point(aes(color=scenario_), size=3) +
        # geom_errorbar(aes(xmin = est_lower, xmax = est_upper), width = 0.3) +
        theme_classic() +
        theme(
            legend.position = "inside", 
            legend.position.inside = c(0.85,0.2),
            axis.text.y = element_text(color="black"),
            axis.text.x = element_text(color="#989898"),
            ) +
        scale_color_manual(
            name = "Scenario",
            labels = c("Actual", "Net Zero 2050","Below 2", "NDCs", "Delayed Transition", "Fragmented World"),
            values=c("grey", c1, c2, c3, c4, c5)) +
        ylab("") +
        theme(text=element_text(size=TEXT)) +
        xlab("Cost of Borrowing (%)")
ggsave("plots/figure7.png", dpi=300, width=12, height=10, units="in")








# baseline <- read.csv("cleandata/baseline_data_clean.csv", header=TRUE)

# plt1 <- baseline %>% group_by(
#     CountryName
# ) %>% summarise(
#     mean_rat = mean(scale20, na.rm=TRUE),
#     mean_y = mean(S_GDPpercapitaUS, na.rm=TRUE)
# )

# # Load necessary libraries
# library(ggplot2)
# # library(ggrepel)

# # Select a random subset of countries for labeling (e.g., 10% of the data)
# set.seed(123)  # Ensures reproducibility
# labeled_countries <- plt1[sample(nrow(plt1), size = round(0.1 * nrow(plt1))), ]

# # Scatter plot with labels
# ggplot(plt1, aes(x = log(mean_y), y = mean_rat)) +
#   geom_point(color = "blue", size = 3, alpha = 0.7) +  # Blue points
#   geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Trend line+
#   labs(
#     title = "Sovereign Credit Rating vs Log GDP per Capita",
#     x = "Log GDP per Capita",
#     y = "Sovereign Credit Rating (20-notch scale)"
#   ) +
#   theme_minimal(base_size = 14) +  
#   theme(
#     panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(face = "bold", hjust = 0.5)
#   )
# ggsave("C:/Users/mattb/OneDrive/NGFS paper presentation slides/ratingsincome.eps", dpi = 300)





# # Install wbstats if not already installed
# #install.packages("wbstats")

# # Load the package
# library(wbstats)
# library(dplyr)
# library(ggplot2)

# # Define countries and indicator
# countries <- c(
#     "JP",
#     "CN", 
#     "NO",
#     # "IN",
#     "LV"
#     )  # Canada, UK, China, Norway
# indicator <- "NY.GDP.PCAP.PP.KD"        # GDP per capita, PPP (constant 2017 international $)

# # Fetch data
# gdp_ppp_pc_data <- wb_data(
#   indicator = indicator,
#   country = countries,
#   start_date = 2000,
#   end_date = 2023,
#   return_wide = FALSE
# )


# cr <- read.csv("C:/Users/mattb/OneDrive/GitHub/country-ratings/data/credit_ratings_clean.csv")
# # cr <- cr %>% filter(ISO %in% countries)
# # cr <- cr %>% mutate(year = lubridate::year(date))
# # cr <- cr %>% group_by(ISO, year) %>% summarise(
# #     year = first(year),
# #     country = first(country),
# #     iso = first(ISO),
# #     rating = first(FC_rating_num)
# # )
# cr <- cr %>%
#   filter(ISO %in% countries) %>%
#   mutate(year = year(date)) %>%
#   group_by(ISO, year) %>%
#   summarise(
#     country = first(country),
#     iso = first(ISO),
#     rating = first(FC_rating_num),
#     .groups = "drop"
#   )

# # 🔑 Step: create year-2000 baseline rows
# baseline_2000 <- cr %>%
#   filter(year <= 2000) %>%
#   arrange(ISO, desc(year)) %>%
#   group_by(ISO) %>%
#   slice(1) %>%   # most recent pre-2000
#   mutate(year = 2000)

# # 🔗 Combine back
# cr_final <- bind_rows(cr, baseline_2000) %>%
#   arrange(ISO, year)






# gdp_with_labels <- gdp_ppp_pc_data %>%
#   left_join(cr_final %>% select(date = year, iso, rating), 
#             by = c("date", "iso2c"="iso")) %>%
#     select(date, value, rating, country)



# # Step 1: Reverse the rating_dict
# rating_dict <- c(
#   SD = 1, CC = 2, `CCC-` = 3, CCC = 4, `CCC+` = 5,
#   `B-` = 6, B = 7, `B+` = 8,
#   `BB-` = 9, BB = 10, `BB+` = 11,
#   `BBB-` = 12, BBB = 13, `BBB+` = 14,
#   `A-` = 15, A = 16, `A+` = 17,
#   `AA-` = 18, AA = 19, `AA+` = 20,
#   AAA = 21
# )


# rating_labels <- names(rating_dict)[match(gdp_with_labels$rating, rating_dict)]

# # Assign back to the data frame
# gdp_with_labels$rating <- rating_labels








# png("plots/growth_ratings.png", res=300, width=6, height=5,units="in")

# gdp_with_labels <- gdp_with_labels %>%
#   mutate(year = lubridate::year(date))

# gdp_with_labels <- gdp_with_labels %>%
#   mutate(date = as.Date(date))

# x_max <- max(gdp_with_labels$date)

# ggplot(gdp_with_labels, aes(x = date, y = log_value, color = country)) +
#   geom_line(linewidth = 1) +

#   # --- Rating labels (inside plot) ---
#   geom_text_repel(
#     data = gdp_with_labels %>% filter(!is.na(rating)),
#     aes(label = rating),
#     size = 3.5,
#     box.padding = 0.25,
#     point.padding = 0.15,
#     max.overlaps = 50,
#     segment.alpha = 0.4,
#     show.legend = FALSE,
#     direction = "y",
#     seed = 42
#   ) +

#   # --- Country labels (right side, clean stacking) ---
#   geom_text_repel(
#     data = end_labels %>%
#       mutate(date = x_max + 1),   # push labels outside plot
#     aes(label = country),
#     size = 3.5,
#     hjust = 0,                   # left-align text
#     direction = "y",             # vertical repel only
#     nudge_x = 0.5,
#     box.padding = 0.3,
#     segment.color = NA,          # no connecting lines (clean look)
#     max.overlaps = Inf,
#     show.legend = FALSE,
#     seed = 42
#   ) +

#   # Expand x-axis for label space
#   scale_x_date(expand = expansion(mult = c(0.01, 0.15))) +

#   labs(
#     x = NULL,
#     y = "Log GDP per Capita (PPP)"
#   ) +

#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     plot.margin = margin(10, 40, 10, 10)  # extra right margin
#   )
# dev.off()
