baseline <- read.csv("cleandata/baseline_data_clean.csv", header=TRUE)


# Graphing parameters
c1 <- wes_palette("Zissou1", 5, type="discrete")[1]
c2 <- wes_palette("Zissou1", 5, type="discrete")[2]
c3 <- wes_palette("Zissou1", 5, type="discrete")[3]
c4 <- wes_palette("Zissou1", 5, type="discrete")[4]
c5 <- wes_palette("Zissou1", 5, type="discrete")[5]

FONT = 21
ALPHA=1

set.seed(5)
sample = sample.split(baseline$CountryName, SplitRatio = .8)
train = subset(baseline, sample == TRUE)
test  = subset(baseline, sample == FALSE)

n.obs.benchmark.train <- length(train$CountryName)
n.obs.benchmark.test <- length(test$CountryName)
n.countries.benchmark.train <- length(unique(train$CountryName))
n.countries.benchmark.test <- length(unique(test$CountryName))

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

pred <- round(predict(model.forest, test, type="se")$predictions)
actual <- test$scale20
acc <- actual - pred
a <- table(acc)
a1 <- a[names(a)==0]/length(acc)
a2 <- (a[names(a)==-1]+a[names(a)==0]+a[names(a)==1])/length(acc)
a3 <- (a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2])/length(acc)
a4 <- (a[names(a)==-3]+a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2]+a[names(a)==3])/length(acc)
acc_table <- cbind(c(a1, a2, a3, a4))
acc_table

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

fig <- m1.sum %>% 
    dplyr::filter(country %in% countries) %>%
    rowwise() %>% 
    mutate( mymean = mean(c(actual, est) )) %>% 
    arrange(mymean) %>% 
    mutate(country=factor(country, country))
ggplot(fig, aes(country)) +
    geom_segment(aes(x=country, xend=country, y=5, yend=est_upper), color="grey", lineend="round") +
    geom_errorbar(aes(ymin = est_lower, ymax = est_upper), width = 0.3) +
    geom_point( aes(x=country, y=actual, color=factor(2)), size=3 ) +
    geom_point( aes(x=country, y=est, color=factor(3)), size=3 ) +
    coord_flip()+
    theme_classic() +
    theme(
        legend.position = "inside",         
        legend.position.inside = c(0.85,0.2),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="#989898")
    ) +
    scale_color_manual(
        name = "Rating",
        values=c(c4, c1),
        labels = c("Actual","Simulated")
    ) +
    theme(text=element_text(size=FONT)) +
    xlab("") +
    ylab("Credit Rating (20-point scale)")
ggsave("plots/accuracy_lollipop.png", dpi=300, width=12, height=10, units="in")
