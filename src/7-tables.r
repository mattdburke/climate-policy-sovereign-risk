# Table 1
df1 <- read.csv("cleandata/baseline_data_clean.csv")
pretty_n <- function(n){
    return (format(round(as.numeric(n), 2), nsmall = 2))
}
sink("tables/table1.tex", append=FALSE, split=FALSE)
cat("
\\begin{table}[tb!]
\\footnotesize
\\center
\\caption{Descriptive Statistics}
\\label{tab:tab1}
\\begin{tabularx}{\\textwidth}{p{7cm} X X X X}
\\\\
\\hline
Variable & Mean & Standard deviation & Minimum & Maximum\\\\
\\hline
Rating & ", pretty_n(mean(df1$scale20))," & ", pretty_n(sd(df1$scale20))," & ", pretty_n(min(df1$scale20))," & ", pretty_n(max(df1$scale20)),"\\\\
ln GDP per capita & ", pretty_n(mean(df1$ln_S_GDPpercapitaUS))," & ", pretty_n(sd(df1$ln_S_GDPpercapitaUS))," & ", pretty_n(min(df1$ln_S_GDPpercapitaUS))," & ", pretty_n(max(df1$ln_S_GDPpercapitaUS)),"\\\\
GDP growth & ", pretty_n(mean(df1$S_RealGDPgrowth))," & ", pretty_n(sd(df1$S_RealGDPgrowth))," & ", pretty_n(min(df1$S_RealGDPgrowth))," & ", pretty_n(max(df1$S_RealGDPgrowth)),"\\\\
Net General Government Debt / GDP & ", pretty_n(mean(df1$S_NetGGdebtGDP))," & ", pretty_n(sd(df1$S_NetGGdebtGDP))," & ", pretty_n(min(df1$S_NetGGdebtGDP))," & ", pretty_n(max(df1$S_NetGGdebtGDP)),"\\\\
General Government Balance / GDP & ", pretty_n(mean(df1$S_GGbalanceGDP))," & ", pretty_n(sd(df1$S_GGbalanceGDP))," & ", pretty_n(min(df1$S_GGbalanceGDP))," & ", pretty_n(max(df1$S_GGbalanceGDP)),"\\\\
Narrow Net External debt / CARs & ", pretty_n(mean(df1$S_NarrownetextdebtCARs))," & ", pretty_n(sd(df1$S_NarrownetextdebtCARs))," & ", pretty_n(min(df1$S_NarrownetextdebtCARs))," & ", pretty_n(max(df1$S_NarrownetextdebtCARs)),"\\\\
Current Account Balance / GDP & ", pretty_n(mean(df1$S_CurrentaccountbalanceGDP))," & ", pretty_n(sd(df1$S_CurrentaccountbalanceGDP))," & ", pretty_n(min(df1$S_CurrentaccountbalanceGDP))," & ", pretty_n(max(df1$S_CurrentaccountbalanceGDP)),"\\\\
\\hline
\\\\
\\multicolumn{5}{p{\\textwidth}}{\\begin{footnotesize}Notes: This table shows the descriptive statistics for our model training data. This ranges from 2015 to 2020, for 123 different countries and a total number of observation equalling 723. The sovereign credit rating variable is on the 20-notch scale, where AAA=20. 
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")
sink()


df2 <- read.csv("output/all_scenarios_gdp_results.csv")
df2 <- df2[complete.cases(df2), ]
df2 <- df2 %>% group_by(
    Scenario
) %>% summarise(
    round(mean(X2050)*100, 2)
)
sink("tables/table3.tex", append=FALSE, split=FALSE)
cat("
\\begin{table}[tb!]
\\footnotesize
\\center
\\caption{NGFS Scenarios}
\\label{tab:tab2}
\\begin{tabularx}{\\textwidth}{X X X X X}
\\\\
\\hline
Scenario Category & Scenario Name & Physical/Transition Risk & Description & Average GDP deviation from baseline (\\%) by 2050\\\\
\\hline
Orderly & Net Zero 2050 & Low/Low & Limits warming to 1.5C. Strict climate policy and innovation schedule & ", pretty_n(df2[df2$Scenario=="Net Zero 2050",2]),"\\\\
Orderly & Below 2$^\\circ$C & Low/Low & 67\\% chance of limiting global warming to below 2$^\\circ$C & ", pretty_n(df2[df2$Scenario=="Below 2",2]),"\\\\
Disorderly & Delayed Transition & Low/High & Global emissions do not decrease until 2030. Strong policies are then needed to limit warming below 2C & ", pretty_n(df2[df2$Scenario=="Delayed transition",2]),"\\\\
Hot House World & NDCs & High/Low & All pledged policies & ", pretty_n(df2[df2$Scenario=="Nationally Determined Contributions (NDCs)",2]),"\\\\
Too little, too late & Fragmented World & High/High & Delayed and divergent climate policy ambition globally. High physical risk everywhere & ", pretty_n(df2[df2$Scenario=="Fragmented World",2]),"\\\\
\\hline
\\multicolumn{5}{p{\\textwidth}}{\\begin{footnotesize}Notes: This table shows the range of NGFS scenarios accessed from NGFS Phase 5 Scenario Explorer. The NGFS uses three integrated assessment models (IAMs) including MESSAGEix-GLOBIOM 1.1-M-R12, REMIND-MAgPIE 3.2-4.6, and GCAM 6.0.
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")
sink()


df1 <- read.csv("output/all_scenarios_ratings_results.csv")
df1 <- df1 %>% dplyr::group_by(scenario_) %>%
summarize(
    rating_delta_30 = round(mean(rating_delta),2),
    cd_delta_30 = round(mean(cd_delta),2),
    pd_delta_30 = round(mean(pd_delta),2)
)
sink("tables/table4.tex", append=FALSE, split=FALSE)
cat("
\\begin{table}[tb!]
\\footnotesize
\\center
\\caption{Rating outcomes in 2050}
\\label{tab:tab3}
\\begin{tabularx}{\\textwidth}{X X X X}
\\\\
\\hline
Scenario & Rating change (notches) & Cost of debt premium (\\%) & Change to probability of default (\\%) \\\\
\\hline
Net Zero 2050 &", pretty_n(df1[df1$scenario_=="Net Zero 2050",2]) ," & ",pretty_n(df1[df1$scenario_=="Net Zero 2050",3]) ," & " ,pretty_n(df1[df1$scenario_=="Net Zero 2050",4])," \\\\
Below 2$^\\circ$C &", pretty_n(df1[df1$scenario_=="Below 2",2]) ," & ",pretty_n(df1[df1$scenario_=="Below 2",3]) ," & " ,pretty_n(df1[df1$scenario_=="Below 2",4]),"\\\\
Delayed Transition &", pretty_n(df1[df1$scenario_=="Delayed transition",2]) ," & ",pretty_n(df1[df1$scenario_=="Delayed transition",3]) ," & " ,pretty_n(df1[df1$scenario_=="Delayed transition",4]),"\\\\
NDCs &", pretty_n(df1[df1$scenario_=="Nationally Determined Contributions (NDCs)",2]) ," & ",pretty_n(df1[df1$scenario_=="Nationally Determined Contributions (NDCs)",3]) ," & " ,pretty_n(df1[df1$scenario_=="Nationally Determined Contributions (NDCs)",4]),"\\\\
Fragmented World &", pretty_n(df1[df1$scenario_=="Fragmented World",2]) ," & ",pretty_n(df1[df1$scenario_=="Fragmented World",3]) ," & " ,pretty_n(df1[df1$scenario_=="Fragmented World",4]),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{\\begin{footnotesize}Notes: Column 1 shows the scenario being estimated, Column 2 shows the average change to the credit rating, Column 3 shows the average change to the cost of debt, and Column 4 shows the average change in the probability of default. The results of our baseline simulations are all significant at the 1\\% level.
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")
sink()



sink("tables/tableA1.tex", append=FALSE, split=FALSE)
cat("
\\begin{table}[tb!]
\\footnotesize
\\centering
\\caption{S\\&P Credit Rating Scale}
\\label{tab:tab5}
\\begin{tabularx}{\\textwidth}{p{4cm} p{3cm} p{4cm} }
\\hline
Long-term Foreign Currency Issuer Rating Symbol & Numerical Rating & Grade \\\\
\\hline
AAA & 20 & Prime \\\\
AA+ & 19 & High \\\\
AA & 18 & \\\\
AA- & 17 & \\\\
A+ & 16 & Upper-medium \\\\
A & 15 & \\\\
A- & 14 & \\\\
BBB+ & 13 & Lower-medium \\\\
BBB & 12 & \\\\
BBB- & 11 & \\\\
BB+ & 10 & Speculative \\\\
BB & 9 & \\\\
BB- & 8 & \\\\
B+ & 7 & Highly speculative \\\\
B & 6 & \\\\
B- & 5 & \\\\
CCC+ & 4 & Substantial risks \\\\
CCC & 3 & \\\\
CCC- & 2 & \\\\
CC & 1 & Extremely speculative / In default \\\\
C & 1 & \\\\
D/SD & 1 & \\\\
\\hline
\\multicolumn{3}{p{\\textwidth}}{\\begin{footnotesize}
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")
sink()



# Not in final paper
# baseline <- read.csv("cleandata/baseline_data_clean.csv", header=TRUE)
# set.seed(5)
# sample = sample.split(baseline$CountryName, SplitRatio = .7)
# train = subset(baseline, sample == TRUE)
# test  = subset(baseline, sample == FALSE)
# set.seed(77)
# model.forest <- ranger(scale20 ~
# 	ln_S_GDPpercapitaUS +
# 	S_RealGDPgrowth +
# 	S_NetGGdebtGDP +
# 	S_GGbalanceGDP +
# 	S_NarrownetextdebtCARs +
# 	S_CurrentaccountbalanceGDP,
# 	data=train,
# 	num.trees=2000,
# 	importance='permutation',
# 	write.forest = TRUE,
# 	keep.inbag=TRUE)
# pred <- round(predict(model.forest, test, type="se")$predictions)
# actual <- test$scale20
# acc <- actual - pred
# a <- table(acc)
# a1 <- a[names(a)==0]/length(acc)
# a2 <- (a[names(a)==-1]+a[names(a)==0]+a[names(a)==1])/length(acc)
# a3 <- (a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2])/length(acc)
# a4 <- (a[names(a)==-3]+a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2]+a[names(a)==3])/length(acc)
# acc_table <- cbind(c(a1, a2, a3, a4))
# acc_table # prints the accuracy results discussed in Section 2.3