
baseline <- read.csv("cleandata/baseline_data_clean.csv", header=TRUE)

predict_accuracy_full <- function(data_frame, ratio){
set.seed(5)
sample = sample.split(data_frame, SplitRatio = ratio)
train = subset(data_frame, sample == TRUE)
test  = subset(data_frame, sample == FALSE)

set.seed(77)
model <- ranger(scale20 ~
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

pred <- predict(model, test, type="se")
est <- pred$predictions

pred <- round(est)
actual <- round(test$scale20)
acc <- actual - pred
a <- table(acc)  # frequency of each difference
x <- as.numeric(max(names(a)))
# Function to compute cumulative accuracy within ±k
cumulative_accuracy <- function(k_max, acc_table) {
  sapply(0:k_max, function(k) {
    sum(acc_table[as.character(-k:k)], na.rm = TRUE) / sum(acc_table)
  })
}

acc_table_extended <- cumulative_accuracy(x, a)
names(acc_table_extended) <- paste0("±", 0:x)
return (acc_table_extended)
}



predict_accuracy_gdp <- function(data_frame, ratio){
set.seed(5)
sample = sample.split(data_frame, SplitRatio = ratio)
train = subset(data_frame, sample == TRUE)
test  = subset(data_frame, sample == FALSE)

set.seed(77)
model <- ranger(scale20 ~
	ln_S_GDPpercapitaUS,
	data=train,
	num.trees=2000,
	importance='permutation',
	write.forest = TRUE,
	keep.inbag=TRUE)

pred <- predict(model, test, type="se")
est <- pred$predictions

pred <- round(est)
actual <- round(test$scale20)
acc <- actual - pred
a <- table(acc)  # frequency of each difference
x <- as.numeric(max(names(a)))
# Function to compute cumulative accuracy within ±k
cumulative_accuracy <- function(k_max, acc_table) {
  sapply(0:k_max, function(k) {
    sum(acc_table[as.character(-k:k)], na.rm = TRUE) / sum(acc_table)
  })
}

acc_table_extended <- cumulative_accuracy(x, a)
names(acc_table_extended) <- paste0("±", 0:x)
return (acc_table_extended)
}


t.8 <- predict_accuracy_full(baseline, 0.7)
t.7 <- predict_accuracy_gdp(baseline, 0.7)

cat(paste0("
\\begin{table}[tb!]
\\footnotesize
\\centering
\\caption{Numerical Values for $\\pm$ Notches (up to ±3)}
\\label{tab:tab1}
\\begin{tabularx}{\\textwidth}{X X X X}
\\hline
Notch & \\% Accurate & Notch & \\% Accurate \\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{Panel A: Full model}\\\\
\\hline
$\\pm0 & ",round(t.8[1]*100,2)," & $\\pm2 & ",round(t.8[3]*100,2),"\\\\
$\\pm1 & ",round(t.8[2]*100,2)," & $\\pm3 & ",round(t.8[4]*100,2),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{Panel B: GDP only}\\\\
\\hline
$\\pm0 & ",round(t.7[1]*100,2)," & $\\pm2 & ",round(t.7[3]*100,2),"\\\\
$\\pm1 & ",round(t.7[2]*100,2)," & $\\pm3 & ",round(t.7[4]*100,2),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{\\begin{footnotesize}
This table reports the \\% accuracy of our model. Panel A reveals the accuracy on the full model described by Equation (1) and Panel B reveals the accuracy for a version of the model relying only on the variation in GDP.
\\end{footnotesize}}
\\end{tabularx}
\\end{table}
"), file = "tables/table2.tex")
