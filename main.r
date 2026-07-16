setwd("C:/Users/mattb/OneDrive/GitHub/ngfs-credit-ratings")
source("src/1-packages.r")
source("src/2-prep_ngfs_data.r")
source("src/3-datacleaning.r")
source("src/4-analysis.r")
source("src/5-robustness.r")
source("src/6-figures.r")
source("src/7-tables.r")
source("src/8-ar6.r")
source("src/9-accuracy.r")

# Commented out script to extract the relevant data from the large AR6 database

# df1_a<-read.csv("1668008131197-AR6_Scenarios_Database_ISO3_v1.1.csv/AR6_Scenarios_Database_ISO3_v1.1.csv")
# df_meta <- read.csv("rawdata/scenario_groupings.csv")
# df1<-inner_join(df1_a,df_meta,by=c("Model","Scenario"))
# df1 <- df1 %>% 
#   filter(Variable %in% c("GDP|PPP"))
# write.csv(df1, "rawdata/AR6_data.csv", row.names=FALSE)

