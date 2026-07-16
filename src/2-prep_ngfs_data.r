df1 <- read.csv("rawdata/NiGEM_data_v4.csv")

# Seperate out the name of the model
extract_bracket_text <- function(text) {
  pattern <- "\\[(.*?)\\]"
  matches <- regmatches(text, regexpr(pattern, text))
  extracted_text <- gsub("\\[|\\]", "", matches)
  return(extracted_text)
}
df1$Model <- sapply(df1$Model, extract_bracket_text)

# Below 2C category has some backslashes and is encoded differently. This fixes that.
remove_after_backslash <- function(text) {
  if (substr(text, 1, 1) == "B") {
  pattern <- "\\s.*"
  cleaned_text <- sub(pattern, " 2", iconv(text, from = "ISO-8859-1", to = "UTF-8"))
  return(cleaned_text)
} else {
    return (text)
}}
df1$Scenario <- sapply(df1$Scenario, remove_after_backslash)

# Obtain country name from Region 
extract_country_name <- function(text) {
  parts <- strsplit(text, "\\|")[[1]]
  return(parts[2])
}
df1$parsed_country <- sapply(df1$Region, extract_country_name)

# Parse the countryname to ISO3 code
df1$Region <- countrycode(
    df1$parsed_country,
    origin="country.name",
    destination="iso3c"
)

# We only want the % difference in GDP from the NGFS data
df1 <- df1[df1$Variable=="Gross Domestic Product (GDP)(combined)",]
df1 <- df1[df1$Unit=="% difference,  2017 PPP ; US$ Bn",]

# Remove unused columns
df1 <- df1 %>%
dplyr::select(-c(
    Variable,
    Unit,
    Unit,
    parsed_country)) 

# Place each model/scenario into it's own csv file
unique_models <- unique(df1$Model)
unique_scenarios <- unique(df1$Scenario)
for (i in unique_models){
    for (j in unique_scenarios){
            temp_frame <- df1 %>% dplyr::filter(
                Model==i &
                Scenario==j 
            ) %>% dplyr::select(
                -c(
                Model,
                Scenario
                )
            ) %>% mutate(across(X2023:X2050, ~ ./ 100))
            temp_frame <- dplyr::select(
                temp_frame,
                Region,
                X2045,
                X2046,
                X2047,
                X2048,
                X2049,
                X2050
            )
            if (nrow(temp_frame)>0){
            write.csv(
                temp_frame,
                paste0(
                    "scenario_data/GDPloss_",
                    i,
                    "_",
                    j,
                    ".csv"
                ), row.names = FALSE)
            }
        }
    }


# Obtain the all scenario output for gdp
df1 <- df1 %>% dplyr::mutate(
    X2045 = X2045/100,
    X2046 = X2046/100,
    X2047 = X2047/100,
    X2048 = X2048/100,
    X2049 = X2049/100,
    X2050 = X2050/100
) %>% dplyr::select(
    Model,
    Scenario,
    Region,
    X2045,
    X2046,
    X2047,
    X2048,
    X2049,
    X2050
)
write.csv(df1, "output/all_scenarios_gdp_results.csv", row.names=FALSE)



