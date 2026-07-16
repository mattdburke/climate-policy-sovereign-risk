# Data and Code for paper "Climate Policy and Sovereign Debt: The Impact of Transition Scenarios on Sovereign Creditworthiness"

This repo contains the replication data and code for the paper entitled "Climate Policy and Sovereign Debt: The Impact of Transition Scenarios on Sovereign Creditworthiness". This was written in R, using data from the Network for Greening the Financial System (NGFS)

For any support with this replication package please direct your queries to `matt.burke@sheffield.ac.uk`

## Instructions for running the code

- The raw data is contained within this repository. 
- Use the `setwd()` command, direct R to the root directory.
- Run `source("main.r")`
- To recreate specific elements of the analysis you can run;
    `setwd("path_to_root_directory/ngfs-credit-ratings")`
    `source("src/packages.r")`
    Followed by any other command within the replication package.
- Tables are reproduced using the `tables.r` file, similarly plots are recreated from the `figures.r` file.
- Individual tables and graphs can be reproduced by running the relevant section of code in the above .r files. However, the easiest approach is to simply run the whole package and collect the [tables](tables/) and [graphs](plots/) from the relevant folders.
- The repository will download with all clean and processed data. To test full reproducibility, the researcher can delete all of the files contained within the folders cleandata, output, plots, scenario_data and tables, and then rerun `source("main.r")`.

## Data dictionaries
- rawdata: For descriptions of the raw data click [here](rawdata/dictionary.md)

## Requirements

This code was run on the following system. 
```
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 22631)

Matrix products: default


locale:
[1] LC_COLLATE=English_United Kingdom.utf8 
[2] LC_CTYPE=English_United Kingdom.utf8   
[3] LC_MONETARY=English_United Kingdom.utf8
[4] LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.utf8    

time zone: Europe/London
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] quantmod_0.4.26   TTR_0.24.4        xts_0.14.0        xtable_1.8-4     
 [5] sandwich_3.1-0    caTools_1.18.2    lmtest_0.9-40     zoo_1.8-12       
 [9] wesanderson_0.3.7 passport_0.3.0    ggpubr_0.6.0      lubridate_1.9.3  
[13] forcats_1.0.0     purrr_1.0.2       readr_2.1.5       tidyr_1.3.1      
[17] tibble_3.2.1      tidyverse_2.0.0   ggrepel_0.9.5     ggplot2_3.5.1    
[21] stringr_1.5.1     countrycode_1.6.0 data.table_1.16.0 PEIP_2.2-5       
[25] ranger_0.16.0     dplyr_1.1.4      

loaded via a namespace (and not attached):
 [1] dotCall64_1.1-1   gtable_0.3.5      spam_2.10-0       RSEIS_4.2-0      
 [5] rstatix_0.7.2     lattice_0.22-6    tzdb_0.4.0        vctrs_0.6.5      
 [9] tools_4.4.1       bitops_1.0-8      generics_0.1.3    curl_5.2.2       
[13] fansi_1.0.6       pkgconfig_2.0.3   Matrix_1.7-0      lifecycle_1.0.4  
[17] compiler_4.4.1    fields_16.2       munsell_0.5.1     Rwave_2.6-5      
[21] carData_3.0-5     maps_3.4.2        pracma_2.4.4      pillar_1.9.0     
[25] car_3.1-2         bvls_1.4          abind_1.4-5       tidyselect_1.2.1 
[29] stringi_1.8.4     grid_4.4.1        colorspace_2.1-1  cli_3.6.3        
[33] magrittr_2.0.3    utf8_1.2.4        broom_1.0.6       withr_3.0.1      
[37] scales_1.3.0      backports_1.5.0   timechange_0.3.0  ggsignif_0.6.4   
[41] hms_1.1.3         viridisLite_0.4.2 rlang_1.1.4       Rcpp_1.0.13      
[45] glue_1.7.0        geigen_2.3        R6_2.5.1          RPMG_2.2-7       
```