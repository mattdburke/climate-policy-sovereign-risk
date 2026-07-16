packages = c(
    "dplyr",
    "ranger", 
    "PEIP",
    "data.table",
    "countrycode",
    "stringr",
    "ggrepel",
    "tidyverse",
    "ggpubr",
    "patchwork",
    "passport",
    "ggplot2",
    "wesanderson",
    "lmtest",
    "caTools",
    "sandwich",
    "xtable",
    "quantmod"
    )

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)