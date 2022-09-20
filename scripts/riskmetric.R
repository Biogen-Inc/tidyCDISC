library(glue)
library(dplyr)
library(riskmetric)

riskmetric_score <- "tidyCDiSC" %>%
  pkg_ref() %>%
  as_tibble() %>%
  pkg_assess() %>%
  pkg_score() %>%
  pull("pkg_score")

riskmetric_score_quintile <- ntile(riskmetric_score, 5)

template <- "https://img.shields.io/badge/{label}-{value}-{colour}"

badge_riskmetric = case_when(
  riskmetric_score_quintile == 1 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "brightgreen",
    value = riskmetric_score
  )),
  riskmetric_score_quintile == 2 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "green",
    value = riskmetric_score
  )),
  riskmetric_score_quintile == 3 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "yellowgreen",
    value = riskmetric_score
  )),
  riskmetric_score_quintile == 4 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "orange",
    value = riskmetric_score
  )),
  riskmetric_score_quintile == 5 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "red",
    value = riskmetric_score
  ))
)
