library(glue)
library(dplyr)
library(riskmetric)

riskmetric_score <- "tidyCDISC" %>%
  pkg_ref() %>%
  as_tibble() %>%
  pkg_assess() %>%
  pkg_score() %>%
  pull("pkg_score") %>%
  round(2)

template <- "https://img.shields.io/badge/{label}-{sprintf('%.2f', value)}-{colour}"

badge_riskmetric = case_when(
  riskmetric_score >= .9 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "brightgreen",
    value = riskmetric_score
  )),
  riskmetric_score >= .8 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "green",
    value = riskmetric_score
  )),
  riskmetric_score >= .6 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "yellowgreen",
    value = riskmetric_score
  )),
  riskmetric_score  >= .4 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "orange",
    value = riskmetric_score
  )),
  TRUE ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "red",
    value = riskmetric_score
  ))
)
