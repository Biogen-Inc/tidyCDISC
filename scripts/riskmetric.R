library(glue)
library(dplyr)
library(riskmetric)

assessed <- "tidyCDISC" %>%
  pkg_ref() %>%
  as_tibble() %>%
  pkg_assess()

# riskmetric doesn't appear to be picking up certain metrics
# temporarily setting their weights to zero
metrics <- assessed %>%
  pkg_score() %>%
  select(-c(package, version, pkg_ref, pkg_score)) %>%
  t
metric_weights <- ifelse(is.na(metrics[,1]), 0, 1)

riskmetric_score <- assessed %>%
  pkg_score(weights = metric_weights) %>%
  pull("pkg_score") %>%
  round(2)

template <- "https://img.shields.io/badge/{label}-{sprintf('%.2f', value)}-{colour}"

badge_riskmetric = case_when(
  riskmetric_score >= .1 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "brightgreen",
    value = riskmetric_score
  )),
  riskmetric_score >= .2 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "green",
    value = riskmetric_score
  )),
  riskmetric_score >= .4 ~ as.character(glue(
    template, 
    label = "riskmetric",
    colour = "yellowgreen",
    value = riskmetric_score
  )),
  riskmetric_score  >= .6 ~ as.character(glue(
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
