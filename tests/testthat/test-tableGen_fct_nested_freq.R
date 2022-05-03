context("table generator NESTED FREQ stat block")

no_grp_tots <- df <- tg_data %>% 
  distinct(USUBJID) %>% 
  summarise(n_tot = n())

grp_it <- function(df, var){
  df <- df %>%
    mutate(temp = 'Total') %>%
    rename_with(~paste(var), "temp")
  
  grp_lvls <- getLevels(tg_data[[var]])  # PUT ADAE() somehow?
  xyz <- data.frame(grp_lvls) %>%
    rename_with(~paste(var), grp_lvls)
  
  groups <- 
    xyz %>%
    left_join(
      tg_data %>%
        group_by(!!sym(var)) %>%
        distinct(USUBJID) %>%
        summarise(n_tot = n())
    )%>%
    mutate(n_tot = tidyr::replace_na(n_tot, 0)) 
  
  bind_rows(groups, df)
}

# country_grp_tots <- grp_it(df, var = "COUNTRY")
# avisit_grp_tots <- grp_it(df, var = "AVISIT")
sex_grp_tots <- grp_it(df, var = "SEX")

# test_that("NESTED FREQ block on ADSL", {
#   app_nested_freq.ADSL("SEX", "COUNTRY", NULL, tg_data, no_grp_tots)
#   app_nested_freq.ADSL("SEX", "NONE", NULL, tg_data, no_grp_tots)
#   app_nested_freq.ADSL("SEX", "COUNTRY", "AVISIT", tg_data, avisit_grp_tots)
#   app_nested_freq.ADSL("SEX", "NONE", "AVISIT", tg_data, avisit_grp_tots)
# })
# 
# test_that("NESTED FREQ block ADSL group", {
#   app_nested_freq.ADSL("ITTFL", "NONE", "COUNTRY", tg_data, country_grp_tots)
# })

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("NESTED FREQ MUST have differnt var and grouping variables", {
  expect_error(app_nested_freq.ADSL("SEX", "NONE", "SEX", data = tg_data, sex_grp_tots))
})
test_that("NESTED FREQ block on BDS fails", {
  expect_error(app_nested_freq.BDS("AGE", "NONE", NULL, tg_data, no_grp_tots))
})
test_that("NESTED FREQ block on NUMERIC column fails", {
  expect_error(app_nested_freq.ADSL("AGE", "NONE", NULL, tg_data, no_grp_tots))
})
test_that("NESTED FREQ block will return a data frame with num rows equal to each var's unique combinations PLUS the unique levels of the var dragged into left drop zone", {
  expect_equal(
    nrow(app_nested_freq.ADSL("SEX", "COUNTRY", NULL, tg_data, no_grp_tots)),
    nrow(unique(tg_data[,c("SEX","COUNTRY")])) + length(unique(tg_data$SEX))
    )
})




rm(no_grp_tots, grp_it, country_grp_tots, avisit_grp_tots, sex_grp_tots)

