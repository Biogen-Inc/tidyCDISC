context("table generator non missing stat block")

no_grp_tots <- df <- tg_data %>% 
  distinct(USUBJID) %>% 
  summarise(n_tot = n())

df <- df %>%
  mutate(temp = 'Total') %>%
  rename_with(~paste("SEX"), "temp")

grp_lvls <- getLevels(tg_data[["SEX"]])  # PUT ADAE() somehow?
xyz <- data.frame(grp_lvls) %>%
  rename_with(~paste("SEX"), grp_lvls)

groups <- 
  xyz %>%
  left_join(
    tg_data %>%
      group_by(!!sym("SEX")) %>%
      distinct(USUBJID) %>%
      summarise(n_tot = n())
  )%>%
  mutate(n_tot = tidyr::replace_na(n_tot, 0)) 

grp_tots <- bind_rows(groups, df)

test_that("non missing block on ADSL", {
  app_non_missing.ADSL("DTHDT", NULL, tg_data, no_grp_tots)
  app_non_missing.ADSL("ITTFL", NULL, tg_data, no_grp_tots)
  app_non_missing.ADSL("SEX", NULL, tg_data, no_grp_tots)
})

test_that("non missing block ADSL group", {
  app_non_missing.ADSL("ITTFL", "SEX", tg_data, grp_tots)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("non missing block on BDS fails", {
  expect_error(app_non_missing.BDS("AGE", NULL, tg_data, no_grp_tots))
})

test_that("non missing MUST have differnt var and grouping variables", {
  expect_error(app_non_missing.ADSL("SEX", "SEX", data = tg_data, grp_tots))
})


rm(no_grp_tots, df, grp_lvls, xyz, groups, grp_tots)