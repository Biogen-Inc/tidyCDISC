context("table generator y freq block")


no_grp_tots <- df <- tg_data %>% 
  distinct(USUBJID) %>% 
  summarise(n_tot = n())

df <- df %>%
  mutate(temp = 'Total') %>%
  rename_with(~paste("COUNTRY"), "temp")

grp_lvls <- getLevels(tg_data[["COUNTRY"]])  # PUT ADAE() somehow?
xyz <- data.frame(grp_lvls) %>%
  rename_with(~paste("COUNTRY"), grp_lvls)

groups <- 
  xyz %>%
  left_join(
    tg_data %>%
      group_by(!!sym("COUNTRY")) %>%
      distinct(USUBJID) %>%
      summarise(n_tot = n())
  )%>%
  mutate(n_tot = tidyr::replace_na(n_tot, 0)) 

grp_tots <- bind_rows(groups, df)

test_that("Y freq block ADSL", {
  app_y.ADSL("ITTFL", NULL, tg_data, no_grp_tots)
  app_y.ADAE("ITTFL", NULL, tg_data, no_grp_tots)
  app_y.ADSL("SAFFL", NULL, tg_data, no_grp_tots)
  app_y.ADAE("SAFFL", NULL, tg_data, no_grp_tots)
})

test_that("Y freq block ADSL group", {
  app_y.ADSL("ITTFL", "COUNTRY", tg_data, grp_tots)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("Y freq block numeric fails", {
  expect_error(app_y.ADSL("AGE", NULL, tg_data, no_grp_tots))
})

# test_that("Y freq must be applied to flag variable", {
#   expect_error(app_y.ADSL("SEX", data = tg_data))
# })


rm(no_grp_tots, df, grp_lvls, xyz, groups, grp_tots)