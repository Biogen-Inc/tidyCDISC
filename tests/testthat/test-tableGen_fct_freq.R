context("table generator freq block")

no_grp_tots <- df <- tg_data %>% 
  distinct(USUBJID) %>% 
  summarise(n_tot = n())

df <- df %>%
  mutate(temp = 'Total') %>%
  rename_with(~paste("COUNTRY"), "temp")

grp_lvls <- get_levels(tg_data[["COUNTRY"]])  # PUT ADAE() somehow?
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

# test_that("freq block ADSL", {
#   app_freq.ADSL("SEX", NULL, tg_data, no_grp_tots)
# })
# 
# test_that("freq block ADSL with group", {
#   app_freq.ADSL("SEX", "COUNTRY", tg_data, grp_tots)
# })

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("freq block numeric fails", {
  expect_error(app_freq.ADSL("AGE", NULL, tg_data, no_grp_tots))
})

test_that("freq block BDS exits", {
  expect_error(app_freq.BDS("SEX", tg_data, no_grp_tots))
})

test_that("freq block OCCDS exits", {
  expect_error(app_freq.OCCDS("SEX", tg_data, no_grp_tots))
})

test_that("freq block default exits", {
  expect_error(app_freq.default("SEX", tg_data, no_grp_tots))
})

rm(no_grp_tots, df, grp_lvls, xyz, groups, grp_tots)
