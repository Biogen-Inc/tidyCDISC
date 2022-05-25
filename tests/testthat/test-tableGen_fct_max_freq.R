context("table generator max FREQ stat block")

# test_that("max FREQ block on ADSL", {
#   app_max_freq.ADSL("AVISITf2", NULL, tg_data)
# })
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

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("max FREQ MUST have differnt var and grouping variables", {
  expect_error(app_max_freq.ADSL("SEX", "SEX", data = tg_data, grp_tots))
})
test_that("max FREQ block on BDS fails", {
  expect_error(app_max_freq.BDS("AGE", NULL, tg_data, as.integer(no_grp_tots)))
})
test_that("max FREQ block on character variable fails", {
  expect_error(app_max_freq.ADSL("AVISIT", NULL, tg_data, as.integer(no_grp_tots)))
})
test_that("max FREQ block on factor fails if missing VARN in data", {
  expect_error(app_max_freq.ADSL("AVISITf1", NULL, tg_data, as.integer(no_grp_tots)))
})
test_that("max FREQ block will be ordered by factor levels in data", {
  expect_equal(
    as.character(app_max_freq.ADSL("AVISITf2", NULL, tg_data, no_grp_tots)$AVISITf2[1]),
    "Week 2"
    )
})


rm(no_grp_tots, df, grp_lvls, xyz, groups, grp_tots)

