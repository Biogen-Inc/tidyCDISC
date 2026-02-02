test_that("Cox PH reference group changes HR", {
  d <- dplyr::mutate(adtte, SEX = as.factor(SEX))
  fit_a <- survival::coxph(Surv(AVAL, CNSR) ~ SEX, data = d)
  d$SEX <- relevel(d$SEX, ref = levels(d$SEX)[2])
  fit_b <- survival::coxph(Surv(AVAL, CNSR) ~ SEX, data = d)
  expect_false(all(coef(fit_a) == coef(fit_b)))
  expect_equal(unname(coef(fit_b)), unname((-1 * coef(fit_a))))
})
