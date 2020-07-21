

context("Module: dataComplyRules function helpers")


test_that("gather_reqs Return Classes", {
  
  # output is a shiny tagList
  expect_equal(
    class(
      gather_rules(all_df_rules = list(error = c("Hey"), warn = c("") ),
                   expl_rules = list( list(error = c(""), warn = c("")) ),
                   df_incl_rules = list( list(error = c(""), warn = c(""))) )
    ),
    class(shiny::tagList())
  )
  # output is comprised of 3 tagLists
  expect_equal(
    length(
      gather_rules(all_df_rules = list(error = c(""), warn = c("") ),
                   expl_rules = list( list(error = c(""), warn = c("")) ),
                   df_incl_rules = list( list(error = c(""), warn = c(""))) )
    ),
    3
  )
})







