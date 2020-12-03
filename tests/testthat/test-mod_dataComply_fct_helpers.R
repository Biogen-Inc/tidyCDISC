

context("Module: dataComply function helpers")


library(shiny)

# Reactive console simulation  -------------------------------------------------
# See discussion at https://github.com/rstudio/shiny/issues/2518
reactive_console_funs <- list(
  reactiveVal = function(value = NULL, label = NULL) {
    if (missing(label)) {
      call <- sys.call()
      label <- shiny:::rvalSrcrefToLabel(attr(call, "srcref", exact = TRUE))
    }
    
    rv <- shiny::reactiveVal(value, label)
    function(x) {
      if (missing(x)) {
        rv()
      } else {
        on.exit(shiny:::flushReact(), add = TRUE, after = FALSE)
        rv(x)
      }
    }
  },
  reactiveValues = function(...) {
    rv <- shiny::reactiveValues(...)
    class(rv) <- c("rv_flush_on_write", class(rv))
    rv
  },
  `$<-.rv_flush_on_write` = function(x, name, value) {
    on.exit(shiny:::flushReact(), add = TRUE, after = FALSE)
    NextMethod()
  }
)

consoleReactive <- function(state) {
  if (state) {
    options(shiny.suppressMissingContextError = TRUE)
    attach(reactive_console_funs, name = "reactive_console", warn.conflicts = FALSE)
  } else {
    options(shiny.suppressMissingContextError = FALSE)
    detach("reactive_console")
  }
}


consoleReactive(TRUE) # turn on reactive state


test_that("gather_reqs Return Classes", {
  
  # df_list output is a List
  expect_equal(
               class(
                 gather_reqs(
                    disp_type = "error",
                    datalist = reactive(list("mtcars" = mtcars)),
                    all_df_rules = all_df_rules,
                    expl_rules = expl_rules,
                    df_incl_rules = df_incl_rules)$df_list
              ),
              class(list())
  )
  
  # df output is class data frame
  gr_df_clss <- class(
    gather_reqs(
      disp_type = "error",
      datalist = reactive(list("mtcars" = mtcars)),
      all_df_rules = all_df_rules,
      expl_rules = expl_rules,
      df_incl_rules = df_incl_rules)$df
  )
  expect_equal(
    gr_df_clss[gr_df_clss == "data.frame"],
    class(mtcars)
  )
  
  # gt output is class gt
  expect_equal(
    class(
      gather_reqs(
        disp_type = "error",
        datalist = reactive(list("mtcars" = mtcars)),
        all_df_rules = all_df_rules,
        expl_rules = expl_rules,
        df_incl_rules = df_incl_rules)$gt
    ),
    class(mtcars %>% gt())
  )
  
  
})

# Test Ideas:
# - Expect Error if no rules given



###############################
#
# Test Errors
#
###############################
##############
# all_df_rules
##############
test_that("gather_reqs errors return expectations for all_df_rules", {
  
  # df_list should be an empty list
  expect_output(str(gather_reqs(
        disp_type = "error",
        datalist = reactive(list("mtcars" = mtcars)),
        all_df_rules = list(error = c("USUBJID"), warn = c("")),
        expl_rules = list( list(error = c(""), warn = c("")) ),
        df_incl_rules = list( list(error = c(""), warn = c("")) ) 
      )$df_list
    ),
    "Named list()"
  )
  
  # df should be one row with USUBJID
  expect_equal(
    gather_reqs(
      disp_type = "error",
      datalist = reactive(list("mtcars" = mtcars)),
      all_df_rules = list(error = c("USUBJID"), warn = c("")),
      expl_rules = list( list(error = c(""), warn = c("")) ),
      df_incl_rules = list( list(error = c(""), warn = c("")) ))$df$type_col
    ,
    "USUBJID"
  )

  # df_list should not be an empty list if USUBJID exists
  expect_output(str(gather_reqs(
        disp_type = "error",
        datalist = reactive(list("mtcars" = mtcars %>% mutate(USUBJID = "1"))),
        all_df_rules = list(error = c("USUBJID"), warn = c("")),
        expl_rules = list( list(error = c(""), warn = c("")) ),
        df_incl_rules = list( list(error = c(""), warn = c("")) ) 
      )$df_list
    ),
    "List of 1"
  )
  
  # df should be an empty data frame
  expect_equal(
    nrow(gather_reqs(
        disp_type = "error",
        datalist = reactive(list("mtcars" = mtcars %>% mutate(USUBJID = "1"))),
        all_df_rules = list(error = c("USUBJID"), warn = c("")),
        expl_rules = list( list(error = c(""), warn = c("")) ),
        df_incl_rules = list( list(error = c(""), warn = c("")) ) 
      )$df
    ),
    0
  )
})

##############
# expl_rules
##############
test_that("gather_reqs errors return expectations for expl_rules", {
  
  # df_list should be an empty list
  expect_output(str(gather_reqs(
    disp_type = "error",
    datalist = reactive(list("mtcars" = mtcars)),
    all_df_rules = list(error = c(""), warn = c("")),
    expl_rules = list(mtcars = list(error = c("USUBJID"), warn = c("")) ),
    df_incl_rules = list( list(error = c(""), warn = c("")) ) 
  )$df_list
  ),
  "Named list()"
  )
  
  # df should be one row with USUBJID
  expect_equal(
    gather_reqs(
      disp_type = "error",
      datalist = reactive(list("mtcars" = mtcars)),
      all_df_rules = list(error = c(""), warn = c("")),
      expl_rules = list(mtcars = list(error = c("USUBJID"), warn = c("")) ),
      df_incl_rules = list( list(error = c(""), warn = c("")) ))$df$type_col
    ,
    "USUBJID"
  )
  
  # df_list should not be an empty list if USUBJID exists
  expect_output(str(gather_reqs(
    disp_type = "error",
    datalist = reactive(list("mtcars" = mtcars %>% mutate(USUBJID = "1"))),
    all_df_rules = list(error = c(""), warn = c("")),
    expl_rules = list(mtcars = list(error = c("USUBJID"), warn = c("")) ),
    df_incl_rules = list( list(error = c(""), warn = c("")) ) 
  )$df_list
  ),
  "List of 1"
  )
  
  # df should be an empty data frame
  expect_equal(
    nrow(gather_reqs(
      disp_type = "error",
      datalist = reactive(list("mtcars" = mtcars %>% mutate(USUBJID = "1"))),
      all_df_rules = list(error = c(""), warn = c("")),
      expl_rules = list(mtcars = list(error = c("USUBJID"), warn = c("")) ),
      df_incl_rules = list( list(error = c(""), warn = c("")) ) 
    )$df
    ),
    0
  )
})

###############
# df_incl_rules
###############
test_that("gather_reqs errors return expectations for df_incl_rules", {
  
  # df_list should be an empty list
  expect_output(
    str(
      gather_reqs(
        disp_type = "error",
        datalist = reactive(list("mtcars" = mtcars)),
        all_df_rules = list(error = c(""), warn = c("")),
        expl_rules = list(list(error = c(""), warn = c("")) ),
        df_incl_rules = list(mpg = list(error = c("USUBJID"), warn = c(""))) 
      )$df_list
    ),
    "Named list()"
  )
  
  # df should be one row with USUBJID
  expect_equal(
    gather_reqs(
      disp_type = "error",
      datalist = reactive(list("mtcars" = mtcars)),
      all_df_rules = list(error = c(""), warn = c("")),
      expl_rules = list(list(error = c(""), warn = c("")) ),
      df_incl_rules = list(mpg = list(error = c("USUBJID"), warn = c("")) ))$df$type_col
    ,
    "USUBJID"
  )

  # df_list should not be an empty list if USUBJID exists
  expect_output(str(gather_reqs(
    disp_type = "error",
    datalist = reactive(list("mtcars" = mtcars %>% mutate(USUBJID = "1"))),
    all_df_rules = list(error = c(""), warn = c("")),
    expl_rules = list( list(error = c(""), warn = c("")) ),
    df_incl_rules = list(mpg = list(error = c("USUBJID"), warn = c("")) )
  )$df_list
  ),
  "List of 1"
  )

  # df should be an empty data frame
  expect_equal(
    nrow(gather_reqs(
      disp_type = "error",
      datalist = reactive(list("mtcars" = mtcars %>% mutate(USUBJID = "1"))),
      all_df_rules = list(error = c(""), warn = c("")),
      expl_rules = list( list(error = c(""), warn = c("")) ),
      df_incl_rules = list(mpg = list(error = c("USUBJID"), warn = c("")) )
    )$df
    ),
    0
  )
  
})



###############################
#
# Test Warnings
#
###############################
##############
# all_df_rules
##############
test_that("gather_reqs warnings return expectations for all_df_rules", {
  
  # df_list should be not be empty list when warning
  expect_output(str(gather_reqs(
        disp_type = "warn",
        datalist = reactive(list("mtcars" = mtcars)),
        all_df_rules = list(error = c(""), warn = c("USUBJID")),
        expl_rules = list( list(error = c(""), warn = c("")) ),
        df_incl_rules = list( list(error = c(""), warn = c("")) ) 
      )$df_list
    ),
    "List of 1"
  )
  
  # df should be one row with USUBJID
  expect_equal(
    gather_reqs(
      disp_type = "warn",
      datalist = reactive(list("mtcars" = mtcars)),
      all_df_rules = list(error = c(""), warn = c("USUBJID")),
      expl_rules = list( list(error = c(""), warn = c("")) ),
      df_incl_rules = list( list(error = c(""), warn = c("")) ))$df$type_col
    ,
    "USUBJID"
  )
  
  # No warning should be thrown, df should be an empty data frame.
  expect_equal(
      nrow(gather_reqs(
        disp_type = "warn",
        datalist = reactive(list("mtcars" = mtcars %>% mutate(USUBJID = "1"))),
        all_df_rules = list(error = c(""), warn = c("USUBJID")),
        expl_rules = list( list(error = c(""), warn = c("")) ),
        df_incl_rules = list( list(error = c(""), warn = c("")) ) 
      )$df
    ),
    0
  )
})


##############
# expl_rules
##############
test_that("gather_reqs warnings return expectations for expl_rules", {
  
  # df_list should be not be empty list when warning
  expect_output(str(gather_reqs(
    disp_type = "warn",
    datalist = reactive(list("mtcars" = mtcars)),
    all_df_rules = list(error = c(""), warn = c("")),
    expl_rules = list(mtcars = list(error = c(""), warn = c("USUBJID")) ),
    df_incl_rules = list( list(error = c(""), warn = c("")) ) 
  )$df_list
  ),
  "List of 1"
  )
  
  # df should be one row with USUBJID
  expect_equal(
    gather_reqs(
      disp_type = "warn",
      datalist = reactive(list("mtcars" = mtcars)),
      all_df_rules = list(error = c(""), warn = c("")),
      expl_rules = list(mtcars = list(error = c(""), warn = c("USUBJID")) ),
      df_incl_rules = list( list(error = c(""), warn = c("")) ))$df$type_col
    ,
    "USUBJID"
  )
  
  # No warning should be thrown, df should be an empty data frame.
  expect_equal(
    nrow(gather_reqs(
      disp_type = "warn",
      datalist = reactive(list("mtcars" = mtcars %>% mutate(USUBJID = "1"))),
      all_df_rules = list(error = c(""), warn = c("")),
      expl_rules = list(mtcars = list(error = c(""), warn = c("USUBJID")) ),
      df_incl_rules = list( list(error = c(""), warn = c("")) ) 
    )$df
    ),
    0
  )
})

###############
# df_incl_rules
###############
test_that("gather_reqs warnings return expectations for df_incl_rules", {

  # df_list should be not be empty list when warning
  expect_output(str(gather_reqs(
    disp_type = "warn",
    datalist = reactive(list("mtcars" = mtcars)),
    all_df_rules = list(error = c(""), warn = c("")),
    expl_rules = list( list(error = c(""), warn = c("")) ),
    df_incl_rules = list(mpg = list(error = c(""), warn = c("USUBJID")) )
  )$df_list
  ),
  "List of 1"
  )

  # df should be one row with USUBJID
  expect_equal(
    gather_reqs(
      disp_type = "warn",
      datalist = reactive(list("mtcars" = mtcars)),
      all_df_rules = list(error = c(""), warn = c("")),
      expl_rules = list( list(error = c(""), warn = c("")) ),
      df_incl_rules = list(mpg = list(error = c(""), warn = c("USUBJID")) ))$df$type_col
    ,
    "USUBJID"
  )

  # No warning should be thrown, df should be an empty data frame.
  expect_equal(
    nrow(gather_reqs(
      disp_type = "warn",
      datalist = reactive(list("mtcars" = mtcars %>% mutate(USUBJID = "1"))),
      all_df_rules = list(error = c(""), warn = c("")),
      expl_rules = list( list(error = c(""), warn = c("")) ),
      df_incl_rules = list(mpg = list(error = c(""), warn = c("USUBJID")) )
    )$df
    ),
    0
  )
})

consoleReactive(FALSE) # turn off reactive state






