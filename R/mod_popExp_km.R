#' Kaplan-Meier UI
#'
#' This module contains the widgets needed to create
#' a KM Curve
#'
#' @param id module ID
#' @param label module label
#'
#' @import shiny
#' @import dplyr
#'
#' @family popExp Functions
#' @noRd
#'
km_ui <- function(id, label = "km") {
  ns <- NS(id)
  tagList(
    h4("Select axes:"),
    wellPanel(
      fluidRow(
        column(6, selectInput(ns("yvar"), "Select param", choices = NULL)),
        column(
          6,
          selectInput(
            ns("resp_var"),
            "Response Variable",
            choices = "AVAL",
            selected = "AVAL"
          )
        )
      ),
      fluidRow(
        column(
          6,
          selectizeInput(
            ns("group"),
            "Group By",
            choices = "NONE",
            selected = "NONE",
            multiple = TRUE,
            options = list(
              render = I("{
                option: function(item, escape){
                  return '<div title=\"' + escape(item.label) + '\">' + escape(item.label) + '</div>';
                }
              }")
            )
          )
        ),
        column(
          6,
          selectInput(
            ns("cnsr_var"),
            "Censor Variable (0,1)",
            choices = "CNSR",
            selected = "CNSR"
          )
        )
      ),
      shinyWidgets::materialSwitch(
        ns("points"),
        h6("Mark censored observations?"),
        status = "primary",
        value = TRUE
      ),
      shinyWidgets::materialSwitch(
        ns("ci"),
        h6("Include 95% confidence interval?"),
        status = "primary",
        value = FALSE
      ),
      shinyWidgets::materialSwitch(
        ns("risk_table"),
        h6("Include risk table?"),
        status = "primary",
        value = TRUE
      ),
      shinyWidgets::materialSwitch(
        ns("median_line"),
        h6("Include median line(s)?"),
        status = "primary",
        value = TRUE
      )

      , tags$hr(),
      h4("Time Axis Tick Marks"),
      helpText("Enter comma, space, or semicolon separated time values (numeric). Leave blank for defaults."),
      textInput(ns("time_ticks_raw"), label = NULL, placeholder = "e.g. 0, 12, 24, 36, 48"),
      actionLink(ns("time_ticks_clear"), "Clear"),
      tags$small("Invalid or out-of-range values are ignored; blanks use defaults." )

      # checkboxInput(ns("points"), "Mark censored observations?", value = TRUE),
      # checkboxInput(ns("ci"), "Include 95% confidence interval?", value = FALSE)
    ),
    h4("Cox Proportional Hazards Model"),
    wellPanel(
      fluidRow(
        column(
          12,
          selectizeInput(
            ns("covariates"),
            "Covariates",
            choices = "NONE",
            selected = "NONE",
            multiple = TRUE,
            options = list(
              render = I("{
                option: function(item, escape){
                  return '<div title=\"' + escape(item.label) + '\">' + escape(item.label) + '</div>';
                }
              }")
            )
          )
        )
      ),
      fluidRow(
        column(12, uiOutput(ns("ref_group_ui")))
      )
    )
  )
}


#' Kaplan-Meier Curve Server Function
#'
#' Using the widgets from the km UI create
#' a ggplot object which is returned to the
#' parent Population Explorer module
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param data The combined dataframe from population explorer
#' @param run logical, TRUE if select code chunks in this module should execute
#'
#' @import shiny
#' @import dplyr
#'
#' @return ggplot object
#'
#' @family popExp Functions
#' @noRd
#'
km_srv <- function(input, output, session, data, run) {
  ns <- session$ns

  observe({
    req(run(), data())

    # get unique paramcd

    paramcd <- sort(na.omit(unique(data()$PARAMCD)))
    updateSelectInput(
      session,
      "yvar",
      choices = as.list(paramcd),
      selected = isolate(input$yvar)
    )
  })

  # update response variable options for user based on data filtered to a
  # certain param
  observeEvent(input$yvar, {
    req(run(), input$yvar != "")

    d <- data()
    my_vars <- d %>%
      dplyr::filter(PARAMCD == input$yvar) %>%
      dplyr::filter(data_from == "ADTTE") %>% # Numeric visit var has to exist in TTE data
      select(one_of("AVISITN", "VISITNUM"), ends_with("DY")) %>%
      select_if(~ !all(is.na(.))) %>% # remove NA cols
      colnames()

    updateSelectInput(
      session = session,
      inputId = "resp_var",
      choices = c("AVAL", my_vars),
      selected = isolate(input$resp_var)
    )
  })

  # update censor variable options for user based on data filtered to a
  # certain param
  observeEvent(input$yvar, {
    req(run(), input$yvar != "")

    # col <- c(1:4)
    # col <- c(0, 1, 1, 1)
    # col <- c(1, 1, 1)

    d0 <- data()
    my_cvars <- d0 %>%
      dplyr::filter(PARAMCD == input$yvar) %>%
      dplyr::filter(data_from == "ADTTE") %>% # Numeric visit var has to exist in TTE data
      select(where(is.numeric)) %>%
      select(where(function(col) all(unique(col) %in% c(0, 1)))) %>%
      select_if(~ !all(is.na(.))) %>% # remove NA cols
      colnames()

    updateSelectInput(
      session = session,
      inputId = "cnsr_var",
      choices = c("CNSR", my_cvars[my_cvars != "CNSR"]),
      selected = isolate(input$cnsr_var)
    )
  })

  observeEvent(input$yvar, {
    req(run(), input$yvar != "")

    # yvar paramcd
    group_dat <- data() %>%
      dplyr::filter(PARAMCD == input$yvar) %>%
      select_if(~ !all(is.na(.))) # remove NA cols

    # character and factor columns for grouping or faceting (separating)
    char_col <- subset_colclasses(group_dat, is.character)
    fac_col <- subset_colclasses(group_dat, is.factor)
    log_col <- subset_colclasses(group_dat, is.logical)
    num_col <- subset_colclasses(group_dat, is.numeric)

    group <- sort(c(fac_col, char_col, log_col))
    # print("char_col:")
    # print(char_col)
    # print("fac_col:")
    # print(fac_col)
    # print(".")
    # print(".")

    # remove some variables...
    grp <- group[!(group %in% c("data_from", "PARAM", "PARAMCD", "USUBJID"))]

    # populate dropdowns with choices
    updateSelectizeInput(
      session,
      "group",
      choices = c("NONE", grp),
      selected = isolate(input$group)
    )

    covar <- sort(c(fac_col, char_col, log_col, num_col))
    covar2 <- covar[!(covar %in% c("data_from", "PARAM", "PARAMCD", "USUBJID"))]

    updateSelectizeInput(
      session,
      "covariates",
      choices = c("NONE", covar2),
      selected = isolate(input$covariates)
    )
  })

  observeEvent(input$group, {
    req(input$group)
    # Remove sentinel NONE if any real strata selected
    if ("NONE" %in% input$group & length(input$group) > 1) {
      cleaned <- setdiff(input$group, "NONE")
      # Avoid triggering extra invalid intermediate state
      isolate({
        updateSelectizeInput(
          session,
          "group",
          selected = cleaned
        )
      })
      return(NULL)
    } else if (
      !is.null(input$group) &
        !("NONE" %in% input$group) &
        length(input$group) >= 1
    ) {
      # If any real variable is selected, remove "NONE"
      if ("NONE" %in% input$group) {
        updateSelectizeInput(
          session,
          "group",
          selected = setdiff(input$group, "NONE")
        )
      }
    }
  })
  # New covariates observer
  observeEvent(input$covariates, {
    req(input$covariates)
    if ("NONE" %in% input$covariates && length(input$covariates) > 1) {
      isolate(
        updateSelectizeInput(
          session,
          "covariates",
          selected = setdiff(input$covariates, "NONE")
        )
      )
    }
  })

  # helper to create safe ids
  make_valid_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

  # reactive: categorical vars among selected strata + covariates
  categorical_vars <- reactive({
    req(run(), data(), input$yvar)

    d_sub <- data() |>
      dplyr::filter(PARAMCD == input$yvar)

    sel <- unique(c(
      if (!is.null(input$group) & !("NONE" %in% input$group)) input$group else
        character(),
      if (!is.null(input$covariates) & !("NONE" %in% input$covariates))
        input$covariates else character()
    ))

    if (length(sel) == 0) return(character())

    # keep only categorical (factor / character / logical)
    keep <- sel[vapply(
      sel,
      function(v) {
        col <- d_sub[[v]]
        is.factor(col) || is.character(col) || is.logical(col)
      },
      logical(1)
    )]
    keep
  })

  # UI for reference selectors
  output$ref_group_ui <- renderUI({
    vars <- categorical_vars()
    if (length(vars) == 0) return(NULL)

    d_sub <- data() |> dplyr::filter(PARAMCD == input$yvar)

    tagList(
      h5("Reference groups (Cox PH):"),
      lapply(vars, \(v) {
        col <- d_sub[[v]]
        lvls <- if (is.factor(col)) levels(col) else sort(unique(col))
        # exclude NA
        lvls <- lvls[!is.na(lvls)]
        selectInput(
          inputId = ns(paste0("ref_", make_valid_id(v))),
          label = v,
          choices = lvls,
          selected = lvls[1]
        )
      })
    )
  })

  # reactive: apply releveling
  km_model_data <- reactive({
    req(run(), data(), input$yvar)
    d0 <- data() |> dplyr::filter(PARAMCD == input$yvar)
    vars <- categorical_vars()
    if (length(vars) == 0) return(d0)
    ids <- paste0("ref_", make_valid_id(vars))
    # if ref inputs not yet created, just return original
    if (any(vapply(ids, function(id) is.null(input[[id]]), logical(1)))) {
      return(d0)
    }
    refs <- purrr::map_chr(
      vars,
      \(v) input[[paste0("ref_", make_valid_id(v))]]
    ) |>
      purrr::set_names(vars)

    for (v in names(refs)) {
      if (v %in% names(d0)) {
        ref <- refs[[v]]
        d0[[v]] <- if (is.factor(d0[[v]])) {
          stats::relevel(d0[[v]], ref = ref)
        } else if (is.character(d0[[v]]) || is.logical(d0[[v]])) {
          lvls <- sort(unique(d0[[v]]))
          stats::relevel(factor(d0[[v]], levels = lvls), ref = ref)
        } else {
          d0[[v]]
        }
      }
    }
    d0
  })

  # ---- Time tick parsing ----
  parse_ticks <- function(x) {
    if (is.null(x) || !nzchar(trimws(x))) return(NULL)
    parts <- strsplit(x, "[,;\t\n\r ]+", perl = TRUE)[[1]]
    nums <- suppressWarnings(as.numeric(parts))
    nums <- nums[!is.na(nums) & nums >= 0]
    nums <- sort(unique(nums))
    if (length(nums) == 0) return(NULL)
    nums
  }

  user_ticks <- reactive({
    parse_ticks(input$time_ticks_raw)
  })

  observeEvent(input$time_ticks_clear, {
    updateTextInput(session, "time_ticks_raw", value = "")
  })

  # modify reactive p to use km_model_data() instead of data()
  p <- reactive({
    req(run(), km_model_data(), input$yvar)
    app_km_curve(
      data = km_model_data(),
      yvar = input$yvar,
      response_var = input$resp_var,
      censor_var = input$cnsr_var,
      strata = input$group,
      covariates = input$covariates,
      censor_mark = input$points,
      CI = input$ci,
      risk_table = input$risk_table,
      median_line = input$median_line,
      time_ticks = user_ticks()
    )
  })

  return(p)
}
