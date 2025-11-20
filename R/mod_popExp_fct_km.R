#' Kaplan-Meier Curve
#'
#' Create scatter plot where if the variables are numeric then they
#' are plotted, and if they are PARAMCD's then a week and value
#' must be selected for plotting.
#'
#' @param data Merged data to be used in plot
#' @param yvar Selected xy-axis
#' @param response_var character, the response variable (paramcd)
#' @param censor_var character, the censor variable. Usually CNSR
#' @param CI logical, whether the curve(s) should be accompanied with a 95\% CI
#' @param risk_table logical, whether to display risk table below plot
#' @param censor_mark logical, whether to plot + symbols when patients censored
#' @param strata character, variable name(s) of strata variable(s) (categorical or factor)
#' @param covariates character, variable name(s) of covariate variable(s) (categorical or factor)
#'
#' @importFrom stats as.formula
#' @importFrom GGally ggsurv
#' @importFrom survival survfit Surv
#' @importFrom ggsurvfit survfit2 ggsurvfit add_risktable add_confidence_interval add_censor_mark
#' @importFrom stats reformulate
#'
#' @family popExp functions
#' @keywords popEx
#'
#' @return A ggplot object containing the KM curve plot
#'
#' @noRd
app_km_curve <- function(
  data,
  yvar,
  response_var,
  censor_var,
  CI,
  risk_table,
  censor_mark,
  median_line,
  strata = NULL,
  covariates = NULL,
  time_ticks = NULL
) {
  # Symbols
  response_var <- rlang::sym(response_var)
  censor_var <- rlang::sym(censor_var)

  # Filter to selected PARAMCD retaining all columns
  suppressWarnings(
    d_param <- data |>
      dplyr::filter(PARAMCD == yvar)
  )

  d_model <- d_param |>
    dplyr::filter(!is.na(!!response_var)) |>
    dplyr::distinct()

  # Package checks
  rlang::check_installed("survival", reason = "to use this function.")
  rlang::check_installed("ggsurvfit", reason = "to use this function.")
  rlang::check_installed("purrr", reason = "to use this function.")

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a dataframe.")
  }
  if (!rlang::is_symbol(rlang::enexpr(response_var))) {
    cli::cli_abort("{.arg response_var} must be an unquoted column name.")
  }
  if (!rlang::is_symbol(rlang::enexpr(censor_var))) {
    cli::cli_abort("{.arg censor_var} must be an unquoted column name.")
  }

  # LHS construction
  LHS <- glue::glue(
    "survival::Surv({rlang::ensym(response_var)}, {rlang::ensym(censor_var)})"
  )
  if (
    rlang::as_label(censor_var) == "CNSR" &&
      rlang::as_label(response_var) == "AVAL"
  ) {
    LHS <- glue::glue(
      "ggsurvfit::Surv_CNSR({rlang::ensym(response_var)}, {rlang::ensym(censor_var)})"
    )
  }

  # Remove sentinel values
  strata <- setdiff(strata, "NONE")
  covariates <- setdiff(covariates, "NONE")

  # RHS for KM fit (strata only)
  if (length(strata) == 0) {
    RHS <- "1"
    no_strata <- TRUE
  } else {
    RHS <- strata
    no_strata <- FALSE
  }

  # Ensure required columns exist
  needed <- unique(c(
    rlang::as_label(response_var),
    rlang::as_label(censor_var),
    strata,
    covariates
  ))
  missing_vars <- setdiff(needed, colnames(d_model))
  if (length(missing_vars) > 0) {
    cli::cli_abort("Missing {missing_vars} column{?s} in filtered data.")
  }

  fit_formula <- stats::reformulate(response = LHS, termlabels = RHS)
  fit <- ggsurvfit::survfit2(fit_formula, data = d_model)

  by_title <- if (no_strata) "" else {
    glue::glue(
      "\nby {glue::glue_collapse(purrr::map_chr(strata, ~ best_lab(d_model, .x)), sep = ', ', last = ' and ')}"
    )
  }

  # Build base plot to get automatic breaks
  base_p <- ggsurvfit::ggsurvfit(fit)
  auto_breaks <- ggplot2::ggplot_build(base_p)$layout$panel_params[[1]]$x$breaks

  # Determine final ticks (user-specified or defaults). Server parse already
  # ensures numeric, non-negative, unique values; just range-filter here.
  ticks <- time_ticks
  if (is.null(ticks) || length(ticks) == 0) {
    ticks <- auto_breaks
  } else {
    rng <- range(d_model[[rlang::as_label(response_var)]], na.rm = TRUE)
    # Force lower bound to 0 to allow baseline tick even if earliest observed time > 0
    rng[1] <- 0
    # Keep ticks within upper range but never drop 0 if user specified it
    ticks <- ticks[ticks <= rng[2]]
    if (0 %in% time_ticks && !(0 %in% ticks)) {
      ticks <- c(0, ticks)
    }
    ticks <- sort(unique(ticks))
    if (length(ticks) == 0) ticks <- auto_breaks
  }

  p2 <- base_p +
    (if (risk_table) ggsurvfit::add_risktable(risktable_stats = c("n.risk"), time_breaks = ticks)) +
    (if (CI) ggsurvfit::add_confidence_interval()) +
    (if (censor_mark) ggsurvfit::add_censor_mark()) +
    (if (median_line) ggsurvfit::add_quantile(y_value = 0.5)) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 16)
    ) +
    ggplot2::ggtitle(glue::glue("{unique(d_model$PARAM)} {by_title}")) +
    (if (!no_strata)
      ggplot2::theme(plot.margin = ggplot2::margin(t = 1.2, unit = "cm"))) +
      ggplot2::scale_x_continuous(breaks = ticks, minor_breaks = NULL)

  table_est <- ggsurvfit::tidy_survfit(
    fit,
    times = ticks,
    type = "survival"
  ) |>
    (\(x) {
      if (!"strata" %in% names(x)) {
        dplyr::mutate(x, strata = "Overall")
      } else {
        x
      }
    })() |>
    dplyr::mutate(
      est_ci = glue::glue(
        "{round(estimate, 2)} ({round(conf.low, 2)}, {round(conf.high, 2)})"
      )
    ) |>
    dplyr::select(Strata = strata, time, est_ci) |>
    dplyr::filter(time > 0) |>
    dplyr::mutate(
      est_ci = dplyr::if_else(est_ci == "NA (NA, NA)", "-", est_ci)
    ) |>
    tidyr::pivot_wider(names_from = time, values_from = est_ci)

  table_median0 <- summary(fit)$table
  table_median1 <- if (no_strata) {
    dplyr::bind_rows(table_median0) |>
      dplyr::mutate(Strata = "Overall")
  } else {
    tibble::as_tibble(table_median0, rownames = "Strata")
  }
  table_median <- table_median1 |>
    dplyr::select(Strata, median, `0.95LCL`, `0.95UCL`) |>
    dplyr::mutate(
      est_ci = glue::glue(
        "{round(median, 2)} ({round(`0.95LCL`, 2)}, {round(`0.95UCL`, 2)})"
      )
    ) |>
    dplyr::select(Strata, `Median (95% CI)` = est_ci) |>
    dplyr::mutate(
      Strata = stringr::str_replace_all(
        Strata,
        "(^|,\\s*)[^=,]+\\s*=\\s*",
        "\\1"
      )
    )

  # Cox PH model construction
  no_covar <- length(covariates) == 0
  termlabels <- NULL
  if (!no_strata && no_covar) {
    termlabels <- strata
  } else if (no_strata && !no_covar) {
    termlabels <- covariates
  } else if (!no_strata && !no_covar) {
    termlabels <- unique(c(strata, covariates))
  }

  table_cox <- NULL
  if (!is.null(termlabels) && length(termlabels) > 0) {
    cox_formula <- stats::reformulate(response = LHS, termlabels = termlabels)
    cox_fit <- survival::coxph(cox_formula, data = d_model)
    table_cox <- gtsummary::tbl_regression(
      cox_fit,
      exponentiate = TRUE,
      conf.level = 0.95,
      add_estimate_to_reference_rows = TRUE
    )
  }

  list(
    plot_km = p2,
    table_est = table_est,
    table_median = table_median,
    table_cox = table_cox
  )
}
