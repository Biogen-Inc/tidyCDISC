#' Suggest ADaM Domain
#'
#' Heuristically suggests the most likely ADaM domain for a dataset
#' based on the presence of required variables.
#'
#' @param df A data.frame to evaluate.
#'
#' @return A character string fo the most likely domain name (e.g., "ADLB"), or NULL.
#' @export
suggest_adam_domain <- function(df) {

  scores <- purrr::map_int(adam_requirements, \(def) {
    sum(def$required %in% names(df))
  })

  best <- which.max(scores)
  if (scores[best] > 1) {
    names(adam_requirements)[best]
  } else {
    NULL
  }
}

#' Validate ADaM Structure
#'
#' Validates that a dataset conforms to required variables for a given ADaM domain.
#' If no domain is supplied, it will attempt to infer one based on variable names.
#'
#' @param df A data.frame to validate
#' @param domain Optional. A string naming the ADaM domain to validate against.
#'
#' @returns A list with:
#' - `domain`: inferred or provided domain
#' - `is_valid`: logical
#' - `missing_required`: character vector
#' - `missing_recommended`: character vector
#' - `error`: character or NULL
#'
#' @export
validate_adamish <- function(df, domain = NULL) {

  if (is.null(domain)) {
    domain <- suggest_adam_domain(df)
  }

  if (is.null(domain) || !domain %in% names(adam_requirements)) {
    out_list <- structure(
      list(
        domain = NULL,
        is_valid = FALSE,
        missing_required = character(0),
        missing_recommended = character(0),
        error = "Unknown or unspecified ADaM domain."
      ),
      class = "adam_validation"
    )
    return(out_list)
  }

  req_vars <- adam_requirements[[domain]]$required
  rec_vars <- adam_requirements[[domain]]$recommended

  missing_required <- setdiff(req_vars, names(df))
  missing_recommended <- setdiff(rec_vars, names(df))
  is_valid <- length(missing_required) == 0

  out_list <- structure(
    list(
      domain = domain,
      is_valid = is_valid,
      missing_required = missing_required,
      missing_recommended = missing_recommended,
      error = NULL
    ),
    class = "adam_validation"
  )

  return(out_list)
}

#' Suggest Column Mappings for ADaM
#'
#' Based on validation results, attempt to suggest which columns to rename to standard ADaM variable names.
#'
#' @param df A data.frame to evaluate
#' @param validation A list returned from `validate_adamish()`
#' @param string_distance Logical. Use string distance matching to provide suggestions, requires `stringdist` package
#' @param session A Shiny session object (optional).
#' @param input_ids A named list of input IDs corresponding to ADaM variable names (optional)
#'
#' @returns A named list with best-guess mappings for missing variables
#' @export
suggest_adam_column_mapping <- function(
  df,
  validation,
  string_distance = FALSE,
  session = NULL,
  input_ids = NULL
) {
  if (!inherits(validation, "adam_validation")) {
    stop("Invalid validation object passed. Must be from validate_adamish()")
  }

  if (validation$is_valid) {
    return(stats::setNames(vector("list", 0), character(0)))
  }

  candidates <- names(df)
  suggestions <- list()

  # Alias dictionary
  aliases <- list(
    USUBJID = c("subject_id", "subjid", "usrid"),
    STUDYID = c("study", "study_id"),
    PARAM = c("test", "variable", "paramname"),
    PARAMCD = c("testcd", "param_code"),
    AVAL = c("value", "avalval", "score"),
    AVISIT = c("visit", "timepoint"),
    AVISITN = c("visitnum", "visitn"),
    CNSR = c("censor", "cnsrflag"),
    ADT = c("date", "adtm", "obsdt"),
    QNAM = c("question", "qname", "item"),
    AEDECOD = c("event", "aeterm"),
    AESTDTC = c("startdt", "aestdt", "start_date")
  )

  for (var in c(validation$missing_required, validation$missing_recommended)) {
    # browser()
    lc_candidates <- tolower(candidates)
    lc_var <- tolower(var)

    # 1. Exact match
    exact_match <- candidates[lc_candidates == lc_var]

    # 2. Alias match
    alias_match <- NULL
    if (var %in% names(aliases)) {
      alias_match <- candidates[lc_candidates %in% tolower(aliases[[var]])]
    }

    # 3. Fuzzy contains match
    fuzzy_match <- candidates[grepl(lc_var, lc_candidates)]

    # 4. String distance match
    stringdist_match <- NULL

    if (rlang::is_true(string_distance)) {
      rlang::check_installed(
        "stringdist",
        reason = "is required to perform string distance matching."
      )

      # Create set of targets for expected var names and aliases
      targets <- tolower(c(var, aliases[[var]] %||% character(0)))

      # Compute distances between each target and column name
      dist_matrix <- stringdist::stringdistmatrix(
        targets,
        lc_candidates,
        method = "jw"
      )
      min_idx <- which(dist_matrix == min(dist_matrix), arr.ind = TRUE)

      # Get best column candidate if distance is small enough
      if (nrow(min_idx) > 0) {
        min_val <- dist_matrix[min_idx[1, 1], min_idx[1, 2]]
        if (min_val < 0.25) {
          stringdist_match <- candidates[min_idx[1, 2]]
        }
      }
    }

    # 5. Select suggestion
    suggestion <- dplyr::first(
      c(exact_match, alias_match, fuzzy_match, stringdist_match),
      default = NULL
    )
    suggestions[[var]] <- suggestion

    # If session + input_ids provided, update selectizeInput()
    if (
      !rlang::is_null(session) &&
        !rlang::is_null(input_ids) &&
        !rlang::is_null(input_ids[[var]]) &&
        !rlang::is_null(suggestion)
    ) {
      shiny::updateSelectizeInput(
        session = session,
        inputId = input_ids[[var]],
        choices = candidates,
        selected = suggestion
      )
    }
  }

  return(suggestions)
}


#' Show ADaM Compatibility Alert
#'
#' Display a shinyalert-style popup based on the results of `validate_adamish()`
#'
#' @param validation A list returned from `validate_adamish()`
#' @param session The Shiny session (typically `session` from server)
#' @export
show_adamish_alert <- function(validation, session) {
  rlang::check_installed("shinyalert", reason = "to use this function.")

  if (isTRUE(validation$is_valid)) {
    shinyalert::shinyalert(
      title = "\u2705 ADaM Compatibility Passed",
      text = glue::glue("Domain: {validation$domain}"),
      type = "success",
      session = session
    )
  } else {
    message <- if (!is.null(validation$error)) {
      validation$error
    } else {
      domain_msg <- glue::glue("Domain: {validation$domain}")

      req_msg <- glue::glue(
        "\u274C Missing required: {glue::glue_collapse(validation$missing_required, sep = ', ')}"
      )
      rec_msg <- glue::glue(
        "\u26A0\uFE0F Missing recommended: {glue::glue_collapse(validation$missing_recommended, sep = ', ')}"
      )

      message <- dplyr::if_else(
        length(validation$missing_recommended) > 0,
        glue::glue("{domain_msg}\n{req_msg}\n{rec_msg}"),
        req_msg
      )
    }

    shinyalert::shinyalert(
      title = "\u274C ADaM Compatibility Failed",
      text = message,
      type = "error",
      session = session
    )
  }
}

#' Normalize ADaM datasets
#'
#' @description Checks ADaM domain of uploaded datasets and renames based on the domain. Returns filename if no ADaM domain.
#'
#' @param datafile_list list A list of data.frames dataset to normalize
#' @return A list of normalized data.frames.
#'
#' @noRd
#'
normalize_adam_datasets <- function(datafile_list) {
  normalized_list <- list()

  for (name in names(datafile_list)) {
    df <- datafile_list[[name]]
    validation <- validate_adamish(df)

    if (!is.null(validation$domain)) {
      # Rename to standard domain name
      normalized_list[[validation$domain]] <- df
    } else {
      # Keep original name if can't validate
      normalized_list[[name]] <- df
    }
  }

  return(normalized_list)
}
