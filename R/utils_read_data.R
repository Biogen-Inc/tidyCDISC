#' Reads datasets in various formats
#'
#' @description Imports data in various formats and cleans column names. Accommodates .txt, .csv, .xls, .xlsx, .sas7bdat
#'
#' @param path Character Path to a tabular dataset in .txt, .csv, .xls, .xlsx, or .sas7bdat format
#' @return A data.frame of the imported dataset.
#'
#' @noRd
#'
read_data <- function(path) {
  ext <- fs::path_ext(path)

  data1 <- switch(
    ext,
    txt = vroom::vroom(path) |> as.data.frame() |> coerce_ids_to_char(),
    csv = vroom::vroom(path, delim = ",") |>
      as.data.frame() |>
      coerce_ids_to_char(),
    xls = readxl::read_xls(path) |> as.data.frame() |> coerce_ids_to_char(),
    xlsx = readxl::read_xlsx(path) |> as.data.frame() |> coerce_ids_to_char(),
    sas7bdat = haven::read_sas(path) |>
      haven::zap_formats() |>
      coerce_ids_to_char(),
    parquet = arrow::read_parquet(path) |> coerce_ids_to_char(),
    stop("Unsupported file format")
  )

  return(data1)
}

#' Converts known ID variables to characters
#'
#' @description Certain file formats have no column type metadata. This function converts known ID variables to character.
#'
#' @param data data.frame A dataset to transform
#' @return A data.frame of the modified dataset.
#'
#' @noRd
#'
coerce_ids_to_char <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::matches(
          "STUDYID|USUBJID|SUBJID|SITEID",
          ignore.case = TRUE
        ),
        .fns = \(x) as.character(x)
      )
    )
}

#' Dummy function to remove note from R CMD check
#' @noRd
dummy <- function() {
  broom.helpers::model_get_assign
  pkgload::check_dep_version
}

