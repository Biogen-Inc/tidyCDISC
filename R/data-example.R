#' Example Data Set 1
#' 
#' Pre-processed data for purposes of demonstrating \link[tidyCDISC]{app_methods}.
#' 
#' @format A list with 3 elements:
#' \describe{
#'   \item{AE}{data frame, pre-processed AE dataset}
#'   \item{BDS}{data frame, pre-processed BDS dataset}
#'   \item{totals}{data frame, contains totals by grouping variable for pre-processed data}
#' }
#' @export
#' @return a list, containing three data.frames
"example_dat1"

#' Example Data Set 2
#' 
#' Pre-processed data for the purposes of demonstrating \link[tidyCDISC]{col_for_list_expr}.
#' 
#' @format A list with 3 elements:
#' \describe{
#'   \item{TG_table}{data frame, pre-processed \code{gt} table object with basic column names}
#'   \item{col_names}{vector, the column names}
#'   \item{col_totals}{vector, totals corresponding to each column}
#' }
#' @export
#' @return a list of length 3
"example_dat2"