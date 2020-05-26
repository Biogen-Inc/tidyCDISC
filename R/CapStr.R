CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

transpose_df <- function(df, num) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.)
  return(t_df[-num,])
}

common_rownames <- function(data, group) {
  if (is.null(group)) {
    vars <- c("Variable", "TOTAL")
  } else {
    vars <- c("Variable", unique(data[[group]]))
    vars[vars == ""] <- "Missing"
  }
  return(vars)
}