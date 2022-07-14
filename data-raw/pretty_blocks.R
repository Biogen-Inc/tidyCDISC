pretty_blocks <- tidyr::tibble(
  Pattern = c("MEAN", "FREQ", "CHG", "Y_FREQ", "MAX_FREQ", "NON_MISSING",
              "NESTED_FREQ_DSC", "NESTED_FREQ_ABC"),
  Replacement = c("Descriptive Statistics", 
                  "Summary Counts", 
                  "Descriptive Statistics of Change from Baseline",
                  "Subject Count for those with 'Y' values",
                  "Subject Count for maximum",
                  "Subject Count for those with Non Missing values",
                  "Subject Count at each variable level, sorted descending by total counts",
                  "Subject Count at each variable level, sorted alphabetically by name")
)
usethis::use_data(pretty_blocks, internal = TRUE)
