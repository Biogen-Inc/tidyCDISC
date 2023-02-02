datalist <- list(ADSL = tidyCDISC::adsl, ADVS = tidyCDISC::advs, ADAE = tidyCDISC::adae, ADLBC = tidyCDISC::adlbc)
pre_adsl <- tidyCDISC::prep_adsl(datalist$ADSL, input_recipe = 'NONE')
# Create AE data set
pre_adae <- datalist %>%
  tidyCDISC::prep_adae(pre_adsl$data, 'NONE')
ae_data <- pre_adae$data
bds_data <- datalist %>% tidyCDISC::prep_bds(ADSL = pre_adsl$data)

pop_data <- pre_adsl$data %>% tidyCDISC::varN_fctr_reorder()

# get drop zone area from tidyCDISC
# and create table using data
blockData <- structure(list(agg = c("NON_MISSING", "ANOVA", "CHG", "MEAN"),
                            block = c("USUBJID", "TEMP", "WEIGHT", "PULSE"),
                            dataset = c("ADSL", "ADVS", "ADVS", "ADVS"),
                            dropdown = c(NA, "Week 2", "Week 12", "Baseline"),
                            S3 = list(structure("USUBJID", class = c("character", "ADSL")),
                                      structure("TEMP", class = c("character", "BDS")),
                                      structure("WEIGHT", class = c("character", "BDS")),
                                      structure("PULSE", class = c("character", "BDS"))),
                            gt_group = structure(c("NON_MISSING of USUBJID", "ANOVA of TEMP at Week 2", "CHG of WEIGHT at Week 12", "MEAN of PULSE at Baseline"), class = c("glue", "character")),
                            label = c("Unique Subject Identifier", "Temperature (C)", "Weight (kg)", "Pulse Rate (beats/min)"),
                            label_source = c("SAS \"label\" attribute", "PARAM", "PARAM", "PARAM")),
                       row.names = c(NA, -4L),
                       class = c("tbl_df", "tbl", "data.frame"))

# Calculate totals for population set
all <- pop_data %>%
  distinct(USUBJID) %>%
  summarise(n_tot = n(), .groups='drop_last') %>%
  mutate(TRT01P = 'Total')

grp_lvls <- tidyCDISC::get_levels(pop_data[['TRT01P']])
xyz <- data.frame(grp_lvls) %>%
  rename_with(~paste('TRT01P'), grp_lvls)

groups <-
  xyz %>%
  left_join(
    pop_data %>%
      group_by(TRT01P) %>%
      distinct(USUBJID) %>%
      summarise(n_tot = n(), .groups='drop_last')
  ) %>%
  mutate(n_tot = tidyr::replace_na(n_tot, 0))

total_df <- bind_rows(groups, all)
col_total <- total_df$n_tot

# Example data 1
example_dat1 <- list(AE = ae_data,
                     BDS = bds_data,
                     totals = total_df)
# rm(list=setdiff(ls(), "example_dat1"))
usethis::use_data(example_dat1)

tg_table <- purrr::pmap(list(
  blockData$agg,
  blockData$S3,
  blockData$dropdown,
  blockData$dataset),
  function(x,y,z,d) tidyCDISC::app_methods(x,y,z,
                                           group = 'TRT01P',
                                           data = tidyCDISC::data_to_use_str(d, ae_data, bds_data),
                                           totals = total_df)) %>%
  map(setNames, tidyCDISC::common_rownames(pop_data, 'TRT01P')) %>%
  setNames(paste(blockData$gt_group)) %>%
  bind_rows(.id = 'ID') %>%
  mutate(ID = tidyCDISC::pretty_IDs(ID))

# get the column names for the table
col_names <- names(tg_table)[-c(1:2)]

tg_table2 <-
  tg_table %>%
  gt(groupname_col = 'ID') %>%
  fmt_markdown(columns = c(Variable),
               rows = stringr::str_detect(Variable,'&nbsp;') |
                 stringr::str_detect(Variable,'<b>') |
                 stringr::str_detect(Variable,'</b>')) %>%
  tab_options(table.width = gt::px(700)) %>%
  tab_header(
    title = md('Table Title'),
    subtitle = md(" ")
  ) %>%
  tab_style(
    style = cell_text(weight = 'bold'),
    locations = cells_row_groups()
  ) %>%
  cols_label(Variable = '')

example_dat2 <- list(TG_table = tg_table2,
                     col_names = col_names,
                     col_totals = col_total)
usethis::use_data(example_dat2)