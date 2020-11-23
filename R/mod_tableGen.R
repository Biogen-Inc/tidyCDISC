#' tableGen Server Function
#'
#' @param input,output,session 
#' Internal parameters for {shiny}
#' @param datafile all uploaded data files 
#' from the dataImport module
#' 
#' @import IDEAFilter
#' @importFrom rlang sym
#' @importFrom rlang !!
#' @importFrom rlang call2
#' @importFrom rlang as_quosure
#' @importFrom rlang set_names
#' @import shiny
#' @import dplyr
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr imap
#' @import gt
#' @importFrom stringi stri_replace_all_regex
#' @importFrom stringi %s+%
#' @importFrom glue glue
#' @importFrom forcats fct_reorder
#' @import tidyr
#'
#' @family tableGen Functions


mod_tableGen_server <- function(input, output, session, datafile = reactive(NULL), filePaths = reactive(NULL)) {
  
  observeEvent( input$help, {
    tg_guide$init()$start()
  })
  
  output$stan_recipe_ui <- renderUI({
      HTML(paste('
           <select id="RECIPE" class="selectize-input">
           <option  id="none">NONE</option>
           <option  id="demography">Table 5: Demography</option>',
           ifelse("ADAE" %in% names(datafile()),'<option  id="demography">Table 18: Overall summary of adverse events</option>','')
           ,'</select>'))
  })
  
  # ----------------------------------------------------------------------
  # input prep for table manipulation
  # ----------------------------------------------------------------------
  
  output$col_ADSL <- renderUI({
    sel_grp <- dplyr::case_when(
      is.null(input$recipe) | length(input$recipe) == 0 ~ "NONE",
      input$recipe %in% c("Table 5: Demography",
                          "Table 18: Overall summary of adverse events") ~ "TRT01P",
      input$recipe == "Table 5: Demography" ~ "TRT01P",
      TRUE ~ "NONE"
    )
    selectInput(session$ns("COLUMN"), "Group Data By:",
                choices = c("NONE", unique(c(
                  colnames(ADSL())[sapply(ADSL(), class) %in% c('character', 'factor')],
                  colnames(ADAE())[sapply(ADAE(), class) %in% c('character', 'factor')]
                ))),
                selected = sel_grp
    )
  })
  
  
  # ----------------------------------------------------------------------
  # convert list of dataframes to a single joined dataframe
  # containing only BDS and ADSL files
  # this code will need to be changed if you want to add in OCCDS 
  # ----------------------------------------------------------------------
  
  
  my_loaded_adams <- reactive({
    req(!is.null(datafile()))
    sasdata0 <- toupper(names(datafile()))
    sasdata <- names(which(sapply(sasdata0,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
    return(sasdata)
  })
  
  
  # If User wants to perform advance filtering, update drop down of data frames they can filter on
  observe({
    req(my_loaded_adams())
    updateSelectInput("filter_df", session = session, choices = as.list(my_loaded_adams()), selected = "ADSL")
  })
  
  ADSL <- reactive({ datafile()$ADSL })
  BDS <- reactive({ datafile()[sapply(datafile(), function(x) "PARAMCD" %in% colnames(x))] })
  ADAE <- reactive({
    if("ADAE" %in% names(datafile())){
      # find columns the ADAE & ADSL have in common (besides Usubjid), remove
      # them from the ADAE, so that the ADSL cols are used instead. Then join
      # on usubjid and re-order the colnames to match the adae
      adae_cols <- colnames(datafile()$ADAE)
      common_cols <- dplyr::intersect(adae_cols, colnames(ADSL()))
      com_cols_excp_u <- common_cols[common_cols != "USUBJID"]
      adae_adsl <- datafile()$ADAE %>% 
        select(-one_of(com_cols_excp_u)) %>%
        full_join(ADSL(), by = "USUBJID")
      preferred_col_order <- c(adae_cols, dplyr::setdiff(colnames(ADSL()), adae_cols))
      if(sort(colnames(adae_adsl)) == sort(preferred_col_order)){
        adae_adsl[,preferred_col_order]
      } else {
        adae_adsl
      }
    } else {
      ADSL()
    }
  })
 
   bds_data <- reactive({ 
    # Seperate ADSL and the PARAMCD dataframe
    PARAMCD <- map(BDS(), ~ if(!"CHG" %in% names(.)) {update_list(., CHG = NA)} else {.})
    
    if (!is_empty(PARAMCD)) {
      # Bind all the PARAMCD files 
      all_PARAMCD <- bind_rows(PARAMCD, .id = "data_from")  %>% 
        arrange(USUBJID, AVISITN, PARAMCD) %>% 
        select(USUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG, data_from)
      
      # Join ADSL and all_PARAMCD
      combined_data <- full_join(ADSL(), all_PARAMCD, by = "USUBJID")
    } else {
      combined_data <- ADSL() %>%
        mutate(data_from = "ADSL", PARAMCD = NA, AVAL = NA, CHG = NA)
    }
  })
  
  processed_data <- eventReactive(input$filter_df, {
      select_dfs <- datafile()[input$filter_df]
      
      # Separate out non BDS and BDS data frames. Note: join may throw some
      # warnings if labels are different between two datasets, which is fine!
      # Ignore
      non_bds <- select_dfs[sapply(select_dfs, function(x) !("PARAMCD" %in% colnames(x)) )] 
      bds <- select_dfs[sapply(select_dfs, function(x) "PARAMCD" %in% colnames(x) )]
      
      # Make CHG var doesn't exist, create the column and populate with NA
      PARAMCD_dat <- purrr::map(bds, ~ if(!"CHG" %in% names(.)) {purrr::update_list(., CHG = NA)} else {.})
      
      # Combine selected data into a 1 usable data frame
      if (!rlang::is_empty(PARAMCD_dat)) {
        all_PARAMCD <- bind_rows(PARAMCD_dat, .id = "data_from") %>% distinct(.keep_all = T)
        
        if (!rlang::is_empty(non_bds)){
          combined_data <- inner_join(non_bds %>% purrr::reduce(inner_join), all_PARAMCD)
        } else {
          combined_data <-all_PARAMCD
        }
      } else {
        combined_data <- non_bds %>% reduce(inner_join)
      }
      
      return(combined_data)
  })
  
  
  
  # create a reactive for the data with filters applied
  filtered_data <- callModule(shiny_data_filter, "data_filter", data = processed_data, verbose = FALSE)
  
  # apply filters from selected dfs to tg data to create all data
  all_data <- reactive({bds_data() %>% semi_join(filtered_data()) %>% varN_fctr_reorder()})
  ae_data <- reactive({ADAE() %>% semi_join(filtered_data()) %>% varN_fctr_reorder()})
  
  # get the list of PARAMCDs
  PARAMCD_names <- reactive({
    all_data() %>% 
      select(PARAMCD) %>% 
      distinct() %>%
      pull(PARAMCD)
  })
  
  # prepare the AVISIT dropdown of the statistics blocks
  # by converting them to a factor in the order of AVISITN
  # this allows our dropdown to be in chronological order
  avisit_words <- reactive({ 
    req(any(purrr::map_lgl(datafile(), ~"AVISIT" %in% colnames(.x))))
    
    purrr::map(BDS(), function(x) x %>% dplyr::select(AVISIT)) %>%
      dplyr::bind_rows() %>%
      dplyr::pull(AVISIT)
  })
  
  avisit_fctr  <- reactive({ 
    req(any(purrr::map_lgl(datafile(), ~"AVISIT" %in% colnames(.x))))
    purrr::map(BDS(), function(x) x %>% dplyr::select(AVISITN)) %>%
      dplyr::bind_rows() %>%
      dplyr::pull(AVISITN)
  })
  
  AVISIT <- reactive({
    req(BDS())
    if (is.null(avisit_words())) {
      avisit_words <- " "
    } else {
      avisit_words <-
        tibble(AVISIT = avisit_words(), AVISITN = avisit_fctr()) %>%
        mutate(AVISIT = as.factor(AVISIT)) %>%
        mutate(AVISIT = forcats::fct_reorder(AVISIT, AVISITN)) %>%
        pull(AVISIT) %>%
        unique()
    }
    avisit_words
  })
  
  observe({
    req(AVISIT())
    session$sendCustomMessage("my_data", AVISIT())
  })
  
  
  # ----------------------------------------------------------------------
  # Generate table given the dropped blocks
  # ----------------------------------------------------------------------
  
  # convert the custom shiny input to a table output
  blocks_and_functions <- reactive({
    convertTGOutput(input$agg_drop_zone, input$block_drop_zone) 
  })
  
  column <- reactive( if (input$COLUMN == "NONE") NULL else input$COLUMN)
  
  data_to_use <- function(x) {
    if (x == "ADAE") { ae_data() }
    else all_data()
  }
  
  is_grp_col_adae <- reactive({
    input$COLUMN %in% dplyr::setdiff(colnames(ae_data()), colnames(all_data()))
  })
  
  use_data <- reactive({
    # Identify which class data set dragged variables are from
    # dat_types <- list()
    # for (i in 1:nrow(blocks_and_functions())) {
    #   dat_types[i] <- class(blocks_and_functions()$S3[[i]])[2]
    # }
    # check <- c("BDS", "ADAE", "ADMH")
    # 
    # if(any(intersect(check, unlist(dat_types)) == "ADAE")) {
    if(is_grp_col_adae()){
      ae_data()
    } else { # do the same for mh_data()
      all_data()
    }
    # } else {
    #   all_data()
    # }
  })
  
  # calculate the totals to input after N= in the table headers
  # a single N if data is not grouped
  total <- reactive({
    
    all <- use_data() %>% 
      distinct(USUBJID) %>% 
      summarise(n = n()) %>%
      pull(n)
    
    
    if (input$COLUMN == "NONE") {
      all
    } else {
      groups <- use_data() %>%
        group_by(!!sym(input$COLUMN)) %>%
        distinct(USUBJID) %>%
        summarise(n = n()) %>%
        pull(n)
      c(groups, all)
    }
  })
  
  
  
  # create a gt table output by mapping over each row in the block input
  # and performing the correct statistical method given the blocks S3 class
  for_gt <- reactive({
    
    validate(
      need((nrow(blocks_and_functions()) > 0),'Add variable and statistics blocks to create table.')
    )
    # if grouping by a column that only exists in the ADAE
    if(is_grp_col_adae()){
      validate(
        need(all(blocks_and_functions()$dataset == "ADAE"), glue::glue("{column()} doesn't exist in all data files, please select new grouping variable or only drag variables to left-hand side from ADAE source"))
      )
    }
    
    pmap(list(blocks_and_functions()$agg, 
                      blocks_and_functions()$S3, 
                      blocks_and_functions()$dropdown,
                      blocks_and_functions()$dataset), 
                 function(x,y,z,d) 
                   IDEA_methods(x,y,z, 
                                group = column(), 
                                data  = data_to_use(d))) %>%
    map(setNames, common_rownames(use_data(), column())) %>%
    setNames(paste(blocks_and_functions()$gt_group)) %>%
    bind_rows(.id = "ID")  %>%
      mutate(
        ID = stringi::stri_replace_all_regex(
          ID, 
          pattern = "\\b"%s+%block_lookup()$Pattern%s+%"\\b",
          replacement = block_lookup()$Replacement,
          vectorize_all = FALSE))
  })
  
  # remove the first two columns from the row names to use
  # since these are used for grouping in gt
  row_names_n <- reactive({ 
    test <- names(for_gt())[-c(1:2)] 
    test[grepl("\\.\\.\\.", test)] <- "Missing"
    test <- test[test != "Total"]
    append(test, "Total")
  })
  
  # create the labels for each column using the total function
  # so the columns are now NAME N= X
  col_for_list <- function(nm, x) {
    if (is.numeric(use_data()[[input$COLUMN]])) {
      stop("Need categorical column for grouping")
    }
    nm = md(glue::glue("**{row_names_n()}** <br> N={total()}"))
  }
  
  # Create the tables subtitle if the table has been filtered
  subtitle <- reactive({
    if (any(regexpr("%>%", capture.output(attr(filtered_data(), "code"))) > 0)) {
      filters_in_english(filtered_data()) 
    } else {
      " "
    }
  })
  
  # create gt table
  gt_table <- reactive({
    for_gt() %>%
      gt(rowname_col = "Variable", groupname_col = "ID") %>%
      tab_options(table.width = px(700)) %>%
      cols_label(.list = imap(for_gt()[-c(1:2)], ~col_for_list(.y, .x))) %>%
      tab_header(
        title = md(input$table_title),
        subtitle = md(subtitle())
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
      ) %>%
      tab_style(
        style = list(
          cell_text(align = "right")
        ),
        locations = cells_stub(rows = TRUE)
      )
  })
  
  output$all <- render_gt({  gt_table() })
  
  
  # ----------------------------------------------------------------------
  # Block UI based on the data files that are uploaded
  # create a two column dataframe of the the ADSL column names
  # and their labels
  # and for BDS files the PARAMCDs and their names
  # these will be used as block names and hover text
  # and blocks will be grouped by the datafile they came from
  # ----------------------------------------------------------------------
  
  # create dataframe of block names and their labels
  # for BDS use param cd as column names and params as their labels 
  block_data <- reactive({
      metadata <- data.frame(col_names = colnames(ADSL()))
      metadata$code <- NA
      
      for (i in 1:nrow(metadata)) {
        metadata$code[i] <- attr(ADSL()[[colnames(ADSL())[i]]], "label")
      }
      
      new_list <- lapply(BDS(), function(x) x %>% select(PARAMCD, PARAM) %>% distinct())
      
      new_list[[length(new_list) + 1 ]] <- metadata
      names(new_list)[length(new_list)] <- "ADSL"
      
      # only display ADAE column blocks if an ADAE is uploaded!
      # print(!is.null(ADAE))
      # print("ADAE" %in% names(datafile()))
      # print(names(datafile()))
      if (!is.null(ADAE) &  "ADAE" %in% names(datafile())) {  #
        ADAE_blocks <- data.frame(col_names = colnames(ADAE()))
        
        for (i in 1:nrow(ADAE_blocks)) {
            ADAE_blocks$code[i] <- attr(ADAE()[[colnames(ADAE())[i]]], "label")
        }
        new_list[[length(new_list) + 1 ]] <- ADAE_blocks
        names(new_list)[length(new_list)] <- "ADAE"
      }
        
      return(new_list)
  })
  
  # create a lookup table for each stats block
  # and what to print in the tab headers instead 
  # of the name of the block:
  # rather than print STATSBLOCKNAME of COLUMN
  # replace all STATSBLOCKNAME with a more table friendly label
  # MEAN becomes Descriptive Statistics etc.
  block_lookup <- reactive({
    
    pretty_blocks <- tibble(
      Pattern = c("MEAN", "FREQ", "CHG"),
      Replacement = c("Descriptive Statistics", 
                      "Summary Counts", 
                      "Descriptive Statistics of Change from Baseline")
    )
    
    test <- block_data() %>%
      map(rlang::set_names, c("Pattern", "Replacement")) %>%
      bind_rows() %>%
      rbind(pretty_blocks)
  })
  
  # ----------------------------------------------------------------------
  # Download table
  # Currently CSV and HTML but easy to add more!
  # ----------------------------------------------------------------------
  
  output$download_gt <- downloadHandler(
    filename = function() {
      paste0("TableGenerator", input$download_type)
    },
    content = function(file) {
      if(input$download_type == ".csv") {
        write.csv(for_gt(), file, row.names = FALSE)
      } else if(input$download_type == ".html") {
        exportHTML <- gt_table()
        gtsave(exportHTML, file)
      }
    }
  ) 
  
  # ----------------------------------------------------------------------
  # Download table code 
  # Create code as string to be exported
  # as primary or secondary table
  # ----------------------------------------------------------------------
  
  # capture output of filtering expression
  filter_expr <- reactive({
    filter_code <- gsub("processed_data","bds_data",capture.output(attr(filtered_data(), "code")))
    if(any(regexpr("%>%", filter_code) > 0)){
      glue::glue("
          # add filter code to R script
          bds_data <- eval(parse(text = '{filter_code}'))
          "
                 )
    } else {
      ""
    }
  })
  # If ADAE exists, then prep that data too
  adae_expr <- reactive({
    filter_code <- gsub("processed_data","bds_data",capture.output(attr(filtered_data(), "code")))
    if(any(regexpr("%>%", filter_code) > 0)){
      glue::glue("
          # add filter code to R script
          bds_data <- eval(parse(text = '{filter_code}'))
          "
      )
    } else {
      ""
    }
  })#"ADAE" %in% names(datafile())
  
  
  # get filepaths
  filenames <- reactive({
    options(useFancyQuotes = FALSE)
    paste(tolower(sQuote(paste0(names(datafile()), '.sas7bdat'))), collapse = ",")
  })
  
  # create code to generate table as dataframe object
  text_code <- reactive({
    glue::glue(
    "
    options(digits = 3)
    
    pkgs_req <- c('IDEA', 'purrr', 'haven', 'dplyr', 'diffdf')
    pkgs_needed <- pkgs_req[!(pkgs_req %in% installed.packages()[,'Package'])]
    
    non_idea_needed <- pkgs_needed[pkgs_needed != 'IDEA']
    
    if(length(non_idea_needed)) install.packages(non_idea_needed)
    if('IDEA' %in% pkgs_needed) remotes::install_github('IDEA')
    
    library(purrr)
    library(IDEA)
    library(haven)
    library(dplyr)
        
    # User must manually set file paths for study
    study_dir <- 'path/to/study/directory/'
        
    # use HAVEN to extract data, then merge
    filenames <- c({filenames()})
        
    # create list of dataframes
    bds_data <- IDEA::readData(study_dir, filenames) %>% IDEA::combineData()
        
    {filter_expr()}
        
    # get drop zone area from IDEA
    # and create table using data
    blockData <- {paste0(capture.output(dput(blocks_and_functions())), collapse = '\n')}
    "
    )
  })
  
  total <- reactive({
    
    all <- use_data() %>% 
      distinct(USUBJID) %>% 
      summarise(n = n()) %>%
      pull(n)
    
    
    if (input$COLUMN == "NONE") {
      all
    } else {
      groups <- use_data() %>%
        group_by(!!sym(input$COLUMN)) %>%
        distinct(USUBJID) %>%
        summarise(n = n()) %>%
        pull(n)
      c(groups, all)
    }
  })
  
  # create the total column names
  total_for_code <- reactive({
    
    if (!!input$COLUMN == 'NONE') {
      "total <- bds_data %>% 
        distinct(USUBJID) %>% 
        summarise(n = n(), .groups='drop_last') %>%
        pull(n)"
    } else {
      glue::glue(
        "
        all <- bds_data %>% 
        distinct(USUBJID) %>% 
        summarise(n = n(), .groups='drop_last') %>%
        pull(n)
        
        groups <- bds_data %>%
        group_by({input$COLUMN}) %>%
        distinct(USUBJID) %>%
        summarise(n = n(), .groups='drop_last') %>%
        pull(n)
        
        total <- c(groups, all)
        "
      )
    }
  })
  
  generate_table_output <- reactive({
    glue::glue(
      "
      {text_code()}
      
      tg_table <- purrr::pmap(list(blockData$agg, blockData$S3,blockData$dropdown), 
                            function(x,y,z) IDEA::IDEA_methods(x,y,z, 
                                                   group = {column() %quote% 'NULL'}, 
                                                   data = bds_data)) %>%
      map(setNames, IDEA::common_rownames(bds_data, {column() %quote% 'NULL'})) %>%
      setNames(paste(blockData$gt_group)) %>%
      bind_rows(.id = 'ID') 
    
      # create a total variable
      {total_for_code()}
      
      # get the rownames for the rable
      row_names_n <- names(tg_table)[-c(1:2)]
    
      # create the gt output
      tg_table %>%
          gt(rowname_col = 'Variable', groupname_col = 'ID') %>%
          tab_options(table.width = px(700)) %>%
          cols_label(.list = imap(tg_table[-c(1:2)], ~ IDEA::col_for_list_expr(.y, .x))) %>%
          tab_header(
            title = md('{input$table_title}'),
            subtitle = md(\"{subtitle()}\")
          ) %>%
          tab_style(
          style = cell_text(weight = 'bold'),
          locations = cells_row_groups()
          ) %>%
          tab_style(
          style = list(
          cell_text(align = 'right')
          ),
          locations = cells_stub(rows = TRUE)
        )
      "
    )
  })
  
  generate_comparison_output <- reactive({
    glue::glue(
      "
      {text_code()}
      
      blockData$label <- 
        purrr::map(blockData$block, function(x) attr(bds_data[[x]], 'label')) %>% 
        unname() %>% str_trim()
      
      tg_table <- purrr::pmap(list(blockData$agg, blockData$S3,blockData$dropdown), 
                              function(x,y,z) IDEA::IDEA_methods(x,y,z, 
                                                     group = {column() %quote% 'NULL'}, 
                                                     data = bds_data)) %>%
      map(setNames, IDEA::common_rownames(bds_data, {column() %quote% 'NULL'})) %>%
      setNames(paste(blockData$label)) %>%
      bind_rows(.id = 'ID')
    
      # read in SAS table and convert to DF
      sas_data_dir <- 'path/to/sas/table/dataset/'
      sas_filename <- '{input$sas$name}'
      sas_table <- haven::read_sas(file.path(sas_data_dir, sas_filename))
      
      # prepare SAS table for comparison
      sas_comp_ready <- IDEA::prep_sas_table(data = sas_table,
                                             machine_readable = TRUE,
                                             keep_orig_ids = FALSE,
                                             rm_desc_col = FALSE
                                             )

      # prepare TG Table for comparison
      tg_comp_ready <- IDEA::prep_tg_table(data = tg_table,
                                           machine_readable = TRUE,
                                           keep_orig_ids = FALSE,
                                           rm_desc_col = FALSE,
                                           generic_colnames = TRUE
                                           )
      
      # Compare the two tables...
      library(diffdf)
      
      # ... by location in the tables
      order_diff <- diffdf(base = sas_comp_ready, 
              compare = tg_comp_ready,
              keys = c('id_block', 'id_rn'),
              tolerance = 0.001,
              strict_numeric = TRUE, # Integer != Double
              strict_factor = TRUE,  # Factor != Character
              file = '{paste0(stringr::str_remove(input$sas$name, '.sas7bdat'), '_v_IDEA_order_diff_study_dir.log')}'
      )
      order_diff # view output
      
      diffdf_has_issues(order_diff) # any issues?
      
      # which rows have an issue in each data frame
      diffdf_issuerows(sas_comp_ready, order_diff)
      diffdf_issuerows(tg_comp_ready, order_diff)
      
      
      # # ... by variable labels and values. Note, must match!
      # var_diff <- diffdf(base = sas_comp_ready, 
      #         compare = tg_comp_ready,
      #         keys = c('id_desc', 'Variable'),
      #         tolerance = 0.001,
      #         strict_numeric = TRUE, # Integer != Double
      #         strict_factor = TRUE,  # Factor != Character
      #         file = '{paste0(stringr::str_remove(input$sas$name, '.sas7bdat'), '_v_IDEA_var_diff_study_dir.log')}'
      # )
      "
    )
  })
  
  observeEvent(input$sas, {
    shinyjs::enable("code")
  })
  
  output$code <- downloadHandler(
      filename = function() {
        paste0("Compare_IDEA_v_SASTables_Code.R")
      },
      content = function(file) {
        writeLines(generate_comparison_output(), file)
      }
    ) 

  output$tblcode <- downloadHandler(
    filename = function() {
      paste0("Reproduce_IDEA_Table.R")
    },
    content = function(file) {
      writeLines(generate_table_output(), file)
    }
  ) 

  # return the block area to be created in app_ui
  p <- reactive({ rowArea(col = 12, block_data()) })
  
  return(p)
  
}

