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
  
  # ----------------------------------------------------------------------
  # input prep for table manipulation
  # ----------------------------------------------------------------------
  
  output$col_ADSL <- renderUI({
    x <- input$recipe
    if (is.null(x) | length(x) == 0) { 
      recipe_column(session$ns("COLUMN"), ADSL(), "NONE") 
    } else if (x == "DEMOGRAPHY") {
      recipe_column(session$ns("COLUMN"), ADSL(), "TRT01P") 
    } else {
      recipe_column(session$ns("COLUMN"), ADSL(), "NONE")
    }
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
  
  tg_data <- reactive({ 
    # Seperate ADSL and the PArAMCD dataframe
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

  
  # get the list of PARAMCDs
  PARAMCD_names <- reactive({
    all_data() %>% 
      select(PARAMCD) %>% 
      distinct() %>%
      pull(PARAMCD)
  })
  
  # create a reactive for the data with filters applied
  # use all_data() for any analyses
  filtered_data <- callModule(shiny_data_filter, "data_filter", data = processed_data, verbose = FALSE)
  
  all_data <- reactive({ tg_data() %>% semi_join(filtered_data()) })
  
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
  
  # calculate the totals to input after N= in the table headers
  # a single N if data is not grouped
  total <- reactive({
    if (input$COLUMN == "NONE") {
      all_data() %>% 
        distinct(USUBJID) %>% 
        summarise(n = n()) %>%
        pull(n)
    } else {
      all_data() %>%
        group_by(!!sym(input$COLUMN)) %>%
        distinct(USUBJID) %>%
        summarise(n = n()) %>%
        pull(n)
    }
  })
  
  # create a gt table output by mapping over each row in the block input
  # and performing the correct statistical method given the blocks S3 class
  for_gt <- reactive({
    validate(
      need((nrow(blocks_and_functions()) > 0),'Add variable and statistics blocks to create table.')
    )
    
    pmap(list(blocks_and_functions()$agg, 
                      blocks_and_functions()$S3, 
                      blocks_and_functions()$dropdown), 
                 function(x,y,z) 
                   IDEA_methods(x,y,z, 
                                group = column(), 
                                data = all_data())) %>%
    map(setNames, common_rownames(all_data(), column())) %>%
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
  row_names_n <- reactive({ names(for_gt())[-c(1:2)] })
  
  # create the labels for each column using the total function
  # so the columns are now NAME N= X
  col_for_list <- function(nm, x) {
    if (is.numeric(all_data()[[input$COLUMN]])) {
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
    filter_code <- gsub("processed_data","tg_data",capture.output(attr(filtered_data(), "code")))
    if(any(regexpr("%>%", filter_code) > 0)){
      glue::glue("tg_data <- eval(parse(text = {filter_code}))")
    } else {
      ""
    }
  })
  
  # get filepaths
  filenames <- reactive({
    options(useFancyQuotes = FALSE)
    paste(tolower(sQuote(paste0(names(datafile()), '.sas7bdat'))), collapse = ",")
  })
  
  # create code to generate table as dataframe object
  text_code <- reactive({
    glue::glue(
    "
    pkgs_req <- c('IDEA', 'purrr', 'haven', 'dplyr')
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
    tg_data <- IDEA::readData(study_dir, filenames) %>% IDEA::combineData()
        
    # conditionally add filter code to R script
    {filter_expr()}
        
    # get drop zone area from IDEA
    # and create table using data
    blockData <- {paste0(capture.output(dput(blocks_and_functions())), collapse = '\n')}
      
    tg_table <- purrr::pmap(list(blockData$agg, blockData$S3,blockData$dropdown), 
                            function(x,y,z) IDEA::IDEA_methods(x,y,z, 
                                                   group = {column() %||% 'NULL'}, 
                                                   data = tg_data)) %>%
    map(setNames, IDEA::common_rownames(tg_data, {column() %||% 'NULL'})) %>%
    setNames(paste(blockData$gt_group)) %>%
    bind_rows(.id = 'ID') 
    "
    )
  })
  
  # create the total column names
  total_for_code <- reactive({
    if (!!input$COLUMN == 'NONE') {
      "total <- tg_data %>% 
        distinct(USUBJID) %>% 
        summarise(n = n()) %>%
        pull(n)"
    } else {
      "total <- tg_data %>%
        group_by(!!sym(input$COLUMN)) %>%
        distinct(USUBJID) %>%
        summarise(n = n()) %>%
        pull(n)"
    }
  })
  
  generate_table_output <- reactive({
    glue::glue(
      "
      {text_code()}
      
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
            subtitle = md('{subtitle()}')
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
      
      # read in SAS table and convert to DF
      sas_data <- !!input$sas$datapath
      sas_table <- haven::read_sas(sas_data)
      # Aaron's function to compare two tables
      IDEA::compareTables(tg_table, sas_table)
      "
    )
  })
  
  output$code <- downloadHandler(
      filename = function() {
        paste0("Compare_IDEA_v_SASTables_Code.R")
      },
      content = function(file) {
        writeLines(generate_comparison_output(), file)
      }
    ) 
  
  observeEvent(input$sas, {
     shinyjs::enable("code")
   })
  
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

