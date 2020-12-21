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
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl18">Table 18: Overall summary of adverse events</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl19">Table 19: Adverse events by system organ class and preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl25">Table 25: Severe adverse events by system organ class and preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl29">Table 29: Related adverse events by system organ class and preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl29">Table 30: Serious adverse events by system organ class and preferred term</option>',''),
           '</select>'))
  })
  
  RECIPE <- reactive( if(rlang::is_empty(input$recipe)) "NONE" else input$recipe)

  # observeEvent(RECIPE(), {
  #   req(input$table_title)
  #   val <- ifelse(RECIPE() == "NONE", "Table Title", RECIPE())
  #   updateTextInput(session, session$ns("table_title"), label = "Table Title",
  #                   value = val, width = '100%')
  # })
  
  
  # ----------------------------------------------------------------------
  # input prep for table manipulation
  # ----------------------------------------------------------------------
  
  output$grp_col_ui <- renderUI({
    sel_grp <- dplyr::case_when(
      is.null(RECIPE()) | length(RECIPE()) == 0 ~ "NONE",
      !is.null(RECIPE()) & RECIPE() != "NONE" ~ "TRT01P",
      TRUE ~ "NONE"
    )
    selectInput(session$ns("COLUMN"), "Group Data By:",
                choices = c("NONE", unique(c(
                  colnames(ADSL)[sapply(ADSL, class) %in% c('character', 'factor')],
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
  
  # Create vector of loaded adams that could be filtered on
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
  
  
  # observe({
  #   print(input$recipe)
  #   print(RECIPE())
  #   print(stan_table_num())
  #   
  # })
  
  
  # perform any pre-filters on the data, when a STAN table is selected
  pre_ADSL <- reactive({
    req(RECIPE())
    prep_adsl(ADSL = datafile()$ADSL,input_recipe = RECIPE())
  })
  
  # cleanADAE() now happens inside this reactive!
  # use potentially pre-filtered ADSL when building/ joining w/ ADAE
  # Then filter ADAE based on STAN table selected.
  pre_ADAE <- reactive({
    req(RECIPE())
    prep_adae(datafile = datafile(),ADSL = pre_ADSL()$data,input_recipe = RECIPE())
  })
  
  # Create cleaned up versions of raw data
  ADSL <- reactive({ pre_ADSL()$data })
  BDS <- reactive({  datafile()[sapply(datafile(), function(x) "PARAMCD" %in% colnames(x))] })
  ADAE <- reactive({ pre_ADAE()$data })
 
  # combine BDS data into one large data set
  bds_data <- reactive({ 
    # Seperate ADSL and the PARAMCD dataframe
    PARAMCD <- map(BDS(), ~ if(!"CHG" %in% names(.)) {update_list(., CHG = NA)} else {.})
    
    if (!is_empty(PARAMCD)) {
      # Bind all the PARAMCD files 
      all_PARAMCD <- bind_rows(PARAMCD, .id = "data_from")  %>% 
        arrange(USUBJID, AVISITN, PARAMCD) %>% 
        select(USUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG, data_from)
      
      # Join ADSL and all_PARAMCD
      combined_data <- inner_join(ADSL(), all_PARAMCD, by = "USUBJID")
    } else {
      combined_data <- ADSL() %>%
        mutate(data_from = "ADSL", PARAMCD = NA, AVAL = NA, CHG = NA)
    }
  })
  
   
   
   
   
   
  processed_data <- eventReactive(input$filter_df, {
    data_to_filter(datafile(), input$filter_df)
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
    session$sendCustomMessage("my_weeks", AVISIT())
  })
  
  observe({
    req(datafile()) # this also doesn't need to depend on pre-filters, so grabbing root df cols
    
    all_cols <- unique(c(
      colnames(datafile()$ADSL)[sapply(datafile()$ADSL, class) %in% c('character', 'factor')],
      colnames(datafile()$ADAE)[sapply(datafile()$ADAE, class) %in% c('character', 'factor')]
    ))
    session$sendCustomMessage("all_cols", all_cols)
  })
  
  
  # ----------------------------------------------------------------------
  # Generate table given the dropped blocks
  # ----------------------------------------------------------------------
  
  # convert the custom shiny input to a table output
  blocks_and_functions <- reactive({
    convertTGOutput(input$agg_drop_zone, input$block_drop_zone) 
  })
  
  column <- reactive( if (input$COLUMN == "NONE") NULL else input$COLUMN)
  
  data_to_use_str <- function(x) {
    if (x == "ADAE") { ae_data() }
    else all_data()
  }
  
  is_grp_col_adae <- reactive({
    input$COLUMN %in% dplyr::setdiff(colnames(ae_data()), colnames(all_data()))
  })
  
  use_data_reactive <- reactive({
    if(is_grp_col_adae()){
      ae_data()
    } else { # do the same for mh_data()
      all_data()
    }
  })
  
  # calculate the totals to input after N= in the table headers
  # a single N if data is not grouped
  total <- reactive({
    
    all <- use_data_reactive() %>% 
      distinct(USUBJID) %>% 
      summarise(n = n()) %>%
      pull(n)
    
    
    if (input$COLUMN == "NONE") {
      all
    } else {
      groups <- use_data_reactive() %>%
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
    
    # print(IDEA_distinct_freq.ADAE(column = "AEBODSYS",
    #                               group = NULL,
    #                               data = ae_data()))
    # print(paste("ae_data():", "" %in% getLevels(ae_data()$AEBODSYS)))
    # print(paste("ADAE():", "" %in% getLevels(ADAE()$AEBODSYS)))
    # print(paste("datafile$ADAE:", "" %in% getLevels(datafile()$ADAE$AEBODSYS)))
    
    purrr::pmap(list(blocks_and_functions()$agg, 
                      blocks_and_functions()$S3, 
                      blocks_and_functions()$dropdown,
                      blocks_and_functions()$dataset), 
                 function(x,y,z,d) 
                   IDEA_methods(x,y,z, 
                                group = column(), 
                                data  = data_to_use_str(d))) %>%
    purrr::map(setNames, common_rownames(use_data_reactive(), column())) %>%
    setNames(paste(blocks_and_functions()$gt_group)) %>%
    bind_rows(.id = "ID")  %>%
      mutate(
        ID = stringi::stri_replace_all_regex(
          ID, 
          pattern = '\\b'%s+%pretty_blocks$Pattern%s+%'\\b',
          replacement = pretty_blocks$Replacement,
          vectorize_all = FALSE))
  })
  
  # output$for_gt_table <- renderTable({ for_gt() })
  
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
    if (is.numeric(use_data_reactive()[[input$COLUMN]])) {
      stop("Need categorical column for grouping")
    }
    nm = md(glue::glue("**{row_names_n()}** <br> N={total()}"))
  }
  
  pre_filter_msgs <- reactive({
    req(RECIPE())
    paste0(pre_ADSL()$message, "<br/>", pre_ADAE()$message, collapse = "<br/>")
  })
  
  # Create the tables subtitle if the table has been filtered
  subtitle <- reactive({
    # recipe selected AND IDEAFilter applied
    if (RECIPE() != "NONE" & any(regexpr("%>%", capture.output(attr(filtered_data(), "code"))) > 0) ) {
      paste0("<b>Filters Applied:</b><br/>",pre_filter_msgs(), 
              filters_in_english(filter_header = "", filtered_data()),
             collapse = "<br/>")
    # recipe selected, no IDEAFilter
    } else if(RECIPE() != "NONE" & !(any(regexpr("%>%", capture.output(attr(filtered_data(), "code"))) > 0))){
      paste0("<b>Filters Applied:</b><br/>",pre_filter_msgs(), collapse = "<br/>")
    # no recipe selected, but IDEAFilter
    } else if(RECIPE() == "NONE" & any(regexpr("%>%", capture.output(attr(filtered_data(), "code"))) > 0)){
      filters_in_english(filtered_data())
    } else {
      " "
    }
  })
  
  # create gt table
  gt_table <- reactive({
    for_gt() %>%
      # gt(rowname_col = "Variable", groupname_col = "ID") %>%
      gt(groupname_col = "ID") %>%
      fmt_markdown(columns = vars(Variable),
                   rows = stringr::str_detect(Variable,'&nbsp;') |
                     stringr::str_detect(Variable,'<b>') |
                     stringr::str_detect(Variable,'</b>')) %>%
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
      )%>%
      cols_label(Variable = "")
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
      # We don't want the column names to update based on prefilters by recipe
      # so we are going to use the original column names here
      ADSL <- datafile()$ADSL
      metadata <- data.frame(col_names = colnames(ADSL))
      metadata$code <- NA
      
      for (i in 1:nrow(metadata)) {
        if("label" %in% names(attributes(ADSL[[metadata$col_names[i]]]))){
          metadata$code[i] <- attr(ADSL[[metadata$col_names[i]]], "label")
        }
      }
      
      new_list <- lapply(BDS(), function(x) x %>% select(PARAMCD, PARAM) %>% distinct())
      
      new_list[[length(new_list) + 1 ]] <- metadata
      names(new_list)[length(new_list)] <- "ADSL"
      
      # only display ADAE column blocks if an ADAE is uploaded!
      if ("ADAE" %in% names(datafile())) {
        # this also doesn't need to depend on pre-filters
        ADAE <- datafile()$ADAE
        # Display variable blocks that are only unique to ADAE
        ADAE_blocks <- data.frame(
          col_names = dplyr::setdiff(colnames(ADAE), metadata$col_names)
        )
        ADAE_blocks$code <- NA
        
        for (i in 1:nrow(ADAE_blocks)) {
          if("label" %in% names(attributes(ADAE[[ADAE_blocks$col_names[i]]]))){ 
            ADAE_blocks$code[i] <- attr(ADAE[[ADAE_blocks$col_names[i]]], "label") 
          }
        }
        # print(ADAE_blocks)
        new_list[[length(new_list) + 1 ]] <- ADAE_blocks
        names(new_list)[length(new_list)] <- "ADAE"
      }
        
      return(new_list)
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
  
  
  
  # Depending on data source used in the app, create data for R script
  create_script_data <- reactive({
    if(any("CDISCPILOT01" %in% ADSL()$STUDYID)){
      glue::glue("
        # create list of dataframes from CDISC pilot study
            datalist <- list(ADSL = adsl, ADAE = adae, ADVS = advs, ADLBC = adlbc)
        "
      )
    } else {glue::glue("
      # User must manually set file paths for study
          study_dir <- 'path/to/study/directory/'
          
          # use HAVEN to extract data, then merge
          filenames <- c({filenames()})
          
          # create list of dataframes
          datalist <- IDEA::readData(study_dir, filenames)
      ")}
  })
  
  # If ADAE exists, then prep that data too
  adae_expr <- reactive({
    if("ADAE" %in% names(datafile())){
      glue::glue("
        # Create AE data set
            pre_adae <- datalist %>%
                IDEA::prep_adae(pre_adsl$data, '{RECIPE()}')
            ae_data <- pre_adae$data
        "
      )
    } else {""}
  })
  # capture output of filtering expression
  # input_filter_df <- c("one","mild","Moderate")
  # paste('"',dput(input_filter_df),'"')
  data_to_filter_expr <- reactive({
    
    filter_code <- gsub("processed_data","dat_to_filt",gsub("    ","",paste(capture.output(attr(filtered_data(), "code")), collapse = "")))
    print(paste(filter_code, collapse = ""))
    if(any(regexpr("%>%", filter_code) > 0)){
      options(useFancyQuotes = FALSE)
      filter_dfs <- paste(sQuote(input$filter_df), collapse = ",")
      # options(useFancyQuotes = TRUE)
      glue::glue("
          # Create small filtered data set
              dat_to_filt <- IDEA::data_to_filter(datalist, c({filter_dfs}))
              filtered_data <- eval(parse(text = '{filter_code}')) %>% varN_fctr_reorder()
          ")
    } else {""}
  })
  filter_bds_expr <- reactive({
    filter_code <- gsub("processed_data","bds_data",capture.output(attr(filtered_data(), "code")))
    if(any(regexpr("%>%", filter_code) > 0)){
      glue::glue("
          # Apply small filtered data set to BDS data
              bds_data <- bds_data %>% semi_join(filtered_data) 
          ")
    } else {""}
  })
  # capture output of filtering expression
  filter_ae_expr <- reactive({
    filter_code <- gsub("processed_data","ae_data",capture.output(attr(filtered_data(), "code")))
    if(any(regexpr("%>%", filter_code) > 0)){
      glue::glue("
          # Apply small filtered data set to ADAE data
              ae_data <- ae_data %>% semi_join(filtered_data) 
          ")
    } else {""}
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
        
    {create_script_data()}
    pre_adsl <- IDEA::prep_adsl(datalist$ADSL, input_recipe = '{RECIPE()}')
    bds_data <- datalist %>% IDEA::combineBDS(ADSL = pre_adsl$data)
    {adae_expr()}
        
    {data_to_filter_expr()}
    {filter_bds_expr()}
    {filter_ae_expr()}
        
    # get drop zone area from IDEA
    # and create table using data
    blockData <- {paste0(capture.output(dput(blocks_and_functions())), collapse = '\n')}
    pretty_blocks <- {paste0(capture.output(dput(pretty_blocks)), collapse = '\n')}
    "
    )
  })
  
  
  # create the total column names
  total_for_code <- reactive({
    use_data <- ifelse(is_grp_col_adae(), "ae_data","bds_data")
    if (!!input$COLUMN == 'NONE') {
      glue::glue("total <- {use_data} %>% 
        distinct(USUBJID) %>% 
        summarise(n = n(), .groups='drop_last') %>%
        pull(n)")
    } else {
      glue::glue(
        "
        all <- {use_data} %>% 
        distinct(USUBJID) %>% 
        summarise(n = n(), .groups='drop_last') %>%
        pull(n)
        
        groups <- {use_data} %>%
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
    use_data <- ifelse(is_grp_col_adae(), "ae_data","bds_data")
    
    glue::glue(
      "
      {text_code()}
      
      tg_table <- purrr::pmap(list(blockData$agg,
                                    blockData$S3,
                                    blockData$dropdown,
                                    blockData$dataset), 
                            function(x,y,z,d) IDEA::IDEA_methods(x,y,z,
                                                   group = {column() %quote% 'NULL'}, 
                                                   data = IDEA::data_to_use_str(d))) %>%
      map(setNames, IDEA::common_rownames({use_data}, {column() %quote% 'NULL'})) %>%
      setNames(paste(blockData$gt_group)) %>%
      bind_rows(.id = 'ID') %>%
      mutate(
        ID = stringi::stri_replace_all_regex(
          ID, 
          pattern = '\\\\b'%s+%pretty_blocks$Pattern%s+%'\\\\b',
          replacement = pretty_blocks$Replacement,
          vectorize_all = FALSE))
    
      # create a total variable
      {total_for_code()}
      
      # get the rownames for the rable
      row_names_n <- names(tg_table)[-c(1:2)]
    
      # create the gt output
      library(gt)
      tg_table %>%
          gt(groupname_col = 'ID') %>%
          fmt_markdown(columns = vars(Variable),
                 rows = stringr::str_detect(Variable,'&nbsp;') |
                   stringr::str_detect(Variable,'<b>') |
                   stringr::str_detect(Variable,'</b>')) %>%
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
          cols_label(Variable = '')
      "
    )
  })
  
  generate_comparison_output <- reactive({
    use_data <- ifelse(is_grp_col_adae(), "ae_data","bds_data")
    glue::glue(
      "
      {text_code()}
      
      blockData$label <- 
        purrr::map(blockData$block, function(x) attr(bds_data[[x]], 'label')) %>% 
        unname() %>% str_trim()
      
      tg_table <- purrr::pmap(list(blockData$agg,
                                  blockData$S3,
                                  blockData$dropdown,
                                  blockData$dataset), 
                              function(x,y,z,d) IDEA::IDEA_methods(x,y,z, 
                                                     group = {column() %quote% 'NULL'}, 
                                                     data = IDEA::data_to_use_str(d))) %>%
      map(setNames, IDEA::common_rownames({use_data}, {column() %quote% 'NULL'})) %>%
      setNames(paste(blockData$label)) %>%
      bind_rows(.id = 'ID') %>%
      mutate(
        ID = stringi::stri_replace_all_regex(
          ID, 
          pattern = '\\\\b'%s+%pretty_blocks$Pattern%s+%'\\\\b',
          replacement = pretty_blocks$Replacement,
          vectorize_all = FALSE),
        # remove html
        Variable = gsub('<b>','', gsub('</b>','', gsub('&nbsp;',' ', Variable)))
  )
    
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

