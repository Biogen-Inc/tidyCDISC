#' tableGen Server Function
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param datafile all uploaded data files from the dataImport module
#' @param filePaths NULL
#'
#' @importFrom IDEAFilter shiny_data_filter
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
#' @importFrom gt gt fmt_markdown tab_options cols_label tab_header md tab_style
#'   cell_text cells_row_groups cells_stub render_gt gtsave
#' @importFrom purrr transpose
#' @importFrom glue glue
#' @importFrom tidyr replace_na
#'
#' @family tableGen Functions
#' @noRd
mod_tableGen_server <- function(input, output, session, datafile = reactive(NULL), filePaths = reactive(NULL)) {
  
  old <- options()
  on.exit(options(old))
  
  observeEvent( input$help, {
    tg_guide$init()$start()
  })
  
  output$study_table_gen <- renderUI({
    req(datafile())
    
    studies <- unique(unlist(lapply(datafile(), `[[`, "STUDYID")))
    study_ids <- paste(studies, collapse = " & ")
    h4(paste("Study ID: ", study_ids))
  })
  
  output$stan_recipe_ui <- renderUI({
      HTML(paste('
           <select id="RECIPE" class="selectize-input">
           <option  id="none">NONE</option>
           <option  id="tbl03">Table 3: Accounting of Subjects</option>
           <option  id="demography">Table 5: Demography</option>',
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl18">Table 18: Overall summary of adverse events</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl19">Table 19: Adverse events by system organ class and preferred term sorted by decreasing frequency</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl20">Table 20: Adverse events by system organ class and preferred term sorted by alphabetical order</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl21">Table 21: Adverse events by system organ class</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl23">Table 23: Adverse events by preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl25">Table 25: Severe adverse events by system organ class and preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl26">Table 26: Severe adverse events by preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl29">Table 29: Related adverse events by system organ class and preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl30">Table 30: Serious adverse events by system organ class and preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl31">Table 31: Serious adverse events by preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl33">Table 33: Related serious adverse events by system organ class and preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl34">Table 34: Adverse events that led to discontinuation of study treatment by system organ class and preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl36">Table 36: Adverse events that led to withdrawl from study by system organ class and preferred term</option>',''),
           ifelse("ADAE" %in% names(datafile()),'<option  id="tbl38">Table 38: Adverse events that led to drug interrupted, dose reduced, or dose increased by system organ class and preferred term</option>',''),
           ifelse(!rlang::is_empty(loaded_labs()) & chem_params()$exist,'<option  id="tbl41_b">Table 41: Blood Chemistry actual values by visit</option>',''),
           ifelse(!rlang::is_empty(loaded_labs()) & hema_params()$exist,'<option  id="tbl41_h">Table 41: Hematology actual values by visit</option>',''),
           ifelse(!rlang::is_empty(loaded_labs()) & urin_params()$exist,'<option  id="tbl41_u">Table 41: Urinalysis actual values by visit</option>',''),
           '</select>'))
  })
  
  RECIPE <- reactive( if(rlang::is_empty(input$recipe)) "NONE" else input$recipe)

  observeEvent(RECIPE(), {
    req(input$table_title)
    val <- ifelse(RECIPE() == "NONE", "Table Title", RECIPE())
    updateTextInput(session, "table_title", value = val)
  })
  
  
  # ----------------------------------------------------------------------
  # input prep for table manipulation
  # ----------------------------------------------------------------------
  categ_vars <- reactive({
    req(datafile()) # this also doesn't need to depend on pre-filters, so grabbing root df cols
    
    if("ADAE" %in% names(datafile())){
      all_cols <- unique(c(
        colnames(datafile()$ADSL)[sapply(datafile()$ADSL, class) %in% c('character', 'factor')],
        colnames(datafile()$ADAE)[sapply(datafile()$ADAE, class) %in% c('character', 'factor')]
      ))
    } else { # just adsl cols
      all_cols <- unique(c(
        colnames(datafile()$ADSL)[sapply(datafile()$ADSL, class) %in% c('character', 'factor')]
      ))
    }
    return( all_cols )
  })
    
  output$grp_col_ui <- renderUI({
    sel_grp <- dplyr::case_when(
      is.null(RECIPE()) | length(RECIPE()) == 0 ~ "NONE",
      !is.null(RECIPE()) & RECIPE() != "NONE" ~ "TRT01P",
      TRUE ~ "NONE"
    )
    selectInput(session$ns("COLUMN"), "Group Data By:",
                choices = c("NONE", categ_vars()),
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
  
  
  # perform any pre-filters on the data, when a STAN table is selected
  pre_ADSL <- reactive({
    req(RECIPE())
    prep_adsl(ADSL = datafile()$ADSL,input_recipe = RECIPE())
  })
  
  # clean_ADAE() now happens inside this reactive!
  # use potentially pre-filtered ADSL when building/ joining w/ ADAE
  # Then filter ADAE based on STAN table selected.
  pre_ADAE <- reactive({
    req(RECIPE())
    prep_adae(datafile = datafile(),ADSL = pre_ADSL()$data, input_recipe = RECIPE())
  })
  
  # Create cleaned up versions of raw data
  ADSL <- reactive({ 
    pre_ADSL()$data 
    # CANNOT inner_join on the ADAE subjects that were filtered because some
    # subjects had no adverse events so you'd make a mistake by excluding them.
    # Really, we'd have to identify the subjects in pre_ADSL$data and not in the
    # datalist$ADAE Then keep those, plus subjects that exist in the inner_join
    # of pre_ADSL$data & pre_ADAE()$data. Have to take this out of R script
    # still too ############################################################
    
      #%>% 
      # inner_join(
      #   pre_ADAE()$data %>%
      #     distinct(USUBJID)
      #   )
  })
  BDS <- reactive({ 
    init <- sapply(datafile(), function(x) "PARAMCD" %in% colnames(x) & !("CNSR" %in% colnames(x)))
    datafile()[init] 
    # datafile()[sapply(datafile(), function(x) "PARAMCD" %in% colnames(x))]
  })
  ADAE <- reactive({ pre_ADAE()$data })
 
  # combine all BDS data files into one large data set
  bds_data <- reactive({ 
    prep_bds(datafile = datafile(), ADSL = ADSL())
    # OLD code removed 2/17/2021
  })
  
   
   
   
   
  # Allow users to filter on any combination of data, even values that are
  # outside of table prefilters. If you only want users to apply filters ontop
  # of existing (pre) filters (from stan tables), then you need to have the
  # filters applied to ADSL(), ADAE(), and BDS_DATA()
  processed_data <- eventReactive(input$filter_df, {
    data_to_filter(datafile(), input$filter_df)
  })
  
  # create a reactive for the data with filters applied
  filtered_data <- callModule(IDEAFilter::shiny_data_filter, "data_filter", data = processed_data, verbose = FALSE)
  
  # apply filters from selected dfs to tg data to create all data
  all_data <- reactive({suppressMessages(bds_data() %>% semi_join(filtered_data()))})
  ae_data <- reactive({suppressMessages(ADAE() %>% semi_join(filtered_data()))})
  pop_data <- reactive({
    suppressMessages(
      pre_ADSL()$data %>% # Cannot be ADSL() because that has potentially been filtered to ADAE subj's
      semi_join(filtered_data()) %>%
      varN_fctr_reorder()
    )
  })
  
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
    req(datafile())
    
    if(any(purrr::map_lgl(datafile(), ~"AVISIT" %in% colnames(.x)))){
        purrr::map(BDS(), function(x) x %>% dplyr::select(AVISIT)) %>%
          dplyr::bind_rows() %>%
          dplyr::distinct(AVISIT) %>%
          dplyr::pull()
    } else {
      NULL #c("fake_weeky","dummy_weeky") # DON'T use this comment part.
                                          # It's handled in AVISIT()
    }
    
  })
  
  avisit_fctr  <- reactive({
    req(datafile())
    req(any(purrr::map_lgl(datafile(), ~"AVISIT" %in% colnames(.x))))
    
    if(any(purrr::map_lgl(datafile(), ~"AVISIT" %in% colnames(.x)))){
        purrr::map(BDS(), function(x) x %>% dplyr::select(AVISITN)) %>%
          dplyr::bind_rows() %>%
          dplyr::distinct(AVISITN) %>%
          dplyr::pull()
    } else {
      1:2
    }
    
  })
  
  AVISIT <- reactive({
    req(datafile())

    if (is.null(avisit_words())) {
      avisit_words <- c("fake_weeky","dummy_weeky")
    } else {
      # for testing
      # nums <- c(2,4,6,8,12,16,20,24,26)
      # avisit_words <- function() c("", "Baseline",paste("Week", nums), "End of Treatment")
      # avisit_fctr <- function()c(NA, 0, nums, 99)
      # rm(nums, avisit_words, avisit_fctr)
      
      awd <- tidyr::tibble(AVISIT = avisit_words(), AVISITN = avisit_fctr())
      avisit_words <-
        # tidyr::tibble(AVISIT = avisit_words(), AVISITN = avisit_fctr()) %>%
        # dplyr::mutate(AVISIT = as.factor(AVISIT)) %>%
        # dplyr::mutate(AVISIT = forcats::fct_reorder(AVISIT, AVISITN)) %>%
        awd %>%
        dplyr::mutate(AVISIT = factor(AVISIT,
            levels = awd[order(awd$AVISITN), "AVISIT"][[1]] %>% unique() )) %>%
        dplyr::pull(AVISIT) %>%
        unique() %>%
        # Arrange by factor level (AVISITN)
        sort()
    }
    avisit_words[avisit_words != ""]
  })
  
  
  # Send any and all AVISITs that exist to javascript side (script.js)
  observe({
    req(AVISIT())
    session$sendCustomMessage("my_weeks", c("ALL", as.vector(AVISIT())))
  })
  
  
  # Sending columns names that could be selected for nested freq stat block
  # just character of factor vars from the ADSL or ADAE
  observe({
    req(categ_vars())
    all_cols <- categ_vars()
    session$sendCustomMessage("all_cols", all_cols)
  })
  
  AVALS <- reactive({
    req(datafile())
    req(purrr::map_lgl(datafile(), ~ "ATPT" %in% colnames(.x)))
    
    atpt_datasets <- purrr::map_lgl(datafile(), ~ "ATPT" %in% colnames(.x))

    avals <- 
      purrr::map(datafile()[atpt_datasets], ~ .x %>%
      dplyr::select(PARAMCD, dplyr::any_of(c("ATPT"))) %>%
      dplyr::filter(dplyr::if_any(-PARAMCD, ~ !is.na(.x) & .x != "")) %>%
      dplyr::pull(PARAMCD) %>%
      get_levels()
      )
    
    ## TODO: Make this less confusing. I pity the soul who has to edit this.
    purrr::imap(avals, ~ purrr::map(.x, function(i, j =.y) {
      datafile()[[j]] %>% 
                 dplyr::filter(PARAMCD == i) %>%
                 dplyr::select(dplyr::any_of(c("ATPT", "ATPTN"))) %>%
                 varN_fctr_reorder() %>%
                 dplyr::select(dplyr::any_of(c("ATPT"))) %>%
                 purrr::map(~ .x %>%
                              addNA(ifany = TRUE) %>%
                              purrr::possibly(relevel, otherwise = .)(NA_character_) %>%
                              get_levels() %>%
                              tidyr::replace_na("N/A") %>%
                              {if (length(.) > 1) c("ALL", .) else .} %>%
                              as.list())
      }) %>%
      purrr::set_names(.x)
    )
  })
  
  observe({
    req(AVALS())
    session$sendCustomMessage("my_avals", AVALS())
  })
  
  # Verify if certain lab params exist, and if so, which dataset they live in
  # in case there are multiple ADLBs- to use later to send data to js side
  chem_params <- reactive({
    req(datafile())
    check_params(datafile(), chem)
  })
  hema_params <- reactive({
    req(datafile())
    check_params(datafile(), hema)
  }) 
  urin_params <- reactive({
    req(datafile())
    check_params(datafile(), urin)
  }) 
  loaded_labs <- reactive({
    my_loaded_adams()[substr(my_loaded_adams(),1,4) == "ADLB"] 
  })
  
  
  # Send to Client (JS) side:
  # Hematology
  # Blood Chemistry
  # Urinalysis
  
  # Sending vector of specific params (if they exist) for certain labs (if they exist)
  observe({
    req(datafile(), chem_params(), hema_params(), urin_params()) # don't req("ADLBC") because then the custom message will never get sent, and hang up the UI

    send_chem <- send_urin <- send_hema <- c("not","used","fake","vector","to","convert","to","js","array")
    send_chem_wks <- send_urin_wks <- send_hema_wks <- c("fake_weeky","fake_weeky2")
    
    if(!(rlang::is_empty(loaded_labs())) &
        (chem_params()$exist | hema_params()$exist| urin_params()$exist)
       ){
      
      
      # Blood Chem
      if(chem_params()$exist){ # add recipe() = 'tab 41'?
        send_chem <- chem_params()$vctr
        send_chem_wks <- chem_params()$tp
      } 
      
      # Hematology
      if(hema_params()$exist){ # add specific recipe() = 'tab 41'?
        send_hema <- hema_params()$vctr
        send_hema_wks <- hema_params()$tp
      } 

      # Urinalysis
      if(urin_params()$exist){ # add specific recipe() = 'tab 41'?
        send_urin <- urin_params()$vctr
        send_urin_wks <- urin_params()$tp
      } 
      
    } # end of "if labs exist"
    
    session$sendCustomMessage("adlbc", list(params = as.vector(send_chem), weeks = as.vector(send_chem_wks)))
    session$sendCustomMessage("adlbh", list(params = as.vector(send_hema), weeks = as.vector(send_hema_wks)))
    session$sendCustomMessage("adlbu", list(params = as.vector(send_urin), weeks = as.vector(send_urin_wks)))
    
    
  })
  
  # ----------------------------------------------------------------------
  # Generate table given the dropped blocks
  # ----------------------------------------------------------------------
  
  
  # convert the custom shiny input to a table output
  blocks_and_functions <- reactive({
    # create initial dataset
    blockData <- convertTGOutput(input$agg_drop_zone, input$block_drop_zone)

    blockData$label <- 
      purrr::map2(blockData$block, blockData$dataset, function(var, dat) {
        if(!is.null(attr(data_to_use_str(dat, ae_data(), all_data())[[var]], 'label'))){
          attr(data_to_use_str(dat, ae_data(), all_data())[[var]], 'label')
        } else if(all(c("PARAM","PARAMCD") %in% colnames(data_to_use_str(dat, ae_data(), all_data())))){
          data_to_use_str(dat, ae_data(), all_data()) %>%
            filter(PARAMCD == var) %>%
            distinct(PARAM) %>%
            pull() %>% as.character()
        } else {
          var
        }
      }) %>% unname() %>% stringr::str_trim()
    
    blockData$label_source <- 
      purrr::map2(blockData$block, blockData$dataset, function(var, dat) {
        if(!is.null(attr(data_to_use_str(dat, ae_data(), all_data())[[var]], 'label'))){
          'SAS "label" attribute'
        } else if("PARAMCD" %in% colnames(data_to_use_str(dat, ae_data(), all_data()))){
          'PARAM'
        } else {
          'No Label'
        }
      }) %>% unname() %>% stringr::str_trim()
    
    return(blockData)
  })
  
  column <- reactive( if (input$COLUMN == "NONE") NULL else input$COLUMN)
  

  # check if the grouping column only exists in the ADAE
  is_grp_col_adae <- reactive({
    input$COLUMN %in% dplyr::setdiff(colnames(ae_data()), colnames(all_data()))
  })
  
  # Decide which reactive data frame to use below
  use_data_reactive <- reactive({
    if(is_grp_col_adae() | numeric_stan_table(RECIPE()) %in% c(18:39)){
      ae_data()
    } else { # do the same for mh_data()
      all_data()
    }
  })
  # Only needed for app, not for R script
  use_preferred_pop_data <- reactive({
    if(is_grp_col_adae()){
      ae_data()
    } else { # do the same for mh_data()
      pop_data()
    }
  })
  
  # calculate the totals to input after N= in the table headers
  # a single N if data is not grouped
  total_df <- reactive({
    df <- use_preferred_pop_data() %>% 
      distinct(USUBJID) %>% 
      summarise(n_tot = n())
    
    if (input$COLUMN == "NONE") {
      df
      
    } else {
      df <- df %>%
        mutate(temp = 'Total') %>%
        rename_with(~paste(input$COLUMN), "temp")
      
      grp_lvls <- get_levels(use_preferred_pop_data()[[input$COLUMN]])  # PUT ADAE() somehow?
      xyz <- data.frame(grp_lvls) %>%
        rename_with(~paste(input$COLUMN), grp_lvls)
      
      groups <- 
        xyz %>%
        left_join(
          use_preferred_pop_data() %>%
          group_by(!!sym(input$COLUMN)) %>%
          distinct(USUBJID) %>%
          summarise(n_tot = n())
        )%>%
        mutate(n_tot = tidyr::replace_na(n_tot, 0)) 
      
      bind_rows(groups, df)
    }
  })
  
  col_total <- reactive({
    total_df()$n_tot
  })

  pre_filter_msgs <- reactive({
    req(RECIPE())
    paste0(pre_ADSL()$message, "<br/>", pre_ADAE()$message, collapse = "<br/>")
  })
  
  # Create the tables subtitle if the table has been filtered
  subtitle_html <- reactive({
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
    # if no data in the source, do not run the pmap, just show this msg:
    if(nrow(use_data_reactive()) == 0){
      stop(paste0("No subjects remain when the following filters are applied.\n        "
                      ,gsub("<br/>", "\n        ", pre_filter_msgs())))
    }

    d <- tg_gt(list(ADAE = ae_data(), ADSL = all_data(), POPDAT = use_preferred_pop_data()),
               blocks_and_functions(),
               total_df(),
               column())
    return(d)
  })
  
  output$for_gt_table <- renderTable({ for_gt() })
  
  # remove the first two columns from the row names to use since 
  # these are used for grouping in gt. Make sure Total is at the end
  row_names_n <- reactive({ 
    some_names <- names(for_gt())[-c(1:2)] 
    some_names[grepl("\\.\\.\\.", some_names)] <- "Missing"
    some_names_no_tot <- some_names[some_names != "Total"]
    append(some_names_no_tot, "Total")
  })
  

  # create gt table
  gt_table <- reactive({
    for_gt() %>%
      create_gt_table(., input_table_title = input$table_title, 
                      input_table_footnote = input$table_footnote, 
                      col_names = row_names_n(), 
                      col_total = col_total(), 
                      subtitle = subtitle_html())
  })
  
  # # TODO To be implemented when gt package fixes issue with col_widths()
  # # Column widths for RTF output
  # col_widths_rtf <- reactive({
  #   
  #   # Number of columns, excluding grouping column
  #   ncols <- ncol(rtf_table()[["_data"]]) - 1
  #   
  #   # Width of first column, in percentage
  #   width_col_1 <- 40
  #   width_col_rest <- (100 - width_col_1) / (ncols - 1) 
  #   
  #   widths <- c(width_col_1, rep(x = width_col_rest, times = ncols - 1))
  #   widths <- gt::pct(widths)
  #   
  #   return(widths)
  #   
  # })
  
  # Create RTF table
  rtf_table <- reactive({
    
    data <- for_gt()
    
    # Convert to gt table object
    gt_tab <- create_gt_table(data, 
                              input_table_title = input$table_title, 
                              input_table_footnote = input$table_footnote, 
                              col_names = row_names_n(), 
                              col_total = col_total(), 
                              subtitle = subtitle_html()) 
    
    # Add formatting for RTF output
    rtf_tab <- gt_tab %>%
      gt::tab_options(
        
        # Not currently possible to change font family or size
        # Open issue: https://github.com/rstudio/gt/issues/687
        # table.font.names = c("Times", "Arial"),
        # table.font.size = gt::px(18),
        # heading.title.font.size = NULL,
        # footnotes.font.size = NULL,
        
        page.numbering = TRUE,
        page.orientation = "landscape",
        
        # Set table width to be the width of the page. Column widths
        # will be distributed equally.
        table.width = gt::pct(100)
      ) #%>%
    
    # Set column widths
    # Unable to pass dynamic column width to gt::cols_width() in a shiny
    # app. Open issue: https://github.com/rstudio/gt/issues/641
    # gt::cols_width(
    #   TRUE ~ col_widths_rtf()
    # )
    
    # Page breaks
    # Not currently possible
    # Sample open issue: https://github.com/rstudio/gt/issues/1081
    
    # Convert HTML to markdown in the Source footnote
    rtf_tab[["_footnotes"]]$footnotes[[1]] <- 
      rtf_tab[["_footnotes"]]$footnotes[[1]] %>%
      as.character() %>%
      
      # Convert bold from HTML to markdown
      stringr::str_replace_all("</b>", "**") %>%
      stringr::str_replace_all("<b>", "**") %>%
      # Delete HTML elements
      stringr::str_replace_all("<.+?>", "") %>%
      trimws(which = "left") %>%
      # Convert to markdown for formatting
      gt::md()
    
    return(rtf_tab)
  })
  
  output$all <- gt::render_gt({  gt_table() })
  
  
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
        new_list[[length(new_list) + 1 ]] <- ADAE_blocks
        names(new_list)[length(new_list)] <- "ADAE"
      }
        
      return(new_list)
  })
  

  # ----------------------------------------------------------------------
  # Download table
  # Currently RTF, CSV, and HTML 
  # ----------------------------------------------------------------------
  
  output$download_gt <- downloadHandler(
    filename = function() {
      paste0("TableGenerator", input$download_type)
    },
    content = function(file) {
      progress <- Progress$new(max = 3)
      progress$set(message = "Preparing Table...")
      on.exit(progress$close())
      
      if(input$download_type == ".csv") {
        progress$inc(1) # increment progress bar
        
        write.csv(for_gt(), file, row.names = FALSE)
        progress$inc(1) # increment progress bar
        
      } else if(input$download_type == ".html") {
        progress$inc(1) # increment progress bar
        
        exportHTML <- gt_table()
        progress$inc(1) # increment progress bar
        
        gt::gtsave(exportHTML, file)
        progress$inc(1) # increment progress bar
        
      } else if(input$download_type == ".rtf") {
        progress$inc(1) # increment progress bar
        
        export_rtf <- rtf_table()
        progress$inc(1) # increment progress bar
        
        gt::gtsave(export_rtf, file, page_numbering = "header")
        progress$inc(1) # increment progress bar
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
        datalist <- list({paste(purrr::map_chr(names(datafile()), ~ paste0(.x, ' = tidyCDISC::', tolower(.x))), collapse = ', ')})
        "
      )
      # names_datafile <- function() c("ADSL", "ADAE")
      # paste(purrr::map_chr(names_datafile(), ~ paste0(.x, " = tidyCDISC::", tolower(.x))), collapse = ", ")
    } else {glue::glue("
      # User must manually set file paths for study
          study_dir <- 'path/to/study/directory/'
          
          # use HAVEN to extract data, then merge
          filenames <- c({filenames()})
          
          # create list of dataframes
          datalist <- 
            purrr::map(file_names, ~ haven::read_sas(file.path(study_directory,.x))) %>%
            setNames(toupper(stringr::str_remove(file_names, '.sas7bdat')))
      ")}
  })
  
  footnote_src <- reactive({
    if(any("CDISCPILOT01" %in% ADSL()$STUDYID)){
      "'tidyCDISC app'"
    } else {
      "study_dir"
    }
    })
  
  # If ADAE exists, then prep that data too
  adae_expr <- reactive({
    if("ADAE" %in% names(datafile())){
      glue::glue("
        # Create AE data set
        pre_adae <- datalist %>%
          tidyCDISC::prep_adae(pre_adsl$data, '{RECIPE()}')
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

    if(any(regexpr("%>%", filter_code) > 0)){
      options(useFancyQuotes = FALSE)
      filter_dfs <- paste(sQuote(input$filter_df), collapse = ",")
      # options(useFancyQuotes = TRUE)
      glue::glue("
          # Create small filtered data set
              dat_to_filt <- tidyCDISC::data_to_filter(datalist, c({filter_dfs}))
              filtered_data <- {filter_code} %>% tidyCDISC::varN_fctr_reorder()
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
    if(any(regexpr("%>%", filter_code) > 0) & "ADAE" %in% names(datafile())){
      glue::glue("
          # Apply small filtered data set to ADAE data
              ae_data <- ae_data %>% semi_join(filtered_data) 
          ")
    } else {""}
  })
  filter_pop_expr <- reactive({
    filter_code <- gsub("processed_data","bds_data",capture.output(attr(filtered_data(), "code")))
    if(any(regexpr("%>%", filter_code) > 0)){
      glue::glue("
          # Apply small filtered data set to population dataset
              pop_data <- pre_adsl$data %>% semi_join(filtered_data) %>% tidyCDISC::varN_fctr_reorder()
          ")
    } else {"pop_data <- pre_adsl$data %>% tidyCDISC::varN_fctr_reorder()"}
  })
  # capture output for empty df warning
  df_empty_expr <- reactive({
    if(nrow(use_data_reactive()) == 0) {
      glue::glue("
          # Check if No subject's remain
              if(nrow({Rscript_use_data()}) == 0) stop(\"{paste0('No subjects remain when the following filters are applied. \n        ',gsub('<br/>', '\n        ', pre_filter_msgs()))}\")
          ")
    } else {""}
  })
  
  
  
  
  # get filepaths
  filenames <- reactive({
    options(useFancyQuotes = FALSE)
    paste(tolower(sQuote(paste0(names(datafile()), '.sas7bdat'))), collapse = ",")
  })
  Rscript_use_data <- reactive({
    ifelse(is_grp_col_adae() | 
           numeric_stan_table(RECIPE()) %in% c(18:39), "ae_data","bds_data")
  })
  Rscript_use_preferred_pop_data <- reactive({
    ifelse(is_grp_col_adae() , "ae_data","pop_data")
  })
  
  # install packages (as needed) code. Assigning this as text string outside of
  # text_code because it's too difficult to escape the { character in
  # glue::glue()
  install_text <- "invisible(sapply(pkgs_req, function(x){
    if(length(find.package(x, quiet = TRUE)) == 0) {
      message(paste('Installing package:', x))
      install.packages(x)
    }}))"
  # create code to generate table as dataframe object
  text_code <- reactive({
    glue::glue(
    "
    options(digits = 3)

    # Code installs required packages if needed
    pkgs_req <- c('tidyCDISC', 'purrr', 'haven', 'dplyr', 'stringr', 'tidyr', 'gt')
    {install_text}
    
    if (utils::compareVersion(as.character(utils::packageVersion('tidyCDISC')), '{packageVersion('tidyCDISC')}') < 0) {{
      install.packages('remotes')
      remotes::install_github('Biogen-Inc/tidyCDISC')
    }}
    
    library(tidyCDISC)
    library(purrr)
    library(haven)
    library(dplyr)

    {create_script_data()}
    pre_adsl <- tidyCDISC::prep_adsl(datalist$ADSL, input_recipe = '{RECIPE()}')
    {adae_expr()}
    bds_data <- datalist %>% tidyCDISC::prep_bds(ADSL = pre_adsl$data)
        
    {data_to_filter_expr()}
    {filter_pop_expr()}
    {filter_bds_expr()}
    {filter_ae_expr()}
        
    # get drop zone area from tidyCDISC
    # and create table using data
    blockData <- {prep_blocks(blocks_and_functions())}
    
    {df_empty_expr()}
    "
    )
  })
  
  # create the total column names
  total_for_code <- reactive({
    if (!!input$COLUMN == 'NONE') {
      glue::glue("
        total_df <- {Rscript_use_preferred_pop_data()} %>% 
        distinct(USUBJID) %>% 
        summarise(n_tot = n(), .groups='drop_last')
        
        col_total <- total_df$n_tot
        ")
    } else {
      glue::glue(
        "
        all <- {Rscript_use_preferred_pop_data()} %>% 
        distinct(USUBJID) %>% 
        summarise(n_tot = n(), .groups='drop_last') %>%
        mutate({input$COLUMN} = 'Total') 
        
        grp_lvls <- tidyCDISC::get_levels({Rscript_use_preferred_pop_data()}[['{input$COLUMN}']])
        xyz <- data.frame(grp_lvls) %>%
            rename_with(~paste('{input$COLUMN}'), grp_lvls)

        groups <- 
          xyz %>%
          left_join(
            {Rscript_use_preferred_pop_data()} %>%
            group_by({input$COLUMN}) %>%
            distinct(USUBJID) %>%
            summarise(n_tot = n(), .groups='drop_last')
          ) %>%
          mutate(n_tot = tidyr::replace_na(n_tot, 0)) 
        
        total_df <- bind_rows(groups, all)
        col_total <- total_df$n_tot
        "
      )
    }
  })
  
  generate_table_output <- reactive({
    
    glue::glue(
      "
      {text_code()}
      
      # Calculate totals for population set
      {total_for_code()}
      
      tg_datalist <- list({ifelse(adae_expr() == '', '', 'ADAE = ae_data, ')}ADSL = bds_data, POPDAT = {Rscript_use_preferred_pop_data()})
      
      tg_table <- tidyCDISC::tg_gt(tg_datalist, blockData, total_df, {column() %quote% 'NULL'})
      
      # get the column names for the table
      col_names <- names(tg_table)[-c(1:2)]
    
      # create the gt output
      library(gt)
      tg_table %>%
          gt(groupname_col = 'ID') %>%
          fmt_markdown(columns = c(Variable),
                 rows = stringr::str_detect(Variable,'&nbsp;') |
                   stringr::str_detect(Variable,'<b>') |
                   stringr::str_detect(Variable,'</b>')) %>%
          tab_options(table.width = px(700)) %>%
          cols_label(.list = tidyCDISC::col_for_list_expr(col_names, col_total)) %>%
          tab_header(
            title = md('{input$table_title}'),
            subtitle = md(\"{subtitle_html()}\")
          ) %>%
          tab_style(
          style = cell_text(weight = 'bold'),
          locations = cells_row_groups()
          ) %>%
          cols_label(Variable = '') %>%
          tidyCDISC::std_footnote({footnote_src()}) %>%
          tab_footnote('{input$table_footnote}')
      "
    )
  })
  
  
  generate_comparison_output <- reactive({
    glue::glue(
      "
      {text_code()}
      
      # Calculate totals for population set
      {total_for_code()}
      
      tg_table <- purrr::pmap(list(
              blockData$agg,
              blockData$S3,
              blockData$dropdown,
              blockData$dataset), 
          function(x,y,z,d) tidyCDISC::app_methods(x,y,z, 
                       group = {column() %quote% 'NULL'}, 
                       data = tidyCDISC::data_to_use_str(d, ae_data, bds_data),
                       totals = total_df)) %>%
      map(setNames, tidyCDISC::common_rownames({Rscript_use_preferred_pop_data()}, {column() %quote% 'NULL'})) %>%
      setNames(paste(blockData$gt_group)) %>%
      bind_rows(.id = 'ID') %>%
      mutate(
        # remove html
        Variable = gsub('<b>','', gsub('</b>','', gsub('&nbsp;',' ', Variable)))
      ) %>%
      left_join(
        blockData %>% 
          select(ID = gt_group, block, label, label_source, dropdown) %>%
          distinct()
      )
    
      # read in SAS table and convert to DF
      sas_data_dir <- 'path/to/sas/table/dataset/'
      sas_filename <- '{input$sas$name}'
      sas_table <- haven::read_sas(file.path(sas_data_dir, sas_filename))
      
      # prepare SAS table for comparison
      sas_comp_ready <- tidyCDISC::prep_sas_table(sas_data = sas_table,
                     # block_names = 'by1lbl',
                     # block_ord_names = 'cat',
                     # stat_names = 'by2lbl',
                     # stat_ord_names = 'subcat',
                     tg_data = tg_table,
                     machine_readable = TRUE,
                     keep_orig_ids = FALSE,
                     rm_desc_col = TRUE
      )

      # prepare TG Table for comparison
      colx <- names(sas_table)[stringr::str_detect(names(sas_table), '^col[0-9]')]
      tg_comp_ready <- tidyCDISC::prep_tg_table(data = tg_table,
                                           machine_readable = TRUE,
                                           keep_orig_ids = FALSE,
                                           rm_desc_col = TRUE,
                                           generic_colnames = colx
                                           )
      
      # Compare the two tables...
      library(diffdf)
      
      # ... by location in the tables
      output_file <- file.path(sas_data_dir,paste0(
        gsub('[^[:alnum:]]','_',gsub(tools::file_ext(sas_filename),'',sas_filename)),
        'v_tidyCDISC.log')) 
      order_diff <- diffdf(base = sas_comp_ready, 
              compare = tg_comp_ready,
              keys = c('id_block', 'id_rn'),
              tolerance = 0.01,
              strict_numeric = TRUE, # Integer != Double
              strict_factor = TRUE,  # Factor != Character
              file = output_file
      )
      file.edit(output_file) # open log file
      
      # # Additional functions to review differences
      # diffdf_has_issues(order_diff) # any issues?
      # 
      # # which rows have an issue in each data frame
      # diffdf_issuerows(sas_comp_ready, order_diff)
      # diffdf_issuerows(tg_comp_ready, order_diff)
      
      "
    )
  })
  
  observeEvent(input$sas, {
    shinyjs::enable("code")
  })
  
  output$code <- downloadHandler(
      filename = function() {
        paste0("Compare_tidyCDISC_v_SASTables_Code.R")
      },
      content = function(file) {
        writeLines(generate_comparison_output(), file)
      }
    ) 

  output$tblcode <- downloadHandler(
    filename = function() {
      paste0("Reproduce_tidyCDISC_Table.R")
    },
    content = function(file) {
      writeLines(generate_table_output(), file)
    }
  ) 

  # return the block area to be created in app_ui
  p <- reactive({ rowArea(col = 12, block_data()) })
  
  return(p)
  
}

