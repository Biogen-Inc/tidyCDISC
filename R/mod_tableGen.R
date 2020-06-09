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
#' @noRd 


mod_tableGen_server <- function(input, output, session, datafile = reactive(NULL)) {
  
  output$col_ADSL <- renderUI({
    x <- input$recipe
    if (is.null(x) | length(x) == 0) {
      selectInput(session$ns("COLUMN"), "Group Data By:", choices = c("NONE", colnames(ADSL())), selected = "NONE")
    } else if (x == "NONE") {
      selectInput(session$ns("COLUMN"), "Group Data By:", choices = c("NONE", colnames(ADSL())), selected = "NONE")
    } else {
      selectInput(session$ns("COLUMN"), "Group Data By:", choices = c("NONE", colnames(ADSL())), selected = "TRT01P")
    }
  })
  
  
  ######################################################################
  # Data Preperation
  ######################################################################
  
  ADSL <- reactive({ datafile()$ADSL })
  BDS <- reactive({ datafile()[sapply(datafile(), function(x) "PARAMCD" %in% colnames(x))] })
  
  processed_data <- reactive({ 
    
    # Seperate ADSL and the PArAMCD dataframes
    
    PARAMCD <- map(BDS(), ~ if(!"CHG" %in% names(.)) update_list(., CHG = NA) else .)
    
    if (!is_empty(PARAMCD)) {
      # Bind all the PARAMCD files 
      all_PARAMCD <- bind_rows(PARAMCD, .id = "data_from")  %>% 
        arrange(USUBJID, AVISITN, PARAMCD) %>% 
        select(USUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG, data_from)
      # distinct(USUBJID, AVISITN, AVISIT, PARAMCD, .keep_all = TRUE) 
      
      # Join ADSL and all_PARAMCD
      combined_data <- full_join(ADSL(), all_PARAMCD, by = "USUBJID")
    } else {
      combined_data <- ADSL() %>%
        mutate(data_from = "ADSL", PARAMCD = NA, AVAL = NA, CHG = NA)
    }
  })
  
  # get the list of PARAMCDs
  PARAMCD_names <- reactive({
    all_data() %>% 
      select(PARAMCD) %>% 
      distinct() %>%
      pull(PARAMCD)
  })
  
  all_data <- callModule(
    shiny_data_filter,
    "data_filter",
    data = processed_data,
    verbose = FALSE)
  
  avisit_words <- reactive({ 
    req("ADSL" %in% names(datafile()))
    processed_data()$AVISIT 
  })
  avisit_fctr  <- reactive({ 
    req("ADSL" %in% names(datafile()))
    processed_data()$AVISITN 
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
  
  
  #####################################################################
  # Table Generator
  #####################################################################
  
  blocks_and_functions <- reactive({
    convertTGOutput(input$agg_drop_zone, input$block_drop_zone) 
  })
  
  total <- reactive({ 
    all_data() %>% 
      distinct(USUBJID) %>%
      summarise(n = n())
  })
  
  column <- reactive( if (input$COLUMN == "NONE") NULL else input$COLUMN)
  
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
  
  for_gt <- reactive({
    validate(
      need((nrow(blocks_and_functions()) > 0),'Add variable and statistics blocks to create table.')
    )
    
    test <- pmap(list(blocks_and_functions()$agg, 
                      blocks_and_functions()$S3, 
                      blocks_and_functions()$dropdown), 
                 function(x,y,z) 
                   IDEA_methods(x,y,z, 
                                group = column(), 
                                data = all_data())) %>% 
      map(setNames, common_rownames(all_data(), column())) %>%
      setNames(paste(blocks_and_functions()$gt_group)) %>%
      bind_rows(.id = "ID") %>%
      mutate(
        ID = stringi::stri_replace_all_regex(
          ID, 
          pattern = "\\b"%s+%block_lookup()$Pattern%s+%"\\b",
          replacement = block_lookup()$Replacement,
          vectorize_all = FALSE))
  })
  
  row_names_n <- reactive({ names(for_gt())[-c(1:2)] })
  
  col_for_list <- function(nm, x) {
    if (is.numeric(all_data()[[input$COLUMN]])) {
      stop("Need categorical column for grouping")
    }
    nm = md(glue::glue("**{row_names_n()}** <br> N={total()}"))
  }
  
  subtitle <- reactive({
    if (any(regexpr("%>%", capture.output(attr(all_data(), "code"))) > 0)) {
      filters_in_english(all_data()) 
    } else {
      " "
    }
  })
  
  
  gt_table <- reactive({
    for_gt() %>%
      gt(rowname_col = "Variable", 
         groupname_col = "ID") %>%
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
  
  
  output$all <- render_gt({ gt_table() })
  
  #####################################################################
  # Block Preperation
  #####################################################################
  
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
  
  # rbind despite diffrent names, new names pattern replacement
  # then use those in gt
  block_lookup <- reactive({
    
    pretty_blocks <- tibble(
      Pattern = c("MEAN", "FREQ", "CHG"),
      Replacement = c("Descriptive Statistics", 
                      "Summary Counts", 
                      "Descriptive Statistics of Change from Baseline")
    )
    
    test <- block_data() %>%
      map(set_names, c("Pattern", "Replacement")) %>%
      bind_rows() %>%
      rbind(pretty_blocks)
  })
  
  ###############################
  # Download
  ###############################
  
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
  
  
  p <- reactive({
    rowArea(col = 12, block_data())
  })
  
  return(p)
 
}
 
