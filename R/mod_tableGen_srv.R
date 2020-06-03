#' The tableGen Function creates summary statistics using blocks and a gt table output
#'
#' @description tableGen module.
#'
#' @return \code{rowArea}, the UI needed to create blocks for each dataset
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @import dplyr
#' @import gt
#' 
#' @importFrom shiny renderUI observe req selectInput updateSelectInput renderUI reactive callModule observe session validate need downloadHandler
#' @importFrom rlang abort sym call2 as_quosure
#' @importFrom glue glue
#' @importFrom dplyr select distinct bind_rows arrange full_join mutate pull tibble summarise group_by
#' @importFrom gt gt tab_options cols_label tab_header tab_style render_gt gtsave
#' @importFrom IDEAFilter shiny_data_filter_ui
#' @importFrom IDEA rowArea filters_in_english
#' @importFrom tippy tippy
#' @importFrom purrr map pmap
#' 
mod_tableGen_server <- function(input, output, session, datafile = reactive(NULL)){
  # ns <- session$ns # this is not used in Maya's code!
  
  # output$title <- renderText( input$table_title )
  
  filtering_expr <- function(input) {
    column <- rlang::sym(input$filtering)
    operator <- allowed_operators[[input$condition]]
    if (is.null(operator)) {
      rlang::abort(glue::glue("Can't use operator `{input$condition}`"))
    }
    
    if (grepl("[A-Za-z]", datafile()$ADSL[[input$filtering]][1])) {
      value <- input$filt_grp
    } else {
      value <- as.numeric(input$filt_grp)
    }
    
    call <- rlang::call2(operator, column, value)
    rlang::as_quosure(call, env = emptyenv())
  }
  
  output$filtering_by <- renderUI({
    selectInput(session$ns("filtering"), "Filter Column:", colnames(datafile()$ADSL)[colnames(datafile()$ADSL) != "STUDYID"])
  })
  
  observe({
    req(input$filtering)
    x <- input$filtering
    if (is.na(x)) x <- character(0)
    t <- datafile()$ADSL %>% 
      dplyr::select(!!rlang::sym(input$filtering)) %>% 
      dplyr::distinct(!!rlang::sym(x))
    updateSelectInput(session, "filt_grp", choices = t)
  })
  
  
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
    
    PARAMCD <- purrr::map(BDS(), ~ if(!"CHG" %in% names(.)) update_list(., CHG = NA) else .)
    
    if (!is_empty(PARAMCD)) {
      # Bind all the PARAMCD files 
      all_PARAMCD <- dplyr::bind_rows(PARAMCD, .id = "data_from")  %>% 
        dplyr::arrange(USUBJID, AVISITN, PARAMCD) %>% 
        dplyr::select(USUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG, data_from)
      # distinct(USUBJID, AVISITN, AVISIT, PARAMCD, .keep_all = TRUE) 
      
      # Join ADSL and all_PARAMCD
      combined_data <- dplyr::full_join(ADSL(), all_PARAMCD, by = "USUBJID")
    } else {
      combined_data <- ADSL() %>%
        dplyr::mutate(data_from = "ADSL", PARAMCD = NA, AVAL = NA, CHG = NA)
    }
  })
  
  # get the list of PARAMCDs
  PARAMCD_names <- reactive({
    all_data() %>% 
      dplyr::select(PARAMCD) %>% 
      dplyr::distinct() %>%
      dplyr::pull(PARAMCD)
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
        dplyr::tibble(AVISIT = avisit_words(), AVISITN = avisit_fctr()) %>%
        dplyr::mutate(AVISIT = as.factor(AVISIT)) %>%
        dplyr::mutate(AVISIT = fct_reorder(AVISIT, AVISITN)) %>%
        dplyr::pull(AVISIT) %>%
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
      dplyr::distinct(USUBJID) %>%
      dplyr::summarise(n = n())
  })
  
  column <- reactive( if (input$COLUMN == "NONE") NULL else input$COLUMN)
  
  total <- reactive({
    if (input$COLUMN == "NONE") {
      all_data() %>% 
        dplyr::distinct(USUBJID) %>% 
        dplyr::summarise(n = n()) %>%
        dplyr::pull(n)
    } else {
      all_data() %>%
        dplyr::group_by(!!sym(input$COLUMN)) %>%
        dplyr::distinct(USUBJID) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::pull(n)
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
      IDEA:::filters_in_english(all_data()) 
    } else {
      " "
    }
  })
  
  
  gt_table <- reactive({
    for_gt() %>%
      gt::gt(rowname_col = "Variable", 
         groupname_col = "ID") %>%
      gt::tab_options(table.width = px(700)) %>%
      gt::cols_label(.list = imap(for_gt()[-c(1:2)], ~col_for_list(.y, .x))) %>%
      gt::tab_header(
        title = md(input$table_title),
        subtitle = md(subtitle())
      ) %>%
      gt::tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
      ) %>%
      gt::tab_style(
        style = list(
          cell_text(align = "right")
        ),
        locations = cells_stub(rows = TRUE)
      )
  })
  
  
  output$all <- gt::render_gt({ gt_table() })
  
  #####################################################################
  # Block Preperation
  #####################################################################
  
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
        gt::gtsave(exportHTML, file)
      }
    }
  )  
  
  
  p <- reactive({
    IDEA:::rowArea(col = 12, block_data())
  })
  
  return(p)
}

## To be copied in the server -- done
# callModule(mod_tableGen_server, "tableGen_ui_1")
