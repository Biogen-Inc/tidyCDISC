tableGenerator <- function(input, output, session, datafile = reactive(NULL)) {
  
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
    t <- datafile()$ADSL %>% select(!!sym(input$filtering)) %>% distinct(!!sym(x))
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
    
    PARAMCD <- map(BDS(), ~ if(!"CHG" %in% names(.)) update_list(., CHG = NA) else .)
    
    if (!is_empty(PARAMCD)) {
      # Bind all the PARAMCD files 
      all_PARAMCD <- bind_rows(PARAMCD, .id = "data_from")  %>% 
        arrange(SUBJID, AVISITN, PARAMCD) %>% 
        select(USUBJID, SUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG, data_from)
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
  
  AVISITN <- reactive({ 
    req(BDS())
    t <- sort(unique(unlist(lapply(BDS(), '[[', "AVISIT"))))
    ifelse((length(t) == 0), t <- " ", t <- t)
    t
  })
  
  observe({
    req(AVISITN())
    session$sendCustomMessage("my_data", AVISITN())
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
      return(nrow(all_data()))
    } else {
      return(all_data() %>%
        group_by(!!sym(input$COLUMN)) %>%
        summarise(n = n()) %>%
        pull(n))
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
  
  gt_table <- reactive({
    for_gt() %>%
      gt(rowname_col = "Variable", 
         groupname_col = "ID") %>%
      tab_options(table.width = px(700)) %>%
      cols_label(.list = imap(for_gt()[-c(1:2)], ~col_for_list(.y, .x))) %>%
      tab_header(
        title = md(input$table_title),
        subtitle = "Filtering logic"
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
    rowArea(col = 2, block_data())
    })
  
  return(p)
}