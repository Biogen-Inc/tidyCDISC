tableGenerator <- function(input, output, session, datafile = reactive(NULL)) {
  
  output$title <- renderText({ 
    # paste the title to the top of the table
    # paste reactive filtering expression as subheader
    paste("<b>", input$table_title, "</b><br>", subheader()) 
  })
  
  subheader <- reactive({
    # change condition to word 
    if (input$condition == "==") {
      condition <- "Equals"
    } else if (input$condition == "!=") {
      condition <- "Not Equal To"
    } else if (input$condition == "<") {
      condition <- "Less Than"
    } else if (input$condition == "<=") {
      condition <- "Less Than or Equal To"
    } else if (input$condition == ">") {
      condition <- "Greater Than"
    } else if (input$condition == ">=") {
      condition <- "Greater Than or Equal To"
    }
    
    if (input$to_filter == "No") {
      subheader <- ""
    } else {
      subheader <- 
        paste("Where", input$filtering, condition, input$filt_grp)
    }
  })
  
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
    selectInput(session$ns("filtering"), "Filter By:", colnames(datafile()$ADSL))
  })

  observe({
    req(input$filtering)
    x <- input$filtering
    if (is.na(x)) x <- character(0)
    t <- datafile()$ADSL %>% select(!!sym(input$filtering)) %>% distinct(!!sym(x))
    updateSelectInput(session, "filt_grp", choices = t)
  })
  
  observe({
    req(input$recipe)
    x <- input$recipe
    if (x == "DEMOGRAPHY") {
      updateRadioGroupButtons(session, "COLUMN", "Group Data By:", choices = c("TRT01P", "SEX", "RACE", "NONE"), selected = "TRT01P")
    } else {
      updateRadioGroupButtons(session, "COLUMN", "Group Data By:", choices = c("TRT01P", "SEX", "RACE", "NONE"), selected = "none")
    }
  })
  
  AGGREGATE <- reactive({
    req(length(input$agg_drop_zone) > 0 & !(is.na(input$agg_drop_zone)))
    as.data.frame(read_html(input$agg_drop_zone) %>% html_table(fill=TRUE)) %>%
      separate(1, into = c("Aggregate", "Select"), sep=":")
  })
  
  ROWS <- reactive({
    req(length(input$block_drop_zone) > 0 & !(is.na(input$block_drop_zone)))
    as.data.frame(read_html(input$block_drop_zone) %>% html_table(fill=TRUE))
  })
  
  blocks <- reactive({

    if (nrow(AGGREGATE()) != nrow(ROWS())) {
      if (nrow(AGGREGATE()) < nrow(ROWS())) {
        stop(call. = FALSE, "Missing statistic block")
      } else {
        stop(call. = FALSE, "Missing row block")
      }
    } else if (is.na(input$block_drop_zone & is.na(input$agg_drop_zone))) {
      stop(call. = FALSE, "Add blocks to dropzone to create tables")
    } else {
      t <- AGGREGATE()
      p <- ROWS()
      t$Row <- p$X1
      return(t)
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
        select(USUBJID, SUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG, data_from) %>% 
        distinct(USUBJID, AVISITN, AVISIT, PARAMCD, .keep_all = TRUE) 
      
      # Join ADSL and all_PARAMCD
      combined_data <- inner_join(ADSL(), all_PARAMCD, by = "USUBJID")
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
  
  all_data <- reactive({
    if (input$to_filter == "No") {
      processed_data()
    } else {
      processed_data() %>%
        dplyr::filter(!!filtering_expr(input))
    }
  })
  
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
  
  total <- reactive({ 
      all_data() %>% 
        distinct(USUBJID) %>%
        summarise(n = n())
  })
  
  datalist <- list()
    
  dataFrame <- reactive({
    
  COLUMN <- ifelse(input$COLUMN == "NONE", "", sym(input$COLUMN))
  # extract the row blocks and agg blocks
  # then convert to syms to be used within tidy pipelines
  for (i in 1:nrow(blocks())) {
    
    ROW <- sym(blocks()$Row[i])
    AGG <- sym(blocks()$Aggregate[i])
    WEEK <- trimws(blocks()$Select[i])
    
    # using a nested if else we will query
    # if mean block
    ### without column grouping
    ### with column grouping
    # if FREQ block
    ### without column grouping
    ### with column grouping
    # if t-test block
    ### without column grouping
    ### with column grouping
    
    # search for mean in the block name
    # we can't just search for mean because
    # the blocks have unique ids, mean.1, mean.2 etc.
    if (AGG == "MEAN") {
      
      if (as.character(ROW) %in% PARAMCD_names()) {
        
        variables <- c(ROW, COLUMN, sym("USUBJID"))
        variables <- variables[variables != ""]
        
        if (COLUMN == sym("")) {
          df <- all_data() %>%
            filter(PARAMCD == ROW & AVISIT == WEEK) %>%
            summarise(N = n(),
                      `Mean (SD)` = paste0(round(mean(AVAL), 2), " (", round(sd(AVAL), 2), ")"),
                      Median = median(AVAL),
                      `Q1 | Q3` = paste(round(quantile(AVAL, 0.25), 2) , "|", round(quantile(AVAL, 0.75), 2)),
                      `Min | Max` = paste(round(min(AVAL), 2), " | ", round(max(AVAL), 2)))
          tdf <- setNames(data.frame(t(df[,-1])), paste0("Total (N  = ", total(), ")"))
        } else {
          df <- all_data() %>%
            filter(PARAMCD == ROW & AVISIT == WEEK) %>%
            group_by(!!COLUMN) %>%
            summarise(N = n(),
                      `Mean (SD)` = paste0(round(mean(AVAL), 2), " (", round(sd(AVAL), 2), ")"),
                      Median = median(AVAL),
                      `Q1 | Q3` = paste(round(quantile(AVAL, 0.25), 2) , "|", round(quantile(AVAL, 0.75), 2)),
                      `Min | Max` = paste(round(min(AVAL), 2), " | ", round(max(AVAL), 2)))
          
          header_df <- all_data() %>%
            distinct(USUBJID, !!COLUMN) %>%
            group_by(!!COLUMN) %>%
            summarise(count = n())
          
          header_df %>% mutate_if(is.factor, as.character) -> header_df
          tdf <- setNames(data.frame(t(df[,-1])), lapply(paste0(unlist(header_df[,1]), 
                                                                " (N = ", 
                                                                unlist(header_df[,2]), ")"),
                                                         CapStr))
        }
        
      } else {
        
        variables <- c(ROW, COLUMN, sym("USUBJID"))
        variables <- variables[variables != ""]
        
        df <- all_data() %>%
          # if it's not a PARAMCD, it's from ADSL
          # which doesn't have an AVISIT column
          # filter(AVISIT == WEEK) %>%
          filter(!is.na(!!ROW)) %>%
          group_by(!!COLUMN) %>%
          summarise(N = n(),
                    `Mean (SD)` = paste0(round(mean(!!ROW), 2), " (", round(sd(!!ROW), 2), ")"),
                    Median = median(!!ROW),
                    `Q1 | Q3` = paste(round(quantile(!!ROW, 0.25),2) , "|", (round(quantile(!!ROW, 0.75),2))),
                    `Min | Max` = paste0(round(min(!!ROW), 2), " | ", round(max(!!ROW), 2)))
        if (COLUMN == "") {
          # if there's no grouping factor we set 
          # the single column name to Total (N = X)
          # where X was calculated as a reactive above
          tdf <- setNames(data.frame(t(df[,-1])), paste0("Total (N  = ", total(), ")"))
        } else {
          # create a header_df where we group the data by COLUMN
          # to get their totals to be used within the column headers
          header_df <- all_data() %>%
            distinct(USUBJID, !!COLUMN) %>%
            group_by(!!COLUMN) %>%
            summarise(count = n())
          header_df %>% mutate_if(is.factor, as.character) -> header_df
          tdf <- setNames(data.frame(t(df[,-1])), lapply(paste0(unlist(header_df[,1]), 
                                                                " (N = ", 
                                                                unlist(header_df[,2]), ")"),
                                                         CapStr))
        }
        
      }
      
      insert <- data.frame(t(data.frame("X" = c(rep(" ", length(tdf))))))
      row.names(insert) <- paste(CapStr(as.character(ROW)))
      colnames(insert) <- colnames(tdf)
      data <- rbind(insert, tdf)
      datalist[[i]] <- rownames_to_column(data, "row_name")
      
    } else if (AGG == "FREQ") {
      # without a column to group by
      # count the row block instances and create a prop table
      # then mutate the prop table 
      # so rather than have two columns n and prop
      # we can have one column with the value "n (prop)"
      if (COLUMN == "") {
        df <-
          all_data() %>%
          distinct(USUBJID, !!ROW) %>%
          count(!!ROW) %>%
          group_by(!!ROW) %>%
          summarise(n = sum(n)) %>%
          ungroup() %>%
          mutate(prop = n/sum(n)) %>%
          mutate(x = paste0(n, " (", round(prop, 2), ")")) %>%
          select(!!ROW, x)
        
        # make the row variable the row names
        d <- textshape::column_to_rownames(df, loc = 1)
        # and convert the column name to have to total N displayed
        colnames(d) <- paste0("Total (N  = ", total(), ")")
        
      } else {
        
        # as above create the same prop table
        # but also group by columns
        # turn n and prop into a single column with value "N (prop)"
        # then spread the dataframe so the column values are columns
        df <-
          all_data() %>%
          distinct(USUBJID, !!ROW, !!COLUMN) %>%
          count(!!ROW, !!COLUMN) %>%
          group_by(!!ROW) %>%
          mutate(prop = prop.table(n)) %>%
          mutate(v1 = paste0(n, ' (', round(prop, 2), ')')) %>%
          select(-n, -prop) %>% 
          spread(!!COLUMN, v1)
        
        # make the unique row values row names
        d <- textshape::column_to_rownames(df, loc = 1)
        
        # use the entire dataset to get the N for each column
        header_df <- 
          all_data() %>%
          distinct(USUBJID, !!COLUMN) %>%
          group_by(!!COLUMN) %>%
          summarise(count = n())
        
        # use the header_df to replace the column names to add the N
        
        if (input$COLUMN %in% blocks()$Row) {
          stop(call. = FALSE, "Cannot aggregate and group by ", paste(input$COLUMN))
        } else {
          colnames(d) <- lapply(paste0(unlist(header_df[,1]),
                                       " (N = ", 
                                       unlist(header_df[,2]), ")"),
                                CapStr)
        }
      }
      
      # add an empty row into the dataframe
      # with the row block name as its value
      # append the rowname dataframe to actual data above
      insert <- data.frame(t(data.frame("X" = c(rep(" ", length(d))))))
      row.names(insert) <- CapStr(as.character(ROW))
      colnames(insert) <- colnames(d)
      data <- rbind(insert, d)
      colnames(data) <- lapply(colnames(data), CapStr)
      datalist[[i]] <-  rownames_to_column(data, var = "row_name")
      
    } else if (AGG == "T-TEST") {
      # withot a column block the t-test block returns NA
      # because you can't write a t-test without a comparison!
      if (COLUMN == "") {
        
        d <- data.frame(TTEST = NA)
        colnames(d) <- paste0("Total (N  = ", total(), ")")
        row.names(d) <- "T-Test"
        
      } else {
        # get the total Ns for each column group
        # to use within the column names below
        
        header_df <- 
          all_data() %>%
          distinct(USUBJID, !!COLUMN) %>%
          group_by(!!COLUMN) %>%
          summarise(count = n())
        
        # take the filtered data, then filter even further
        # by removing NAs and using only the week selected from dropdown
        if (as.character(ROW) %in% PARAMCD_names()) {
          all_dat <- all_data() %>%  filter(PARAMCD == ROW & AVISIT == WEEK)
          ttest <- janitor::tidy(aov(all_dat$AVAL ~ all_dat[[paste(COLUMN)]], data=all_dat))
        } else {
          all_dat <- all_data() %>% filter(AVISIT == WEEK)
          ttest <- janitor::tidy(aov(all_dat[[paste(ROW)]] ~ all_dat[[paste(COLUMN)]], data=all_dat))
        }
        
        # use broom for an ANOVA
        
        # need to make a dataframe with length of columns 
        # and the values are NA except for the last column which is the p.value
        
        # Group 1 | Group 2 | Group 3
        #   NA        NA      p-value
        
        len = length(unique(all_data()[[COLUMN]])) - 1
        d <- data.frame(t(data.frame("X" = c(rep(" ", len), round(ttest$p.value[1], 3)))))
        row.names(d) <- "P-Value"
        colnames(d) <- lapply(paste0(unlist(header_df[,1]), " (N = ", unlist(header_df[,2]), ")"), CapStr) 
        
      }
      # set datalist for ttest to either 
      # if there was a column selected or not
      # add an empty row into the dataframe
      # with the row block name as its value
      # append the rowname dataframe to actual data above
      insert <- data.frame(t(data.frame("X" = c(rep(" ", length(d))))))
      row.names(insert) <- CapStr(as.character(ROW))
      colnames(insert) <- colnames(d)
      data <- rbind(insert, d)
      colnames(data) <- lapply(colnames(data), CapStr)
      datalist[[i]] <- rownames_to_column(data, var = "row_name")
      
    } else {
      if (COLUMN == "") {
        # CHG from Baseline total
        # use the filtered dataset where we pivot CHG rather than AVAL
        # get the mean change from baseline given the selected row block
        # and filter based on week
        
        intermediate <- all_data() %>%
          filter(PARAMCD == ROW & AVISIT == WEEK) %>%
          summarise(mean = round(mean(CHG), 3))
        # use the mean in wide format for table
        # add column name showing the total N
        d <- data.frame(CHG = intermediate$mean)
        colnames(d) <- paste0("Total (N  = ", total(), ")")
        row.names(d) <- "Change from Baseline"
      } else {
        
        # CHG from Baseline total
        # as above but now we group by the column block
        intermediate <- all_data() %>%
          group_by(!!COLUMN) %>% 
          filter(AVISIT == WEEK & PARAMCD == ROW) %>%
          summarise(mean = round(mean(CHG), 3))
        
        # using the entire dataset we get the column counts to append to column names
        header_df <- 
          all_data() %>%
          distinct(USUBJID, !!COLUMN) %>%
          group_by(!!COLUMN) %>%
          summarise(count = n())
        
        # rather than error, if there's no change display NA/blank
        ifelse(
          (nrow(intermediate) > 0),
          d <- data.frame(t(data.frame("X" = intermediate$mean))),
          d <- data.frame(t(data.frame("X" = c(rep(" ", length(unique(datafile()[[COLUMN]])))))))
        )
        
        # add proper rownames and column names
        row.names(d) <- "Change from Baseline"
        colnames(d) <- lapply(paste0(unlist(header_df[,1]), " (N = ", unlist(header_df[,2]), ")"), CapStr)
      }
      
      # add an empty row into the dataframe
      # with the row block name as its value
      # append the rowname dataframe to actual data above
      # need to use rbind.fill instead of rbind here, not sure why...
      insert <- data.frame(t(data.frame("X" = c(rep(" ", length(d))))))
      row.names(insert) <- CapStr(as.character(ROW))
      colnames(insert) <- colnames(d)
      data <- plyr::rbind.fill(insert, d)
      rownames(data) <- c(ROW, "Change from Baseline")
      colnames(data) <- lapply(colnames(data), CapStr)
      datalist[[i]] <- rownames_to_column(data, var = "row_name")
    }
  }
  big_data = do.call(rbind, datalist)
  big_data
  })
  
  
  
  output$all <- renderTable({
    dataFrame()
  })
  
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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("TableGenerator.csv")
    },
    content = function(file) {
      write.csv(dataFrame(), file, row.names= FALSE)
    }
  )
  
  # output$downloadXPT <- downloadHandler(
  #   filename = function() {
  #     paste("TableGenerator.xpt")
  #   },
  #   content = function(file) {
  #     # remove the spe
  #     df_remove_special_char <- janitor::clean_names(dataFrame())
  #     colnames(df_remove_special_char) <- 
  #       janitor::make_clean_names(str_trunc(colnames(df_remove_special_char), 
  #                                           8, side = "right"))
  #     write_xpt(df_remove_special_char, file)
  #   }
  # )
  
  # output$downloadSAS <- downloadHandler(
  #   filename = function() {
  #     paste("TableGenerator_", Sys.Date(), ".sas7bdat", sep = "")
  #   },
  #   content = function(file) {
  #     df_remove_special_char <- janitor::clean_names(dataFrame())
  #     write_sas(df_remove_special_char, file)
  #   }
  # )
  
  output$downloadRTF <- downloadHandler(
    filename = function() {
      paste("TableGenerator_", Sys.Date(), ".doc", sep = "")
    },
    content = function(file) {
      df <- as.data.frame(dataFrame())
      rtffile <- RTF(file)
      addHeader(rtffile, title = input$table_title, subtitle = subheader())
      addTable(rtffile, df)
      done(rtffile)
    }
  )
  
  output$downloadPDF = downloadHandler(
    filename = "TableGenerator.pdf",
    content = function(file){
      out <- rmarkdown::render("kable.Rmd", pdf_document())
      file.rename(out, file)
    }
  )
  
  p <- reactive({
    rowArea(col = 2, block_data())
    })
  
  return(p)
}