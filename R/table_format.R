tdf <- setNames(data.frame(t(df[,-1])), paste0("Total (N  = ", total(), ")"))


header_df <- all_data() %>%
  distinct(USUBJID, !!COLUMN) %>%
  group_by(!!COLUMN) %>%
  summarise(count = n())

header_df %>% mutate_if(is.factor, as.character) -> header_df

tdf <- setNames(data.frame(t(df[,-1])), lapply(paste0(unlist(header_df[,1]), 
                                                      " (N = ", 
                                                      unlist(header_df[,2]), ")"),
                                               CapStr))

tdf <- setNames(data.frame(t(df[,-1])), paste0("Total (N  = ", total(), ")"))

header_df <- all_data() %>%
  distinct(USUBJID, !!COLUMN) %>%
  group_by(!!COLUMN) %>%
  summarise(count = n())

header_df %>% mutate_if(is.factor, as.character) -> header_df

if (!nrow(df) == nrow(header_df)) {
  test <- data.frame(matrix(nrow = 1, ncol = ncol(df)))
  colnames(test) <- colnames(df)
  df <- rbind(test, df)
  tdf <- setNames(data.frame(t(df)), lapply(paste0(unlist(header_df[,1]), 
                                                   " (N = ", 
                                                   unlist(header_df[,2]), ")"),
                                            CapStr))
} else {
  tdf <- setNames(data.frame(t(df[,-1])), lapply(paste0(unlist(header_df[,1]), 
                                                        " (N = ", 
                                                        unlist(header_df[,2]), ")"),
                                                 CapStr))
}

insert <- data.frame(t(data.frame("X" = c(rep(" ", length(tdf))))))
row.names(insert) <- paste(CapStr(as.character(ROW)))
colnames(insert) <- colnames(tdf)
data <- rbind(insert, tdf)
datalist[[i]] <- rownames_to_column(data, "row_name")

# make the row variable the row names
d <- textshape::column_to_rownames(df, loc = 1)
# and convert the column name to have to total N displayed
colnames(d) <- paste0("Total (N  = ", total(), ")")

# use the entire dataset to get the N for each column
header_df <- 
  all_data() %>%
  distinct(USUBJID, !!COLUMN) %>%
  group_by(!!COLUMN) %>%
  summarise(count = n())

colnames(d) <- lapply(paste0(unlist(header_df[,1]),
                             " (N = ", 
                             unlist(header_df[,2]), ")"),
                      CapStr)

insert <- data.frame(t(data.frame("X" = c(rep(" ", length(d))))))
row.names(insert) <- CapStr(as.character(ROW))
colnames(insert) <- colnames(d)
data <- rbind(insert, d)
colnames(data) <- lapply(colnames(data), CapStr)
datalist[[i]] <-  rownames_to_column(data, var = "row_name")

header_df <- 
  all_data() %>%
  distinct(USUBJID, !!COLUMN) %>%
  group_by(!!COLUMN) %>%
  summarise(count = n())

len = length(unique(all_data()[[COLUMN]])) - 1
d <- data.frame(t(data.frame("X" = c(rep(" ", len), round(ttest$p.value[1], 3)))))
row.names(d) <- "P-Value"
colnames(d) <- lapply(paste0(unlist(header_df[,1]), " (N = ", unlist(header_df[,2]), ")"), CapStr) 

insert <- data.frame(t(data.frame("X" = c(rep(" ", length(d))))))
row.names(insert) <- CapStr(as.character(ROW))
colnames(insert) <- colnames(d)
data <- rbind(insert, d)
colnames(data) <- lapply(colnames(data), CapStr)
datalist[[i]] <- rownames_to_column(data, var = "row_name")

d <- data.frame(CHG = intermediate$mean)
colnames(d) <- paste0("Total (N  = ", total(), ")")
row.names(d) <- "Change from Baseline"

all_data() %>%
  distinct(USUBJID, !!COLUMN) %>%
  group_by(!!COLUMN) %>%
  summarise(count = n())

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