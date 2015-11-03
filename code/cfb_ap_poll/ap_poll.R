source("ap_poll_urls.R")

library(dplyr)
library(stringr)

# A subset of poll_url_all from ap_poll_urls.R
poll_sub <- poll_url_all[20:25]

#####
# Function to get date from one poll's html
get_date <- function(url) {
    poll <- read_html(url)
    poll_date <- poll %>%
        html_node("td h2") %>%
        html_text()
    return(poll_date)
}

# Function to remove string from get_date
remove_ap_football_from_get_date <- function(url){
    # get the poll date string
    date <- get_date(url)

    # sub " AP Football Poll"
    date2 <- gsub(" AP Football Poll", "", date)
    date2
}

# Function that takes binds poll date and poll information
ap_table <- function(url){
    html <- read_html(url)
    date <- remove_ap_football_from_get_date(url)
    table <- html_table(html, fill = T)[[9]]
    date_length <- rep(date, length.out = nrow(table))
    poll <- cbind(date_length, table)
    print(poll)
}

# lapply(poll_sub, ap_table)

#### To Do Next ####
## Clean the tables

df <- poll_url_all[5] %>% ap_table()

# Remove last row (all NAs)
remove_na_row <- function(poll_table){
    poll_nrow <- nrow(poll_table)
    print(poll_nrow)
    poll_table[-(poll_nrow),]
}

# Remove last five columns (all NAs)
remove_na_columns <- function(poll_table){
    poll_length <- length(poll_table)
    first_na_column <- poll_length-4
    df <- poll_table[,-c(first_na_column:poll_length)]
    print(df)
}

# Combines remove_na_row & remove_na_columns functions
remove_na <- function(poll_table){
    # remove last row of NA
    df <- remove_na_row(poll_table)

    # remove last five columns (all NAs)
    df2 <- remove_na_columns(df)

    print(df2)
}

# Split date_length column into Month | Day | Year columns
split_date <- function(poll_table){
    x <- str_split_fixed(poll_table$date_length, ", ", 2)
    x2 <- str_split_fixed(x[,1], " ", 2)
    poll_date <- cbind(x2,x[,2])
    colnames(poll_date) <- c("month", "date", "year")
    print(poll_date)
}

# Split WLT column into W | L | T columns
split_wlt <- function(poll_table){
    x <- str_split_fixed(poll_table$WLT, "-", 3)
    colnames(x) <- c("win", "loss", "tie")
    print(x)
}

# Change remaining NA column to Last Week

# Team (FPV) slit into two columns
split_fpv <- function(poll_table){
    x <- str_split_fixed(df$`Team (FPV)`, "\r\n\t\t\t ", 2)
    x[,2] <- str_replace(x[,2],"\\(", "") %>% str_replace("\\)","")
    colnames(x) <- c("team", "fpv")
    print(x)
}
