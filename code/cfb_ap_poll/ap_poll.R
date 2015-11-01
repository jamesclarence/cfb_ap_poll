library(rvest)
library(dplyr)
library(stringr)

#### Get Each Weekly AP Poll's URL from collegepollarchive.com

## To get each URL:
# 1. Split the whole URL in 3 parts
# 2. Replace 2nd part with new ascending number
# 3. Combine the three parts

poll_url1 <- "http://collegepollarchive.com/football/ap/seasons.cfm?appollid="
poll_url2 <- "32"
poll_url3 <- "#.VihxlhCrSRs"

# poll_url2 goes up to 1096 (Oct 25, 2015)
# 32:1096

poll_number <- data.frame(32:1096)

poll_url_begin <- rep(poll_url1, length = nrow(poll_number))
poll_url_end <- rep(poll_url3, length = nrow(poll_number))
poll_url <- cbind(begin = poll_url_begin, num = poll_number, end = poll_url_end) %>%
                sapply(as.character)

# Each Poll's URL
poll_url_all <- apply(poll_url, 1, paste, collapse="")

# A subset of poll_url_all
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

remove_na <- function(poll_table){
    # remove last row of NA
    df <- remove_na_row(poll_table)

    # remove last five columns (all NAs)
    df2 <- remove_na_columns(df)

    print(df2)
}

# Split date_length column into Month | Day | Year columns

# Split WLT column into W | L | T columns

# Change remaining NA column to Last Week
