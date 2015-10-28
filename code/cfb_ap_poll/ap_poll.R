library(rvest)
library(dplyr)
library(stringr)

# AP Polls

poll <- read_html("http://collegepollarchive.com/football/ap/seasons.cfm?appollid=32#.VihxlhCrSRs")

### 1 Get Date of Poll

# 1A Get Table with: Rank, Team, FPV, Conf, WLT, PTS

poll_table <- html_table(poll, fill = T)[[9]]

# Remove Columns With NA
poll_table[,-(8:12)]

# 1B Date Line
poll_date <- poll %>%
    html_node("td h2") %>%
    html_text()

# [1] "October 14, 1940 AP Football Poll"

# 1C Remove " AP Football Poll" from poll_date

date_only <- gsub(" AP Football Poll", "", poll_date)

# 1D Repeat date_only length of poll_table
date_only2 <- rep(date_only, length = nrow(poll_table))

# 1E Add date_only2 to poll_table
poll_table2 <- cbind(date_only2, poll_table)

# 1F Add week_number to poll_table

### Get Each Weekly AP Poll's URL

## Try #1:
    # Split URL in 3 parts
    # Replace 2nd part with new ascending Number
    # Combine Three parts

poll_url1 <- "http://collegepollarchive.com/football/ap/seasons.cfm?appollid="
poll_url2 <- "32"
poll_url3 <- "#.VihxlhCrSRs"

# poll_url2 goes up to 1096 (Oct 25, 2015)
# 32:1096

poll_number <- data.frame(32:1096)

# apply(1, as.character)

poll_url_begin <- rep(poll_url1, length = nrow(poll_number))
poll_url_end <- rep(poll_url3, length = nrow(poll_number))
poll_url <- cbind(begin = poll_url_begin, num = poll_number, end = poll_url_end) %>%
                sapply(as.character)

# Each Poll's URL
poll_url_all <- apply(poll_url, 1, paste, collapse="")

get_date <- function(url) {
    poll <- read_html(url)
    poll_date <- poll %>%
        html_node("td h2") %>%
        html_text()
    print(poll_date)
}

# all_polls function should have get_date function incorporated somehow
all_polls <- function(poll_list){
    df <- data.frame()
    for (i in poll_list)
        x <- read_html(i)
        x_table <- html_table(x, fill = T)[[9]]
        x_date <- x %>%
            html_node("td h2") %>%
            html_text()
        print(x_date)
        x_date2 <- gsub(" AP Football Poll", "", x_date)
        print(x_date2)
        x_date3 <- rep(x_date2, length.out = nrow(x))
        df <- cbind(x_date3, x_table)
        df
}

poll_sub <- poll_url_all[20:25]
