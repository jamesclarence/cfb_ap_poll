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
