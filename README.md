# NFL-Analysis

## Web Scraping
To gather NFL statstics from 2010 - 2024, I pulled data from pro-football-reference.com:
### Web Scraping Quarterback Passing Data

```R

#### PASSING #####

rm(list = ls())

# install and load neccessary packages
library(rvest)
library(tidyverse)
library(dplyr)

# start of web scraping passing statistics 2010 - 2024
urlprefix <- "https://www.pro-football-reference.com/years/"
urlend <- '/passing.htm'
startyear <- 2010
endyear <- 2024

passing <- data.frame()
for (i in startyear:endyear){
  url <- paste(urlprefix, as.character(i), urlend, sep = "") 
  table <- url %>%
    read_html() %>%
    html_node("table") %>%
    html_table()
  table$Year <- i
  passing <- rbind(table, passing)
}

# save scraped data into new csv file
write.csv(passing, file = "pro-football-reference-2010-2024-passing.csv", row.names = TRUE)
```

### Web Scraping Receiver Data

```R
#### RECIEVING ####

rm(list = ls())

# install and load neccessary packages
library(rvest)
library(tidyverse)
library(dplyr)

# start of web scraping receiving statistics 2010-2024

urlprefix <- "https://www.pro-football-reference.com/years/"
urlend <- '/receiving.htm'
startyear <- 2010
endyear <- 2024

receiving <- data.frame()
for (i in startyear:endyear){
  url <- paste(urlprefix, as.character(i), urlend, sep = "")
  table <- url %>%
    read_html() %>%
    html_node("table") %>%
    html_table()
  table$Year <- i
  receiving <- rbind(table, receiving)
}

# transform column names before saving as csv file

colnames(receiving)[1] <- 'Rank'
colnames(receiving)[2] <- 'Player'
colnames(receiving)[3] <- 'Age'
colnames(receiving)[4] <- 'Team'
colnames(receiving)[5] <- 'Position'
colnames(receiving)[6] <- 'Games_Played'
colnames(receiving)[7] <- 'Games_Started'
colnames(receiving)[8] <- 'Targets'
colnames(receiving)[9] <- 'Receptions'
colnames(receiving)[10] <- 'Receiving_Yards'
colnames(receiving)[11] <- 'Yards_Per_Reception'
colnames(receiving)[12] <- 'Touchdowns'
colnames(receiving)[13] <- 'First_Down_Receptions'
colnames(receiving)[14] <- 'Receiving_Success_Rate'
colnames(receiving)[15] <- 'Longest_Reception'
colnames(receiving)[16] <- 'Receptions_Per_Game'
colnames(receiving)[17] <- 'Yards_Per_Game'
colnames(receiving)[20] <- 'Fumbles'

receiving <- receiving[-1, ]

# save scraped data into new csv file
write.csv(receiving, file = "pro-football-reference-2010-2024-receiving.csv", row.names = TRUE)

```

### Web Scraping Running Back Rushing Data

```R
#### RUSHING ####

rm(list = ls())


# install and load neccessary packages
library(rvest)
library(tidyverse)
library(dplyr)

# start of web scraping rushing statistics 2010-2024

urlprefix <- "https://www.pro-football-reference.com/years/"
urlend <- '/rushing.htm'
startyear <- 2010
endyear <- 2024

rushing <- data.frame()
for (i in startyear:endyear){
  url <- paste(urlprefix, as.character(i), urlend, sep = "")
  table <- url %>%
    read_html() %>%
    html_node("table") %>%
    html_table()
  table$Year <- i
  rushing <- rbind(table, rushing)
}

# change column names
rushing[1, ]

colnames(rushing)[1] <- 'Rank'
colnames(rushing)[2] <- 'Player'
colnames(rushing)[3] <- 'Age'
colnames(rushing)[4] <- 'Team'
colnames(rushing)[5] <- 'Position'
colnames(rushing)[6] <- 'Games_Played'
colnames(rushing)[7] <- 'Games_Started'
colnames(rushing)[8] <- 'Rushing_Attempts'
colnames(rushing)[9] <- 'Rushing_Yards'
colnames(rushing)[10] <- 'Rushing_Touchdowns'
colnames(rushing)[11] <- 'First_Downs_Rushing'
colnames(rushing)[12] <- 'Rushing_Success_Rate'
colnames(rushing)[13] <- 'Longest_Rush'
colnames(rushing)[14] <- 'Yards_Per_Carry'
colnames(rushing)[15] <- 'Rushing_Yards_Per_Game'
colnames(rushing)[16] <- 'Rushing_Attempts_Per_Game'
colnames(rushing)[17] <- 'Fumbles'

# drop first row
rushing <- rushing[-1, ]

# save scraped data into new csv file
write.csv(rushing, file = "pro-football-reference-2010-2024-rushing.csv", row.names = TRUE)


```
