#### PASSING - 2025 ONLY ####
# Clear workspace
rm(list = ls())

# Load necessary packages
library(rvest)
library(tidyverse)
library(dplyr)


# Define the URL for 2025
url <- "https://www.pro-football-reference.com/years/2025/passing.htm"

# Read and parse the passing table
passing_2025 <- url %>%
  read_html() %>%
  html_node("table") %>%
  html_table()

# Add year column
passing_2025$Year <- 2025

write.csv(passing_2025, file = "pro-football-reference-2025-passing.csv", row.names = TRUE)

