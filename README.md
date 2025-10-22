# 🏈 NFL Analysis

Scraping and analyzing NFL data from [Pro-Football-Reference.com](https://www.pro-football-reference.com) to explore team and player trends across multiple seasons.  
This project automates data collection for **passing**, **receiving**, and **rushing** statistics from 2010–2024, preparing clean datasets for exploratory and predictive analysis.

---

## 🧠 Project Overview

This project was built to automate the extraction and analysis of NFL player statistics from **Pro-Football-Reference**.  
The resulting datasets can be used to:
- Track performance trends across seasons  
- Compare teams and player positions  
- Build machine learning or regression models for predictive insights  

Whether you’re a football fan, data analyst, or sports data engineer, this repository provides a clean foundation for exploring professional football analytics.

---

## ⚙️ Features

- 📊 Scrapes NFL **passing**, **receiving**, and **rushing** data (2010–2024)  
- 🧹 Cleans, merges, and standardizes player statistics  
- 💾 Outputs ready-to-analyze CSV datasets  
- 📈 Enables quick visual and statistical exploration in R or Python  

---

## 🚀 Getting Started

### Prerequisites

Ensure you have the following installed:
- **R (≥ 4.0)**  
- Required packages:  
  ```r
  install.packages(c("rvest", "tidyverse", "dplyr"))

---
---

## 🗂️ Data Collection

### Data Sources

All data is sourced from:
- [Pro-Football-Reference – Passing](https://www.pro-football-reference.com/years/2024/passing.htm)  
- [Pro-Football-Reference – Rushing](https://www.pro-football-reference.com/years/2024/rushing.htm)  
- [Pro-Football-Reference – Receiving](https://www.pro-football-reference.com/years/2024/receiving.htm)  

### Methodology

Data is scraped from yearly tables for each stat type using the **rvest** library in R:

```r
urlprefix <- "https://www.pro-football-reference.com/years/"
urlend <- '/passing.htm'
startyear <- 2010
endyear <- 2024
passing <- data.frame()

for (i in startyear:endyear) {
  url <- paste0(urlprefix, i, urlend)
  table <- url %>%
    read_html() %>%
    html_node("table") %>%
    html_table()
  table$Year <- i
  passing <- rbind(table, passing)
}

write.csv(passing, "pro-football-reference-2010-2024-passing.csv", row.names = FALSE)
```
---

# 📊 NFL Analysis – Results & Visuals

This document showcases the results and visual insights generated from the **NFL Analysis** project.  
The visuals highlight trends, distributions, and player performance patterns from data scraped via [Pro-Football-Reference.com](https://www.pro-football-reference.com).

---

## 🏈 Overview

Using the cleaned datasets (`passing`, `rushing`, and `receiving` stats from 2010–2024), we explore:

- Long-term performance trends by year  
- Team and player efficiency across positions  
- Distribution of yardage and touchdowns  
- Seasonal comparisons and career trajectories  

These graphics are designed to make league-wide patterns and player development more interpretable for analysts, scouts, and fans alike.

---

## 📈 Passing Trends

### Quarterback Passing Yards by Year

Shows the evolution of total passing yards across NFL seasons, highlighting the league’s shift toward high-volume passing offenses.

```r
ggplot(passing, aes(x = Year, y = Yds, color = Tm)) +
  geom_line(alpha = 0.7) +
  labs(title = "Quarterback Passing Yards by Year",
       x = "Season",
       y = "Passing Yards")
```
