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

NFL-Analysis/
│
├── Data/
│   ├── pro-football-reference-2010-2024-passing.csv
│   ├── pro-football-reference-2010-2024-receiving.csv
│   ├── pro-football-reference-2010-2024-rushing.csv
│
├── src/
│   ├── passing_scraper.R
│   ├── rushing_scraper.R
│   ├── receiving_scraper.R
│
├── visuals/
│   ├── passing_trends.png
│   ├── rushing_distribution.png
│
└── README.md
