# Packages ----------------------------------------------------------------
library(rvest)
library(tidyverse)

statcan_url <- "https://www12.statcan.gc.ca/"
base_url <- "census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=F&SearchText="
postal_code <- "g1r"

url <- paste0(statcan_url, base_url, postal_code)

csv_url <- rvest::read_html(url) %>% 
  html_element(css = "a.mrgn-tp-sm:nth-child(1)") %>% 
  html_attr("href")

full_csv_url <- paste0(statcan_url, csv_url)

test <- read.csv(full_csv_url)

download.file(full_csv_url, destfile = "fichier.csv", method = "curl")
