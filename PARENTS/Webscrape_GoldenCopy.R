library("httr")
library("jsonlite")
library("XML")
library("rvest")
library("sjmisc")
library("stringr")
library("dplyr")

setwd("/Users/oliverashtari/desktop/Liliana/PARENTS/RAW")

## Create end of month dates, from 2017 (when data starts) to today BY 6 MONTHS 
# note Concatednated available from May 2017
eom <-  format(seq(as.Date("2018-03-01"), as.Date("2021-01-01"), by="1 month") - 1,"%Y%m%d")

## Create url links
url_first <- "https://goldencopy.gleif.org/api/v2/golden-copies/publishes/rr/"
url_all   <- c(gsub(" ", "", paste(url_first, eom, "-0000.csv")))

for (u in url_all) {

        download.file(u, basename(u), mode = "wb")
        unzip(basename(u))
  
  
}

