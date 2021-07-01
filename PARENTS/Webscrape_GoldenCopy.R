###################################################
######  Golden Copy Webscrape               ######
######  Oliver Ashtari Tafti                ######
######  Created 24 June 2021                ######
###################################################
######  INSTRUCTIONS                          #####
######                                        #####
######                                        #####
###################################################

## Load libraries
library("httr")
library("jsonlite")
library("XML")
library("rvest")
library("sjmisc")
library("stringr")
library("dplyr")

## Set working directory
setwd("/Users/oliverashtari/desktop/Liliana/PARENTS/RAW")

## Create end of month dates, from 2018-02 (when data starts) to today
# note Concatednated available from May 2017
eom <-  format(seq(as.Date("2018-03-01"), 
                   as.Date("2021-01-01"), by="7 day") - 1,"%Y%m%d")

## Create URLs
url_first <- "https://goldencopy.gleif.org/api/v2/golden-copies/publishes/rr/"
url_all   <- c(gsub(" ", "", paste(url_first, eom, "-0000.csv")))

## Loop over all URLs
for (u in url_all) {
                    if (file.exists(basename(u))) {
                      next
                    }
  
        download.file(u, basename(u), mode = "wb")
        unzip(basename(u))
}
