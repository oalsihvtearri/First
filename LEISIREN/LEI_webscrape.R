###################################################
######  SCRAPE ALL LEI ZIP FILES             ######
######  Oliver Ashtari Tafti                 ######
######  Created 10 June 2021                 ######
###################################################
######  INSTRUCTIONS                          #####
######  This file has two parts               #####
######  description is below                  #####
###################################################

library(XML)
library(rvest)
library(sjmisc)
library(stringr)
library(dplyr)

###################################################
######  PART 1: read website and identify all #####
######  filenames "fullfile"                  #####
###################################################

## Set working directory
setwd("/Users/oliverashtari/desktop/Liliana/LEISIREN/RAW")

## URL for download
url <- "https://lei-france.insee.fr/telechargement"

## Read website
tb <- html_nodes(read_html(url), "table")

## Find structure of table
pop <- html_table(tb)

## Fetch names of files
files <- pop[[1]][["Nom du fichier"]]

## Keep file names of "fullfile" only
files2 <- files[grep("leifrancefullfile", files)] 

###################################################
######  PART 2: select only end-of-month files
######  and download
###################################################

## Extract all dates
yearmonthday  <- sub(".*?fullfile(\\d{8})t.*", "\\1", files2) # keeps 8 numbers between "fullfile" and "t"
yearmonth     <- substr(sub( ".*?fullfile(\\d{8})t.*", "\\1", files2), 1, 6) # keeps 6 digits of date, namely YYYYMM

## Find end of month date
date_df       <- data.frame(yearmonthday, yearmonth) %>%             # put in dataframe to use dplyr
                      mutate(numymd = as.Date(yearmonthday, "%Y%m%d"), # create date from string
                                              numym = format(numymd, "%Y-%m")) %>%
                      group_by(numym) %>%                             # create identifier for each year-month to select only largest value
                      filter(numymd == max(numymd))                   # keep only largest day for each year-month

## Keep only string of end of month date
eomstring <- unlist(date_df[,1])

## Create filename with end-of-month string
names <- gsub(" ", "", paste("leifrancefullfile", eomstring))

## Create pattern for string match
patterny <- gsub(" ", "", paste(".*leifrancefullfile", eomstring))

matches <- unique(grep(paste(patterny,collapse="|"), 
                       files2, value=TRUE))

## Create URL based on matched files
url_first <- "https://lei-france.insee.fr/telecharger?fichier="
url_all   <- c(gsub(" ", "", paste(url_first, matches)))

## Download and unzip all
for (u in url_all) {
  
  if (file.exists(basename(u))) {
    next
  }
        download.file(u, basename(u), mode = "wb")
        unzip(basename(u))
  
}


