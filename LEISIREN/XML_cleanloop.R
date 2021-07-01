###################################################
######  XML Cleaning Loop                   ######
######  Oliver Ashtari Tafti                ######
######  Created 9 June 2021                 ######
###################################################
######  INSTRUCTIONS                          #####
######  Save all the XML files you want to run ###
######  in the "RAW" folder                   #####
###################################################

## The necessary libraries are

library("XML")
library("dplyr")
library("plyr")
library("writexl")
library("tidyr")
library("data.table")

## Set working directory
setwd("/Users/oliverashtari/desktop/Liliana/LEISIREN/RAW")

## Find names of all files in folder
input_files <- list.files(".", pattern = "[.]xml$")

## Create list to store output in loop
datalist = list()

## Looping over all input_files
for (files in input_files) {
  
  ## Import XML
  leiraw <- files
  xmlData <- xmlTreeParse(leiraw, useInternal = TRUE)
  
  ## Here we select only the subtree with relevant data
  extension <- ldply(xpathApply(xmlData, '//lei:Extension', getChildrenStrings), rbind) 
  entity    <- ldply(xpathApply(xmlData, '//lei:Entity', getChildrenStrings), rbind) 
  lei       <- ldply(xpathApply(xmlData, '//lei:LEI', getChildrenStrings), rbind) 
  
  ## Economic activity merges together NACE and NAF, I split them using the first 5 digits for nace
  extension <- separate(extension, 
                        EconomicActivity, into = c("nace", "naf"), 
                        sep = 5, remove = FALSE)
  
  ## Bind in a dataframe
  dataf   <- data.frame("lei"       = lei,
                        "legalname" = select(entity, "LegalName"),
                        "othername" = select(entity, contains('OtherEntityNames')),
                        "siren"     = select(extension, "SIREN"),
                        "nace"      = select(extension, "nace"),
                        "naf"       = select(extension, "naf"),
                        "fundmgrid" = select(extension, "FundManagerBusinessRegisterID"),
                        "assoclei"  = select(entity, "AssociatedEntity"),
                        "year"      = substr(sub(".*[fullfile]([^.]+)[t].*", "\\1", files), #sub() extracts YYYYMMDD, substr() keeps only year
                                             start = 1, stop = 4),
                        "month"  = substr(sub(".*[fullfile]([^.]+)[t].*", "\\1", files), 
                                          start = 5, stop = 6))
  
  ## Rename columns to deal with problem that in some datasets they have different names and rbind does not work with different col names
  colnames(dataf) <- c("lei", 
                       "legalname", 
                       "othername", 
                       "siren", 
                       "nace", 
                       "naf", 
                       "fundmgrid", 
                       "assoclei",
                       "year",
                       "month")
  
  ## Store output in datalist at each iteration
  datalist[[files]] <- dataf
  
}

## Append all data stored
full_dta = do.call(rbind, datalist) 

## For fund managers, siren is in a different field called "fund manager business id"
## here we consolidate into siren
full_dta$siren = ifelse(is.na(full_dta$siren), 
                        paste(full_dta$fundmgrid),paste(full_dta$siren))

## Transform year and month as numeric not factors 
full_dta$year <- as.numeric(levels(full_dta$year))[full_dta$year]
full_dta$month <- as.numeric(levels(full_dta$month))[full_dta$month]

## Arrange so NA are last and when apply unique keep all info
full_dta <- arrange(full_dta, siren, nace, assoclei)

## Dataset 1: full variables all months 
# Remove year 2021
full_dta <- full_dta %>% filter (year != 2021) %>%
                         group_by(lei, year, month) %>% 
                         distinct(lei, siren, .keep_all = TRUE)

## Dataset2: only lei, siren, year, month, all months
full_dta_small <- full_dta %>% 
                  select(lei, siren, year, month) %>% 
                  group_by(lei, year, month) %>% 
                  distinct(lei, siren, .keep_all = TRUE)


## Dataset 3: yearly all vars
# keep latest month for each lei-year
full_dta_yearly <- full_dta %>%
                    group_by(lei, year) %>%
                    slice(which.max(month)) %>%
                    distinct(lei, legalname, siren, .keep_all = TRUE)

## export in csv
write.csv(full_dta, "/Users/oliverashtari/desktop/Liliana/Output/full_monthly.csv", row.names = FALSE)
write.csv(full_dta_small, "/Users/oliverashtari/desktop/Liliana/Output/reduced_monthly.csv", row.names = FALSE)
write.csv(full_dta_yearly, "/Users/oliverashtari/desktop/Liliana/Output/full_yearly.csv", row.names = FALSE)

