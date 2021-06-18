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

## Set working directory
setwd("/Users/oliverashtari/desktop/Liliana/RAW")

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
  
  ## Economic activity merges together NACE and NAF, I split using the first 5 digits for nace
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
full_dta$siren = ifelse(is.na(full_dta$siren), paste(full_dta$fundmgrid),paste(full_dta$siren))

## Arrange so NA are last and when apply unique keep all info
full_dta <- arrange(full_dta, siren, nace, assoclei)
full_dta_small <- full_dta %>% select(lei, siren, year, month)


## For each year, keep only rows where we have unique lei, name and siren
full_dta_unique <- full_dta %>%
  group_by(year) %>%
  distinct(lei, legalname, siren, .keep_all = TRUE) 

## keep only duplicates to check them
test2 <- test[test$lei %in% test$lei[duplicated(test$lei)],]



n_occur <- data.frame(table(test$lei))

y <- test[88500:88505,]

n_occur[,1]


######## END OF WORK IN PROGRESS SECTION

# keep only unique values
full_dta <- unique(full_dta)
full_dta_small <- unique(full_dta_small)

## export in csv
write.csv(full_dta, "/Users/oliverashtari/desktop/Liliana/Output/identifiers.csv", row.names = FALSE)
write.csv(full_dta_small, "/Users/oliverashtari/desktop/Liliana/Output/identifiers_leisirenonly.csv", row.names = FALSE)
