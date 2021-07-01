###################################################
######  Golden Copy Files                   ######
######  Oliver Ashtari Tafti                ######
######  Created 24 June 2021                ######
###################################################
######  INSTRUCTIONS                          #####
######                                        #####
######                                        #####
###################################################

## Load libraries
library("dplyr")
library("data.table")
library("lubridate")
library("assertthat")

## Load functions
source("/Users/oliverashtari/desktop/Liliana/PARENTS/R Codes/function_datefix.R")

## Set working directory
setwd("/Users/oliverashtari/desktop/Liliana/PARENTS/RAW")

## Find files to read in folder
input_files <- list.files(".", pattern = "rr-golden-copy.csv$")


## Create list to store each iteration
datalist    = list()

## Looping over all downloaded files
for (files in input_files){

## Import data, selecting only relevant columns, create new column with row no
gc <- readr::read_csv(file = files,
         col_types = readr::cols_only(
           Relationship.StartNode.NodeID    = "c",
           Relationship.EndNode.NodeID      = "c",
           Relationship.RelationshipType    = "c",
           Relationship.Period.1.startDate  = "c",
           Relationship.Period.1.endDate    = "c",
           Relationship.Period.1.periodType = "c",
           Relationship.Period.2.startDate  = "c",
           Relationship.Period.2.endDate    = "c",
           Relationship.Period.2.periodType = "c",
           Relationship.Period.3.startDate  = "c",
           Relationship.Period.3.endDate    = "c",
           Relationship.Period.3.periodType = "c",
           Relationship.Period.4.startDate  = "c",
           Relationship.Period.4.endDate    = "c",
           Relationship.Period.4.periodType = "c",
           Relationship.Period.5.startDate  = "c",
           Relationship.Period.5.endDate    = "c",
           Relationship.Period.5.periodType = "c",
           Registration.RegistrationStatus  = "c",
           Relationship.RelationshipStatus  = "c"
           )) %>%
        mutate(row_id = dplyr::row_number())

### THIS WHOLE PART DEALS WITH RELATIONSHIP START DATES
## Select relationship periods date
date1 <- gc %>% select(row_id,
                       start_date = Relationship.Period.1.startDate,
                       end_date   = Relationship.Period.1.endDate,
                       type       = Relationship.Period.1.periodType)

date2 <- gc %>% select(row_id,
                       start_date = Relationship.Period.2.startDate,
                       end_date   = Relationship.Period.2.endDate,
                       type       = Relationship.Period.2.periodType)

date3 <- gc %>% select(row_id,
                       start_date = Relationship.Period.3.startDate,
                       end_date   = Relationship.Period.3.endDate,
                       type       = Relationship.Period.3.periodType)

date4 <- gc %>% select(row_id,
                       start_date = Relationship.Period.4.startDate,
                       end_date   = Relationship.Period.4.endDate,
                       type       = Relationship.Period.4.periodType)

date5 <- gc %>% select(row_id,
                       start_date = Relationship.Period.5.startDate,
                       end_date   = Relationship.Period.5.endDate,
                       type       = Relationship.Period.5.periodType)


rel_dates <- dplyr::bind_rows(date1,date2,date3,date4,date5) %>%
             dplyr::filter(type == "RELATIONSHIP_PERIOD") %>%
             dplyr::select(row_id,
                           start_date,
                           end_date)

## Check if we have more rows than distinct LEI, if so keep min as start date and max as end date
if(nrow(rel_dates) > dplyr::n_distinct(rel_dates$row_id)){
                      rel_dates <- rel_dates %>%
                                   group_by(row_id) %>%
                                   summarise(start_date = min(start_date),
                                             end_date   = max(end_date))
}

## Here we call the function that checks if date is: NA, unix or standard format 
## and formats it as standard date.
clean_rel_dates <- rel_dates %>%
                   dplyr::mutate(
                                  start_date = parse_lei_ts(start_date),
                                  end_date = parse_lei_ts(end_date)
                                )


### END OF DEALING WITH DATES


## Create final dataframe
gc <- gc %>%
            left_join(clean_rel_dates, by = "row_id") %>%
            select(
                    Relationship.StartNode.NodeID,
                    Relationship.EndNode.NodeID,
                    Relationship.RelationshipType,
                    Relationship.RelationshipStatus,
                    relationship_start_date = start_date,
                    relationship_end_date   = end_date
                    ) %>%
            rename(lei          = Relationship.StartNode.NodeID,
                   parent_lei   = Relationship.EndNode.NodeID,
                   rel_type     = Relationship.RelationshipType,
                   rel_status   = Relationship.RelationshipStatus
                  ) %>%
          mutate(filename = as.Date(substr(files, start =  1, stop = 8), "%Y %m %d"))

## Store in datalist and go to next
datalist[[files]] <- gc

}

## Merge in single dataframe
full_dta = rbindlist(datalist, fill = TRUE)

## Reshape wide, rename and order columns
full_dta      <- setDT(full_dta)
full_dta_wide <- dcast(full_dta, lei + relationship_start_date + relationship_end_date + filename ~ rel_type, 
                       value.var = "parent_lei"
                       ) %>%
                rename(direct_parent         = IS_DIRECTLY_CONSOLIDATED_BY,
                       internationalbranchof = IS_INTERNATIONAL_BRANCH_OF,
                       ultimate_parent       = IS_ULTIMATELY_CONSOLIDATED_BY,
                       ) %>%
                relocate(lei, 
                         direct_parent, 
                         ultimate_parent, 
                         internationalbranchof, 
                         relationship_start_date, 
                         relationship_end_date, 
                         filename
                         ) %>%
                arrange(lei, desc(filename)) %>%
                distinct(lei, relationship_start_date, relationship_end_date, .keep_all = TRUE) ## discard duplicates

## save file as csv
write.csv(full_dta_wide, 
          file = "/Users/oliverashtari/Desktop/Liliana/Concatenated/Output/parents.csv", 
          stdout(), row.names=FALSE)

## save as zip
write.csv(full_dta_wide, 
          file = gzfile("/Users/oliverashtari/Desktop/Liliana/Concatenated/Output/parents.csv.gz",), 
          row.names=FALSE)

