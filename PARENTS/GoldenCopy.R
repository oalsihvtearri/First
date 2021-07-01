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

## Set working directory
setwd("/Users/oliverashtari/desktop/Liliana/PARENTS/RAW")

## Find files to read in folder
input_files <- list.files(".", pattern = "rr-golden-copy.csv$")


## Create list to store each iteration
datalist    = list()

## Loop over all files
for (files in input_files){

          ## Import file with only selected columns
  gc <- fread(files,  # Import columns
              select = c("Relationship.StartNode.NodeID", 
                         "Relationship.EndNode.NodeID", 
                         "Relationship.RelationshipType",
                         "Relationship.Period.1.startDate",
                         "Relationship.Period.1.endDate",
                         "Relationship.Period.1.periodType",
                         "Relationship.Period.2.startDate",
                         "Relationship.Period.2.endDate",
                         "Relationship.Period.2.periodType",
                         "Relationship.Period.3.startDate",
                         "Relationship.Period.3.endDate",
                         "Relationship.Period.3.periodType",
                         "Relationship.Period.4.startDate",
                         "Relationship.Period.4.endDate",
                         "Relationship.Period.4.periodType",
                         "Relationship.Period.5.startDate",
                         "Relationship.Period.5.endDate",
                         "Relationship.Period.5.periodType",
                         "Registration.RegistrationStatus",
                         "Relationship.RelationshipStatus"
              )) %>%
             filter(Registration.RegistrationStatus == "PUBLISHED" & 
                    Relationship.RelationshipStatus == "ACTIVE")
                               
          ## In a few instances relationship period is swapped with accounting period.
          ## This fixes it
          
          gc$Relationstart = ifelse(gc$Relationship.Period.1.periodType == "RELATIONSHIP_PERIOD", 
                                    paste(gc$Relationship.Period.1.startDate), 
                                            ifelse(gc$Relationship.Period.2.periodType == "RELATIONSHIP_PERIOD", 
                                                   paste(gc$Relationship.Period.2.startDate),  
                                                      ifelse(gc$Relationship.Period.3.periodType == "RELATIONSHIP_PERIOD", 
                                                             paste(gc$Relationship.Period.3.startDate), 
                                                                ifelse(gc$Relationship.Period.4.periodType == "RELATIONSHIP_PERIOD", 
                                                                       paste(gc$Relationship.Period.4.startDate),
                                                                            ifelse(gc$Relationship.Period.5.periodType == "RELATIONSHIP_PERIOD", 
                                                                                   paste(gc$Relationship.Period.5.startDate), NA )))))
          
          
          gc$Relationend = ifelse(gc$Relationship.Period.1.periodType == "RELATIONSHIP_PERIOD", 
                                    paste(gc$Relationship.Period.1.endDate), 
                                    ifelse(gc$Relationship.Period.2.periodType == "RELATIONSHIP_PERIOD", 
                                           paste(gc$Relationship.Period.2.endDate),  
                                           ifelse(gc$Relationship.Period.3.periodType == "RELATIONSHIP_PERIOD", 
                                                  paste(gc$Relationship.Period.3.endDate), 
                                                  ifelse(gc$Relationship.Period.4.periodType == "RELATIONSHIP_PERIOD", 
                                                         paste(gc$Relationship.Period.4.endDate),
                                                         ifelse(gc$Relationship.Period.5.periodType == "RELATIONSHIP_PERIOD", 
                                                                paste(gc$Relationship.Period.5.endDate), NA )))))
          
          
          
          ## Removes unnecessary Period1 variables
          
          gc <- gc %>% arrange(gc$Relationship.StartNode.NodeID) %>%
            mutate(filename = as.Date(substr(files, start =  1, stop = 8), "%Y %m %d")) %>%
            select(Relationship.StartNode.NodeID, 
                   Relationship.EndNode.NodeID, 
                   Relationship.RelationshipType,
                   Relationstart, 
                   Relationend,
                   filename)
          
          datalist[[files]] <- gc


}

## Bind in single dataframe
full_dta = rbindlist(datalist, fill = TRUE)

## Change format to reshape wide, rename variables and order columns
full_dta      <- setDT(full_dta)
full_dta_wide <- dcast(full_dta, Relationship.StartNode.NodeID + Relationstart + Relationend + filename ~ Relationship.RelationshipType, 
                       value.var = "Relationship.EndNode.NodeID") %>%
                 rename(lei = Relationship.StartNode.NodeID,
                        start_relationship    = Relationstart,
                        end_relationship      = Relationend,
                        direct_parent         = IS_DIRECTLY_CONSOLIDATED_BY,
                        internationalbranchof = IS_INTERNATIONAL_BRANCH_OF,
                        ultimate_parent       = IS_ULTIMATELY_CONSOLIDATED_BY,
                        filename              = filename) %>%
                 relocate(lei, direct_parent, ultimate_parent, internationalbranchof, start_relationship, end_relationship, filename) %>%
                 arrange(lei, desc(filename))

## There is an issue with some relationship start dates in older files (2018 for example).
## Here we sort to have newer files on top for each LEI (those files also have dates formatted nicely)
## We replace for same observations the date in good format across all obs
## Keep only unique


full_dta_wide   <- full_dta_wide %>% group_by(lei, direct_parent, ultimate_parent) %>% 
                                     arrange(lei, desc(filename)) %>%
                                     mutate(start_fix = dplyr::first(start_relationship)) %>%
                                     distinct(lei, lei, direct_parent, ultimate_parent, start_relationship, .keep_all = TRUE)

## save file as csv
write.csv(full_dta_wide, file = "/Users/oliverashtari/Desktop/Liliana/Concatenated/Output/parents.csv", stdout(), row.names=FALSE)

## save as zip
write.csv(full_dta_wide, file = gzfile("/Users/oliverashtari/Desktop/Liliana/Concatenated/Output/parents.csv.gz",), row.names=FALSE)

