#######################
## Merge Casey's spreadsheets of annual ledger data into a single .csv file
##
## inputs: the .csv files of digitized bird data (one file for each year)
##        available from https://doi.org/10.5281/zenodo.3250827
##
## outputs:   * bird - data frame with a row for every species x week grid cell
##                    in the original paper data logs.  This includes a presence/
##                    absence column that assumes empty grid cells in the 
##                    original paper log are non-detection data.
##            * bird_presence_only - data frame with a row for every bird x 
##                    week detection record.
##
## author: Willson Gaul wgaul@hotmail.com
## created: 16 April 2019
## last modified: 4 Sep 2019
########################

# user will need to set working directory and path to data
warning("User must set working directory and path to data")
setwd("~/Documents/Data_Analysis/UCD/ucd_birds/")
# user will need to set the path to read the data
fnames <- list.files("~/Documents/UCD/birds/casey_project/data/casey_digitised_ledgers/")
library(Hmisc)
library(tidyverse)
library(lubridate)

fnames <- fnames[grepl(pattern = ".*csv$", x = fnames)]

bird <- lapply(fnames, FUN = function(x) {
  read_csv(paste0("~/Documents/UCD/birds/casey_project/data/casey_digitised_ledgers/", 
                  x))})
names(bird) <- gsub(".csv", replacement = "", x = fnames)

# remove columns with only NAs
# (Casey made columns for every column that appeared in the physical ledgers,
# including columns that were visual separators)
bird <- lapply(bird, FUN = function(x) {
  x[, sapply(x, FUN = function(y) {any(!is.na(y))})]
})

# drop 'weather' rows
bird <- lapply(bird, FUN = function(x) {
  x[x$Original_Entry != "Weather", ]
})

# fix 1982 week end date issues
# a typo in CP's original file
colnames(bird$ledger_1982)[colnames(bird$ledger_1982) == "24/06"] <- "25/06"
# a mistake in original ledgers where Thursday was used as the week end date 
# instead of using Friday as before.  This matters b/c 1 Jan was on Friday in 
# 1982 and later I calculate week number as number of 7 day periods since 1 Jan.
# So to adequately represent successive weeks of survey, I need to make the
# week ending days all be on Friday (or later, Sat and Sun are ok).
colnames(bird$ledger_1982)[colnames(bird$ledger_1982) == "01/07"] <- "02/07"
# same issue with 1989 - need to keep week marking days on Sunday for this year
colnames(bird$ledger_1989)[colnames(bird$ledger_1989) == "07/10"] <- "08/10"

# gather week columns
bird <- lapply(bird, FUN = function(x) {
  gather_cols <- which(colnames(x) %nin% c("Record_Type", "Original_Entry", 
                                           "Year", "Notes"))
  gather(x, key = "week", value = "observation", gather_cols)
})

# bind all years into a single data frame
bird <- bind_rows(bird) %>%
  select(-Record_Type)

# clean date character strings in 'week' column so that they are all possible.
bird$week[bird$week == "31/11"] <- rep("01/12", 
                                         length(which(bird$week == "31/11")))
bird$week[bird$week == "31/09"] <- rep("01/10", 
                                         length(which(bird$week == "31/09")))

# convert "week" values that are dates into week of year
bird$week[bird$week %nin% as.character(1:54)] <- week(
  as.Date(paste0(bird$week[bird$week %nin% as.character(1:54)], "/",
                 bird$Year[bird$week %nin% as.character(1:54)]), 
          format = "%d/%m/%Y"))

# make a presence-absence column
# This treats empty cells in the original paper log sheets as non-detection data
pa <- function(x){
  x[is.na(x) | x == "?"] <- 0 # treat questions marks and empty cells as absences
  x[x != 0] <- 1 # treat any other entry as a presence
  x}
bird$observed <- pa(bird$observation)

# spread if you want
# bird_wide <- spread(bird[, colnames(bird) != "Notes"], key = week, 
#                     value = observation)

# subset to only occurrence records
bird_presence_only <- bird[bird$observed == 1, ]
colnames(bird_presence_only)[1] <- "Species"
