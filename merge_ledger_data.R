#######################
## Merge Casey's spreadsheets of annual ledger data into a single .csv file
##
## author: Willson Gaul wgaul@hotmail.com
## created: 16 April 2019
## last modified: 19 June 2019
########################

setwd("~/Documents/Data_Analysis/UCD/ucd_birds/")
library(Hmisc)
library(tidyverse)
library(lubridate)

# user will need to set the path to read the data
fnames <- list.files("~/Documents/UCD/birds/casey_project/data/casey_digitised_ledgers/")

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
# a mistake by Kevin where he used a Thursday as the week end date instead of
# using Friday as he had been.  This matters b/c 1 Jan was on Friday in 1982, 
# and later I calculate week number as number of 7 day periods since 1 Jan.
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
pa <- function(x){
  x[x > 0] <- 1
  x}
bird$observed_pa <- pa(bird$observation)

# spread if you want
# bird_wide <- spread(bird[, colnames(bird) != "Notes"], key = week, 
#                     value = observation)