######
# Loading and cleaning the data
######
rm(list = ls())
library(tidyverse)

# setting analysis folder

folder <- "/Users/shambhavi/Google Drive/Experiments & Data/Timing_2021/analysis/data/"

#------------------
# Compiling of data
#------------------
# reading in the MasterTable

MasterTable <- read.csv2(
  file = paste0(folder, "meta_data/MasterTableTiming.csv", sep = ""),
  header = TRUE, sep = ",", na.strings = "NA"
)

# reading in the table of conditions
Conditions <- read.csv2(
  file = paste0(folder, "meta_data/ConditionsTiming.csv", sep = ""),
  header = TRUE, sep = ",", na.strings = "NA"
)

# setting the colnames

mastercolnames <- c(
  "DateTime", "IdRFID", "IdLabel",
  "unitLabel", "eventDuration", "sense1duration",
  "sense1Events", "senseRFIDrecords", "reinforce1value",
  "reinforce1Total", "reinforce1Account", "outFuncLabel",
  "outLabel", "SystemMsg", "MsgValue1", "MsgValue2", "MsgValue3"
)

# writing a function to prepare the raw data from a single day

load_raw_csv <- function(path) {
  nthday <- read.csv2(
    file = path, sep = ";", dec = ".", header = TRUE,
    fileEncoding = "UTF-16LE", as.is = TRUE, row.names = NULL
  ) %>% 
    select(1:17)
  
  # renaming the columns
  if (exists("mastercolnames")) {
    colnames(nthday) <- mastercolnames
  }
  
  # extracting the relevant rows
  
  firstrow <- c()
  lastrow <- c()
  firstrow <- max(which(nthday$SystemMsg == "start")) + 1
  lastrow <- nrow(nthday)
  
  if (is.na(lastrow) | is.infinite(lastrow)) {
    lastrow <- nrow(nthday)
  }
  
  nthday <- nthday %>%
    slice(firstrow:lastrow)
}

# taking the list of days from the master table
days <- as.list(MasterTable$Day)
# taking the list of file paths from the master table
paths <- as.list(paste0(folder, MasterTable$Path))

# writing a function to aggregate the data from many days

aggregate_days <- function(paths, days) {
  map(paths, load_raw_csv) %>%
    set_names(days) %>%
    enframe("day", "day_data") %>%
    unnest(day_data) %>%
    mutate(day = as.numeric(day)) %>% 
    rename(Day = day)
}

# using the functions that were written to create one big data frame of raw data

alldays <- aggregate_days(paths, days) %>%
  # removing the columns with irrelevant data 
  select(
    -sense1Events, -senseRFIDrecords,
    -reinforce1Total, -MsgValue2, -MsgValue3
  ) %>% 
  # removing the Bat registered as unknown because the RFID number was wrong 
  filter(IdLabel != "unknown")

# Changing the DateTime to the proper format 
alldays$DateTime <- sub(",", ".", alldays$DateTime)
alldays <- alldays %>% 
  mutate(DateTime = as.POSIXct(as.numeric(alldays$DateTime) * (60 * 60 * 24),
                               origin = "1899-12-30", tz = "GMT"))

# Merge the Conditions table together to get one big table with all the meta data in it

alldays <- alldays %>%
  select(-IdLabel)

alldays <- left_join(alldays, Conditions,
                     by = c("Day", "IdRFID")
) 

# Marking the choices
Allchoices <- alldays %>%
  # removing unrecognised bats
  filter(!is.na(IdLabel), 
         Discard == 0) %>% 
  mutate(
    # boolean column for whether the animal made a choice
    choice = str_detect(unitLabel, "Cond")|str_detect(unitLabel, "RFID")
  ) %>% 
  select(-choice)

# removing the unnecessary dataframes 
rm(Conditions, MasterTable, days, paths)

#------------------------------------------------
# Creating a data table with the pump information
#------------------------------------------------

Pump <- alldays %>% 
  filter(SystemMsg == "start pump" | SystemMsg == "end pump")

write.csv2(Pump, file = paste0(folder, "/processed_data/Pump_data.csv"), row.names = FALSE)

#------------------------------------------------
# Creating a data table with the training data
#------------------------------------------------
Training <- Allchoices %>% 
  filter(Stage == "Training")

write.csv2(Training, file = paste0(folder, "/processed_data/Training.csv"), row.names = FALSE)

#-----------------------------------------------
# Creating a dataset with the data from the bats
#-----------------------------------------------
Main <- Allchoices %>% 
  filter(Stage == "Main")

# generating a CSV file of the raw data including all the 
write.csv2(Main, file = paste0(folder, "/processed_data/Main.csv"), row.names = FALSE)
