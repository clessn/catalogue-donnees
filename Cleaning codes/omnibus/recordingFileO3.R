
## Library used ##
library(foreign) # For reading Stata files
library(tidyverse)
library(haven)
library(readr)
library(labelled)

#********************************#
####_____ 0. LOADING DATA_____####
#********************************#  

## Loading/Reading a Datafile ##

Data  <- read_sav("_SharedFolder-omnibus/3-March2022/data/raw/OMN03-data.Sav")
Data2 <- read.spss("_SharedFolder-omnibus/3-March2022/data/raw/OMN03-data.Sav",to.data.frame = T,
                   reencode="utf-8")
Post <- readxl::read_excel("_SharedFolder-omnibus/3-March2022/data/raw/OMN03-POSTAL_CODES.xlsx")
pathToMonth <- "_SharedFolder-omnibus/3-March2022/"

source("codeR/cleaningSource.R")
names(Data)


#***********************************#
####_____2. Weights  ____________####
#***********************************# 

CleanData$weight <- Data$POND

#***********************************#
####_____3. Exporting Data ______####
#***********************************#  

# Fast check on the data frame
fastT19 = sapply(CleanData, table)
fastT20 = sapply(CleanData, class)

# Setting today's date
today = Sys.Date()

# Exporting CleanData for future use
write.csv(CleanData, paste0("_SharedFolder-omnibus/3-March2022/data/Clean/omnibus3Partial_", today, ".csv"))

