library(tidyverse)
library(readxl)


Journalists <- read_xlsx("journalists.xlsx")

commas <- grep(",", Journalists$data.fullName)

Journalists1 <- Journalists[commas, ]


no_commas <- as.numeric(!(1:nrow(Journalists) %in% commas))
Journalists2 <- Journalists[no_commas, ]

Journalists1 <- Journalists1 %>% 
  mutate(data.fullName = paste(data.firstName, data.lastName))

Data <- bind_rows(Journalists1, Journalists2)

Data$id1 <- NA
Data$id1 <- tolower(Data$data.fullName)
Data$id2 <- gsub(" ", "", Data$id1)



CleanData <- Data %>% 
  select(full_name = data.fullName, female = data.isFemale, twitter_handle = data.twitterHandle, 
         current_media = data.currentMedia, country = metadata.country,
         province_or_state = metadata.province_or_state)
  
  
CleanData$country <-  sub("CA", "CAN", CleanData$country)



CleanData$id <- NA
CleanData$id <- paste0(Data$id2, "1")


