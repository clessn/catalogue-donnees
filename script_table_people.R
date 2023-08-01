library(tidyverse)
library(readxl)
library(stringr)

Journalists <- read_xlsx("journalists.xlsx")

commas <- grep(",", Journalists$data.fullName)

Journalists1 <- Journalists[commas,]


no_commas <- as.numeric(!(1:nrow(Journalists) %in% commas))
Journalists2 <- Journalists[no_commas,]

Journalists1 <- Journalists1 %>%
  mutate(data.fullName = paste(data.firstName, data.lastName))

Data <- bind_rows(Journalists1, Journalists2)

Data$id1 <- NA
Data$id1 <- tolower(Data$data.fullName)
Data$id2 <- gsub(" ", "", Data$id1)



CleanDataJ <- Data %>%
  select(
    full_name = data.fullName,
    female = data.isFemale,
    twitter_handle = data.twitterHandle,
    current_media = data.currentMedia,
    country = metadata.country,
    province_or_state = metadata.province_or_state
  )


CleanDataJ$country <-  sub("CA", "CAN", CleanData$country)



CleanDataJ$id <- NA
CleanDataJ$id <- paste0(Data$id2, "1")


Decideurs <- read_xlsx("decideurs.xlsx")

Decideurs$nom <- sub(",.*", "", Decideurs$data.fullName)
Decideurs$prenom <- sub(".*,", "", Decideurs$data.fullName)

Decideurs$full_name <- paste(Decideurs$prenom, Decideurs$nom, sep = " ")

Decideurs$id <- NA
Decideurs$id <- str_to_lower(Decideurs$full_name)
Decideurs$id <- str_replace_all(Decideurs$id, "[[:space:]]+", "")
Decideurs$id <- paste0(Decideurs$id, 1)

CleanDataD <- Decideurs %>% 
  select(
    full_name = full_name,
    female = data.isFemale, 
    twitter_handle = data.twitterHandle, 
    country = metadata.country, 
    province_or_state = metadata.province_or_state, 
    id = id, 
    current_party = data.currentParty, 
    current_district = data.currentDistrict, 
    type = type
  )

CleanDataD$country <-  sub("CA", "CAN", CleanDataD$country)

People <- merge(CleanDataD, CleanDataJ, all = T)

People$id <- gsub("-", "", People$id)
People$id <- gsub("é", "e", People$id)
People$id <- gsub("è", "e", People$id)
People$id <- gsub("ç", "c", People$id)
People$id <- gsub("ê", "e", People$id)
People$id <- gsub("ë", "e", People$id)
People$id <- gsub("â", "a", People$id)
People$id <- gsub("ï", "i", People$id)
People$id <- gsub("î", "i", People$id)
People$id <- gsub("ô", "o", People$id)
People$id <- gsub("û", "u", People$id)

write_xlsx(People, "_SharedFolder_catalogue-donnees/People.xlsx")


