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
