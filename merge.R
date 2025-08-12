library(tidyverse)

files <- list.files("sources", full.names = T)


ExportCsv <- data.frame(nom = as.character(),
                        bd = as.character(),
                        projets = as.character(),
                        label = as.character(),
                        description = as.character(),
                        ses = as.character(),
                        type = as.character(),
                        unique = as.character(),
                        nVals = as.numeric())

for (j in 1:length(files)){
  source(file = files[j])
  ExportCsv <- rbind(ExportCsv, Export)
  print(files[j])
}

write.table(ExportCsv, paste0("_SharedFolder_catalogue-donnees/varsList_", Sys.Date(), ".csv"),
                 sep = ";", fileEncoding = "UTF-8")
