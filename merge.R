library(tidyverse)

files <- list.files("sources", full.names = T)

for (j in 1:length(files)){
  source(file = files[j])
  print(files[j])
}