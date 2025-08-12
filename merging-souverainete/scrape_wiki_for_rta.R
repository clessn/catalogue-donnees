library(WikidataR)
library(rvest)

### G ####
url <- "https://fr.wikipedia.org/wiki/Liste_des_codes_postaux_canadiens_d%C3%A9butant_par_G"

# Lire le contenu de la page
page <- read_html(url)

# Extraire les tables
tables <- html_table(page)

# La table qui nous intéresse est probablement la première
postal_codes <- tables[[3]]

g <- postal_codes %>%
  pivot_longer(cols = everything(), values_to = "value") %>% 
  mutate(rta = substr(value, 1, 3),
         villes = substr(value, 4, nchar(value))) %>% 
  select(rta, villes)


### H ####
url <- "https://fr.wikipedia.org/wiki/Liste_des_codes_postaux_canadiens_d%C3%A9butant_par_H"

# Lire le contenu de la page
page <- read_html(url)

# Extraire les tables
tables <- html_table(page)

# La table qui nous intéresse est probablement la première
postal_codes <- tables[[3]]

h <- postal_codes %>%
  pivot_longer(cols = everything(), values_to = "value") %>% 
  mutate(rta = substr(value, 1, 3),
         villes = substr(value, 4, nchar(value))) %>% 
  select(rta, villes)


### J ####
url <- "https://fr.wikipedia.org/wiki/Liste_des_codes_postaux_canadiens_d%C3%A9butant_par_J"

# Lire le contenu de la page
page <- read_html(url)

# Extraire les tables
tables <- html_table(page)

# La table qui nous intéresse est probablement la première
postal_codes <- tables[[3]]

j <- postal_codes %>%
  pivot_longer(cols = everything(), values_to = "value") %>% 
  mutate(rta = substr(value, 1, 3),
         villes = substr(value, 4, nchar(value))) %>% 
  select(rta, villes)


data <- rbind(g, h, j) %>% 
  filter(villes != "" &
           villes != "Non assigné")

write.csv(data, "_SharedFolder_catalogue-donnees/merging-souverainete/aux_data/rta_geoloc.csv")
