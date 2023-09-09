library(rvest)

# Naviguer vers la page et lire son contenu
page_content <- read_html("https://www.poltext.org/fr/plateformes-aux-elections-canadiennes")

# Extraire les liens des documents (ajustez le selecteur CSS si nécessaire)
doc_links <- page_content %>% html_nodes("div.view-grouping > div > div > div > span > a") %>% html_attr("href")

for (link in doc_links) {
  # Créer un nom de fichier basé sur l'URL ou une autre logique
  file_name <- basename(link)
  download.file(link, file_name, mode = "wb")
}
