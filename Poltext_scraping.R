library(rvest)
library(curl)
options(HTTPUserAgent = "Mozilla/5.0")

####tentative de tout scraper####

scrape_and_download <- function(url, css_selector, folder_path) {
  page_content <- read_html(url)
  
  # Extraire les liens à partir du sélecteur CSS fourni
  doc_links <- page_content %>% html_nodes(css_selector) %>% html_attr("href")
  
  # Convertir les URL relatives en URL absolues
  base_url <- "https://www.poltext.org"
  doc_links <- sapply(doc_links, function(link) {
    # Encoder l'URL
    link <- URLencode(link, reserved = TRUE)
    
    # Vérifier si l'URL est relative ou absolue
    ifelse(substr(link, 1, 4) == "http", link, paste0(base_url, link))
  })
  
  for (link in doc_links) {
    dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    file_name <- paste0(folder_path, "/", basename(link))
    
    # Utilisation de tryCatch pour gérer les erreurs
    result <- tryCatch({
      download.file(link, file_name, mode = "wb")
      TRUE
    }, error = function(e) {
      message(paste("Erreur lors du téléchargement du lien :", link))
      message("Erreur :", e$message)
      FALSE
    })
    
    if (!result) {
      message(paste("Le lien", link, "a été omis. Passage au lien suivant."))
    }
  }
}
scrape_and_download_nodes <- function(url, folder_path) {
  page_content <- read_html(url)
  
  # Obtenir tous les éléments avec une classe commençant par "node-" suivie de chiffres
  nodes <- page_content %>% html_nodes("[class^='node-'][class*=' ']")
  
  for (node in nodes) {
    link <- node %>% html_node("a") %>% html_attr("href")
    
    # Convertir les URL relatives en URL absolues
    base_url <- "https://www.poltext.org"
    link <- ifelse(substr(link, 1, 4) == "http", link, paste0(base_url, link))
    
    dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    file_name <- paste0(folder_path, "/", basename(link))
    
    # Utilisation de tryCatch pour gérer les erreurs
    result <- tryCatch({
      download.file(link, file_name, mode = "wb")
      TRUE
    }, error = function(e) {
      message(paste("Erreur lors du téléchargement du lien :", link))
      message("Erreur :", e$message)
      FALSE
    })
    
    if (!result) {
      message(paste("Le lien", link, "a été omis. Passage au lien suivant."))
    }
  }
}

scrape_and_download2 <- function(url, folder_path) {
  # Récupérer le contenu de la page web
  web_content <- read_html(url)
  
  # Extraction des liens
  links <- web_content %>%
    html_nodes("ul li a") %>%
    html_attr("href")
  
  base_url <- "https://poltext.org" # Assurez-vous que c'est l'URL de base correcte
  full_links <- paste0(base_url, links)
  
  # Assurez-vous que le dossier spécifié existe
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  
  for(i in seq_along(full_links)) {
    # Obtenez le nom du fichier à partir de l'URL
    file_name <- basename(full_links[i])
    
    # Chemin complet où le fichier sera enregistré
    full_path <- file.path(folder_path, file_name)
    
    # Téléchargez le fichier
    tryCatch({
      curl_download(url = full_links[i], destfile = full_path)
      message(paste("Fichier téléchargé :", full_path))
    }, error = function(e) {
      message(paste("Erreur lors du téléchargement du lien :", full_links[i]))
      message("Erreur :", e$message)
    })
  }
}

# Utilisation de la fonction
base_folder <- "_SharedFolder_catalogue-donnees/poltext"
scrape_and_download("https://www.poltext.org/fr/plateformes-aux-elections-canadiennes", "div.view-grouping > div > div > div > span > a", paste0(base_folder, "/plateformes/federales"))
scrape_and_download("https://www.poltext.org/fr/plateformes-%C3%A9lectorales-provinces-canadiennes", ".view-content > div > div > div > div > div > div > div > a", paste0(base_folder, "/plateformes/provinciales"))
scrape_and_download("https://www.poltext.org/fr/discours-du-tr%C3%B4ne-canadiens", "div.views-row > div > span > a", paste0(base_folder, "/discours_trone/federaux"))
scrape_and_download("https://www.poltext.org/fr/volet-1-textes-politiques-informatis%C3%A9s/discours-du-tr%C3%B4ne-des-provinces-canadiennes", "div.view-grouping > div > div > div > span > a", paste0(base_folder, "/discours_trone/provinciaux"))
scrape_and_download("https://www.poltext.org/fr/volet-1-textes-politiques-informatis%C3%A9s/discours-du-budget-canadiens", "div.views-row > div > span > a", paste0(base_folder, "/discours_budget/federaux"))
scrape_and_download("https://www.poltext.org/fr/volet-1-textes-politiques-informatis%C3%A9s/discours-du-budget-des-provinces-canadiennes", "div.view-grouping > div > div > div > span > a", paste0(base_folder, "/discours_budget/provinciaux"))
scrape_and_download("https://www.poltext.org/fr/volet-1-textes-politiques-informatises/discours-relatifs-au-budget-par-les-ministres-de-la-sant%C3%A9", "div.view-grouping > div > div > div > span > a", paste0(base_folder, "/discours_budget/sante_provinciaux"))
scrape_and_download("https://www.poltext.org/fr/volet-1-textes-politiques-informatis%C3%A9s/discours-relatifs-au-budget-par-les-ministres-de-l%C3%A9ducation", "div.view-grouping > div > div > div > span > a", paste0(base_folder, "/discours_budget/education_provinciaux"))
scrape_and_download("https://www.poltext.org/fr/les-textes-constitutionnels", ".node-4448 > div > div > div > ul > li > a", paste0(base_folder, "/textes_constitutionnels"))
scrape_and_download("https://www.poltext.org/fr/rapports-des-verificateurs-generaux/Canada", ".field-content > ul > li > ul > li > a", paste0(base_folder, "/rapports_VG/federaux"))
scrape_and_download("https://www.poltext.org/fr/rapports-des-verificateurs-generaux", "div.views-row > div > div > ul > li > a", paste0(base_folder, "/rapports_VG/provinciaux"))
scrape_and_download_nodes("https://www.poltext.org/fr/rapports-annuels-du-protecteur-du-citoyen", paste0(base_folder, "/rapports_protecteur_citoyen/annuels"))
scrape_and_download("https://www.poltext.org/fr/rapports-dintervention-du-protecteur-du-citoyen", ".node-7768 > div > div > div > p > a", paste0(base_folder, "/rapports_protecteur_citoyen/intervention"))
scrape_and_download("https://www.poltext.org/fr/rapports-dintervention-du-protecteur-du-citoyen", ".node-6461 > div > div > div > p > a", paste0(base_folder, "/rapports_protecteur_citoyen/intervention"))
scrape_and_download("https://www.poltext.org/fr/rapports-dintervention-du-protecteur-du-citoyen", ".node-6405 > div > div > div > p > a", paste0(base_folder, "/rapports_protecteur_citoyen/intervention"))
scrape_and_download("https://www.poltext.org/fr/rapports-dintervention-du-protecteur-du-citoyen", ".node-6400 > div > div > div > p > a", paste0(base_folder, "/rapports_protecteur_citoyen/intervention"))
scrape_and_download("https://www.poltext.org/fr/rapports-dintervention-du-protecteur-du-citoyen", ".node-6401 > div > div > div > p > a", paste0(base_folder, "/rapports_protecteur_citoyen/intervention"))
scrape_and_download("https://www.poltext.org/fr/rapports-dintervention-du-protecteur-du-citoyen", ".node-6403 > div > div > div > p > a", paste0(base_folder, "/rapports_protecteur_citoyen/intervention"))
scrape_and_download("https://www.poltext.org/fr/rapports-dintervention-du-protecteur-du-citoyen", ".node-6404 > div > div > div > p > a", paste0(base_folder, "/rapports_protecteur_citoyen/intervention"))
scrape_and_download_nodes("https://www.poltext.org/fr/rapports-speciaux-du-protecteur-du-citoyen", paste0(base_folder, "/rapports_protecteur_citoyen/speciaux"))
scrape_and_download("https://www.poltext.org/fr/textes/sondages-et-opinion-publique/rapports-annuels-sur-la-recherche-sur-lopinion-publique", ".field-item > ul > li > a", paste0(base_folder, "/sondages/rapports_annuels"))
scrape_and_download2("https://www.poltext.org/fr/textes/lettres-de-mandat", paste0(base_folder, "/lettres_mandat"))
scrape_and_download2("https://www.poltext.org/fr/textes/sondages-et-opinion-publique/rapports-annuels-sur-la-recherche-sur-lopinion-publique", paste0(base_folder, "/sondages/rapports_annuels"))



