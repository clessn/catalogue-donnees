library(tidyverse)

#### Data ####

Omnibus <- read.csv("_SharedFolder_catalogue-donnees/Data/omnibus/omnibusclessn.csv")
Pilot2_2021 <- read.csv("_SharedFolder_catalogue-donnees/Data/datagotchi/CleanData-Lifestyle2.csv")
GlobalES <- read.csv("_SharedFolder_catalogue-donnees/Data/GlobalEs/Canada/CES_MERGED_1968-2019_2021-11-16.csv")
Quorum <- read.csv("_SharedFolder_catalogue-donnees/Data/quorum/QRM3_2021-01-20.csv")
INSPQ <- read.csv("_SharedFolder_catalogue-donnees/Data/sondage_INSPQ/MainlineData_CleanData_2020-06-01.csv")

#### Data Wrangling ####

## rows
Nrows_df <- data.frame(do.call("rbind", lapply(ls(), function(x) { # Créer un data frame qui indique le nombre d'observations pour chaque jeu de données
  obj = get(x)
  if (class(obj) == "data.frame" & x != "Nrows_df")
    c(name = x, rows = nrow(obj))
}))) %>%
  mutate(rows = as.numeric(rows))
nrows <- sum(Nrows_df$rows) # Nombre d'observations totales

## sourceID
for (i in 1:nrow(Nrows_df)) {
  veci <- rep(Nrows_df$name[i], Nrows_df$rows[i])
  if (i == 1) {
    vec <- veci
  } else {
    vec <- c(vec, veci) # vecteur qui indique la source de chaque observation
  }
}

main <- data.frame(mainID = 1:nrows, sourceID = vec) 


imput_from_source <- function(sourceID, varNameSource, varNameMain) {
  sourceID_index <-
    main[main$sourceID == sourceID,]$mainID # Fonction pour prendre la variable source et l'ajouter avec le bon nom dans main.
  
  if (varNameMain %in% names(main)) {
    main[[varNameMain]][sourceID_index] <<-
      eval(parse(text = sourceID))[[varNameSource]]
  }
  else {
    main[[varNameMain]] <<- NA
    main[[varNameMain]][sourceID_index] <<-
      eval(parse(text = sourceID))[[varNameSource]]
  }
  
}


# Merging SES -------------------------------------------------------------

## Language ####

imput_from_source("GlobalES", "langEn", "ses_langEn")
imput_from_source("GlobalES", "langFr", "ses_langFr")
imput_from_source("GlobalES", "langOth", "ses_langOth")
imput_from_source("INSPQ", "langSurveyFr", "ses_langFr")
imput_from_source("INSPQ", "langSurveyEn", "ses_langEn")
imput_from_source("Omnibus", "ses_langFr", "ses_langFr")
imput_from_source("Omnibus", "ses_langEn", "ses_langEn")
imput_from_source("Omnibus", "ses_langOthr", "ses_langOth")
imput_from_source("Pilot2_2021", "langFr", "ses_langFr")
imput_from_source("Pilot2_2021", "langEn", "ses_langEn")
imput_from_source("Pilot2_2021", "ses_languageOther", "ses_langOth")
imput_from_source("Quorum", "ses_langFR", "ses_langFr")
imput_from_source("Quorum", "ses_langEN", "ses_langEn")
imput_from_source("Quorum", "ses_langOther", "ses_langOth")

## Gender ####

imput_from_source("GlobalES", "female", "ses_female")
imput_from_source("INSPQ", "female", "ses_female")
imput_from_source("Omnibus", "ses_female", "ses_female")
imput_from_source("Pilot2_2021", "female", "ses_female")
imput_from_source("Pilot2_2021", "ses_genderOther", "ses_genderOth")
imput_from_source("Quorum", "ses_female", "ses_female")

## Age ####

imput_from_source("GlobalES", "age34m", "ses_age34m")
imput_from_source("GlobalES", "age35p54", "ses_age35p54")
imput_from_source("GlobalES", "age55p", "ses_age55p")
imput_from_source("INSPQ", "age34m", "ses_age34m")
imput_from_source("INSPQ", "age35p64", "ses_age35p64")
imput_from_source("INSPQ", "age65p", "ses_age65p")
imput_from_source("Omnibus", "ses_age34m", "ses_age34m")
imput_from_source("Omnibus", "ses_age35p54", "ses_age35p54")
imput_from_source("Omnibus", "ses_age55p", "ses_age55p")
imput_from_source("Pilot2_2021", "ses_age34m", "ses_age34m")
imput_from_source("Pilot2_2021", "ses_age35p54", "ses_age35p54")
imput_from_source("Pilot2_2021", "ses_age55p", "ses_age55p")
imput_from_source("Quorum", "ses_age34m", "ses_age34m")
imput_from_source("Quorum", "ses_age35p54", "ses_age35p54")
imput_from_source("Quorum", "ses_age55p", "ses_age55p")

## Education ####

imput_from_source("GlobalES", "educBHS", "ses_educBHS")
imput_from_source("GlobalES", "educHS", "ses_educHS")
imput_from_source("GlobalES", "educUniv", "ses_educUniv")
imput_from_source("INSPQ", "educHS", "ses_educHS")
imput_from_source("INSPQ", "educDEC", "ses_educDEC")
imput_from_source("INSPQ", "educUniv", "ses_educUniv")
imput_from_source("Omnibus", "ses_educHsOrBelow", "ses_educHS")
imput_from_source("Omnibus", "ses_educColl", "ses_educDEC")
imput_from_source("Omnibus", "ses_educUniv", "ses_educUniv")
imput_from_source("Pilot2_2021", "educBHS", "ses_educBHS")
imput_from_source("Pilot2_2021", "educHS", "ses_educHS")
imput_from_source("Pilot2_2021", "educUniv", "ses_educUniv")
imput_from_source("Quorum", "ses_educ_BHS", "ses_educBHS")
imput_from_source("Quorum", "ses_educ_coll", "ses_educDEC")
imput_from_source("Quorum", "ses_educ_univBA", "ses_educUniv")
imput_from_source("Quorum", "ses_educ_univOverBA", "ses_educUniv") # Arranger ça


