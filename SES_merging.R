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

## Religion ##


#cleaning de la variable religion dans omnibus

Omnibus$ses_religion_jewish <- NA
Omnibus$ses_religion_jewish[Omnibus$ses_religion == "Juda\xefsme"] <- 1
Omnibus$ses_religion_jewish[Omnibus$ses_religion != "Juda\xefsme"] <- 0
table(Omnibus$ses_religion_jewish)

Omnibus$ses_religion_muslim <- NA
Omnibus$ses_religion_muslim[Omnibus$ses_religion == "Islam"] <- 1
Omnibus$ses_religion_muslim[Omnibus$ses_religion != "Islam"] <- 0
table(Omnibus$ses_religion_muslim)

Omnibus$ses_religion_protest <- NA
Omnibus$ses_religion_protest[Omnibus$ses_religion == "Protestantisme"] <- 1
Omnibus$ses_religion_protest[Omnibus$ses_religion != "Protestantisme"] <- 0
table(Omnibus$ses_religion_protest)

Omnibus$ses_religion_none <- NA

Omnibus$ses_religion_none[Omnibus$ses_religion =="Ath\xe9isme/Aucune" | 
                            Omnibus$ses_religion == "Agnosticisme"] <- 1
Omnibus$ses_religion_none[Omnibus$ses_religion !="Ath\xe9isme/Aucune" & 
                            Omnibus$ses_religion != "Agnosticisme"] <- 0
table(Omnibus$ses_religion_none)

Omnibus$ses_religion_other <- NA
Omnibus$ses_religion_other[Omnibus$ses_religion == "Autre" |
                             Omnibus$ses_religion == "Bouddhisme" |
                             Omnibus$ses_religion == "Hindouisme" |
                             Omnibus$ses_religion == "Sikhisme"] <- 1
Omnibus$ses_religion_other[!(Omnibus$ses_religion %in% c("Autre", "Bouddhisme", 
                                                         "Hindouisme", "Sikhisme"))] <- 0
table(Omnibus$ses_religion_other)

Omnibus$ses_religion_christianity <- NA
Omnibus$ses_religion_christianity[Omnibus$ses_religion == "Catholicisme" |
                                    Omnibus$ses_religion == "Protestantisme" |
                                    Omnibus$ses_religion == "Orthodoxe"] <- 1
Omnibus$ses_religion_christianity[!(Omnibus$ses_religion %in% c("Catholicisme", "Protestantisme", 
                                                                "Orthodoxe"))] <- 0
table(Omnibus$ses_religion_christianity)


imput_from_source("GlobalES", "relProtest", "ses_religion_protest")
imput_from_source("GlobalES", "relCatho", "ses_religion_catho")
imput_from_source("GlobalES", "relJewish", "ses_religion_jewish")
imput_from_source("GlobalES", "relMuslim", "ses_religion_muslim")
imput_from_source("GlobalES", "relNone", "ses_religion_none")
imput_from_source("GlobalES", "relAllOther", "ses_religion_other")
imput_from_source("GlobalES", "relAllChrist", "ses_religion_christianity")
imput_from_source("INSPQ", "relProtest", "ses_religion_protest")
imput_from_source("INSPQ", "relCatho", "ses_religion_catho")
imput_from_source("INSPQ", "relJewish", "ses_religion_jewish")
imput_from_source("INSPQ", "relMuslim", "ses_religion_muslim")
imput_from_source("INSPQ", "relNone", "ses_religion_none")
imput_from_source("INSPQ", "relAllChrist", "ses_religion_christianity")
imput_from_source("Omnibus", "ses_isCatho", "ses_religion_catho")
imput_from_source("Omnibus", "ses_religion_protest", "ses_religion_protest")
imput_from_source("Omnibus", "ses_religion_muslim", "ses_religion_muslim")
imput_from_source("Omnibus", "ses_religion_jewish", "ses_religion_jewish")
imput_from_source("Omnibus", "ses_religion_other", "ses_religion_other")
imput_from_source("Omnibus", "ses_religion_none", "ses_religion_none")
imput_from_source("Omnibus", "ses_religion_christianity", "ses_religion_christianity")

#### Income ####

imput_from_source("GlobalES", "incomeLow", "ses_income_low")
imput_from_source("GlobalES", "incomeMid", "ses_income_mid")
imput_from_source("GlobalES", "incomeHigh", "ses_income_high")
imput_from_source("INSPQ", "incomeLow", "ses_income_low")
imput_from_source("INSPQ", "incomeMid", "ses_income_mid")
imput_from_source("INSPQ", "incomeHigh", "ses_income_high")
imput_from_source("Omnibus", "ses_incomeLow", "ses_income_low")
imput_from_source("Omnibus", "ses_incomeMid", "ses_income_mid")
imput_from_source("Omnibus", "ses_incomeHigh", "ses_income_high")
imput_from_source("Pilot2_2021", "incomeLow", "ses_income_low")
imput_from_source("Pilot2_2021", "incomeMid", "ses_income_mid")
imput_from_source("Pilot2_2021", "incomeHigh", "ses_income_high")
imput_from_source("Quorum", "ses_income_low", "ses_income_low")
imput_from_source("Quorum", "ses_income_mid", "ses_income_mid")
imput_from_source("Quorum", "ses_income_high", "ses_income_high")

####Sexual orientation####

imput_from_source("Pilot2_2021", "ses_hetero", "ses_orientation_hetero")
imput_from_source("Pilot2_2021", "ses_gai", "ses_orientation_homosexual")
imput_from_source("Pilot2_2021", "ses_bisex", "ses_orientation_bisexual")
imput_from_source("INSPQ", "heterosexual", "ses_orientation_hetero")
imput_from_source("INSPQ", "homosexual", "ses_orientation_homosexual")
imput_from_source("INSPQ", "bisexual", "ses_orientation_bisexual")

####Mariage status####

imput_from_source("INSPQ", "married", "ses_married")
imput_from_source("GlobalES", "married", "ses_married")

#### Province ####

imput_from_source("INSPQ", "nfld", "ses_province_nfld")
imput_from_source("INSPQ", "pei", "ses_province_pei")
imput_from_source("INSPQ", "ns", "ses_province_ns")
imput_from_source("INSPQ", "nb", "ses_province_nb")
imput_from_source("INSPQ", "quebec", "ses_province_quebec")
imput_from_source("INSPQ", "ontario", "ses_province_ontario")
imput_from_source("INSPQ", "manitoba", "ses_province_manitoba")
imput_from_source("INSPQ", "sask", "ses_province_sask")
imput_from_source("INSPQ", "alberta", "ses_province_alberta")
imput_from_source("INSPQ", "bc", "ses_province_bc")
imput_from_source("INSPQ", "north", "ses_province_north")
imput_from_source("GlobalES", "nfld", "ses_province_nfld")
imput_from_source("GlobalES", "pei", "ses_province_pei")
imput_from_source("GlobalES", "ns", "ses_province_ns")
imput_from_source("GlobalES", "nb", "ses_province_nb")
imput_from_source("GlobalES", "quebec", "ses_province_quebec")
imput_from_source("GlobalES", "ontario", "ses_province_ontario")
imput_from_source("GlobalES", "manitoba", "ses_province_manitoba")
imput_from_source("GlobalES", "sask", "ses_province_sask")
imput_from_source("GlobalES", "alberta", "ses_province_alberta")
imput_from_source("GlobalES", "bc", "ses_province_bc")
imput_from_source("GlobalES", "nunavut", "ses_province_nunavut")
imput_from_source("Pilot2_2021", "ses_prov_Nfl", "ses_province_nfld")
imput_from_source("Pilot2_2021", "ses_prov_Pei", "ses_province_pei")
imput_from_source("Pilot2_2021", "ses_prov_Ns", "ses_province_ns")
imput_from_source("Pilot2_2021", "ses_prov_Nb", "ses_province_nb")
imput_from_source("Pilot2_2021", "quebec", "ses_province_quebec")
imput_from_source("Pilot2_2021", "ontario", "ses_province_ontario")
imput_from_source("Pilot2_2021", "ses_prov_Manitoba", "ses_province_manitoba")
imput_from_source("Pilot2_2021", "ses_prov_Skt", "ses_province_sask")
imput_from_source("Pilot2_2021", "ses_prov_Alb", "ses_province_alberta")
imput_from_source("Pilot2_2021", "ses_prov_Bc", "ses_province_bc")
imput_from_source("Pilot2_2021", "ses_prov_Yukon", "ses_province_yukon")
imput_from_source("Quorum", "ses_QC", "ses_province_quebec")

#### Immigration ####

imput_from_source("Pilot2_2021", "immigrant", "ses_immigrant")
imput_from_source("GlobalES", "immigrant", "ses_immigrant")
imput_from_source("INSPQ", "immigrant", "ses_immigrant")

#### Urban/Rural ####

imput_from_source("GlobalES", "urban", "ses_urban")
imput_from_source("GlobalES", "rural", "ses_rural")
imput_from_source("Pilot2_2021", "ses_urbain", "ses_urban")
imput_from_source("Pilot2_2021", "ses_rural", "ses_rural")
imput_from_source("Omnibus", "ses_mtlIsle", "ses_urban")
imput_from_source("Omnibus", "ses_mtl", "ses_urban")
imput_from_source("Omnibus", "ses_qcCity", "ses_urban")
imput_from_source("Omnibus", "ses_noMtlQc", "ses_urban")





















