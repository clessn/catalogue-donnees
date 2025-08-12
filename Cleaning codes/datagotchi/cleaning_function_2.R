######################################################################################## Transformation et cleaning des données du Hub - Datagotchi 2021 ########################################################################################


#### ___ ####


#### 0.1 - libraries ####

library(splitstackshape)
library(kableExtra)
library(tidymodels)
library(tidyverse)
library(clessnhub)
library(workflows)
library(corrr)
library(caret)
library(tune)
library(nnet)
library(mice)
set.seed(123)


#### 0.2 - Load data ####

clessnhub::connect()
clessnhub::list_databank_permissions("datagotchi_answers")
data <- clessnhub::get_databank_items("datagotchi_answers")

write_csv(data, "../bav-2021/_SharedFolder_bav-2021/Data/Raw/RawData-Hub.csv")

#### ___ ####

#### 1 - Cleaning de base ####

DataNew <- data %>%
  # filter(correction != -1) %>%
  mutate(answers.age     = as.numeric(answers.age),
         answers.tattoos = case_when(answers.tattoos == "1" ~ 1,
                                     answers.tattoos == "2" ~ 2,
                                     answers.tattoos == "3" ~ 3,
                                     answers.tattoos == "4" ~ 4,
                                     answers.tattoos == "5 et plus" ~ 5,
                                     answers.tattoos == "Aucun tatouage" ~ 0),
         correction       = as.numeric(correction),
         winner           = as.numeric(winner)) %>%
  ### Put NA for imputation ###
  mutate(correction = case_when(correction == -1 ~ NA_real_,
                                T ~ as.numeric(correction)))

#### 1.1 - Imputation ####

# ImputationData <- mice(DataNew, m=5,maxit=50, meth='pmm', seed=500)
# summary(ImputationData)
# 
# DataNew <- complete(ImputationData,1)

#### 2 - Application de la méga-fonction de cleaning ####

dataCleaning <- function(data)  {
  data$female <- ifelse(data$answers.gender == "Féminin", 1, 0)
  data$ses_genderOther <- ifelse(data$answers.gender != "Féminin" && data$answers.gender != "Masculin", 1, 0)
  # age
  data$age34m <- ifelse(data$answers.age <= 34, 1, 0)
  data$age55p <- ifelse(data$answers.age >= 55, 1, 0)
  # style
#  data$app_swag_VintageHippBoheme <- ifelse(data$answers.clothing %in% c("Hippie", "Vintage", "Bohème"), 1, 0)
#  data$app_swag_Chic <- ifelse(data$answers.clothing == "Chic", 1, 0)
#  data$app_swag_Classique <- ifelse(data$answers.clothing == "Classique", 1, 0)
#  data$app_swag_Casual <- ifelse(data$answers.clothing == "Décontracté", 1, 0)
#  data$app_swag_Formel <- ifelse(data$answers.clothing == "Formel", 1, 0)
#  data$app_swag_Other <- ifelse(data$answers.clothing %in% c("Autre", "Punk"), 1, 0)
#  data$app_swag_Rock <- ifelse(data$answers.clothing == "Rock", 1, 0)
#  data$app_swag_Sport <- ifelse(data$answers.clothing == "Sportif", 1, 0)
  # tatouages
  data$app_noTattoo <- ifelse(data$answers.tattoos >= 1, 0, 1)
  # habitation
# data$ses_dwelling_app <- ifelse(data$answers.dwelling == "Appartement dans un immeuble de moins de cinq étages", 1, 0)
# data$ses_dwelling_condo <- ifelse(data$answers.dwelling == "Condo", 1, 0)
# data$ses_dwelling_coop <- ifelse(data$answers.dwelling == "Coopérative", 1, 0)
# data$ses_dwelling_HLM <- ifelse(data$answers.dwelling == "HLM", 1, 0)
# data$ses_dwelling_semiDetached <- ifelse(data$answers.dwelling == "Jumelé", 1, 0)
# data$ses_dwelling_loft <- ifelse(data$answers.dwelling == "Loft", 1, 0)
# data$ses_dwelling_townHouse <- ifelse(data$answers.dwelling == "Maison de ville", 1, 0)
# data$ses_dwelling_detachedHouse <- ifelse(data$answers.dwelling == "Maison individuelle", 1, 0)
# data$ses_dwelling_mobile <- ifelse(data$answers.dwelling == "Maison mobile (bateau, VR, etc.)", 1, 0)
# data$ses_dwelling_tour <- ifelse(data$answers.dwelling == "Tour d’habitation", 1, 0)
# data$ses_dwelling_other <- ifelse(data$answers.dwelling == "Autre", 1, 0)
  # province
  data$west <- ifelse(data$answers.province %in% c("Alberta", "Colombie-Britannique", "Manitoba", "Saskatchewan"), 1, 0)
  data$maritimes <- ifelse(data$answers.province %in% c("Île-du-Prince-Édouard", "Nouveau-Brunswick", "Nouvelle-Écosse", "Terre-Neuve-et-Labrador"), 1, 0)
  data$quebec <- ifelse(data$answers.province == "Québec", 1, 0)
  # pays
  data$immigrant <- ifelse(data$answers.birthplace.code == "ca", 0, 1)
  # langue
  data$langFr <- ifelse(data$answers.language == "Français", 1, 0)
  data$ses_languageOther <- ifelse(data$answers.language == "Autre", 1, 0)
  # orientation
  data$ses_hetero <- ifelse(data$answers.sexual_orientation == "Hétérosexuel(le)", 1, 0)
  # sport
 #  data$act_Walk <- ifelse(data$answers.sport == "Marche", 1, 0)
 #  data$act_Gym <- ifelse(data$answers.sport == "Entraînement en salle", 1, 0)
 #  data$act_TeamSport <- ifelse(data$answers.sport == "Sport d’équipe", 1, 0)
 #  data$act_Run <- ifelse(data$answers.sport == "Course à pied", 1, 0)
 #  data$act_Yoga <- ifelse(data$answers.sport == "Yoga", 1, 0)
 #  data$act_Swimming <- ifelse(data$answers.sport == "Natation", 1, 0)
 #  data$act_Other <- ifelse(data$answers.sport == "Autre", 1, 0)
 #  data$act_None <- ifelse(data$answers.sport == "Je ne fais pas d’activités physiques", 1, 0)
  # transport
 #  data$act_transport_Car <- ifelse(data$answers.transport == "Voiture", 1, 0)
 #  data$act_transport_SUV <- ifelse(data$answers.transport == "VUS", 1, 0)
 #  data$act_transport_Moto <- ifelse(data$answers.transport == "Moto", 1, 0)
 #  data$act_transport_Walk <- ifelse(data$answers.transport == "Marche", 1, 0)
 #  data$act_transport_Bicycle <- ifelse(data$answers.transport == "Vélo", 1, 0)
 #  data$act_transport_Taxi <- ifelse(data$answers.transport == "Taxi", 1, 0)
 #  data$act_transport_PublicTransportation <- ifelse(data$answers.transport == "Transport en commun", 1, 0)
 #  # véhicule
  # magasins
 #  data$cons_brand_MaR <- ifelse(data$answers.shopping == "Magasins à rayons (La Baie, Simons, etc.)", 1, 0)
 #  data$cons_brand_BInd <- ifelse(data$answers.shopping == "Boutiques indépendantes", 1, 0)
 #  data$cons_brand_ChainesB <- ifelse(data$answers.shopping == "Chaînes de boutiques (Gap, Zara, etc.)", 1, 0)
 #  data$cons_brand_GSurf <- ifelse(data$answers.shopping == "Grandes surfaces (Walmart, Costco, etc.)", 1, 0)
 #  data$cons_brand_OnlineOnly <- ifelse(data$answers.shopping == "Magasins exclusivement en ligne", 1, 0)
 #  data$cons_brand_Frip <- ifelse(data$answers.shopping == "Friperies", 1, 0)
 #  data$cons_brand_Other <- ifelse(data$answers.shopping == "Autre", 1, 0)
  # café
 # data$cons_coffee_place_noCoffee <- ifelse(data$answers.coffee_shop == "Je ne vais pas dans les cafés", 1, 0)
 # data$cons_coffee_TimH <- ifelse(data$answers.coffee_shop == "Tim Hortons", 1, 0)
 # data$cons_coffee_Starbucks <- ifelse(data$answers.coffee_shop == "Starbucks", 1, 0)
 # data$cons_coffee_SC <- ifelse(data$answers.coffee_shop == "Second Cup", 1, 0)
 # data$cons_coffee_McDo <- ifelse(data$answers.coffee_shop == "McDonald’s", 1, 0)
 # data$cons_coffee_Other <- ifelse(data$answers.coffee_shop == "Autre", 1, 0)
 # data$cons_coffee_place_ind <- ifelse(data$answers.coffee_shop == "Cafés indépendants", 1, 0)
  # alimentation
 # data$cons_Meat <- ifelse(data$answers.food %in% c("Oui, beaucoup", "Oui, mais de façon modérée"), 1, 0)
 # data$cons_Vege <- ifelse(data$answers.food == "Non, je suis végétarien(ne)", 1, 0)
 # data$cons_Vegan <- ifelse(data$answers.food == "Non, je suis vegan", 1, 0)
  # alcool
# data$cons_regBeers <- ifelse(data$answers.alcohol == "Bière régulière", 1, 0)
# data$cons_sparklingDrink <- ifelse(data$answers.alcohol == "Vin mousseux ou champagne", 1, 0)
# data$cons_redWineDrink <- ifelse(data$answers.alcohol == "Vin rouge", 1, 0)
# data$cons_roseDrink <- ifelse(data$answers.alcohol == "Vin rosé", 1, 0)
# data$cons_whiteWineDrink <- ifelse(data$answers.alcohol == "Vin blanc", 1, 0)
# data$cons_microBeers <- ifelse(data$answers.alcohol == "Bière artisanale ou de microbrasserie", 1, 0)
# data$cons_spiritDrink <- ifelse(data$answers.alcohol == "Boisson spiritueuse", 1, 0)
# data$cons_cocktailsDrink <- ifelse(data$answers.alcohol == "Cocktail", 1, 0)
# data$cons_noDrink <- ifelse(data$answers.alcohol == "Je ne bois pas d’alcool", 1, 0)
  # cigarette
#  data$cons_Smoke <- ifelse(data$answers.smoking == "Oui", 1, 0)
#  data$cons_SmokeStopping <- ifelse(data$answers.smoking == "Oui, mais j’essaye d’arrêter", 1, 0)
#  data$cons_SmokeStopped <- ifelse(data$answers.smoking == "Non, j’ai arrêté", 1, 0)
#  data$cons_SmokeNever <- ifelse(data$answers.smoking == "Non, je n’ai jamais fumé", 1, 0)
#  data$cons_VapeNation <- ifelse(data$answers.smoking == "Non, mais je vapote", 1, 0)
  # film
#  data$film_Action <- ifelse(data$answers.cinema.genre == "Action", 1, 0)
#  data$film_Adventure <- ifelse(data$answers.cinema.genre == "Adventure", 1, 0)
#  data$film_Animation <- ifelse(data$answers.cinema.genre == "Animation", 1, 0)
#  data$film_Comedy <- ifelse(data$answers.cinema.genre == "Comedy", 1, 0)
#  data$film_Crime <- ifelse(data$answers.cinema.genre == "Crime", 1, 0)
#  data$film_Documentary <- ifelse(data$answers.cinema.genre == "Documentary", 1, 0)
#  data$film_Drama <- ifelse(data$answers.cinema.genre == "Drama", 1, 0)
#  data$film_Family <- ifelse(data$answers.cinema.genre == "Family", 1, 0)
#  data$film_Fantasy <- ifelse(data$answers.cinema.genre == "Fantasy", 1, 0)
#  data$film_History <- ifelse(data$answers.cinema.genre == "History", 1, 0)
#  data$film_Horror <- ifelse(data$answers.cinema.genre == "Horror", 1, 0)
#  data$film_Music <- ifelse(data$answers.cinema.genre == "Music", 1, 0)
#  data$film_Mystery <- ifelse(data$answers.cinema.genre == "Mystery", 1, 0)
#  data$film_Romance <- ifelse(data$answers.cinema.genre == "Romance", 1, 0)
#  data$film_ScienceFiction <- ifelse(data$answers.cinema.genre == "Science Fiction", 1, 0)
#  data$film_Thriller <- ifelse(data$answers.cinema.genre == "Thriller", 1, 0)
#  data$film_TVMovie <- ifelse(data$answers.cinema.genre == "TV Movie", 1, 0)
#  data$film_unknown <- ifelse(data$answers.cinema.genre == "unknown", 1, 0)
#  data$film_War <- ifelse(data$answers.cinema.genre == "War", 1, 0)
#  data$film_Western <- ifelse(data$answers.cinema.genre == "Western", 1, 0)
#  # voiture
#  data$vehicule_4x4 <- ifelse(data$answers.vehicle == "4x4", 1, 0)
#  data$vehicule_other <- ifelse(data$answers.vehicle == "Autre", 1, 0)
#  data$vehicule_Berline <- ifelse(data$answers.vehicle == "Berline régulière ou familiale", 1, 0)
#  data$vehicule_Cabriolet <- ifelse(data$answers.vehicle == "Cabriolet ou roadster", 1, 0)
#  data$vehicule_noCar <- ifelse(data$answers.vehicle == "Je ne conduis ou n’utilise jamais de voitures", 1, 0)
#  data$vehicule_PickUp <- ifelse(data$answers.vehicle == "Pick-up", 1, 0)
#  data$vehicule_Van <- ifelse(data$answers.vehicle == "Van ou minifourgonnette", 1, 0)
#  data$vehicule_luxury <- ifelse(data$answers.vehicle == "Voiture de luxe (Mercedes, Porsche, etc.)", 1, 0)
#  data$vehicule_sport <- ifelse(data$answers.vehicle == "Voiture de sport", 1, 0)
#  data$vehicule_electric <- ifelse(data$answers.vehicle == "Voiture hybride ou électrique", 1, 0)
#  data$vehicule_VUS <- ifelse(data$answers.vehicle == "VUS", 1, 0)
  # Animal
# data$animal_farm <- ifelse(data$answers.pets == "Animaux de la ferme", 1, 0)
# data$animal_domestic <- ifelse(data$answers.pets == "Autres animaux domestiques", 1, 0)
# data$animal_cat <- ifelse(data$answers.pets == "Chat(s)", 1, 0)
# data$animal_catNdog <- ifelse(data$answers.pets == "Chat(s) et chien(s)", 1, 0)
# data$animal_dog <- ifelse(data$answers.pets == "Chien(s)", 1, 0)
# data$animal_noPet <- ifelse(data$answers.pets == "Je n’ai pas d’animaux de compagnie", 1, 0)
  
  # chasse
  # data$act_Hunting <- ifelse(data$answers.hunting == "Jamais", 0, 0)
  # data$act_Hunting <- ifelse(data$answers.hunting == "Presque jamais", 0.25, 0)
  # data$act_Hunting <- ifelse(data$answers.hunting == "Parfois", 0.5, 0)
  # data$act_Hunting <- ifelse(data$answers.hunting == "Souvent", 0.75, 0)
  # data$act_Hunting <- ifelse(data$answers.hunting == "Très souvent", 1, 0)
  # pêche
  # data$act_Fishing <- ifelse(data$answers.fishing == "Jamais", 0, 0)
  # data$act_Fishing <- ifelse(data$answers.fishing == "Presque jamais", 0.25, 0)
  # data$act_Fishing <- ifelse(data$answers.fishing == "Parfois", 0.5, 0)
  # data$act_Fishing <- ifelse(data$answers.fishing == "Souvent", 0.75, 0)
  # data$act_Fishing <- ifelse(data$answers.fishing == "Très souvent", 1, 0)
  # plein air
  # data$act_MotorizedOutdoorActivities <- ifelse(data$answers.outdoor_activities == "Jamais", 0, 0)
  # data$act_MotorizedOutdoorActivities <- ifelse(data$answers.outdoor_activities == "Presque jamais", 0.25, 0)
  # data$act_MotorizedOutdoorActivities <- ifelse(data$answers.outdoor_activities == "Parfois", 0.5, 0)
  # data$act_MotorizedOutdoorActivities <- ifelse(data$answers.outdoor_activities == "Souvent", 0.75, 0)
  # data$act_MotorizedOutdoorActivities <- ifelse(data$answers.outdoor_activities == "Très souvent", 1, 0)
  # bénévolat
  # data$act_Volunteering <- ifelse(data$answers.volunteering == "Jamais", 0, 0)
  # data$act_Volunteering <- ifelse(data$answers.volunteering == "Presque jamais", 0.25, 0)
  # data$act_Volunteering <- ifelse(data$answers.volunteering == "Parfois", 0.5, 0)
  # data$act_Volunteering <- ifelse(data$answers.volunteering == "Souvent", 0.75, 0)
  # data$act_Volunteering <- ifelse(data$answers.volunteering == "Très souvent", 1, 0)
  #art
  # data$act_VisitsMuseumsGaleries <- ifelse(data$answers.art == "Jamais", 0, 0)
  # data$act_VisitsMuseumsGaleries <- ifelse(data$answers.art == "Presque jamais", 0.25, 0)
  # data$act_VisitsMuseumsGaleries <- ifelse(data$answers.art == "Parfois", 0.5, 0)
  # data$act_VisitsMuseumsGaleries <- ifelse(data$answers.art == "Souvent", 0.75, 0)
  # data$act_VisitsMuseumsGaleries <- ifelse(data$answers.art == "Très souvent", 1, 0)
  # éducation
  data$educBHS <- ifelse(data$answers.education %in% c("Aucune scolarité", "École primaire", "École secondaire"), 1, 0)
  data$educUniv <- ifelse(data$answers.education %in% c("Baccalauréat", "Maîtrise", "Doctorat"), 1, 0)
  # revenu
  data$incomeLow <- ifelse(data$answers.income %in% c("Aucun revenu", "1 $ à 30 000 $"), 1, 0)
  data$incomeHigh <- ifelse(data$answers.income %in% c("90 001 $ à 110 000 $", "110 001 $ à 150 000 $", "Plus de 200 000 $"), 1, 0)
  
  data <- data %>% mutate(across(c(answers.hunting, answers.fishing, answers.outdoor_activities, answers.volunteering, answers.art),
                                 ~ case_when(. == "Jamais"        ~ 0,
                                             . == "Presque jamais" ~ 0.25,
                                             . == "Parfois"    ~ 0.5,
                                             . == "Souvent"        ~ 0.75,
                                             . == "Très souvent" ~ 1))) %>%
    rename(act_Hunting                    = answers.hunting,
           act_Fishing                    = answers.fishing,
           act_MotorizedOutdoorActivities = answers.outdoor_activities,
           act_Volunteering               = answers.volunteering,
           act_VisitsMuseumsGaleries      = answers.art)
  return(data)
}

#### ___ ####

#### 2.1 - Sélection des variables pertinentes (à tester pour la suite) ####


#### ____ ####

#### 3 - Retrait des variables inutiles aux modèles et cleaning des choix de vote ####


DataFinalHub <- dataCleaning(DataNew) %>%
  select(-c(answers.income, answers.education, answers.smoking,
            answers.alcohol, answers.food, answers.coffee_shop,
            answers.shopping, answers.transport, answers.sport,
            answers.sexual_orientation, answers.language,
            answers.birthplace.code, answers.province,
            answers.dwelling, answers.tattoos,
            answers.clothing, answers.age, answers.gender,
            answers.appearance, answers.cinema.genre,
            answers.birthplace.artwork_url,
            answers.cinema.artwork_url,
            answers.cinema.id,
            answers.cinema.title,
            answers.music.artist,
            answers.music.term,
            answers.pets,
            answers.skin,
            answers.birthplace,
            answers.cinema,
            answers.cinema.genre,
            answers.cinema.term,
            answers.ethnicity,
            answers.music.artwork_url,
            answers.music.id,
            answers.music.title,
            answers.postal_code,
            answers.vehicle,
            created,
            id,
            is_self,
            survey)) %>%
  mutate(realPred = case_when(winner == 4 ~ 1,   # Lib
                              winner == 2 ~ 2,   # Cons
                              winner == 5 ~ 3,   # Ndp
                              winner == 1 ~ 4,   # Bloc
                              winner == 3 ~ 5    # Green
  ),
  op_voteIntent_Lib   = ifelse(correction  == 4, 1, 0),
  op_voteIntent_Cons  = ifelse(correction  == 2, 1, 0),
  op_voteIntent_Ndp   = ifelse(correction  == 5, 1, 0),
  op_voteIntent_Bloc  = ifelse(correction  == 1, 1, 0),
  op_voteIntent_Green = ifelse(correction  == 3, 1, 0)) %>%
  select(-c("correction",
            "prediction.1.name",   # BQ
            "prediction.1.value",
            "prediction.2.name",   # PCC
            "prediction.2.value",
            "prediction.3.name",   # Green
            "prediction.3.value",
            "prediction.4.name",   # Lib
            "prediction.4.value",
            "prediction.5.name",
            "prediction.5.value",  # NDP
            "winner"
  ))


#### 3.1 - cleaning music variable ####

DataFinalHub <- DataFinalHub %>%
  mutate(musicStyle = case_when(answers.music.genre %in% c("trance",
                                                           "trip hop",
                                                           "techno",
                                                           "alternative dance",
                                                           "breakbeat",
                                                           "breakcore",
                                                           "breaks",
                                                           "broken beat ",
                                                           "chillout",
                                                           "chillwave",
                                                           "chiptune",
                                                           "club",
                                                           "dance",
                                                           "deep house",
                                                           "drum and bass",
                                                           "dub",
                                                           "downtempo",
                                                           "dub techno",
                                                           "dubstep",
                                                           "ebm",
                                                           "edm",
                                                           "electro",
                                                           "electro house",
                                                           "electronic",
                                                           "electronica",
                                                           "hardstyle",
                                                           "house",
                                                           "idm",
                                                           "leftfield",
                                                           "synthwave",
                                                           "tech house")  ~ "electro",
                                answers.music.genre %in% c("trap",
                                                           "chopped and screwed ",
                                                           "contemporary r&b",
                                                           "gangsta rap",
                                                           "g-funk",
                                                           "hardcore hip hop",
                                                           "r&b",
                                                           "east coast hip hop",
                                                           "alternative hip hop",
                                                           "hip hop") ~ "rap_rnb",
                                answers.music.genre %in% c("viking metal",
                                                           "trash metal",
                                                           "technical death metal",
                                                           "power metal",
                                                           "nu metal",
                                                           "alternative metal",
                                                           "atmospheric black metal",
                                                           "black metal",
                                                           "brutal death metal",
                                                           "death metal",
                                                           "death-doom metal ",
                                                           "doom metal",
                                                           "deathcore",
                                                           "deathgrind",
                                                           "folk metal",
                                                           "funk metal",
                                                           "gothic metal",
                                                           "grindcore",
                                                           "groove metal",
                                                           "heavy metal",
                                                           "melodic death metal",
                                                           "metal",
                                                           "metalcore",
                                                           "post-metal",
                                                           "speed metal",
                                                           "symphonic metal") ~ "metal",
                                answers.music.genre %in% c("vocal jazz",
                                                           "acid jazz",
                                                           "afrobeat",
                                                           "cool jazz",
                                                           "contemporary jazz",
                                                           "avant-garde jazz",
                                                           "big band",
                                                           "bossa nova",
                                                           "contemporary jazz",
                                                           "free jazz",
                                                           "fusion",
                                                           "future jazz",
                                                           "hard bop",
                                                           "jazz",
                                                           "swing") ~ "jazz",
                                # answers.music.genre %in% c("yé-yé",
                                #                            "chanson française",
                                #                            "zeuhl",
                                #                            "ballad",
                                #                            "christmas music",
                                #                            "avant-garde",
                                #                            "experimental",
                                #                            "no wave",
                                #                            "christmas music",
                                #                            "dreampop") ~ "unknown",
                                answers.music.genre %in% c("ballad") ~ "ballad",
                                answers.music.genre %in% c("teen pop",
                                                           "Pop",
                                                           "alternative pop",
                                                           "art pop",
                                                           "dance-pop",
                                                           "disco",
                                                           "electropop",
                                                           "europop",
                                                           "folk pop",
                                                           "indian pop",
                                                           "indie pop",
                                                           "j-pop",
                                                           "k-pop",
                                                           "mandopop",
                                                           "nerdcore") ~ "pop",
                                answers.music.genre %in% c("progressive rock",
                                                           "rock",
                                                           "rock and roll",
                                                           "acoustic rock",
                                                           "aor",
                                                           "art rock",
                                                           "baroque pop",
                                                           "canterbury scene ",
                                                           "chamber pop", 
                                                           "classic rock",
                                                           "comedy rock",
                                                           "heartland rock",
                                                           "indie rock", 
                                                           "instrumental rock",
                                                           "math rock",
                                                           "neo-progressive rock ",
                                                           "rockabilly",
                                                           "southern rock",
                                                           "surf rock",
                                                           "symphonic rock") ~ "rock",
                                answers.music.genre %in% c("acid rock", 
                                                           "alternative rock", 
                                                           "arena rock", 
                                                           "boggie rock", 
                                                           "britpop", 
                                                           "experimental rock", 
                                                           "garage rock", 
                                                           "glam",  
                                                           "glam rock",  
                                                           "gothic rock",  
                                                           "pop rock",  
                                                           "shoegaze") ~ "alternative_rock",
                                answers.music.genre %in% c("folk rock",
                                                           "roots rock",
                                                           "soft rock") ~ "folk_rock",
                                answers.music.genre %in% c("blues",
                                                           "blues rock",
                                                           "african blues",
                                                           "blue-eyed soul",
                                                           "funk",
                                                           "funk soul",
                                                           "motown ",
                                                           "neo soul",
                                                           "pop soul",
                                                           "smooth soul",
                                                           "soul",
                                                           "soul jazz") ~ "blues_soul",
                                answers.music.genre %in% c("alternative folk",
                                                           "contemporary folk",
                                                           "folk",
                                                           "indie folk") ~ "folk",
                                answers.music.genre %in% c("alternative punk",
                                                           "anti-folk",
                                                           "dance-punk",
                                                           "dark wave",
                                                           "emo",
                                                           "folk punk",
                                                           "garage punk",
                                                           "gothic",
                                                           "hardcore punk",
                                                           "horror punk",
                                                           "mathcore",
                                                           "melodic hardcore",
                                                           "neo-psychedelia ",
                                                           "pop punk",
                                                           "post-hardcore",
                                                           "post-punk",
                                                           "ska punk") ~ "punk",
                                answers.music.genre %in% c("ambient",
                                                           "ambient pop",
                                                           "new age",
                                                           "celtic music",
                                                           "easy listening",
                                                           "lo-fi",
                                                           "instrumental",
                                                           "lounge",
                                                           "minimal") ~ "ambiance",
                                answers.music.genre %in% c("americana",
                                                           "country",
                                                           "bluegrass",
                                                           "country pop",
                                                           "country rock",
                                                           "gothic country",
                                                           "nashville sound") ~ "country",
                                answers.music.genre %in% c("bachata",
                                                           "cumbia",
                                                           "fado",
                                                           "flamenco",
                                                           "latin",
                                                           "latin pop",
                                                           "latin rock",
                                                           "merengue",
                                                           "mpb",
                                                           "reggaeton") ~ "latin_music",
                                answers.music.genre %in% c("opera",
                                                           "orchestral",
                                                           "baroque",
                                                           "bolero",
                                                           "classical",
                                                           "classical crossover",
                                                           "modern classical") ~ "classical",
                                answers.music.genre %in% c("compas",
                                                           "dancehall") ~ "african",
                                answers.music.genre %in% c("coldwave",
                                                           "new wave",
                                                           "minimal wave",
                                                           "synth-pop") ~ "new_wave",
                                # answers.music.genre %in% c("comedy",
                                #                            "dark ambient",
                                #                            "indie",
                                #                            "medieval",
                                #                            "musical",
                                #                            "noise",
                                #                            "non-music",
                                #                            "schlager",
                                #                            "singer-songwriter",
                                #                            "unknown",
                                #                            "spoken word") ~ "other",
                                answers.music.genre %in% c("ragga",
                                                           "raggae",
                                                           "ska") ~ "raggae",
                                answers.music.genre %in% c("contemporary christian",
                                                           "christian rock") ~ "religious_music",
                                answers.music.genre %in% c("grunge",
                                                           "hard rock",
                                                           "post-grunge",
                                                           "post rock",
                                                           "stoner rock") ~ "hard_rock",
                                answers.music.genre %in% c("industrial",
                                                           "industrial metal",
                                                           "industrial rock",
                                                           "neofolk") ~ "industrial")) %>%
  select(-answers.music.genre)


#write_csv(DataFinalHub, "/Users/nadjimfrechet/Dropbox/CLESSN/bav-2021/_SharedFolder_bav-2021/Data/Clean/27-10-2021-DatagotchiHub-2.csv")


# DataCan <- DataFinalHub %>% filter(quebec == 0)
# 
# 
# DataQc <- DataFinalHub %>% 
#   filter(quebec == 1) %>%
#   mutate(partyVote = case_when(op_voteIntent_Lib    == 1 ~ "Liberal",
#                                op_voteIntent_Cons   == 1 ~ "Conservative",
#                                op_voteIntent_Ndp    == 1 ~ "NDP",
#                                op_voteIntent_Bloc   == 1 ~ "Bloc",
#                                op_voteIntent_Green  == 1 ~ "Green"
#                                )) %>%
#   group_by(partyVote) %>%
#   stratified(., "partyVote", size = c("Liberal"      = .3,
#                                       "Conservative" = .2,
#                                       "NDP"          = .2,
#                                       "Bloc"         = .25,
#                                       "Green"        = .05), replace = F)
# 
# prop.table(table(DataQc$partyVote))
# 
# 
# DataFinalHubNew <- bind_rows(DataCan, DataQc)

# https://www.rdocumentation.org/packages/splitstackshape/versions/1.4.8/topics/stratified

# https://towardsdatascience.com/stratified-sampling-and-how-to-perform-it-in-r-8b753efde1ef


#### ___ ####

write_csv(DataFinalHub, "../bav-2021/_SharedFolder_bav-2021/Data/Clean/15-02-2022-DatagotchiHub-categ.csv")



