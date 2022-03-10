library(foreign)

RES2011 <- readRDS("Dropbox/Travail/CLESSN/catalogue-donnees/_SharedFolder_catalogue-donnees/Data/resultats_electoraux/ResultatsParBureau2011.rds")

RES2015 <- readRDS("Dropbox/Travail/CLESSN/catalogue-donnees/_SharedFolder_catalogue-donnees/Data/resultats_electoraux/ResultatsParBureau2015.rds")

RES2019 <- readRDS("Dropbox/Travail/CLESSN/catalogue-donnees/_SharedFolder_catalogue-donnees/Data/resultats_electoraux/ResultatsParBureau2019.rds")

RES2021 <- readRDS("Dropbox/Travail/CLESSN/catalogue-donnees/_SharedFolder_catalogue-donnees/Data/resultats_electoraux/ResultatsParCirc2021.rds")

CCSA <- read.csv("Dropbox/Travail/CLESSN/catalogue-donnees/_SharedFolder_catalogue-donnees/Data/sondage_CCSA/CleanData2020-10-19.csv")

OMN01 <- read.spss("Dropbox/Travail/CLESSN/catalogue-donnees/_SharedFolder_catalogue-donnees/Data/omnibus/OMN0122.Sav", to.data.frame = T)

LUCID <- read.csv("Dropbox/Travail/CLESSN/catalogue-donnees/_SharedFolder_catalogue-donnees/Data/these_camille/LUCID_ligneBleue_December+31,+2021_14.47.csv")

INSPQ <- read.csv("Dropbox/Travail/CLESSN/catalogue-donnees/_SharedFolder_catalogue-donnees/Data/sondage_INSPQ/MainlineData_CleanData_2020-06-01.csv")

QUORUM2 <- readRDS("Dropbox/Travail/CLESSN/02_projet_quorum/projet-korum/Data/QRM2/QRM2_08-01-2021.rds")

QUORUM2x <- read.csv("Dropbox/Travail/CLESSN/02_projet_quorum/projet-korum/Data/QRM2/QRM2_2020-07-10.csv")

CIVIMETRE <- read.csv("Dropbox/Travail/CLESSN/02_projet_quorum/projet-korum/Data/Civimetre1.csv")

LUCID2 <- read.csv("Dropbox/Travail/CLESSN/02_projet_quorum/projet-korum/Data/lucidQualtrics/LUCID_ligneBleue_01-11-21.csv")
