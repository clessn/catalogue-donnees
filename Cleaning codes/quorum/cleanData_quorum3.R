#### BEGINNING OF DOCUMENT ####

## Library used ##
library(foreign)
library(tidyverse)
library(haven)

## Loading/Reading a Datafile ##

QRM21 <- read.spss("_SharedFolder_projet-quorum/Data/QRM3/ULA003.Sav", to.data.frame = TRUE)

#### 0.1 Creating a clean empty dataframe ####
CleanData <- data.frame(year=seq(2021, 2021, length=nrow(QRM21)))

#### 0.2 Year ####
CleanData$year <- seq(2021, 2021,length=nrow(QRM21))

#### 0.3 Country ####
CleanData$country <- paste0("CAN")

# to get variable names and labels
attributes(Labels)$variable.labels[1]

#### 1. SES ####
#### Age
CleanData$ses_age34m <- NA
CleanData$ses_age34m[as.numeric(QRM21$AGE) == 1 | as.numeric(QRM21$AGE) == 2] <- 1
CleanData$ses_age34m[as.numeric(QRM21$AGE) != 1 & as.numeric(QRM21$AGE) != 2] <- 0

CleanData$ses_age35p54 <- NA
CleanData$ses_age35p54[as.numeric(QRM21$AGE) == 3 | as.numeric(QRM21$AGE) == 4] <- 1
CleanData$ses_age35p54[as.numeric(QRM21$AGE) != 3 & as.numeric(QRM21$AGE) != 4] <- 0

CleanData$ses_age55p <- NA
CleanData$ses_age55p[as.numeric(QRM21$AGE) == 5 | as.numeric(QRM21$AGE) == 6] <- 1
CleanData$ses_age55p[as.numeric(QRM21$AGE) != 5 & as.numeric(QRM21$AGE) != 6] <- 0

#### Province (only Qc respondents for that one)
CleanData$ses_QC <- NA
CleanData$ses_QC[as.numeric(QRM21$PROV) == 11] <- 1
CleanData$ses_QC[as.numeric(QRM21$PROV) != 11] <- 0

#### Sex
CleanData$ses_female <- NA
CleanData$ses_female[as.numeric(QRM21$SEXE) == 2] <- 1
CleanData$ses_female[as.numeric(QRM21$SEXE) != 2] <- 0
CleanData$ses_female[as.numeric(QRM21$SEXE) == 3] <- NA

#### Lang
CleanData$ses_langFR <- NA
CleanData$ses_langFR[as.numeric(QRM21$LANGU) == 1] <- 1
CleanData$ses_langFR[as.numeric(QRM21$LANGU) == 2 | as.numeric(QRM21$LANGU) == 3] <- 0
CleanData$ses_langEN <- NA
CleanData$ses_langEN[as.numeric(QRM21$LANGU) == 2] <- 1
CleanData$ses_langEN[as.numeric(QRM21$LANGU) == 1 | as.numeric(QRM21$LANGU) == 3] <- 0
CleanData$ses_langOther <- NA
CleanData$ses_langOther[as.numeric(QRM21$LANGU) == 3] <- 1
CleanData$ses_langOther[as.numeric(QRM21$LANGU) == 1 | as.numeric(QRM21$LANGU) == 2] <- 0

#### Scolarité
table(QRM21$SD2)
table(as.numeric(QRM21$SD2))

CleanData$ses_educ_BHS <- NA
CleanData$ses_educ_BHS[!is.na(QRM21$SD2)] <- 0
CleanData$ses_educ_BHS[as.numeric(QRM21$SD2) %in% c(1,2,3,4)] <- 1
table(CleanData$ses_educ_BHS)

CleanData$ses_educ_coll <- NA
CleanData$ses_educ_coll[!is.na(QRM21$SD2)] <- 0
CleanData$ses_educ_coll[as.numeric(QRM21$SD2) ==5] <- 1
table(CleanData$ses_educ_coll)

CleanData$ses_educ_univBA <- NA
CleanData$ses_educ_univBA[!is.na(QRM21$SD2)] <- 0
CleanData$ses_educ_univBA[as.numeric(QRM21$SD2) ==6] <- 1

table(CleanData$ses_educ_univBA)

CleanData$ses_educ_univOverBA <- NA
CleanData$ses_educ_univOverBA[!is.na(QRM21$SD2)] <- 0
CleanData$ses_educ_univOverBA[as.numeric(QRM21$SD2) ==7] <- 1

table(CleanData$ses_educ_univOverBA)

#### Income
table(QRM21$SD3, useNA="always")
table(as.numeric(QRM21$SD3), useNA="always")

CleanData$ses_income_low <- NA
CleanData$ses_income_low[!is.na(QRM21$SD3)] <- 0
CleanData$ses_income_low[as.numeric(QRM21$SD3) %in% c(1,2,3)] <- 1
CleanData$ses_income_low[as.numeric(QRM21$SD3) == 9] <- NA
table(CleanData$ses_income_low)

CleanData$ses_income_mid <- NA
CleanData$ses_income_mid[!is.na(QRM21$SD3)] <- 0
CleanData$ses_income_mid[as.numeric(QRM21$SD3) %in% c(4,5,6)] <- 1
CleanData$ses_income_mid[as.numeric(QRM21$SD3) == 9] <- NA
table(CleanData$ses_income_mid)

CleanData$ses_income_high <- NA
CleanData$ses_income_high[!is.na(QRM21$SD3)] <- 0
CleanData$ses_income_high[as.numeric(QRM21$SD3) %in% c(7,8)] <- 1
CleanData$ses_income_high[as.numeric(QRM21$SD3) == 9] <- NA
table(CleanData$ses_income_high)

#### PolInterest
table(QRM21$P1_A1)

CleanData$dem_interestPol <- NA
CleanData$dem_interestPol[QRM21$P1_A1 %in% c(0,1)] <- 0
CleanData$dem_interestPol[QRM21$P1_A1 %in% c(2,3,4)] <- 0.25
CleanData$dem_interestPol[QRM21$P1_A1 %in% c(5)] <- 0.5
CleanData$dem_interestPol[QRM21$P1_A1 %in% c(6,7,8)] <- 0.75
CleanData$dem_interestPol[QRM21$P1_A1 %in% c(9,10)] <- 1
table(CleanData$dem_interestPol)

CleanData$dem_interestPol_alt <- (as.numeric(QRM21$P1_A1) - 1) / 10

#### PersHealth - En général, comment décririez-vous votre santé?
table(QRM21$P2)
table(as.numeric(QRM21$P2))

CleanData$pond_persHealth <- NA
CleanData$pond_persHealth[as.numeric(QRM21$P2) == 1] <- 0 #Mauvaise
CleanData$pond_persHealth[as.numeric(QRM21$P2) == 2] <- 0.25
CleanData$pond_persHealth[as.numeric(QRM21$P2) == 3] <- 0.5
CleanData$pond_persHealth[as.numeric(QRM21$P2) == 4] <- 0.75
CleanData$pond_persHealth[as.numeric(QRM21$P2) == 5] <- 1 #Excellente

#### helpsWithTech - D'autres personnes viennent vers moi pour des conseils sur de nouvelles technologies.
table(QRM21$P3)
table(as.numeric(QRM21$P3))

CleanData$pond_helpsWithTech <- NA
CleanData$pond_helpsWithTech[as.numeric(QRM21$P3) == 5] <- 0 #Fortement en désaccord 
CleanData$pond_helpsWithTech[as.numeric(QRM21$P3) == 4] <- 0.25
CleanData$pond_helpsWithTech[as.numeric(QRM21$P3) == 3] <- 0.5
CleanData$pond_helpsWithTech[as.numeric(QRM21$P3) == 2] <- 0.75
CleanData$pond_helpsWithTech[as.numeric(QRM21$P3) == 1] <- 1 #Fortement en accord

#### Pour quel parti voteriez-vous s'il y avait une élection fédérale canadienne aujourd'hui?
table(QRM21$P4)
table(as.numeric(QRM21$P4))

CleanData$dem_voteIntentLib<- NA
CleanData$dem_voteIntentLib[as.numeric(QRM21$P4) == 1]<- 1
CleanData$dem_voteIntentLib[as.numeric(QRM21$P4) != 1]<- 0
CleanData$dem_voteIntentLib[as.numeric(QRM21$P4) %in% c(7,8,9,10)]<- NA

CleanData$dem_voteIntentCons<- NA
CleanData$dem_voteIntentCons[as.numeric(QRM21$P4) == 2]<- 1
CleanData$dem_voteIntentCons[as.numeric(QRM21$P4) != 2]<- 0
CleanData$dem_voteIntentCons[as.numeric(QRM21$P4) %in% c(7,8,9,10)]<- NA

CleanData$dem_voteIntentNdp<- NA
CleanData$dem_voteIntentNdp[as.numeric(QRM21$P4) == 3]<- 1
CleanData$dem_voteIntentNdp[as.numeric(QRM21$P4) != 3]<- 0
CleanData$dem_voteIntentNdp[as.numeric(QRM21$P4) %in% c(7,8,9,10)]<- NA

CleanData$dem_voteIntentGrn<- NA
CleanData$dem_voteIntentGrn[as.numeric(QRM21$P4) == 4]<- 1
CleanData$dem_voteIntentGrn[as.numeric(QRM21$P4) != 4]<- 0
CleanData$dem_voteIntentGrn[as.numeric(QRM21$P4) %in% c(7,8,9,10)]<- NA

CleanData$dem_voteIntentPeople<- NA
CleanData$dem_voteIntentPeople[as.numeric(QRM21$P4) == 5]<- 1
CleanData$dem_voteIntentPeople[as.numeric(QRM21$P4) != 5]<- 0
CleanData$dem_voteIntentPeople[as.numeric(QRM21$P4) %in% c(7,8,9,10)]<- NA

CleanData$dem_voteIntentBloc<- NA
CleanData$dem_voteIntentBloc[as.numeric(QRM21$P4) == 6]<- 1
CleanData$dem_voteIntentBloc[as.numeric(QRM21$P4) != 6]<- 0
CleanData$dem_voteIntentBloc[as.numeric(QRM21$P4) %in% c(7,8,9,10)]<- NA


#### Pour quel parti avez-vous voté aux élections fédérales de 2019?
table(QRM21$P5)
table(as.numeric(QRM21$P5))

CleanData$dem_voted19Lib<- NA
CleanData$dem_voted19Lib[as.numeric(QRM21$P5) == 1]<- 1
CleanData$dem_voted19Lib[as.numeric(QRM21$P5) != 1]<- 0
CleanData$dem_voted19Lib[as.numeric(QRM21$P5) %in% c(7,8,9,10)]<- NA

CleanData$dem_voted19Cons<- NA
CleanData$dem_voted19Cons[as.numeric(QRM21$P5) == 2]<- 1
CleanData$dem_voted19Cons[as.numeric(QRM21$P5) != 2]<- 0
CleanData$dem_voted19Cons[as.numeric(QRM21$P5) %in% c(7,8,9,10)]<- NA

CleanData$dem_voted19Ndp<- NA
CleanData$dem_voted19Ndp[as.numeric(QRM21$P5) == 3]<- 1
CleanData$dem_voted19Ndp[as.numeric(QRM21$P5) != 3]<- 0
CleanData$dem_voted19Ndp[as.numeric(QRM21$P5) %in% c(7,8,9,10)]<- NA

CleanData$dem_voted19Grn<- NA
CleanData$dem_voted19Grn[as.numeric(QRM21$P5) == 4]<- 1
CleanData$dem_voted19Grn[as.numeric(QRM21$P5) != 4]<- 0
CleanData$dem_voted19Grn[as.numeric(QRM21$P5) %in% c(7,8,9,10)]<- NA

CleanData$dem_voted19People<- NA
CleanData$dem_voted19People[as.numeric(QRM21$P5) == 5]<- 1
CleanData$dem_voted19People[as.numeric(QRM21$P5) != 5]<- 0
CleanData$dem_voted19People[as.numeric(QRM21$P5) %in% c(7,8,9,10)]<- NA

CleanData$dem_voted19Bloc<- NA
CleanData$dem_voted19Bloc[as.numeric(QRM21$P5) == 6]<- 1
CleanData$dem_voted19Bloc[as.numeric(QRM21$P5) != 6]<- 0
CleanData$dem_voted19Bloc[as.numeric(QRM21$P5) %in% c(7,8,9,10)]<- NA

#### 2. Démocratie ####

CleanData$dem_crisisOppTemper <- NA
CleanData$dem_crisisOppTemper[as.numeric(QRM21$Q1_A1) == 1] <- 1
CleanData$dem_crisisOppTemper[as.numeric(QRM21$Q1_A1) == 2] <- 0.75
CleanData$dem_crisisOppTemper[as.numeric(QRM21$Q1_A1) == 3] <- 0.5
CleanData$dem_crisisOppTemper[as.numeric(QRM21$Q1_A1) == 4] <- 0.25
CleanData$dem_crisisOppTemper[as.numeric(QRM21$Q1_A1) == 5] <- 0

CleanData$dem_crisisAuthIsBetter <- NA
CleanData$dem_crisisAuthIsBetter[as.numeric(QRM21$Q1_A2) == 1] <- 1
CleanData$dem_crisisAuthIsBetter[as.numeric(QRM21$Q1_A2) == 2] <- 0.75
CleanData$dem_crisisAuthIsBetter[as.numeric(QRM21$Q1_A2) == 3] <- 0.5
CleanData$dem_crisisAuthIsBetter[as.numeric(QRM21$Q1_A2) == 4] <- 0.25
CleanData$dem_crisisAuthIsBetter[as.numeric(QRM21$Q1_A2) == 5] <- 0

CleanData$dem_crisisMediaCensorOk <- NA
CleanData$dem_crisisMediaCensorOk[as.numeric(QRM21$Q1_A3) == 1] <- 1
CleanData$dem_crisisMediaCensorOk[as.numeric(QRM21$Q1_A3) == 2] <- 0.75
CleanData$dem_crisisMediaCensorOk[as.numeric(QRM21$Q1_A3) == 3] <- 0.5
CleanData$dem_crisisMediaCensorOk[as.numeric(QRM21$Q1_A3) == 4] <- 0.25
CleanData$dem_crisisMediaCensorOk[as.numeric(QRM21$Q1_A3) == 5] <- 0

CleanData$dem_excludeExtrmOps <- NA
CleanData$dem_excludeExtrmOps[as.numeric(QRM21$Q1_A4) == 2] <- 1 # Agree
CleanData$dem_excludeExtrmOps[as.numeric(QRM21$Q1_A4) == 3] <- 0.75
CleanData$dem_excludeExtrmOps[as.numeric(QRM21$Q1_A4) == 4] <- 0.5
CleanData$dem_excludeExtrmOps[as.numeric(QRM21$Q1_A4) == 5] <- 0.25
CleanData$dem_excludeExtrmOps[as.numeric(QRM21$Q1_A4) == 6] <- 0


#### 3. conspiration ####

CleanData$consp_msDeriveAuth <- NA
CleanData$consp_msDeriveAuth[as.numeric(QRM21$Q2_A1) == 1] <- 1 # Agree
CleanData$consp_msDeriveAuth[as.numeric(QRM21$Q2_A1) == 2] <- 0.75
CleanData$consp_msDeriveAuth[as.numeric(QRM21$Q2_A1) == 3] <- 0.5
CleanData$consp_msDeriveAuth[as.numeric(QRM21$Q2_A1) == 4] <- 0.25
CleanData$consp_msDeriveAuth[as.numeric(QRM21$Q2_A1) == 5] <- 0

CleanData$consp_vaccinSurveillance <- NA
CleanData$consp_vaccinSurveillance[as.numeric(QRM21$Q2_A2) == 1] <- 1 # Agree
CleanData$consp_vaccinSurveillance[as.numeric(QRM21$Q2_A2) == 2] <- 0.75
CleanData$consp_vaccinSurveillance[as.numeric(QRM21$Q2_A2) == 3] <- 0.5
CleanData$consp_vaccinSurveillance[as.numeric(QRM21$Q2_A2) == 4] <- 0.25
CleanData$consp_vaccinSurveillance[as.numeric(QRM21$Q2_A2) == 5] <- 0

CleanData$consp_pandPireQueDit <- NA
CleanData$consp_pandPireQueDit[as.numeric(QRM21$Q2_A3) == 1] <- 1 # Agree
CleanData$consp_pandPireQueDit[as.numeric(QRM21$Q2_A3) == 2] <- 0.75
CleanData$consp_pandPireQueDit[as.numeric(QRM21$Q2_A3) == 3] <- 0.5
CleanData$consp_pandPireQueDit[as.numeric(QRM21$Q2_A3) == 4] <- 0.25
CleanData$consp_pandPireQueDit[as.numeric(QRM21$Q2_A3) == 5] <- 0



#### 4. Santé pub ####
#### Les commerces devraient avoir le droit de vérifier la température de leurs clients avant d'entrer.
table(QRM21$Q3_A1)
table(as.numeric(QRM21$Q3_A1))

CleanData$ia_sante_dem_busiCheckTemp <- NA
CleanData$ia_sante_dem_busiCheckTemp[as.numeric(QRM21$Q3_A1) == 1] <- 1 #FA
CleanData$ia_sante_dem_busiCheckTemp[as.numeric(QRM21$Q3_A1) == 2] <- 0.75
CleanData$ia_sante_dem_busiCheckTemp[as.numeric(QRM21$Q3_A1) == 3] <- 0.5
CleanData$ia_sante_dem_busiCheckTemp[as.numeric(QRM21$Q3_A1) == 4] <- 0.25
CleanData$ia_sante_dem_busiCheckTemp[as.numeric(QRM21$Q3_A1) == 5] <- 0 #FD

#### Les personnes infectées par la COVID-19 devraient obligatoirement partager toutes informations personnelles considérées comme utiles par les autorités.
table(QRM21$Q3_A2)
table(as.numeric(QRM21$Q3_A2))

CleanData$ia_sante_dem_covidPersoInfo <- NA
CleanData$ia_sante_dem_covidPersoInfo[as.numeric(QRM21$Q3_A2) == 1] <- 1 #FA
CleanData$ia_sante_dem_covidPersoInfo[as.numeric(QRM21$Q3_A2) == 2] <- 0.75
CleanData$ia_sante_dem_covidPersoInfo[as.numeric(QRM21$Q3_A2) == 3] <- 0.5
CleanData$ia_sante_dem_covidPersoInfo[as.numeric(QRM21$Q3_A2) == 4] <- 0.25
CleanData$ia_sante_dem_covidPersoInfo[as.numeric(QRM21$Q3_A2) == 5] <- 0 #FD

#### Il est nécessaire que le port du masque soit obligatoire dans tous les endroits publics fermés (magasins, restaurants, etc.).
table(QRM21$Q3_A3)
table(as.numeric(QRM21$Q3_A3))

CleanData$ia_sante_dem_maskMandatory <- NA
CleanData$ia_sante_dem_maskMandatory[as.numeric(QRM21$Q3_A3) == 1] <- 1 #FA
CleanData$ia_sante_dem_maskMandatory[as.numeric(QRM21$Q3_A3) == 2] <- 0.75
CleanData$ia_sante_dem_maskMandatory[as.numeric(QRM21$Q3_A3) == 3] <- 0.5
CleanData$ia_sante_dem_maskMandatory[as.numeric(QRM21$Q3_A3) == 4] <- 0.25
CleanData$ia_sante_dem_maskMandatory[as.numeric(QRM21$Q3_A3) == 5] <- 0 #FD

#### Le port du masque obligatoire est une mesure exagérée.
table(QRM21$Q3_A4)
table(as.numeric(QRM21$Q3_A4))

CleanData$ia_sante_dem_maskExagerated <- NA
CleanData$ia_sante_dem_maskExagerated[as.numeric(QRM21$Q3_A4) == 1] <- 1 #FA
CleanData$ia_sante_dem_maskExagerated[as.numeric(QRM21$Q3_A4) == 2] <- 0.75
CleanData$ia_sante_dem_maskExagerated[as.numeric(QRM21$Q3_A4) == 3] <- 0.5
CleanData$ia_sante_dem_maskExagerated[as.numeric(QRM21$Q3_A4) == 4] <- 0.25
CleanData$ia_sante_dem_maskExagerated[as.numeric(QRM21$Q3_A4) == 5] <- 0 #FD

#### Limiter nos droits et libertés est nécessaire à la protection de la santé publique.
table(QRM21$Q3_A5)
table(as.numeric(QRM21$Q3_A5))

CleanData$dem_crisiLimitRights <- NA
CleanData$dem_crisiLimitRights[as.numeric(QRM21$Q3_A5) == 1] <- 1 #FA
CleanData$dem_crisiLimitRights[as.numeric(QRM21$Q3_A5) == 2] <- 0.75
CleanData$dem_crisiLimitRights[as.numeric(QRM21$Q3_A5) == 3] <- 0.5
CleanData$dem_crisiLimitRights[as.numeric(QRM21$Q3_A5) == 4] <- 0.25
CleanData$dem_crisiLimitRights[as.numeric(QRM21$Q3_A5) == 5] <- 0 #FD

#### Si l'intérêt de la santé publique l'exige, le partage des données personnelles de géolocalisation devrait être obligatoire pour tous.
table(QRM21$Q3_A6)
table(as.numeric(QRM21$Q3_A6))

CleanData$geolocMandatory <- NA
CleanData$geolocMandatory[as.numeric(QRM21$Q3_A6) == 1] <- 1 #FA
CleanData$geolocMandatory[as.numeric(QRM21$Q3_A6) == 2] <- 0.75
CleanData$geolocMandatory[as.numeric(QRM21$Q3_A6) == 3] <- 0.5
CleanData$geolocMandatory[as.numeric(QRM21$Q3_A6) == 4] <- 0.25
CleanData$geolocMandatory[as.numeric(QRM21$Q3_A6) == 5] <- 0 #FD

#### La vaccination contre la COVID-19 devrait être obligatoire pour tout le monde.
table(QRM21$Q3_A7)
table(as.numeric(QRM21$Q3_A7))

CleanData$sante_pub_vaccinMandatory <- NA
CleanData$sante_pub_vaccinMandatory[as.numeric(QRM21$Q3_A7) == 1] <- 1 #FA
CleanData$sante_pub_vaccinMandatory[as.numeric(QRM21$Q3_A7) == 2] <- 0.75
CleanData$sante_pub_vaccinMandatory[as.numeric(QRM21$Q3_A7) == 3] <- 0.5
CleanData$sante_pub_vaccinMandatory[as.numeric(QRM21$Q3_A7) == 4] <- 0.25
CleanData$sante_pub_vaccinMandatory[as.numeric(QRM21$Q3_A7) == 5] <- 0 #FD

#### Un couvre-feu est une mesure nécessaire.
table(QRM21$Q3_A8)
table(as.numeric(QRM21$Q3_A8))

CleanData$sante_pub_curfewNecessary <- NA
CleanData$sante_pub_curfewNecessary[as.numeric(QRM21$Q3_A8) == 1] <- 1 #FA
CleanData$sante_pub_curfewNecessary[as.numeric(QRM21$Q3_A8) == 2] <- 0.75
CleanData$sante_pub_curfewNecessary[as.numeric(QRM21$Q3_A8) == 3] <- 0.5
CleanData$sante_pub_curfewNecessary[as.numeric(QRM21$Q3_A8) == 4] <- 0.25
CleanData$sante_pub_curfewNecessary[as.numeric(QRM21$Q3_A8) == 5] <- 0 #FD

#### Le non-respect grave des mesures sanitaires devrait pouvoir mener jusqu'à la prison.
table(QRM21$Q3_A9)
table(as.numeric(QRM21$Q3_A9))

CleanData$sante_dem_imprison_noncompliers <- NA
CleanData$sante_dem_imprison_noncompliers[as.numeric(QRM21$Q3_A9) == 1] <- 1 #FA
CleanData$sante_dem_imprison_noncompliers[as.numeric(QRM21$Q3_A9) == 2] <- 0.75
CleanData$sante_dem_imprison_noncompliers[as.numeric(QRM21$Q3_A9) == 3] <- 0.5
CleanData$sante_dem_imprison_noncompliers[as.numeric(QRM21$Q3_A9) == 4] <- 0.25
CleanData$sante_dem_imprison_noncompliers[as.numeric(QRM21$Q3_A9) == 5] <- 0 #FD

#### 5. Technologie ####
#### La technologie affaiblira les principaux aspects de la démocratie au cours de la prochaine décennie.
table(QRM21$Q4_A1)
table(as.numeric(QRM21$Q4_A1))

CleanData$dem_tech_demWeakenByTech <- NA
CleanData$dem_tech_demWeakenByTech[as.numeric(QRM21$Q4_A1) == 1] <- 1 #FA
CleanData$dem_tech_demWeakenByTech[as.numeric(QRM21$Q4_A1) == 2] <- 0.75
CleanData$dem_tech_demWeakenByTech[as.numeric(QRM21$Q4_A1) == 3] <- 0.5
CleanData$dem_tech_demWeakenByTech[as.numeric(QRM21$Q4_A1) == 4] <- 0.25
CleanData$dem_tech_demWeakenByTech[as.numeric(QRM21$Q4_A1) == 5] <- 0 #FD

#### Les nouvelles technologies sont généralement bénéfiques pour la société.
table(QRM21$Q4_A2)
table(as.numeric(QRM21$Q4_A2))

CleanData$dem_tech_techGoodDSoc <- NA
CleanData$dem_tech_techGoodSoc[as.numeric(QRM21$Q4_A2) == 1] <- 1 #FA
CleanData$dem_tech_techGoodSoc[as.numeric(QRM21$Q4_A2) == 2] <- 0.75
CleanData$dem_tech_techGoodSoc[as.numeric(QRM21$Q4_A2) == 3] <- 0.5
CleanData$dem_tech_techGoodSoc[as.numeric(QRM21$Q4_A2) == 4] <- 0.25
CleanData$dem_tech_techGoodSoc[as.numeric(QRM21$Q4_A2) == 5] <- 0 #FD
#### Je suis très préoccupé(e) par les changements technologiques tels que l'intelligence artificielle.
table(QRM21$Q4_A3)
table(as.numeric(QRM21$Q4_A3))

CleanData$sci_tech_AIConcernsMe <- NA
CleanData$sci_tech_AIConcernsMe[as.numeric(QRM21$Q4_A3) == 1] <- 1 #FA
CleanData$sci_tech_AIConcernsMe[as.numeric(QRM21$Q4_A3) == 2] <- 0.75
CleanData$sci_tech_AIConcernsMe[as.numeric(QRM21$Q4_A3) == 3] <- 0.5
CleanData$sci_tech_AIConcernsMe[as.numeric(QRM21$Q4_A3) == 4] <- 0.25
CleanData$sci_tech_AIConcernsMe[as.numeric(QRM21$Q4_A3) == 5] <- 0 #FD

#### Je crains que l'intelligence artificielle attaque et nuise aux humains.
table(QRM21$Q4_A4)
table(as.numeric(QRM21$Q4_A4))

CleanData$sci_tech_AIHurtHumans <- NA
CleanData$sci_tech_AIHurtHumans[as.numeric(QRM21$Q4_A4) == 1] <- 1 #FA
CleanData$sci_tech_AIHurtHumans[as.numeric(QRM21$Q4_A4) == 2] <- 0.75
CleanData$sci_tech_AIHurtHumans[as.numeric(QRM21$Q4_A4) == 3] <- 0.5
CleanData$sci_tech_AIHurtHumans[as.numeric(QRM21$Q4_A4) == 4] <- 0.25
CleanData$sci_tech_AIHurtHumans[as.numeric(QRM21$Q4_A4) == 5] <- 0 #FD

#### Je détesterais l'idée que l'intelligence artificielle influence les décisions politiques.
table(QRM21$Q4_A5)
table(as.numeric(QRM21$Q4_A5))

CleanData$dem_tech_AIPolDecisions <- NA
CleanData$dem_tech_AIPolDecisions[as.numeric(QRM21$Q4_A5) == 1] <- 1 #FA
CleanData$dem_tech_AIPolDecisions[as.numeric(QRM21$Q4_A5) == 2] <- 0.75
CleanData$dem_tech_AIPolDecisions[as.numeric(QRM21$Q4_A5) == 3] <- 0.5
CleanData$dem_tech_AIPolDecisions[as.numeric(QRM21$Q4_A5) == 4] <- 0.25
CleanData$dem_tech_AIPolDecisions[as.numeric(QRM21$Q4_A5) == 5] <- 0 #FD
#### On peut faire confiance aux gens qui développent l'intelligence artificielle. 
table(QRM21$Q4_A6)
table(as.numeric(QRM21$Q4_A6))

CleanData$dem_tech_AITrustPpl <- NA
CleanData$dem_tech_AITrustPpl[as.numeric(QRM21$Q4_A6) == 1] <- 1 #FA
CleanData$dem_tech_AITrustPpl[as.numeric(QRM21$Q4_A6) == 2] <- 0.75
CleanData$dem_tech_AITrustPpl[as.numeric(QRM21$Q4_A6) == 3] <- 0.5
CleanData$dem_tech_AITrustPpl[as.numeric(QRM21$Q4_A6) == 4] <- 0.25
CleanData$dem_tech_AITrustPpl[as.numeric(QRM21$Q4_A6) == 5] <- 0 #FD

#### 6. P/O ####

CleanData$sante_pub_2ndWaveWorse <- NA
CleanData$sante_pub_2ndWaveWorse[as.numeric(QRM21$Q5_A1) == 1] <- 1 # Agree
CleanData$sante_pub_2ndWaveWorse[as.numeric(QRM21$Q5_A1) == 2] <- 0.75
CleanData$sante_pub_2ndWaveWorse[as.numeric(QRM21$Q5_A1) == 3] <- 0.5
CleanData$sante_pub_2ndWaveWorse[as.numeric(QRM21$Q5_A1) == 4] <- 0.25
CleanData$sante_pub_2ndWaveWorse[as.numeric(QRM21$Q5_A1) == 5] <- 0

CleanData$sante_pub_worseBehindUs <- NA
CleanData$sante_pub_worseBehindUs[as.numeric(QRM21$Q5_A2) == 1] <- 1 # Agree
CleanData$sante_pub_worseBehindUs[as.numeric(QRM21$Q5_A2) == 2] <- 0.75
CleanData$sante_pub_worseBehindUs[as.numeric(QRM21$Q5_A2) == 3] <- 0.5
CleanData$sante_pub_worseBehindUs[as.numeric(QRM21$Q5_A2) == 4] <- 0.25
CleanData$sante_pub_worseBehindUs[as.numeric(QRM21$Q5_A2) == 5] <- 0

CleanData$sante_pub_futureLooksBleak <- NA
CleanData$sante_pub_futureLooksBleak[as.numeric(QRM21$Q5_A3) == 1] <- 1 # Agree
CleanData$sante_pub_futureLooksBleak[as.numeric(QRM21$Q5_A3) == 2] <- 0.75
CleanData$sante_pub_futureLooksBleak[as.numeric(QRM21$Q5_A3) == 3] <- 0.5
CleanData$sante_pub_futureLooksBleak[as.numeric(QRM21$Q5_A3) == 4] <- 0.25
CleanData$sante_pub_futureLooksBleak[as.numeric(QRM21$Q5_A3) == 5] <- 0

CleanData$sante_pub_covidTreathHumankind <- NA
CleanData$sante_pub_covidTreathHumankind[as.numeric(QRM21$Q5_A4) == 1] <- 1 # Agree
CleanData$sante_pub_covidTreathHumankind[as.numeric(QRM21$Q5_A4) == 2] <- 0.75
CleanData$sante_pub_covidTreathHumankind[as.numeric(QRM21$Q5_A4) == 3] <- 0.5
CleanData$sante_pub_covidTreathHumankind[as.numeric(QRM21$Q5_A4) == 4] <- 0.25
CleanData$sante_pub_covidTreathHumankind[as.numeric(QRM21$Q5_A4) == 5] <- 0

CleanData$sante_pub_lifeNeverSame <- NA
CleanData$sante_pub_lifeNeverSame[as.numeric(QRM21$Q5_A5) == 1] <- 1 # Agree
CleanData$sante_pub_lifeNeverSame[as.numeric(QRM21$Q5_A5) == 2] <- 0.75
CleanData$sante_pub_lifeNeverSame[as.numeric(QRM21$Q5_A5) == 3] <- 0.5
CleanData$sante_pub_lifeNeverSame[as.numeric(QRM21$Q5_A5) == 4] <- 0.25
CleanData$sante_pub_lifeNeverSame[as.numeric(QRM21$Q5_A5) == 5] <- 0

#### 7. adding weights ####

#leanData$weights <- QRM21$POIDS

# Fast check on the data frame
fastT = apply(CleanData, 2, table)

#### 8. Saving ####
today <- Sys.Date()
write_excel_csv(CleanData, paste0("_SharedFolder_projet-quorum/Data/QRM3/QRM3_",today,".csv"))
