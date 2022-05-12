
library(foreign)
library(tidyverse)
library(haven)
library(varhandle)

#opening the data frame !! N'oubliez pas d'ouvrir dans le r projet bav-2021 pour éviter la multiplication des paths !!
#  Personne ne devrait avoir à modifier les paths ci-dessous
DD <- read_sav("../bav-2021/_SharedFolder_bav-2021/Data/Raw/ULA008.Sav")

CleanData3 <- data.frame(X = c(1:nrow(DD)))

CleanData3 <- data.frame(year=seq(2021, 2021, length=nrow(DD)))

#### 0.2 Year ####
CleanData3$year <- seq(2021, 2021,length=nrow(DD))


# Fonction min max

minmaxNormalization <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}


## GENDER 

CleanData3$male <- 0
CleanData3$male[DD$SEXE == 1] <- 1 
table(CleanData3$male)

CleanData3$female <- 0
CleanData3$female[DD$SEXE == 2] <- 1
table(CleanData3$female)

CleanData3$ses_genderOther <- 0
CleanData3$ses_genderOther[DD$SEXE == 3] <- 1
table(CleanData3$ses_genderOther)

## ÂGE  

CleanData3$age34m <- 0
CleanData3$age34m[DD$QAGE >= 18 & DD$QAGE < 35] <- 1
table(CleanData3$age34m)

CleanData3$age3554 <- 0
CleanData3$age3554[DD$QAGE >= 35 & DD$QAGE < 55] <- 1
table(CleanData3$age3554)

CleanData3$age55p <- 0
CleanData3$age55p[DD$QAGE >= 55] <- 1
table(CleanData3$age55p)


## LANGUE MATERNELLE - quelle est votre langue maternelle, autrement dit, la première langue que vous avez apprise et que vous comprenez encore aujourd'hui?

CleanData3$langFr <- 0
CleanData3$langFr[DD$LANGM == 2] <- 1
table(CleanData3$langFr)

CleanData3$langEn <- 0
CleanData3$langEn[DD$LANGM == 1] <- 1
table(CleanData3$langEn)

CleanData3$ses_languageOther <- 0
CleanData3$ses_languageOther[DD$LANGM == 3] <- 1
table(CleanData3$ses_languageOther)

## INCOME 

# Approximativement, dans laquelle des catégories suivantes le revenu de votre ménage de situe-t-il? 

table(DD$SES2)

CleanData3$ses_income_None <- NA
CleanData3$ses_income_None[DD$SES2 == 1] <- 1 
CleanData3$ses_income_None[DD$SES2 != 1] <- 0 
CleanData3$ses_income_None[DD$SES2 == 9] <- NA
table(CleanData3$ses_income_None)

CleanData3$ses_income_i1to30 <- NA
CleanData3$ses_income_i1to30[DD$SES2 == 2] <- 1 
CleanData3$ses_income_i1to30[DD$SES2 != 2] <- 0 
CleanData3$ses_income_i1to30[DD$SES2 == 9] <- NA
table(CleanData3$ses_income_i1to30)

CleanData3$ses_income_i31to60 <- NA
CleanData3$ses_income_i31to60[DD$SES2 == 3] <- 1 
CleanData3$ses_income_i31to60[DD$SES2 != 3] <- 0 
CleanData3$ses_income_i31to60[DD$SES2 == 9] <- NA
table(CleanData3$ses_income_i31to60)

CleanData3$ses_income_i61to90 <- NA
CleanData3$ses_income_i61to90[DD$SES2 == 4] <- 1 
CleanData3$ses_income_i61to90[DD$SES2 != 4] <- 0 
CleanData3$ses_income_i61to90[DD$SES2 == 9] <- NA
table(CleanData3$ses_income_i61to90)

CleanData3$ses_income_i91to110 <- NA
CleanData3$ses_income_i91to110[DD$SES2 == 5] <- 1 
CleanData3$ses_income_i91to110[DD$SES2 != 5] <- 0 
CleanData3$ses_income_i91to110[DD$SES2 == 9] <- NA
table(CleanData3$ses_income_i91to110)

CleanData3$ses_income_i111to150  <- NA
CleanData3$ses_income_i111to150 [DD$SES2 == 6] <- 1 
CleanData3$ses_income_i111to150 [DD$SES2 != 6] <- 0 
CleanData3$ses_income_i111to150 [DD$SES2 == 9] <- NA
table(CleanData3$ses_income_i111to150)

CleanData3$ses_income_i151to200  <- NA
CleanData3$ses_income_i151to200 [DD$SES2 == 7] <- 1 
CleanData3$ses_income_i151to200 [DD$SES2 != 7] <- 0 
CleanData3$ses_income_i151to200 [DD$SES2 == 9] <- NA
table(CleanData3$ses_income_i151to200)

CleanData3$ses_income_i201toInf  <- NA
CleanData3$ses_income_i201toInf [DD$SES2 == 8] <- 1 
CleanData3$ses_income_i201toInf [DD$SES2 != 8] <- 0 
CleanData3$ses_income_i201toInf [DD$SES2 == 9] <- NA
table(CleanData3$ses_income_i201toInf)

### Cleaning according to CES 

## Income 

# Low
CleanData3$incomeLow <- NA
CleanData3$incomeLow[DD$SES2==1 | DD$SES2==2] <- 1
CleanData3$incomeLow[DD$SES2!=1 & DD$SES2!=2] <- 0
table(CleanData3$incomeLow)

# Medium
CleanData3$incomeMid <- NA
CleanData3$incomeMid[DD$SES2==3 | DD$SES2==4] <- 1
CleanData3$incomeMid[DD$SES2!=3 & DD$SES2!=4] <- 0
table(CleanData3$incomeMid)

# High
CleanData3$incomeHigh <- NA
CleanData3$incomeHigh[DD$SES2==5 | DD$SES2==6 | DD$SES2==7 | DD$SES2==8] <- 1
CleanData3$incomeHigh[DD$SES2!=5 & DD$SES2!=6 & DD$SES2!=7 & DD$SES2!=8] <- 0
table(CleanData3$incomeHigh)

## EDUCATION - Quel est votre plus haut niveau de scolarité complété? 

table(DD$SES1)

# high school and below
CleanData3$ses_educ_None <- NA 
CleanData3$ses_educ_None[DD$SES1 == 1] <- 1 
CleanData3$ses_educ_None[DD$SES1 != 1] <- 0
table(CleanData3$ses_educ_None)

CleanData3$ses_educ_Prim <- NA 
CleanData3$ses_educ_Prim[DD$SES1 == 2] <- 1 
CleanData3$ses_educ_Prim[DD$SES1 != 2] <- 0
table(CleanData3$ses_educ_Prim)

CleanData3$ses_educ_Sec <- NA 
CleanData3$ses_educ_Sec[DD$SES1 == 3] <- 1 
CleanData3$ses_educ_Sec[DD$SES1 != 3] <- 0
table(CleanData3$ses_educ_Sec)

CleanData3$ses_educ_Coll <- NA 
CleanData3$ses_educ_Coll[DD$SES1 == 4] <- 1 
CleanData3$ses_educ_Coll[DD$SES1 != 4] <- 0
table(CleanData3$ses_educ_Coll)

CleanData3$ses_educ_Bacc <- NA 
CleanData3$ses_educ_Bacc[DD$SES1 == 5] <- 1 
CleanData3$ses_educ_Bacc[DD$SES1 != 5] <- 0
table(CleanData3$ses_educ_Bacc)

CleanData3$ses_educ_Master <- NA 
CleanData3$ses_educ_Master[DD$SES1 == 6] <- 1 
CleanData3$ses_educ_Master[DD$SES1 != 6] <- 0
table(CleanData3$ses_educ_Master)

CleanData3$ses_educ_PhD <- NA 
CleanData3$ses_educ_PhD[DD$SES1 == 7] <- 1 
CleanData3$ses_educ_PhD[DD$SES1 != 7] <- 0
table(CleanData3$ses_educ_PhD)

#### CLEANING ACCORDNING TO CES 

# Education 
# Below high school
CleanData3$educBHS <- NA
CleanData3$educBHS[!is.na(DD$SES1)] <- 0
CleanData3$educBHS[DD$SES1==1 |
                    DD$SES1==2] <- 1
table(CleanData3$educBHS)

# High school
CleanData3$educHS <- NA
CleanData3$educHS[!is.na(DD$SES1)] <- 0
CleanData3$educHS[DD$SES1==3] <- 1
table(CleanData3$educHS)

# College/University 
CleanData3$educUniv <- NA
CleanData3$educUniv[!is.na(DD$SES1)] <- 0
CleanData3$educUniv[DD$SES1==4 |
                     DD$SES1==5 |
                     DD$SES1==6 |
                     DD$SES1==7] <- 1
table(CleanData3$educUniv)


## PROVINCE 

table(DD$PROV)

CleanData3$ses_prov_Alb <- NA
CleanData3$ses_prov_Alb[DD$PROV==1] <- 1
CleanData3$ses_prov_Alb[DD$PROV!=1] <- 0
table(CleanData3$ses_prov_Alb)

CleanData3$ses_prov_Bc <- NA
CleanData3$ses_prov_Bc[DD$PROV==2] <- 1
CleanData3$ses_prov_Bc[DD$PROV!=2] <- 0
table(CleanData3$ses_prov_Bc)

CleanData3$ses_prov_Manitoba <- NA
CleanData3$ses_prov_Manitoba[DD$PROV==3] <- 1
CleanData3$ses_prov_Manitoba[DD$PROV!=3] <- 0
table(CleanData3$ses_prov_Manitoba)

CleanData3$ses_prov_Nb <- NA
CleanData3$ses_prov_Nb[DD$PROV==4] <- 1
CleanData3$ses_prov_Nb[DD$PROV!=4] <- 0
table(CleanData3$ses_prov_Nb)

CleanData3$ses_prov_Nfl <- NA
CleanData3$ses_prov_Nfl[DD$PROV==5] <- 1
CleanData3$ses_prov_Nfl[DD$PROV!=5] <- 0
table(CleanData3$ses_prov_Nfl)

CleanData3$ses_prov_Ns <- NA
CleanData3$ses_prov_Ns[DD$PROV==6] <- 1
CleanData3$ses_prov_Ns[DD$PROV!=6] <- 0
table(CleanData3$ses_prov_Ns)

CleanData3$ontario <- NA
CleanData3$ontario[DD$PROV==9] <- 1
CleanData3$ontario[DD$PROV!=9] <- 0
table(CleanData3$ontario)

CleanData3$ses_prov_Pei <- NA
CleanData3$ses_prov_Pei[DD$PROV==10] <- 1
CleanData3$ses_prov_Pei[DD$PROV!=10] <- 0
table(CleanData3$ses_prov_Pei)

CleanData3$quebec <- NA
CleanData3$quebec[DD$PROV==11] <- 1
CleanData3$quebec[DD$PROV!=11] <- 0
table(CleanData3$quebec)

CleanData3$ses_prov_Skt <- NA
CleanData3$ses_prov_Skt[DD$PROV==12] <- 1
CleanData3$ses_prov_Skt[DD$PROV!=12] <- 0
table(CleanData3$ses_prov_Skt)

CleanData3$ses_prov_Yukon <- NA
CleanData3$ses_prov_Yukon[DD$PROV==13] <- 1
CleanData3$ses_prov_Yukon[DD$PROV!=13] <- 0
table(CleanData3$ses_prov_Yukon)

CleanData3$maritimes <- NA
CleanData3$maritimes[CleanData3$ses_prov_Pei == 1 | CleanData3$ses_prov_Nb == 1 |
                      CleanData3$ses_prov_Ns == 1 | CleanData3$ses_prov_Nfl == 1] <- 1
CleanData3$maritimes[CleanData3$ses_prov_Pei != 1 & CleanData3$ses_prov_Nb != 1 &
                      CleanData3$ses_prov_Ns != 1 & CleanData3$ses_prov_Nfl != 1] <- 0
table(CleanData3$maritimes)

CleanData3$west <- NA
CleanData3$west[CleanData3$ses_prov_Manitoba == 1 | CleanData3$ses_prov_Alb == 1 |
                 CleanData3$ses_prov_Skt == 1 | CleanData3$ses_prov_Bc == 1 | CleanData3$ses_prov_Yukon == 1] <- 1
CleanData3$west[CleanData3$ses_prov_Manitoba != 1 & CleanData3$ses_prov_Alb != 1 &
                 CleanData3$ses_prov_Skt != 1 & CleanData3$ses_prov_Bc != 1 & CleanData3$ses_prov_Yukon != 1] <- 0
table(CleanData3$west)

## EXERCICE - Que faites-vous le plus souvent pour faire de l'exercice? 

CleanData3$act_Gym <- 0
CleanData3$act_Gym[DD$B1_M1 == 1 |
                     DD$B1_M2 == 1 | 
                     DD$B1_M3 == 1 | 
                     DD$B1_M4 == 1 | 
                     DD$B1_M5 == 1 | 
                     DD$B1_M6 == 1 | 
                     DD$B1_M7 == 1] <- 1
table(CleanData3$act_Gym)

CleanData3$act_TeamSport <- 0
CleanData3$act_TeamSport[DD$B1_M1 == 2 |
                           DD$B1_M2 == 2 | 
                           DD$B1_M3 == 2 | 
                           DD$B1_M4 == 2 | 
                           DD$B1_M5 == 2 | 
                           DD$B1_M6 == 2 | 
                           DD$B1_M7 == 2] <- 1
table(CleanData3$act_TeamSport)

CleanData3$act_Walk <- 0
CleanData3$act_Walk[DD$B1_M1 == 3 |
                      DD$B1_M2 == 3 | 
                      DD$B1_M3 == 3 | 
                      DD$B1_M4 == 3 | 
                      DD$B1_M5 == 3 | 
                      DD$B1_M6 == 3 | 
                      DD$B1_M7 == 3] <- 1
table(CleanData3$act_Walk)

CleanData3$act_Run <- 0
CleanData3$act_Run[DD$B1_M1 == 4 |
                     DD$B1_M2 == 4 | 
                     DD$B1_M3 == 4 | 
                     DD$B1_M4 == 4 | 
                     DD$B1_M5 == 4 | 
                     DD$B1_M6 == 4 | 
                     DD$B1_M7 == 4] <- 1
table(CleanData3$act_Run)

CleanData3$act_Yoga <- 0
CleanData3$act_Yoga[DD$B1_M1 == 5 |
                      DD$B1_M2 == 5 | 
                      DD$B1_M3 == 5 | 
                      DD$B1_M4 == 5 | 
                      DD$B1_M5 == 5 | 
                      DD$B1_M6 == 5 | 
                      DD$B1_M7 == 5] <- 1
table(CleanData3$act_Yoga)

CleanData3$act_Swimming <- 0
CleanData3$act_Swimming[DD$B1_M1 == 6 |
                          DD$B1_M2 == 6 | 
                          DD$B1_M3 == 6 | 
                          DD$B1_M4 == 6 | 
                          DD$B1_M5 == 6 | 
                          DD$B1_M6 == 6 | 
                          DD$B1_M7 == 6] <- 1
table(CleanData3$act_Swimming)

CleanData3$act_Other <- 0
CleanData3$act_Other[DD$B1_M1 == 7 |
                       DD$B1_M2 == 7 | 
                       DD$B1_M3 == 7 | 
                       DD$B1_M4 == 7 | 
                       DD$B1_M5 == 7 | 
                       DD$B1_M6 == 7 | 
                       DD$B1_M7 == 7] <- 1
table(CleanData3$act_Other)

CleanData3$act_None <- 0
CleanData3$act_None[DD$B1_M1 == 8 |
                      DD$B1_M2 == 8 | 
                      DD$B1_M3 == 8 | 
                      DD$B1_M4 == 8 | 
                      DD$B1_M5 == 8 | 
                      DD$B1_M6 == 8 | 
                      DD$B1_M7 == 8] <- 1
table(CleanData3$act_None)

## How often do you engage in the following? ... 1) Never; 5) Very often

## PÊCHE 

CleanData3$act_Fishing <- 0
CleanData3$act_Fishing <- minmaxNormalization(DD$B2_A1)
table(CleanData3$act_Fishing) #0 = never, #1 = very often 

CleanData3$act_Fishing2 <- NA
CleanData3$act_Fishing2[DD$B2_A1 == 1] <- 1 #"Never"
CleanData3$act_Fishing2[DD$B2_A1 == 2] <- 2 
CleanData3$act_Fishing2[DD$B2_A1 == 3] <- 3 
CleanData3$act_Fishing2[DD$B2_A1 == 4] <- 4 
CleanData3$act_Fishing2[DD$B2_A1 == 5] <- 5 #"Very often"
table(CleanData3$act_Fishing2)

## CHASSE 

CleanData3$act_Hunting <- 0
CleanData3$act_Hunting <- minmaxNormalization(DD$B2_A2)
table(CleanData3$act_Hunting) #0 = never, #1 = very often 

CleanData3$act_Hunting2 <- NA
CleanData3$act_Hunting2[DD$B2_A2 == 1] <- 1 #"Never"
CleanData3$act_Hunting2[DD$B2_A2 == 2] <- 2 
CleanData3$act_Hunting2[DD$B2_A2 == 3] <- 3 
CleanData3$act_Hunting2[DD$B2_A2 == 4] <- 4 
CleanData3$act_Hunting2[DD$B2_A2 == 5] <- 5 #"Very often"
table(CleanData3$act_Hunting2)

## WINTER BOARD

CleanData3$act_WinterBoard <- 0
CleanData3$act_WinterBoard <- minmaxNormalization(DD$B2_A3)
table(CleanData3$act_WinterBoard) #0 = never, #1 = very often 

CleanData3$act_WinterBoard2 <- NA
CleanData3$act_WinterBoard2[DD$B2_A3 == 1] <- 1 #"Never"
CleanData3$act_WinterBoard2[DD$B2_A3 == 2] <- 2 
CleanData3$act_WinterBoard2[DD$B2_A3 == 3] <- 3 
CleanData3$act_WinterBoard2[DD$B2_A3 == 4] <- 4 
CleanData3$act_WinterBoard2[DD$B2_A3 == 5] <- 5 #"Very often"
table(CleanData3$act_WinterBoard2)

## SPORT D'ÉQUIPE 

CleanData3$act_DoingTeamSport <- 0
CleanData3$act_DoingTeamSport <- minmaxNormalization(DD$B2_A4)
table(CleanData3$act_DoingTeamSport) #0 = never, #1 = very often 

CleanData3$act_DoingTeamSport2 <- NA
CleanData3$act_DoingTeamSport2[DD$B2_A4 == 1] <- 1 #"Never"
CleanData3$act_DoingTeamSport2[DD$B2_A4 == 2] <- 2 
CleanData3$act_DoingTeamSport2[DD$B2_A4 == 3] <- 3 
CleanData3$act_DoingTeamSport2[DD$B2_A4 == 4] <- 4 
CleanData3$act_DoingTeamSport2[DD$B2_A4 == 5] <- 5 #"Very often"
table(CleanData3$act_DoingTeamSport2)

## GALLERIES D'ART

CleanData3$act_VisitsMuseumsGaleries <- 0
CleanData3$act_VisitsMuseumsGaleries <- minmaxNormalization(DD$B2_A5)
table(CleanData3$act_VisitsMuseumsGaleries) #0 = never, #1 = very often 

CleanData3$act_VisitsMuseumsGaleries2 <- NA
CleanData3$act_VisitsMuseumsGaleries2[DD$B2_A5 == 1] <- 1 #"Never"
CleanData3$act_VisitsMuseumsGaleries2[DD$B2_A5 == 2] <- 2 
CleanData3$act_VisitsMuseumsGaleries2[DD$B2_A5 == 3] <- 3 
CleanData3$act_VisitsMuseumsGaleries2[DD$B2_A5 == 4] <- 4 
CleanData3$act_VisitsMuseumsGaleries2[DD$B2_A5 == 5] <- 5 #"Very often"
table(CleanData3$act_VisitsMuseumsGaleries2)

## VOIR ARTS VIVANTS

CleanData3$act_PerformingArts <- 0
CleanData3$act_PerformingArts <- minmaxNormalization(DD$B2_A6)
table(CleanData3$act_PerformingArts) #0 = never, #1 = very often 

CleanData3$act_PerformingArts2 <- NA
CleanData3$act_PerformingArts2[DD$B2_A6 == 1] <- 1 #"Never"
CleanData3$act_PerformingArts2[DD$B2_A6 == 2] <- 2 
CleanData3$act_PerformingArts2[DD$B2_A6 == 3] <- 3 
CleanData3$act_PerformingArts2[DD$B2_A6 == 4] <- 4 
CleanData3$act_PerformingArts2[DD$B2_A6 == 5] <- 5 #"Very often"
table(CleanData3$act_PerformingArts2)

## ALLER DANS PARTYS

CleanData3$act_PartiesAndSocial <- 0
CleanData3$act_PartiesAndSocial <- minmaxNormalization(DD$B2_A7)
table(CleanData3$act_PartiesAndSocial) #0 = never, #1 = very often 

CleanData3$act_PartiesAndSocial2 <- NA
CleanData3$act_PartiesAndSocial2[DD$B2_A7 == 1] <- 1 #"Never"
CleanData3$act_PartiesAndSocial2[DD$B2_A7 == 2] <- 2 
CleanData3$act_PartiesAndSocial2[DD$B2_A7 == 3] <- 3 
CleanData3$act_PartiesAndSocial2[DD$B2_A7 == 4] <- 4 
CleanData3$act_PartiesAndSocial2[DD$B2_A7 == 5] <- 5 #"Very often"
table(CleanData3$act_PartiesAndSocial2)

## FAIRE TÂCHES MANUELLES

CleanData3$act_ManualTasks <- 0
CleanData3$act_ManualTasks <- minmaxNormalization(DD$B2_A8)
table(CleanData3$act_ManualTasks) #0 = never, #1 = very often 

CleanData3$act_ManualTasks2 <- NA
CleanData3$act_ManualTasks2[DD$B2_A8 == 1] <- 1 #"Never"
CleanData3$act_ManualTasks2[DD$B2_A8 == 2] <- 2 
CleanData3$act_ManualTasks2[DD$B2_A8 == 3] <- 3 
CleanData3$act_ManualTasks2[DD$B2_A8 == 4] <- 4 
CleanData3$act_ManualTasks2[DD$B2_A8 == 5] <- 5 #"Very often"
table(CleanData3$act_ManualTasks2)


## PLEIN AIR MOTORISÉ 

CleanData3$act_MotorizedOutdoorActivities <- 0
CleanData3$act_MotorizedOutdoorActivities <- minmaxNormalization(DD$B2_A9)
table(CleanData3$act_MotorizedOutdoorActivities) #0 = never, #1 = very often 

CleanData3$act_MotorizedOutdoorActivities2 <- NA
CleanData3$act_MotorizedOutdoorActivities2[DD$B2_A9 == 1] <- 1 #"Never"
CleanData3$act_MotorizedOutdoorActivities2[DD$B2_A9 == 2] <- 2 
CleanData3$act_MotorizedOutdoorActivities2[DD$B2_A9 == 3] <- 3 
CleanData3$act_MotorizedOutdoorActivities2[DD$B2_A9 == 4] <- 4 
CleanData3$act_MotorizedOutdoorActivities2[DD$B2_A9 == 5] <- 5 #"Very often"
table(CleanData3$act_MotorizedOutdoorActivities2)

## OUTDOOR (PLEIN AIR NON MOTORISÉ)

CleanData3$act_Outdoors <- 0
CleanData3$act_Outdoors <- minmaxNormalization(DD$B2_A10)
table(CleanData3$act_Outdoors) #0 = never, #1 = very often 

CleanData3$act_Outdoors2 <- NA
CleanData3$act_Outdoors2[DD$B2_A10 == 1] <- 1 #"Never"
CleanData3$act_Outdoors2[DD$B2_A10 == 2] <- 2 
CleanData3$act_Outdoors2[DD$B2_A10 == 3] <- 3 
CleanData3$act_Outdoors2[DD$B2_A10 == 4] <- 4 
CleanData3$act_Outdoors2[DD$B2_A10 == 5] <- 5 #"Very often"
table(CleanData3$act_Outdoors2)

## BENEVOLAT

CleanData3$act_Volunteering <- 0
CleanData3$act_Volunteering <- minmaxNormalization(DD$B2_A11)
table(CleanData3$act_Volunteering) #0 = never, #1 = very often 

CleanData3$act_Volunteering2 <- NA
CleanData3$act_Volunteering2[DD$B2_A11 == 1] <- 1 #"Never"
CleanData3$act_Volunteering2[DD$B2_A11 == 2] <- 2 
CleanData3$act_Volunteering2[DD$B2_A11 == 3] <- 3 
CleanData3$act_Volunteering2[DD$B2_A11 == 4] <- 4 
CleanData3$act_Volunteering2[DD$B2_A11 == 5] <- 5 #"Very often"
table(CleanData3$act_Volunteering2)

## FAIRE ACTIVITÉS ARTISTIQUES 

CleanData3$act_Arts <- 0
CleanData3$act_Arts <- minmaxNormalization(DD$B2_A12)
table(CleanData3$act_Arts) #0 = never, #1 = very often 

CleanData3$act_Arts2 <- NA
CleanData3$act_Arts2[DD$B2_A12 == 1] <- 1 #"Never"
CleanData3$act_Arts2[DD$B2_A12 == 2] <- 2 
CleanData3$act_Arts2[DD$B2_A12 == 3] <- 3 
CleanData3$act_Arts2[DD$B2_A12 == 4] <- 4 
CleanData3$act_Arts2[DD$B2_A12 == 5] <- 5 #"Very often"
table(CleanData3$act_Arts2)

## ALLER LIEUX DE CULTE

CleanData3$act_Worship <- 0
CleanData3$act_Worship <- minmaxNormalization(DD$B2_A13)
table(CleanData3$act_Worship) #0 = never, #1 = very often 

CleanData3$act_Worship2 <- NA
CleanData3$act_Worship2[DD$B2_A13 == 1] <- 1 #"Never"
CleanData3$act_Worship2[DD$B2_A13 == 2] <- 2 
CleanData3$act_Worship2[DD$B2_A13 == 3] <- 3 
CleanData3$act_Worship2[DD$B2_A13 == 4] <- 4 
CleanData3$act_Worship2[DD$B2_A13 == 5] <- 5 #"Very often"
table(CleanData3$act_Worship2)

## FAIRE YOGA

CleanData3$act_DoingYoga <- 0
CleanData3$act_DoingYoga <- minmaxNormalization(DD$B2_A14)
table(CleanData3$act_Yoga) #0 = never, #1 = very often 

CleanData3$act_DoingYoga2 <- NA
CleanData3$act_DoingYoga2[DD$B2_A14 == 1] <- 1 #"Never"
CleanData3$act_DoingYoga2[DD$B2_A14 == 2] <- 2 
CleanData3$act_DoingYoga2[DD$B2_A14 == 3] <- 3 
CleanData3$act_DoingYoga2[DD$B2_A14 == 4] <- 4 
CleanData3$act_DoingYoga2[DD$B2_A14 == 5] <- 5 #"Very often"
table(CleanData3$act_DoingYoga2)

## VOYAGER

CleanData3$act_Travel <- 0
CleanData3$act_Travel <- minmaxNormalization(DD$B2_A15)
table(CleanData3$act_Travel) #0 = never, #1 = very often 

CleanData3$act_Travel2 <- NA
CleanData3$act_Travel2[DD$B2_A15 == 1] <- 1 #"Never"
CleanData3$act_Travel2[DD$B2_A15 == 2] <- 2 
CleanData3$act_Travel2[DD$B2_A15 == 3] <- 3 
CleanData3$act_Travel2[DD$B2_A15 == 4] <- 4 
CleanData3$act_Travel2[DD$B2_A15 == 5] <- 5 #"Very often"
table(CleanData3$act_Travel2)

## VIDEO GAMES 

CleanData3$act_VideoGames <- 0
CleanData3$act_VideoGames <- minmaxNormalization(DD$B2_A16)
table(CleanData3$act_VideoGames) #0 = never, #1 = very often 

CleanData3$act_VideoGames2 <- NA
CleanData3$act_VideoGames2[DD$B2_A16 == 1] <- 1 #"Never"
CleanData3$act_VideoGames2[DD$B2_A16 == 2] <- 2 
CleanData3$act_VideoGames2[DD$B2_A16 == 3] <- 3 
CleanData3$act_VideoGames2[DD$B2_A16 == 4] <- 4 
CleanData3$act_VideoGames2[DD$B2_A16 == 5] <- 5 #"Very often"
table(CleanData3$act_VideoGames2)


## LIRE LIVRES 

CleanData3$act_Books <- 0
CleanData3$act_Books <- minmaxNormalization(DD$B2_A17)
table(CleanData3$act_Books) #0 = never, #1 = very often 

CleanData3$act_Books2 <- NA
CleanData3$act_Books2[DD$B2_A17 == 1] <- 1 #"Never"
CleanData3$act_Books2[DD$B2_A17 == 2] <- 2 
CleanData3$act_Books2[DD$B2_A17 == 3] <- 3 
CleanData3$act_Books2[DD$B2_A17 == 4] <- 4 
CleanData3$act_Books2[DD$B2_A17 == 5] <- 5 #"Very often"
table(CleanData3$act_Books2)

## PLAY BINGO 

CleanData3$act_Bingo <- 0
CleanData3$act_Bingo <- minmaxNormalization(DD$B2_A18)
table(CleanData3$act_Bingo) #0 = never, #1 = very often 

CleanData3$act_Bingo2 <- NA
CleanData3$act_Bingo2[DD$B2_A18 == 1] <- 1 #"Never"
CleanData3$act_Bingo2[DD$B2_A18 == 2] <- 2 
CleanData3$act_Bingo2[DD$B2_A18 == 3] <- 3 
CleanData3$act_Bingo2[DD$B2_A18 == 4] <- 4 
CleanData3$act_Bingo2[DD$B2_A18 == 5] <- 5 #"Very often"
table(CleanData3$act_Bingo2)

## WATCH RACING

CleanData3$act_CarRaces <- 0
CleanData3$act_CarRaces <- minmaxNormalization(DD$B2_A19)
table(CleanData3$act_CarRaces) #0 = never, #1 = very often 

CleanData3$act_CarRaces2 <- NA
CleanData3$act_CarRaces2[DD$B2_A19 == 1] <- 1 #"Never"
CleanData3$act_CarRaces2[DD$B2_A19 == 2] <- 2 
CleanData3$act_CarRaces2[DD$B2_A19 == 3] <- 3 
CleanData3$act_CarRaces2[DD$B2_A19 == 4] <- 4 
CleanData3$act_CarRaces2[DD$B2_A19 == 5] <- 5 #"Very often"
table(CleanData3$act_CarRaces2)

## JOUER QUILLES 

CleanData3$act_Bowling <- 0
CleanData3$act_Bowling <- minmaxNormalization(DD$B2_A20)
table(CleanData3$act_Bowling) #0 = never, #1 = very often 

CleanData3$act_Bowling2 <- NA
CleanData3$act_Bowling2[DD$B2_A20 == 1] <- 1 #"Never"
CleanData3$act_Bowling2[DD$B2_A20 == 2] <- 2 
CleanData3$act_Bowling2[DD$B2_A20 == 3] <- 3 
CleanData3$act_Bowling2[DD$B2_A20 == 4] <- 4 
CleanData3$act_Bowling2[DD$B2_A20 == 5] <- 5 #"Very often"
table(CleanData3$act_Bowling2)


# B2B À quel jeu vidéo jouez-vous le plus souvent?|

# Quel type de transport utilisez-vous le plus régulièrement au quotidien?

CleanData3$act_transport_Car <- NA
CleanData3$act_transport_Car[DD$C1 == 1] <- 1
CleanData3$act_transport_Car[DD$C1 != 1] <- 0
table(CleanData3$act_transport_Car)

CleanData3$act_transport_SUV <- NA
CleanData3$act_transport_SUV[DD$C1 == 2] <- 1
CleanData3$act_transport_SUV[DD$C1 != 2] <- 0
table(CleanData3$act_transport_SUV)

CleanData3$act_transport_Moto <- NA
CleanData3$act_transport_Moto[DD$C1 == 3] <- 1
CleanData3$act_transport_Moto[DD$C1 != 3] <- 0
table(CleanData3$act_transport_Moto)

CleanData3$act_transport_Walk <- NA
CleanData3$act_transport_Walk[DD$C1 == 4] <- 1
CleanData3$act_transport_Walk[DD$C1 != 4] <- 0
table(CleanData3$act_transport_Walk)

CleanData3$act_transport_Bicycle <- NA
CleanData3$act_transport_Bicycle[DD$C1 == 5] <- 1
CleanData3$act_transport_Bicycle[DD$C1 != 5] <- 0
table(CleanData3$act_transport_Bicycle)

CleanData3$act_transport_PublicTransportation <- NA
CleanData3$act_transport_PublicTransportation[DD$C1 == 6] <- 1
CleanData3$act_transport_PublicTransportation[DD$C1 != 6] <- 0
table(CleanData3$act_transport_PublicTransportation)

CleanData3$act_transport_Taxi <- NA
CleanData3$act_transport_Taxi[DD$C1 == 7] <- 1
CleanData3$act_transport_Taxi[DD$C1 != 7] <- 0
table(CleanData3$act_transport_Taxi)

## Quel modèle de voiture utilisez-vous le plus régulièrement? 

# 4x4 
table(DD$C2==1)
CleanData3$act_modelCar_4x4 <- NA
CleanData3$act_modelCar_4x4[DD$C2 == 1] <- 1
CleanData3$act_modelCar_4x4[DD$C2 != 1] <- 0
table(CleanData3$act_modelCar_4x4)

# berline régulière ou familiale 
table(DD$C2==2)
CleanData3$act_modelCar_reg <- NA
CleanData3$act_modelCar_reg[DD$C2 == 2] <- 1
CleanData3$act_modelCar_reg[DD$C2 != 2] <- 0
table(CleanData3$act_modelCar_reg)

# Cabriolet ou roadster (décapotable) 
table(DD$C2==3)
CleanData3$act_modelCar_roadster <- NA
CleanData3$act_modelCar_roadster[DD$C2 == 3] <- 1
CleanData3$act_modelCar_roadster[DD$C2 != 3] <- 0
table(CleanData3$act_modelCar_roadster)

# pick-up
table(DD$C2==4)
CleanData3$act_modelCar_pickup <- NA
CleanData3$act_modelCar_pickup[DD$C2 == 4] <- 1
CleanData3$act_modelCar_pickup[DD$C2 != 4] <- 0
table(CleanData3$act_modelCar_pickup)

# Van ou minifourgonnette 
table(DD$C2==5)
CleanData3$act_modelCar_van <- NA
CleanData3$act_modelCar_van[DD$C2 == 5] <- 1
CleanData3$act_modelCar_van[DD$C2 != 5] <- 0
table(CleanData3$act_modelCar_van)

# Voiture de luxe (Mercedes, Porsche, etc.) 
table(DD$C2==6)
CleanData3$act_modelCar_luxury <- NA
CleanData3$act_modelCar_luxury[DD$C2 == 6] <- 1
CleanData3$act_modelCar_luxury[DD$C2 != 6] <- 0
table(CleanData3$act_modelCar_luxury)

#Voiture de sport 
table(DD$C2==7)
CleanData3$act_modelCar_sport <- NA
CleanData3$act_modelCar_sport[DD$C2 == 7] <- 1
CleanData3$act_modelCar_sport[DD$C2 != 7] <- 0
table(CleanData3$act_modelCar_sport)

#Voiture hybride ou électrique 
table(DD$C2==8)
CleanData3$act_modelCar_electric <- NA
CleanData3$act_modelCar_electric[DD$C2 == 8] <- 1
CleanData3$act_modelCar_electric[DD$C2 != 8] <- 0
table(CleanData3$act_modelCar_electric)

#VUS
table(DD$C2==9)
CleanData3$act_modelCar_VUS <- NA
CleanData3$act_modelCar_VUS[DD$C2 == 9] <- 1
CleanData3$act_modelCar_VUS[DD$C2 != 9] <- 0
table(CleanData3$act_modelCar_VUS)

#Other 
table(DD$C2==10)
CleanData3$act_modelCar_Other <- NA
CleanData3$act_modelCar_Other[DD$C2 == 10] <- 1
CleanData3$act_modelCar_Other[DD$C2 != 10] <- 0
table(CleanData3$act_modelCar_Other)

# Don't drive/Don't use a car 
table(DD$C2==11)
CleanData3$act_modelCar_NoDrive <- NA
CleanData3$act_modelCar_NoDrive[DD$C2 == 11] <- 1
CleanData3$act_modelCar_NoDrive[DD$C2 != 11] <- 0
table(CleanData3$act_modelCar_NoDrive)


# Diriez-vous que votre choix de moyen de transport fait partie de qui vous êtes, ou est simplement la façon la plus efficace de vous rendre du point A au point B?

## Diriez-vous que votre choix de moyen de transport fait partie de qui vous êtes, 
# ou est simplement la façon la plus efficace de vous rendre du point A au point B?

table(DD$C3)
CleanData3$act_transportWhoIAm<-NA
CleanData3$act_transportWhoIAm[DD$C3==1] <- 1
CleanData3$act_transportWhoIAm[DD$C3==2] <- 0
table(CleanData3$act_transportWhoIAm)

CleanData3$act_transportAtoB<-NA
CleanData3$act_transportAtoB[DD$C3==2] <- 1
CleanData3$act_transportAtoB[DD$C3==1] <- 0
table(CleanData3$act_transportAtoB)

# Dans dans la vie de tous les jours, lequel des styles vestimentaires suivants vous décrit le mieux?|

table(DD$H3)

CleanData3$app_swag_Formel <- NA
CleanData3$app_swag_Formel[!is.na(DD$H3)] <- 0
CleanData3$app_swag_Formel[DD$H3 == 1] <- 1
table(CleanData3$app_swag_Formel)

CleanData3$app_swag_Classique <- NA
CleanData3$app_swag_Classique[!is.na(DD$H3)] <- 0
CleanData3$app_swag_Classique[DD$H3 == 2] <- 1
table(CleanData3$app_swag_Classique)

CleanData3$app_swag_Casual <- NA
CleanData3$app_swag_Casual[!is.na(DD$H3)] <- 0
CleanData3$app_swag_Casual[DD$H3 == 3] <- 1
table(CleanData3$app_swag_Casual)

CleanData3$app_swag_Sport <- NA
CleanData3$app_swag_Sport[!is.na(DD$H3)] <- 0
CleanData3$app_swag_Sport[DD$H3 == 4] <- 1
table(CleanData3$app_swag_Sport)

CleanData3$app_swag_Chic <- NA
CleanData3$app_swag_Chic[!is.na(DD$H3)] <- 0
CleanData3$app_swag_Chic[DD$H3 == 5] <- 1
table(CleanData3$app_swag_Chic)

CleanData3$app_swag_VintageHippBoheme <- NA
CleanData3$app_swag_VintageHippBoheme[!is.na(DD$H3)] <- 0
CleanData3$app_swag_VintageHippBoheme[DD$H3 == 6 | DD$H3 == 7 | DD$H3 == 8 ] <- 1
table(CleanData3$app_swag_VintageHippBoheme)

# (ne pas oublier que other inclu punk)
CleanData3$app_swag_Other <- NA
CleanData3$app_swag_Other[!is.na(DD$H3)] <- 0
CleanData3$app_swag_Other[DD$H3 == 9 | DD$H3 == 11] <- 1
table(CleanData3$app_swag_Other)

CleanData3$app_swag_Rock <- NA
CleanData3$app_swag_Rock[!is.na(DD$H3)] <- 0
CleanData3$app_swag_Rock[DD$H3 == 10] <- 1
table(CleanData3$app_swag_Rock)

# Combien de tatouages avez-vous? 

CleanData3$app_tatouages <- DD$H4
table(CleanData3$app_tatouages)

## binary variable
CleanData3$app_noTattoo <- NA
CleanData3$app_noTattoo[DD$H4 == 0] <- 1 #no tattoos
CleanData3$app_noTattoo[DD$H4 != 0] <- 0 #tattoos (1 or more)
table(CleanData3$app_noTattoo)


#### Parmi les deux qualités suivantes, laquelle est la plus importante à avoir pour un enfant? La conformité ou la libre-pensée


# Avez-vous des animaux de compagnie?

table(DD$H5_M3)

# cats 
CleanData3$cons_pets_cats <- 0
CleanData3$cons_pets_cats[DD$H5_M1 == 1 | 
                            DD$H5_M2 == 1 | 
                            DD$H5_M3 == 1 | 
                            DD$H5_M4 == 1 | 
                            DD$H5_M5 == 1] <- 1 #having cats 
table(CleanData3$cons_pets_cats)

# dogs
CleanData3$cons_pets_dogs <- 0
CleanData3$cons_pets_dogs[DD$H5_M1 == 2 | 
                            DD$H5_M2 == 2 | 
                            DD$H5_M3 == 2 | 
                            DD$H5_M4 == 2 | 
                            DD$H5_M5 == 2] <- 1 #having dogs 
table(CleanData3$cons_pets_dogs)

# cats and dogs 
CleanData3$cons_pets_catsdogs <- 0
CleanData3$cons_pets_catsdogs[DD$H5_M1 == 3 | 
                            DD$H5_M2 == 3 | 
                            DD$H5_M3 == 3 | 
                            DD$H5_M4 == 3 | 
                            DD$H5_M5 == 3] <- 1 #having cats n dogs 
table(CleanData3$cons_pets_catsdogs)

# other pets
CleanData3$cons_pets_others <- 0
CleanData3$cons_pets_others[DD$H5_M1 == 4 | 
                                DD$H5_M2 == 4 | 
                                DD$H5_M3 == 4 | 
                                DD$H5_M4 == 4 | 
                                DD$H5_M5 == 4] <- 1 #having cats n dogs 
table(CleanData3$cons_pets_others)

# farm animals
CleanData3$cons_pets_farm <- 0
CleanData3$cons_pets_farm[DD$H5_M1 == 5 | 
                              DD$H5_M2 == 5 | 
                              DD$H5_M3 == 5 | 
                              DD$H5_M4 == 5 | 
                              DD$H5_M5 == 5] <- 1 #having cats n dogs 
table(CleanData3$cons_pets_farm)

# no pets
CleanData3$cons_noPets <- 0
CleanData3$cons_noPets[DD$H5_M1 == 6 | 
                            DD$H5_M2 == 6 | 
                            DD$H5_M3 == 6 | 
                            DD$H5_M4 == 6 | 
                            DD$H5_M5 == 6] <- 1 #having cats n dogs 
table(CleanData3$cons_noPets)


# E3 Parmi les valeurs suivantes, quelles sont, dans l'ordre, les trois auxquelles vous vous identifiez le plus?

# belonging

CleanData3$val_belongingAll <- NA 
CleanData3$val_belongingAll[DD$E3_RANK1==1 | DD$E3_RANK2==1 | DD$E3_RANK3==1] <-1 
CleanData3$val_belongingAll[DD$E3_RANK1!=1 & DD$E3_RANK2!=1 & DD$E3_RANK3!=1] <-0 
table(CleanData3$val_belongingAll)

# excitement 

CleanData3$val_excitementAll <- NA 
CleanData3$val_excitementAll[DD$E3_RANK1==2 | DD$E3_RANK2==2 | DD$E3_RANK3==2] <-1 
CleanData3$val_excitementAll[DD$E3_RANK1!=2 & DD$E3_RANK2!=2 & DD$E3_RANK3!=2] <-0 
table(CleanData3$val_excitementAll)

# relationship

CleanData3$val_relationshipAll <- NA 
CleanData3$val_relationshipAll[DD$E3_RANK1==3 | DD$E3_RANK2==3 | DD$E3_RANK3==3] <-1 
CleanData3$val_relationshipAll[DD$E3_RANK1!=3 & DD$E3_RANK2!=3 & DD$E3_RANK3!=3] <-0 
table(CleanData3$val_relationshipAll)

# fulfillment 

CleanData3$val_fulfillmentAll <- NA 
CleanData3$val_fulfillmentAll[DD$E3_RANK1==4 | DD$E3_RANK2==4 | DD$E3_RANK3==4] <-1 
CleanData3$val_fulfillmentAll[DD$E3_RANK1!=4 & DD$E3_RANK2!=4 & DD$E3_RANK3!=4] <-0 
table(CleanData3$val_fulfillmentAll)

# be respected 

CleanData3$val_bRespectedAll <- NA 
CleanData3$val_bRespectedAll[DD$E3_RANK1==5 | DD$E3_RANK2==5 | DD$E3_RANK3==5] <-1 
CleanData3$val_bRespectedAll[DD$E3_RANK1!=5 & DD$E3_RANK2!=5 & DD$E3_RANK3!=5] <-0 
table(CleanData3$val_bRespectedAll)

# fun 

CleanData3$val_funAll <- NA 
CleanData3$val_funAll[DD$E3_RANK1==6 | DD$E3_RANK2==6 | DD$E3_RANK3==6] <-1 
CleanData3$val_funAll[DD$E3_RANK1!=6 & DD$E3_RANK2!=6 & DD$E3_RANK3!=6] <-0 
table(CleanData3$val_funAll)

# security 

CleanData3$val_securityAll <- NA 
CleanData3$val_securityAll[DD$E3_RANK1==7 | DD$E3_RANK2==7 | DD$E3_RANK3==7] <-1 
CleanData3$val_securityAll[DD$E3_RANK1!=7 & DD$E3_RANK2!=7 & DD$E3_RANK3!=7] <-0 
table(CleanData3$val_securityAll)

# self-respect 

CleanData3$val_selfRespectAll <- NA 
CleanData3$val_selfRespectAll[DD$E3_RANK1==8 | DD$E3_RANK2==8 | DD$E3_RANK3==8] <-1 
CleanData3$val_selfRespectAll[DD$E3_RANK1!=8 & DD$E3_RANK2!=8 & DD$E3_RANK3!=8] <-0 
table(CleanData3$val_selfRespectAll)

# accomplishment 

CleanData3$val_accomplishAll <- NA 
CleanData3$val_accomplishAll[DD$E3_RANK1==9 | DD$E3_RANK2==9 | DD$E3_RANK3==9] <-1 
CleanData3$val_accomplishAll[DD$E3_RANK1!=9 & DD$E3_RANK2!=9 & DD$E3_RANK3!=9] <-0 
table(CleanData3$val_accomplishAll)


# E4B Parmi les deux qualités suivantes, laquelle est la plus importante à avoir pour un enfant? (x4) 

## Toutes les valeurs plus conservatrices sont codées 1 (valeurs libérales codées 0)

#  La conformité ou la libre-pensée
table(DD$E4B)
CleanData3$val_kidsConformity<-NA
CleanData3$val_kidsConformity[DD$E4B==1] <- 1 # conformité 
CleanData3$val_kidsConformity[DD$E4B==2] <- 0 # libre-pensée 
table(CleanData3$val_kidsConformity)

#  La curiosité ou les bonnes manières 
table(DD$E4B_2)
CleanData3$val_kidsCuriosity<-NA
CleanData3$val_kidsCuriosity[DD$E4B_2==2] <- 1 # bonnes manières
CleanData3$val_kidsCuriosity[DD$E4B_2==1] <- 0 # curiosité 
table(CleanData3$val_kidsCuriosity)

# autonomie ou obéissance 
table(DD$E4B_3)
CleanData3$val_kidsAutonomy<-NA
CleanData3$val_kidsAutonomy[DD$E4B_3==2] <- 1 # obéissance
CleanData3$val_kidsAutonomy[DD$E4B_3==1] <- 0 # autonomie
table(CleanData3$val_kidsAutonomy)

# faire preuve de considération envers autrui ou bien se comporter 
table(DD$E4B_4)
CleanData3$val_kidsConsideration<-NA
CleanData3$val_kidsConsideration[DD$E4B_4==2] <- 1 # bien se comporter 
CleanData3$val_kidsConsideration[DD$E4B_4==1] <- 0 # considération envers autrui
table(CleanData3$val_kidsConsideration)

####### Creating a "Conservative values" scale #########

# Storing the relevant vectors in a data frame

DataForFactAnal <- data.frame(CleanData3$"val_kidsConformity",
                              CleanData3$"val_kidsCuriosity",
                              CleanData3$"val_kidsAutonomy",
                              CleanData3$"val_kidsConsideration")

# Performing listwise deletion
DataForFactAnal <- na.omit(DataForFactAnal)

# Cronbach's Alpha 
cronbachAlpha <- round(as.numeric(psych::alpha(DataForFactAnal)$total[1]), digit=2)

# Loading ggplot2 now to avoid conflicts with the alpha function on previous line
library(ggplot2)
library(coefplot)
# Factor Analysis
factorAnal <- factanal(DataForFactAnal, factors=1)
factorVarNames <- c("Conformity",
                    "Good manners",
                    "Obediance",
                    "Well-behaved")
factorLoadings <- as.numeric(factorAnal$loadings[,1]) # Save the loadings in a vector (Type = numeric)
factor1stEigen <- round(eigen(cor(DataForFactAnal))$values[1], digit=2) # Get first eigenvalues

# Graph
factorAnalysis <- ggplot(data.frame(factorVarNames,factorLoadings), aes(x=factorVarNames, y=factorLoadings)) + 
  coord_flip() +
  geom_bar(stat="identity", colour="black", fill="black", size=1, width=0.4) +
  geom_text(aes(label=as.character(round(factorLoadings, digits = 2))), vjust=0.35, hjust=-0.3) +
  geom_hline(yintercept=0.3, colour="gray", linetype = "longdash") +
  ggplot2::annotate("text", label=paste("Cronbach's Alpha =", as.character(cronbachAlpha)), x=1.1, y=1.31, size=3.8) +
  ggplot2::annotate("text", label=paste("First Eigenvalue =", as.character(factor1stEigen)), x=0.75, y=1.31, size=3.8) +
  ggplot2::annotate("segment", x = 0.4, xend = 1.45, y = 1, yend = 1, colour = "black") +
  ggplot2::annotate("segment", x = 1.45, xend = 1.45, y = 1, yend = 1.65, colour = "black") +
  scale_y_continuous(name="Factor loadings", limits=c(0, 1.55), breaks=seq(0, 1, by=0.1)) +
  xlab("") +
  theme_classic() +
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(hjust=0.3, vjust=-0.17, size=14), 
        panel.grid=element_blank()) 
 
# Creating a scale named "scaleConsValuesChild"
CleanData3$scaleConsValuesChild <- (CleanData3$"val_kidsConformity"*factorLoadings[1] + CleanData3$"val_kidsCuriosity"*factorLoadings[2] + CleanData3$"val_kidsAutonomy"*factorLoadings[3] + CleanData3$"val_kidsConsideration"*factorLoadings[4])
hist(CleanData3$scaleConsValuesChild)
mean(CleanData3$scaleConsValuesChild,na.rm=T)

# E5 Big 5

# EXTRAVERTI(E)
table(DD$E5_A1)
CleanData3$big5_extroverted <- NA 
CleanData3$big5_extroverted[DD$E5_A1 == 1] <- 1
CleanData3$big5_extroverted[DD$E5_A1 == 2] <- 2
CleanData3$big5_extroverted[DD$E5_A1 == 3] <- 3
CleanData3$big5_extroverted[DD$E5_A1 == 4] <- 4
CleanData3$big5_extroverted[DD$E5_A1 == 5] <- 5
CleanData3$big5_extroverted[DD$E5_A1 == 6] <- 6
CleanData3$big5_extroverted[DD$E5_A1 == 7] <- 7
table(CleanData3$big5_extroverted)

# EXTRAVERTI(E)
table(DD$E5_A2)
CleanData3$big5_critical <- NA 
CleanData3$big5_critical[DD$E5_A2 == 1] <- 1
CleanData3$big5_critical[DD$E5_A2 == 2] <- 2
CleanData3$big5_critical[DD$E5_A2 == 3] <- 3
CleanData3$big5_critical[DD$E5_A2 == 4] <- 4
CleanData3$big5_critical[DD$E5_A2 == 5] <- 5
CleanData3$big5_critical[DD$E5_A2 == 6] <- 6
CleanData3$big5_critical[DD$E5_A2 == 7] <- 7
table(CleanData3$big5_critical)

# EXTRAVERTI(E)
table(DD$E5_A3)
CleanData3$big5_dependable <- NA 
CleanData3$big5_dependable[DD$E5_A3 == 1] <- 1
CleanData3$big5_dependable[DD$E5_A3 == 2] <- 2
CleanData3$big5_dependable[DD$E5_A3 == 3] <- 3
CleanData3$big5_dependable[DD$E5_A3 == 4] <- 4
CleanData3$big5_dependable[DD$E5_A3 == 5] <- 5
CleanData3$big5_dependable[DD$E5_A3 == 6] <- 6
CleanData3$big5_dependable[DD$E5_A3 == 7] <- 7
table(CleanData3$big5_dependable)

# ANXIOUS
table(DD$E5_A4)
CleanData3$big5_anxious <- NA 
CleanData3$big5_anxious[DD$E5_A4 == 1] <- 1
CleanData3$big5_anxious[DD$E5_A4 == 2] <- 2
CleanData3$big5_anxious[DD$E5_A4 == 3] <- 3
CleanData3$big5_anxious[DD$E5_A4 == 4] <- 4
CleanData3$big5_anxious[DD$E5_A4 == 5] <- 5
CleanData3$big5_anxious[DD$E5_A4 == 6] <- 6
CleanData3$big5_anxious[DD$E5_A4 == 7] <- 7
table(CleanData3$big5_anxious)

# OPEN TO NEW EXPERIENCES, COMPLEX 
table(DD$E5_A5)
CleanData3$big5_open <- NA 
CleanData3$big5_open[DD$E5_A5 == 1] <- 1
CleanData3$big5_open[DD$E5_A5 == 2] <- 2
CleanData3$big5_open[DD$E5_A5 == 3] <- 3
CleanData3$big5_open[DD$E5_A5 == 4] <- 4
CleanData3$big5_open[DD$E5_A5 == 5] <- 5
CleanData3$big5_open[DD$E5_A5 == 6] <- 6
CleanData3$big5_open[DD$E5_A5 == 7] <- 7
table(CleanData3$big5_open)

# OPEN TO NEW EXPERIENCES, COMPLEX 
table(DD$E5_A6)
CleanData3$big5_reserved <- NA 
CleanData3$big5_reserved[DD$E5_A6 == 1] <- 1
CleanData3$big5_reserved[DD$E5_A6 == 2] <- 2
CleanData3$big5_reserved[DD$E5_A6 == 3] <- 3
CleanData3$big5_reserved[DD$E5_A6 == 4] <- 4
CleanData3$big5_reserved[DD$E5_A6 == 5] <- 5
CleanData3$big5_reserved[DD$E5_A6 == 6] <- 6
CleanData3$big5_reserved[DD$E5_A6 == 7] <- 7
table(CleanData3$big5_reserved)

# SYMPATHETIC
table(DD$E5_A7)
CleanData3$big5_sympathetic <- NA 
CleanData3$big5_sympathetic[DD$E5_A7 == 1] <- 1
CleanData3$big5_sympathetic[DD$E5_A7 == 2] <- 2
CleanData3$big5_sympathetic[DD$E5_A7 == 3] <- 3
CleanData3$big5_sympathetic[DD$E5_A7 == 4] <- 4
CleanData3$big5_sympathetic[DD$E5_A7 == 5] <- 5
CleanData3$big5_sympathetic[DD$E5_A7 == 6] <- 6
CleanData3$big5_sympathetic[DD$E5_A7 == 7] <- 7
table(CleanData3$big5_sympathetic)

# DISORGANIZED 
table(DD$E5_A8)
CleanData3$big5_disorganized <- NA 
CleanData3$big5_disorganized[DD$E5_A8 == 1] <- 1
CleanData3$big5_disorganized[DD$E5_A8 == 2] <- 2
CleanData3$big5_disorganized[DD$E5_A8 == 3] <- 3
CleanData3$big5_disorganized[DD$E5_A8 == 4] <- 4
CleanData3$big5_disorganized[DD$E5_A8 == 5] <- 5
CleanData3$big5_disorganized[DD$E5_A8 == 6] <- 6
CleanData3$big5_disorganized[DD$E5_A8 == 7] <- 7
table(CleanData3$big5_disorganized)

# CALM
table(DD$E5_A9)
CleanData3$big5_calm <- NA 
CleanData3$big5_calm[DD$E5_A9 == 1] <- 1
CleanData3$big5_calm[DD$E5_A9 == 2] <- 2
CleanData3$big5_calm[DD$E5_A9 == 3] <- 3
CleanData3$big5_calm[DD$E5_A9 == 4] <- 4
CleanData3$big5_calm[DD$E5_A9 == 5] <- 5
CleanData3$big5_calm[DD$E5_A9 == 6] <- 6
CleanData3$big5_calm[DD$E5_A9 == 7] <- 7
table(CleanData3$big5_calm)

# CONVENTIONAL
table(DD$E5_A10)
CleanData3$big5_conventional <- NA 
CleanData3$big5_conventional[DD$E5_A10 == 1] <- 1
CleanData3$big5_conventional[DD$E5_A10 == 2] <- 2
CleanData3$big5_conventional[DD$E5_A10 == 3] <- 3
CleanData3$big5_conventional[DD$E5_A10 == 4] <- 4
CleanData3$big5_conventional[DD$E5_A10 == 5] <- 5
CleanData3$big5_conventional[DD$E5_A10 == 6] <- 6
CleanData3$big5_conventional[DD$E5_A10 == 7] <- 7
table(CleanData3$big5_conventional)

# E6¬A1
# Voici des énoncés qui illustrent deux conceptions de la réalité : Le monde est un endroit dangereux. Il est rempli de menaces. Nous devons nous en protéger, ou Le monde est grand et beau, et nous pouvons l'explorer en toute sécurité. 

table(DD$E6_A1)
CleanData3$worldDangerous <- DD$E6_A1/10
table(CleanData3$worldDangerous)

# Consommez-vous de la viande (ou des produits d'origine animale)?
# 1) Oui, beaucoup 2) Oui, mais de façon modérée; 3) Non, je suis végétarien(e); 4) Non, je suis végan(e) 

table(DD$F3)

# viandeux 
CleanData3$cons_Meat <- NA
CleanData3$cons_Meat[DD$F3 == 1 | DD$F3 == 2] <- 1
CleanData3$cons_Meat[DD$F3 == 3 | DD$F3 == 4] <- 0
table(CleanData3$cons_Meat)

#vege
CleanData3$cons_Vege <- NA
CleanData3$cons_Vege[DD$F3 == 3] <- 1
CleanData3$cons_Vege[DD$F3 != 3] <- 0
table(CleanData3$cons_Vege)

#vegan 
CleanData3$cons_Vegan <- NA
CleanData3$cons_Vegan[DD$F3 == 4] <- 1
CleanData3$cons_Vegan[DD$F3 != 4] <- 0
table(CleanData3$cons_Vegan)


# Généralement, où allez-vous chercher votre café?
table(DD$F4)

CleanData3$cons_coffee_TimH <- NA
CleanData3$cons_coffee_TimH[!is.na(DD$F4)] <- 0
CleanData3$cons_coffee_TimH[DD$F4 == 1] <- 1
table(CleanData3$cons_coffee_TimH)

CleanData3$cons_coffee_Starbucks <- NA
CleanData3$cons_coffee_Starbucks[!is.na(DD$F4)] <- 0
CleanData3$cons_coffee_Starbucks[DD$F4 == 2] <- 1
table(CleanData3$cons_coffee_Starbucks)

CleanData3$cons_coffee_SC <- NA
CleanData3$cons_coffee_SC[!is.na(DD$F4)] <- 0
CleanData3$cons_coffee_SC[DD$F4 == 3] <- 1
table(CleanData3$cons_coffee_SC)

CleanData3$cons_coffee_McDo <- NA
CleanData3$cons_coffee_McDo[!is.na(DD$F4)] <- 0
CleanData3$cons_coffee_McDo[DD$F4 == 4] <- 1
table(CleanData3$cons_coffee_McDo)

CleanData3$cons_coffee_Other <- NA
CleanData3$cons_coffee_Other[!is.na(DD$F4)] <- 0
CleanData3$cons_coffee_Other[DD$F4 == 5] <- 1
table(CleanData3$cons_coffee_Other)

CleanData3$cons_coffee_place_ind <- NA
CleanData3$cons_coffee_place_ind[!is.na(DD$F4)] <- 0
CleanData3$cons_coffee_place_ind[DD$F4 == 6] <- 1
table(CleanData3$cons_coffee_place_ind)

CleanData3$cons_coffee_place_noCoffee <- NA
CleanData3$cons_coffee_place_noCoffee[!is.na(DD$F4)] <- 0
CleanData3$cons_coffee_place_noCoffee[DD$F4 == 7] <- 1
table(CleanData3$cons_coffee_place_noCoffee)

# De façon générale, comment préparez-vous votre café?

table(DD$F5)

CleanData3$cons_coffee_type_Filtre <- NA
CleanData3$cons_coffee_type_Filtre[DD$F5 == 1] <- 1
CleanData3$cons_coffee_type_Filtre[DD$F5 != 1] <- 0
table(CleanData3$cons_coffee_type_Filtre)

CleanData3$cons_coffee_type_Italien <- NA
CleanData3$cons_coffee_type_Italien[DD$F5 == 2] <- 1
CleanData3$cons_coffee_type_Italien[DD$F5 != 2] <- 0
table(CleanData3$cons_coffee_type_Italien)

CleanData3$cons_coffee_type_Perco <- NA
CleanData3$cons_coffee_type_Perco[DD$F5 == 3] <- 1
CleanData3$cons_coffee_type_Perco[DD$F5 != 3] <- 0
table(CleanData3$cons_coffee_type_Perco)

CleanData3$cons_coffee_type_PresseFR <- NA
CleanData3$cons_coffee_type_PresseFR[DD$F5 == 4] <- 1
CleanData3$cons_coffee_type_PresseFR[DD$F5 != 4] <- 0
table(CleanData3$cons_coffee_type_PresseFR)

CleanData3$cons_coffee_type_Capsules <- NA
CleanData3$cons_coffee_type_Capsules[DD$F5 == 5] <- 1
CleanData3$cons_coffee_type_Capsules[DD$F5 != 5] <- 0
table(CleanData3$cons_coffee_type_Capsules)

CleanData3$cons_coffee_type_Expresso <- NA
CleanData3$cons_coffee_type_Expresso[DD$F5 == 6] <- 1
CleanData3$cons_coffee_type_Expresso[DD$F5 != 6] <- 0
table(CleanData3$cons_coffee_type_Expresso)

CleanData3$cons_coffee_type_Instant <- NA
CleanData3$cons_coffee_type_Instant[DD$F5 == 7] <- 1
CleanData3$cons_coffee_type_Instant[DD$F5 != 7] <- 0
table(CleanData3$cons_coffee_type_Instant)

CleanData3$cons_coffee_type_NoCoffee <- NA
CleanData3$cons_coffee_type_NoCoffee[DD$F5 == 8] <- 1
CleanData3$cons_coffee_type_NoCoffee[DD$F5 != 8] <- 0
table(CleanData3$cons_coffee_type_NoCoffee)


# Fumez-vous actuellement la cigarette? 

table(DD$F6)

CleanData3$cons_Smoke <- NA
CleanData3$cons_Smoke[DD$F6 == 1 ] <- 1
CleanData3$cons_Smoke[DD$F6 == 3 | DD$F6 == 4 | DD$F6 == 2 | DD$F6 == 5] <- 0
table(CleanData3$cons_Smoke)

CleanData3$cons_SmokeStopping <- NA
CleanData3$cons_SmokeStopping[DD$F6 == 2 ] <- 1
CleanData3$cons_SmokeStopping[DD$F6 == 3 | DD$F6 == 4 | DD$F6 == 1 | DD$F6 == 5] <- 0
table(CleanData3$cons_SmokeStopping)

CleanData3$cons_SmokeStopped <- NA
CleanData3$cons_SmokeStopped[DD$F6 == 3] <- 1
CleanData3$cons_SmokeStopped[DD$F6 == 1 | DD$F6 == 4 | DD$F6 == 2 | DD$F6 == 5] <- 0
table(CleanData3$cons_SmokeStopped)

CleanData3$cons_VapeNation <- NA
CleanData3$cons_VapeNation[DD$F6 == 5] <- 1
CleanData3$cons_VapeNation[DD$F6 == 1 | DD$F6 == 2 | DD$F6 == 3 | DD$F6 == 4 ] <- 0
table(CleanData3$cons_VapeNation)

CleanData3$cons_SmokeNever <- NA
CleanData3$cons_SmokeNever[DD$F6 == 4] <- 1
CleanData3$cons_SmokeNever[DD$F6 == 1 | DD$F6 == 3 | DD$F6 == 2 | DD$F6 == 5] <- 0
table(CleanData3$cons_SmokeNever)

# Quel est votre type d'alcool préféré?
table(DD$F7)

CleanData3$cons_redWineDrink <- NA
CleanData3$cons_redWineDrink[DD$F7 == 1] <- 1
CleanData3$cons_redWineDrink[DD$F7 != 1] <- 0
table(CleanData3$cons_redWineDrink)

CleanData3$cons_whiteWineDrink <- NA
CleanData3$cons_whiteWineDrink[DD$F7 == 2] <- 1
CleanData3$cons_whiteWineDrink[DD$F7 != 2] <- 0
table(CleanData3$cons_whiteWineDrink)

CleanData3$cons_roseDrink <- NA
CleanData3$cons_roseDrink[DD$F7 == 3] <- 1
CleanData3$cons_roseDrink[DD$F7 != 3] <- 0
table(CleanData3$cons_roseDrink)

CleanData3$cons_sparklingDrink <- NA
CleanData3$cons_sparklingDrink[DD$F7 == 4] <- 1
CleanData3$cons_sparklingDrink[DD$F7 != 4] <- 0
table(CleanData3$cons_sparklingDrink)

CleanData3$cons_regBeers <- NA
CleanData3$cons_regBeers[DD$F7 == 5] <- 1
CleanData3$cons_regBeers[DD$F7 != 5] <- 0
table(CleanData3$cons_regBeers)

CleanData3$cons_microBeers <- NA
CleanData3$cons_microBeers[DD$F7 == 6] <- 1
CleanData3$cons_microBeers[DD$F7 != 6] <- 0
table(CleanData3$cons_microBeers)

CleanData3$cons_spiritDrink <- NA
CleanData3$cons_spiritDrink[DD$F7 == 7] <- 1
CleanData3$cons_spiritDrink[DD$F7 != 7] <- 0
table(CleanData3$cons_spiritDrink)

CleanData3$cons_cocktailsDrink <- NA
CleanData3$cons_cocktailsDrink[DD$F7 == 8] <- 1
CleanData3$cons_cocktailsDrink[DD$F7 != 8] <- 0
table(CleanData3$cons_cocktailsDrink)

CleanData3$cons_noDrink <- NA
CleanData3$cons_noDrink[DD$F7 == 9] <- 1
CleanData3$cons_noDrink[DD$F7 != 9] <- 0
table(CleanData3$cons_noDrink)


# À quelle fréquence consommez-vous de l'alcool?

# À quelle fréquence consommez-vous de la marijuana? 

# Quel est votre groupe ou musicien(ne)/chanteur(se) préféré(e)?|

# Quel est le meilleur livre que vous avez lu au cours des cinq dernières années?

# Quel est votre film préféré?

# Quel serait votre moment de vacances favoris parmi les suivants?

# Si vous aviez le choix d'aller en vacances où vous voulez, quelle serait votre destination préférée?

# Sur quels types de sujet lisez-vous généralement un livre ou une revue?

# Mes choix quotidiens sont souvent influencés par des considérations écologiques.

# J'aime essayer des recettes provenant de cultures variées.

# J'ai tendance à préparer les repas à l'avance.

# Lorsque possible, j'évite d'acheter des produits transformés.

# Je suis organisé(e) dans ma cuisine.

# De façon générale, où préférez-vous faire votre épicerie?

# Posséder les dernières technologies me permet de donner le meilleur de moi-même.

# Je me sens davantage Canadien(ne) que Québécois(e) (PROV == 11)

# Les politiciens ne se soucient pas beaucoup de ce que les gens comme moi pensent.

# Je me sens important pour les autres dans la société.

# Certains termes ne devraient jamais être utilisés, même dans les milieux d'enseignement.

# Quel est votre prénom préféré pour une petite fille?|

# Quel est votre prénom préféré pour un petit garçon?

# Où achetez-vous vos vêtements le plus régulièrement?

table(DD$F1)

CleanData3$cons_brand_MaR <- NA
CleanData3$cons_brand_MaR[DD$F1 == 1] <- 1
CleanData3$cons_brand_MaR[DD$F1 != 1] <- 0
table(CleanData3$cons_brand_MaR)

CleanData3$cons_brand_BInd <- NA
CleanData3$cons_brand_BInd[DD$F1 == 2] <- 1
CleanData3$cons_brand_BInd[DD$F1 != 2] <- 0
table(CleanData3$cons_brand_BInd)

CleanData3$cons_brand_ChainesB <- NA
CleanData3$cons_brand_ChainesB[DD$F1 == 3] <- 1
CleanData3$cons_brand_ChainesB[DD$F1 != 3] <- 0
table(CleanData3$cons_brand_ChainesB)

CleanData3$cons_brand_GSurf <- NA
CleanData3$cons_brand_GSurf[DD$F1 == 4] <- 1
CleanData3$cons_brand_GSurf[DD$F1 != 4] <- 0
table(CleanData3$cons_brand_GSurf)

CleanData3$cons_brand_OnlineOnly <- NA
CleanData3$cons_brand_OnlineOnly[DD$F1 == 5] <- 1
CleanData3$cons_brand_OnlineOnly[DD$F1 != 5] <- 0
table(CleanData3$cons_brand_OnlineOnly)

CleanData3$cons_brand_Frip <- NA
CleanData3$cons_brand_Frip[DD$F1 == 6] <- 1
CleanData3$cons_brand_Frip[DD$F1 != 6] <- 0
table(CleanData3$cons_brand_Frip)

CleanData3$cons_brand_Other <- NA
CleanData3$cons_brand_Other[DD$F1 == 7] <- 1
CleanData3$cons_brand_Other[DD$F1 != 7] <- 0
table(CleanData3$cons_brand_Other)

# Combien de temps par jour passez-vous sur les médias sociaux?

# Le jour de l'élection, est-ce certain que vous irez voter, probable, improbable, ou certain que vous n'irez pas voter?|

# Pour quel parti voteriez-vous si l'élection fédérale canadienne du 20 septembre avait lieu aujourd'hui?

table(DD$I1)

CleanData3$op_intent <- NA 
CleanData3$op_intent[DD$I1 == 1] <- 1 # "LPC"
CleanData3$op_intent[DD$I1 == 2] <- 2  # "CPC"
CleanData3$op_intent[DD$I1 == 3] <- 3 # "NDP"
CleanData3$op_intent[DD$I1 == 4] <- 4  # "BQ"
CleanData3$op_intent[DD$I1 == 5] <- 5 # "GP"
CleanData3$op_intent[DD$I1 == 6] <- 6  # "PPC"
CleanData3$op_intent[DD$I1 == 7] <- 7  # "Other party"
CleanData3$op_intent[DD$I1 == 8] <- 8  # "No vote"
CleanData3$op_intent[DD$I1 == 9] <- 9   # "Would boycott ballot"
CleanData3$op_intent[DD$I1 == 10] <- 10 #  "Undecided"
table(CleanData3$op_intent)

# Parti libéral 

CleanData3$op_voteIntent_Lib <- NA
CleanData3$op_voteIntent_Lib[DD$I1 == 1] <- 1
CleanData3$op_voteIntent_Lib[DD$I1 != 1] <- 0
table(CleanData3$op_voteIntent_Lib)

# Parti conservateur 

CleanData3$op_voteIntent_Cons <- NA
CleanData3$op_voteIntent_Cons[DD$I1 == 2] <- 1
CleanData3$op_voteIntent_Cons[DD$I1 != 2] <- 0
table(CleanData3$op_voteIntent_Cons)

# npd 

CleanData3$op_voteIntent_Ndp <- NA
CleanData3$op_voteIntent_Ndp[DD$I1 == 3] <- 1
CleanData3$op_voteIntent_Ndp[DD$I1 != 3] <- 0
table(CleanData3$op_voteIntent_Ndp)

# Bloc

CleanData3$op_voteIntent_Bloc <- NA
CleanData3$op_voteIntent_Bloc[DD$I1 == 4] <- 1
CleanData3$op_voteIntent_Bloc[DD$I1 != 4] <- 0
table(CleanData3$op_voteIntent_Bloc)

# Vert 

CleanData3$op_voteIntent_Green <- NA
CleanData3$op_voteIntent_Green[DD$I1 == 5] <- 1
CleanData3$op_voteIntent_Green[DD$I1 != 5] <- 0
table(CleanData3$op_voteIntent_Green)

# PPC

CleanData3$op_voteIntent_PPC <- NA
CleanData3$op_voteIntent_PPC[DD$I1 == 6] <- 1
CleanData3$op_voteIntent_PPC[DD$I1 != 6] <- 0
table(CleanData3$op_voteIntent_PPC)

# No vote 

CleanData3$op_voteIntent_NoVote <- NA 
CleanData3$op_voteIntent_NoVote[DD$I1 == 8] <- 1
CleanData3$op_voteIntent_NoVote[DD$I1 != 8] <- 0
table(CleanData3$op_voteIntent_NoVote)


# Dans quelle mesure en êtes-vous certain?

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la 
# prochaine élection fédérale canadienne, en général, quelle est la probabilité que vous appuyiez le Parti libéral du Canada? 
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(DD$I3_A1)
CleanData3$op_potentialG_Lib <- NA
CleanData3$op_potentialG_Lib <- DD$I3_A1/10
table(CleanData3$op_potentialG_Lib)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne,
# en général, quelle est la probabilité que vous appuyiez le Parti conservateur du Canada?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(DD$I3_A2)
CleanData3$op_potentialG_Cons <- NA
CleanData3$op_potentialG_Cons <- DD$I3_A2/10
table(CleanData3$op_potentialG_Cons)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne,
# en général, quelle est la probabilité que vous appuyiez le Nouveau parti démocratique?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(DD$I3_A3)
CleanData3$op_potentialG_Ndp <- NA
CleanData3$op_potentialG_Ndp <- DD$I3_A3/10
table(CleanData3$op_potentialG_Ndp)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne,
# en général, quelle est la probabilité que vous appuyiez le Bloc québécois?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(DD$I3_A4)
CleanData3$op_potentialG_BQ <- NA
CleanData3$op_potentialG_BQ <- DD$I3_A4/10
table(CleanData3$op_potentialG_BQ)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne,
# en général, quelle est la probabilité que vous appuyiez le Parti vert?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(DD$I3_A5)
CleanData3$op_potentialG_PV <- NA
CleanData3$op_potentialG_PV <- DD$I3_A5/10
table(CleanData3$op_potentialG_PV)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise,
# en général, quelle est la probabilité que vous appuyiez la Coalition Avenir Québec?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(DD$I3Q_A6)
CleanData3$op_potentialG_CAQ <- NA
CleanData3$op_potentialG_CAQ <- DD$I3Q_A6/10
table(CleanData3$op_potentialG_CAQ)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise,
# en général, quelle est la probabilité que vous appuyiez le Parti libéral du Québec?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(DD$I3Q_A7)
CleanData3$op_potentialG_PLQ <- NA
CleanData3$op_potentialG_PLQ <- DD$I3Q_A7/10
table(CleanData3$op_potentialG_PLQ)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise,
# en général, quelle est la probabilité que vous appuyiez le Parti québécois?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(DD$I3Q_A8)
CleanData3$op_potentialG_PQ <- NA
CleanData3$op_potentialG_PQ <- DD$I3Q_A8/10
table(CleanData3$op_potentialG_PQ)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise,
# en général, quelle est la probabilité que vous appuyiez Québec solidaire?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(DD$I3Q_A9)
CleanData3$op_potentialG_QS <- NA
CleanData3$op_potentialG_QS <- DD$I3Q_A9/10
table(CleanData3$op_potentialG_QS)

# People predict 

# Pour quel parti avez-vous voté les des élections fédérales canadiennes de 2019?|

# En politique, on parle souvent de gauche et de droite. Sur une échelle de 0 à 10, avec 0 signifiant que vous êtes de gauche et 10 que vous êtes de droite, où vous placeriez-vous?

# Quel est votre niveau d'intérêt pour la politique en général?|

# Vivez-vous dans un environnement urbain, suburbain, ou rural?

table(DD$SES3)

CleanData3$ses_urbain <- NA
CleanData3$ses_urbain[DD$SES3 == 1] <- 1 
CleanData3$ses_urbain[DD$SES3 != 1] <- 0 
table(CleanData3$ses_urbain)

CleanData3$ses_sururbain <- NA
CleanData3$ses_sururbain[DD$SES3 == 2] <- 1 
CleanData3$ses_sururbain[DD$SES3 != 2] <- 0 
table(CleanData3$ses_sururbain)

CleanData3$ses_rural <- NA
CleanData3$ses_rural[DD$SES3 == 3] <- 1 
CleanData3$ses_rural[DD$SES3 != 3] <- 0 
table(CleanData3$ses_rural)

# Parmi les catégories suivantes, laquelle décrit le mieux votre domaine d'emploi?

# Quel est votre status matrimonial?

# Combien d'enfants de moins de 18 ans vivent avec vous?

# À quels groupes ethniques ou culturels vos ancêtres appartenaient-ils?|


################
table(DD$SES6_M3)

# CANADIEN 
CleanData3$ses_ethn_Canadien <- NA 
CleanData3$ses_ethn_Canadien[DD$SES6_M1 == 1] <- 1
CleanData3$ses_ethn_Canadien[DD$SES6_M1 != 1] <- 0
table(CleanData3$ses_ethn_Canadien)

# FRANCAIS
CleanData3$ses_ethn_Francais <- 0
CleanData3$ses_ethn_Francais[DD$SES6_M1 == 2 |
                             DD$SES6_M2 == 2] <- 1
table(CleanData3$ses_ethn_Francais)

# ANGLAIS
CleanData3$ses_ethn_Anglais <- 0
CleanData3$ses_ethn_Anglais[DD$SES6_M1 == 3 |
                            DD$SES6_M2 == 3 |
                            DD$SES6_M3 == 3] <- 1
table(CleanData3$ses_ethn_Anglais)

# ALLEMAND 
CleanData3$ses_ethn_Allemand <- 0 
CleanData3$ses_ethn_Allemand[DD$SES6_M1 == 4 |
                              DD$SES6_M2 == 4 |
                              DD$SES6_M3 == 4 |
                              DD$SES6_M4 == 4] <- 1
table(CleanData3$ses_ethn_Allemand)

# ÉCOSSAIS 

CleanData3$ses_ethn_Scottish <- 0
CleanData3$ses_ethn_Scottish[DD$SES6_M1 == 5 |
                               DD$SES6_M2 == 5 |
                               DD$SES6_M3 == 5 |
                               DD$SES6_M4 == 5 |
                               DD$SES6_M5 == 5] <- 1
table(CleanData3$ses_ethn_Scottish)

# IRLANDAIS 
CleanData3$ses_ethn_Irish <- 0
CleanData3$ses_ethn_Irish[DD$SES6_M1 == 6 |
                               DD$SES6_M2 == 6 |
                               DD$SES6_M3 == 6 |
                               DD$SES6_M4 == 6 |
                               DD$SES6_M5 == 6 |
                               DD$SES6_M6 == 6 |
                               DD$SES6_M9 == 6] <- 1
table(CleanData3$ses_ethn_Irish)

#ITALIEN 
CleanData3$ses_ethn_Italien <- 0
CleanData3$ses_ethn_Italien[DD$SES6_M1 == 7 |
                            DD$SES6_M2 == 7 |
                            DD$SES6_M3 == 7 |
                            DD$SES6_M4 == 7 |
                            DD$SES6_M6 == 7 |
                            DD$SES6_M7 == 7 |
                            DD$SES6_M10 == 7] <- 1
table(CleanData3$ses_ethn_Italien)

#UKRAINIEN
CleanData3$ses_ethn_Ukraine <- 0
CleanData3$ses_ethn_Ukraine[DD$SES6_M1 == 8 |
                              DD$SES6_M2 == 8 |
                              DD$SES6_M3 == 8 |
                              DD$SES6_M4 == 8 |
                              DD$SES6_M5 == 8 |
                              DD$SES6_M6 == 8 |
                              DD$SES6_M10 == 8 |
                              DD$SES6_M12 == 8] <- 1
table(CleanData3$ses_ethn_Ukraine)

#NÉERLANDAIS 
CleanData3$ses_ethn_PaysBas <- 0
CleanData3$ses_ethn_PaysBas[DD$SES6_M1 == 9 |
                              DD$SES6_M2 == 9 |
                              DD$SES6_M3 == 9 |
                              DD$SES6_M4 == 9 |
                              DD$SES6_M5 == 9 |
                              DD$SES6_M6 == 9 |
                              DD$SES6_M7 == 9 |
                              DD$SES6_M8 == 9 |
                              DD$SES6_M9 == 9 |
                              DD$SES6_M10 == 9 |
                              DD$SES6_M11 == 9 |
                              DD$SES6_M12 == 9 |
                              DD$SES6_M13 == 9 |
                              DD$SES6_M14 == 9 |
                              DD$SES6_M15 == 9 |
                              DD$SES6_M16 == 9 |
                              DD$SES6_M17 == 9 ] <- 1
table(CleanData3$ses_ethn_PaysBas)

# CHINOIS 
CleanData3$ses_ethn_Chine <- 0
CleanData3$ses_ethn_Chine[DD$SES6_M1 == 10 |
                              DD$SES6_M2 == 10 |
                              DD$SES6_M3 == 10 |
                              DD$SES6_M4 == 10 |
                              DD$SES6_M5 == 10 |
                              DD$SES6_M6 == 10 |
                              DD$SES6_M7 == 10 |
                              DD$SES6_M8 == 10 |
                              DD$SES6_M9 == 10 |
                              DD$SES6_M10 == 10 |
                              DD$SES6_M11 == 10 |
                              DD$SES6_M12 == 10 |
                              DD$SES6_M13 == 10 |
                              DD$SES6_M14 == 10 |
                              DD$SES6_M15 == 10 |
                              DD$SES6_M16 == 10 |
                              DD$SES6_M17 == 10 ] <- 1
table(CleanData3$ses_ethn_Chine)

# JUIF 
CleanData3$ses_ethn_Jew <- 0
CleanData3$ses_ethn_Jew[DD$SES6_M1 == 11 |
                            DD$SES6_M2 == 11 |
                            DD$SES6_M3 == 11 |
                            DD$SES6_M4 == 11 |
                            DD$SES6_M5 == 11 |
                            DD$SES6_M6 == 11 |
                            DD$SES6_M7 == 11 |
                            DD$SES6_M8 == 11 |
                            DD$SES6_M9 == 11 |
                            DD$SES6_M10 == 11 |
                            DD$SES6_M11 == 11 |
                            DD$SES6_M12 == 11 |
                            DD$SES6_M13 == 11 |
                            DD$SES6_M14 == 11 |
                            DD$SES6_M15 == 11 |
                            DD$SES6_M16 == 11 |
                            DD$SES6_M17 == 11 ] <- 1
table(CleanData3$ses_ethn_Jew)

#POLONAIS
CleanData3$ses_ethn_Polish <- 0
CleanData3$ses_ethn_Polish[DD$SES6_M1 == 12 |
                          DD$SES6_M2 == 12 |
                          DD$SES6_M3 == 12 |
                          DD$SES6_M4 == 12 |
                          DD$SES6_M5 == 12 |
                          DD$SES6_M6 == 12 |
                          DD$SES6_M7 == 12 |
                          DD$SES6_M8 == 12 |
                          DD$SES6_M9 == 12 |
                          DD$SES6_M10 == 12 |
                          DD$SES6_M11 == 12 |
                          DD$SES6_M12 == 12 |
                          DD$SES6_M13 == 12 |
                          DD$SES6_M14 == 12 |
                          DD$SES6_M15 == 12 |
                          DD$SES6_M16 == 12 |
                          DD$SES6_M17 == 12 ] <- 1
table(CleanData3$ses_ethn_Polish)

# PORTUGAIS 
CleanData3$ses_ethn_Portugese <- 0
CleanData3$ses_ethn_Portugese[DD$SES6_M1 == 13 |
                             DD$SES6_M2 == 13 |
                             DD$SES6_M3 == 13 |
                             DD$SES6_M4 == 13 |
                             DD$SES6_M5 == 13 |
                             DD$SES6_M6 == 13 |
                             DD$SES6_M7 == 13 |
                             DD$SES6_M8 == 13 |
                             DD$SES6_M9 == 13 |
                             DD$SES6_M10 == 13 |
                             DD$SES6_M11 == 13 |
                             DD$SES6_M12 == 13 |
                             DD$SES6_M13 == 13 |
                             DD$SES6_M14 == 13 |
                             DD$SES6_M15 == 13 |
                             DD$SES6_M16 == 13 |
                             DD$SES6_M17 == 13 ] <- 1
table(CleanData3$ses_ethn_Portugese)

# SUD-ASIATIQUE 

CleanData3$ses_ethn_SouthAsia <- 0
CleanData3$ses_ethn_SouthAsia[DD$SES6_M1 == 14 |
                                DD$SES6_M2 == 14 |
                                DD$SES6_M3 == 14 |
                                DD$SES6_M4 == 14 |
                                DD$SES6_M5 == 14 |
                                DD$SES6_M6 == 14 |
                                DD$SES6_M7 == 14 |
                                DD$SES6_M8 == 14 |
                                DD$SES6_M9 == 14 |
                                DD$SES6_M10 == 14 |
                                DD$SES6_M11 == 14 |
                                DD$SES6_M12 == 14 |
                                DD$SES6_M13 == 14 |
                                DD$SES6_M14 == 14 |
                                DD$SES6_M15 == 14 |
                                DD$SES6_M16 == 14 |
                                DD$SES6_M17 == 14 ] <- 1
table(CleanData3$ses_ethn_SouthAsia)

# NORVÉGIEN

CleanData3$ses_ethn_Norway <- 0
CleanData3$ses_ethn_Norway[DD$SES6_M1 == 15 |
                                DD$SES6_M2 == 15 |
                                DD$SES6_M3 == 15 |
                                DD$SES6_M4 == 15 |
                                DD$SES6_M5 == 15 |
                                DD$SES6_M6 == 15 |
                                DD$SES6_M7 == 15 |
                                DD$SES6_M8 == 15 |
                                DD$SES6_M9 == 15 |
                                DD$SES6_M10 == 15 |
                                DD$SES6_M11 == 15 |
                                DD$SES6_M12 == 15 |
                                DD$SES6_M13 == 15 |
                                DD$SES6_M14 == 15 |
                                DD$SES6_M15 == 15 |
                                DD$SES6_M16 == 15 |
                                DD$SES6_M17 == 15 ] <- 1
table(CleanData3$ses_ethn_Norway)

# GALLOIS
CleanData3$ses_ethn_Gallois <- 0
CleanData3$ses_ethn_Gallois[DD$SES6_M1 == 16 |
                             DD$SES6_M2 == 16 |
                             DD$SES6_M3 == 16 |
                             DD$SES6_M4 == 16 |
                             DD$SES6_M5 == 16 |
                             DD$SES6_M6 == 16 |
                             DD$SES6_M7 == 16 |
                             DD$SES6_M8 == 16 |
                             DD$SES6_M9 == 16 |
                             DD$SES6_M10 == 16 |
                             DD$SES6_M11 == 16 |
                             DD$SES6_M12 == 16 |
                             DD$SES6_M13 == 16 |
                             DD$SES6_M14 == 16 |
                             DD$SES6_M15 == 16 |
                             DD$SES6_M16 == 16 |
                             DD$SES6_M17 == 16 ] <- 1
table(CleanData3$ses_ethn_Gallois)

#SUÉDOIS 
CleanData3$ses_ethn_Sweden <- 0
CleanData3$ses_ethn_Sweden[DD$SES6_M1 == 17 |
                              DD$SES6_M2 == 17 |
                              DD$SES6_M3 == 17 |
                              DD$SES6_M4 == 17 |
                              DD$SES6_M5 == 17 |
                              DD$SES6_M6 == 17 |
                              DD$SES6_M7 == 17 |
                              DD$SES6_M8 == 17 |
                              DD$SES6_M9 == 17 |
                              DD$SES6_M10 == 17 |
                              DD$SES6_M11 == 17 |
                              DD$SES6_M12 == 17 |
                              DD$SES6_M13 == 17 |
                              DD$SES6_M14 == 17 |
                              DD$SES6_M15 == 17 |
                              DD$SES6_M16 == 17 |
                              DD$SES6_M17 == 17 ] <- 1
table(CleanData3$ses_ethn_Sweden)

# FIRST NATIONS 
CleanData3$ses_ethn_Aborig <- 0
CleanData3$ses_ethn_Aborig[DD$SES6_M1 == 18 |
                             DD$SES6_M2 == 18 |
                             DD$SES6_M3 == 18 |
                             DD$SES6_M4 == 18 |
                             DD$SES6_M5 == 18 |
                             DD$SES6_M6 == 18 |
                             DD$SES6_M7 == 18 |
                             DD$SES6_M8 == 18 |
                             DD$SES6_M9 == 18 |
                             DD$SES6_M10 == 18 |
                             DD$SES6_M11 == 18 |
                             DD$SES6_M12 == 18 |
                             DD$SES6_M13 == 18 |
                             DD$SES6_M14 == 18 |
                             DD$SES6_M15 == 18 |
                             DD$SES6_M16 == 18 |
                             DD$SES6_M17 == 18 ] <- 1
table(CleanData3$ses_ethn_Aborig)

# METIS 
CleanData3$ses_ethn_Metis <- 0
CleanData3$ses_ethn_Metis[DD$SES6_M1 == 19 |
                             DD$SES6_M2 == 19 |
                             DD$SES6_M3 == 19 |
                             DD$SES6_M4 == 19 |
                             DD$SES6_M5 == 19 |
                             DD$SES6_M6 == 19 |
                             DD$SES6_M7 == 19 |
                             DD$SES6_M8 == 19 |
                             DD$SES6_M9 == 19 |
                             DD$SES6_M10 == 19 |
                             DD$SES6_M11 == 19 |
                             DD$SES6_M12 == 19 |
                             DD$SES6_M13 == 19 |
                             DD$SES6_M14 == 19 |
                             DD$SES6_M15 == 19 |
                             DD$SES6_M16 == 19 |
                             DD$SES6_M17 == 19 ] <- 1
table(CleanData3$ses_ethn_Metis)

# INUIT 
CleanData3$ses_ethn_Inuit <- 0
CleanData3$ses_ethn_Inuit[DD$SES6_M1 == 20 |
                             DD$SES6_M2 == 20 |
                             DD$SES6_M3 == 20 |
                             DD$SES6_M4 == 20 |
                             DD$SES6_M5 == 20 |
                             DD$SES6_M6 == 20 |
                             DD$SES6_M7 == 20 |
                             DD$SES6_M8 == 20 |
                             DD$SES6_M9 == 20 |
                             DD$SES6_M10 == 20 |
                             DD$SES6_M11 == 20 |
                             DD$SES6_M12 == 20 |
                             DD$SES6_M13 == 20 |
                             DD$SES6_M14 == 20 |
                             DD$SES6_M15 == 20 |
                             DD$SES6_M16 == 20 |
                             DD$SES6_M17 == 20 ] <- 1
table(CleanData3$ses_ethn_Inuit)

#OTHER 
CleanData3$ses_ethn_Others <- 0
CleanData3$ses_ethn_Others[DD$SES6_M1 == 21 |
                            DD$SES6_M2 == 21 |
                            DD$SES6_M3 == 21 |
                            DD$SES6_M4 == 21 |
                            DD$SES6_M5 == 21 |
                            DD$SES6_M6 == 21 |
                            DD$SES6_M7 == 21 |
                            DD$SES6_M8 == 21 |
                            DD$SES6_M9 == 21 |
                            DD$SES6_M10 == 21 |
                            DD$SES6_M11 == 21 |
                            DD$SES6_M12 == 21 |
                            DD$SES6_M13 == 21 |
                            DD$SES6_M14 == 21 |
                            DD$SES6_M15 == 21 |
                            DD$SES6_M16 == 21 |
                            DD$SES6_M17 == 21 ] <- 1
table(CleanData3$ses_ethn_Others)

#DONT KNOW
CleanData3$ses_ethn_DK <- 0
CleanData3$ses_ethn_DK[DD$SES6_M1 == 22 |
                            DD$SES6_M2 == 22 |
                            DD$SES6_M3 == 22 |
                            DD$SES6_M4 == 22 |
                            DD$SES6_M5 == 22 |
                            DD$SES6_M6 == 22 |
                            DD$SES6_M7 == 22 |
                            DD$SES6_M8 == 22 |
                            DD$SES6_M9 == 22 |
                            DD$SES6_M10 == 22 |
                            DD$SES6_M11 == 22 |
                            DD$SES6_M12 == 22 |
                            DD$SES6_M13 == 22 |
                            DD$SES6_M14 == 22 |
                            DD$SES6_M15 == 22 |
                            DD$SES6_M16 == 22 |
                            DD$SES6_M17 == 22 ] <- 1
table(CleanData3$ses_ethn_DK)

# CATEGORIE 1 - ABORIGINALS 

CleanData3$ses_ethn_Aboriginals <- NA 
CleanData3$ses_ethn_Aboriginals[CleanData3$ses_ethn_Aborig == 1 | CleanData3$ses_ethn_Metis == 1 | CleanData3$ses_ethn_Inuit == 1] <- 1 
CleanData3$ses_ethn_Aboriginals[CleanData3$ses_ethn_Aborig != 1 & CleanData3$ses_ethn_Metis != 1 & CleanData3$ses_ethn_Inuit != 1] <- 0
table(CleanData3$ses_ethn_Aboriginals)

# CATEGORIE 2 - WHITE 

CleanData3$ses_ethn_White <- NA 
CleanData3$ses_ethn_White[CleanData3$ses_ethn_Canadien == 1 | CleanData3$ses_ethn_Francais == 1 | CleanData3$ses_ethn_Scottish == 1
                         | CleanData3$ses_ethn_Irish == 1 | CleanData3$ses_ethn_Allemand == 1
                         | CleanData3$ses_ethn_Sweden == 1 | CleanData3$ses_ethn_Gallois == 1
                         | CleanData3$ses_ethn_Norway == 1 | CleanData3$ses_ethn_Polish == 1
                         | CleanData3$ses_ethn_Portugese == 1 | CleanData3$ses_ethn_PaysBas == 1
                         | CleanData3$ses_ethn_Ukraine  == 1 | CleanData3$ses_ethn_Italien == 1
                         | CleanData3$ses_ethn_Anglais == 1] <- 1 
CleanData3$ses_ethn_White[CleanData3$ses_ethn_Canadien != 1 & CleanData3$ses_ethn_Francais != 1 & CleanData3$ses_ethn_Scottish != 1
                          & CleanData3$ses_ethn_Irish != 1 & CleanData3$ses_ethn_Allemand != 1
                          & CleanData3$ses_ethn_Sweden != 1 & CleanData3$ses_ethn_Gallois != 1
                          & CleanData3$ses_ethn_Norway != 1 & CleanData3$ses_ethn_Polish != 1
                          & CleanData3$ses_ethn_Portugese != 1 & CleanData3$ses_ethn_PaysBas != 1
                          & CleanData3$ses_ethn_Ukraine  != 1 & CleanData3$ses_ethn_Italien != 1
                          & CleanData3$ses_ethn_Anglais != 1] <- 0
table(CleanData3$ses_ethn_White)

# CATEGORIE 3 - ASIATIQUE

CleanData3$ses_ethn_Asiatique <- NA 
CleanData3$ses_ethn_Asiatique[CleanData3$ses_ethn_Chine == 1 | CleanData3$ses_ethn_SouthAsia == 1] <- 1 
CleanData3$ses_ethn_Asiatique[CleanData3$ses_ethn_Chine != 1 & CleanData3$ses_ethn_SouthAsia != 1] <- 0
table(CleanData3$ses_ethn_Asiatique)

# CATEGORIE 3 - OTHERS

CleanData3$ses_ethn_Other <- NA 
CleanData3$ses_ethn_Other[CleanData3$ses_ethn_Jew == 1 | CleanData3$ses_ethn_Others == 1] <- 1 
CleanData3$ses_ethn_Other[CleanData3$ses_ethn_Jew != 1 & CleanData3$ses_ethn_Others != 1] <- 0
table(CleanData3$ses_ethn_Other)


# Parmi les appellations suivantes, laquelle décrit le mieux votre orientation sexuelle?

table(DD$SES7)

CleanData3$ses_hetero <- NA 
CleanData3$ses_hetero[DD$SES7 == 1] <- 1
CleanData3$ses_hetero[DD$SES7 != 1] <- 0
table(CleanData3$ses_hetero)

CleanData3$ses_gai <- NA 
CleanData3$ses_gai[DD$SES7 == 2] <- 1
CleanData3$ses_gai[DD$SES7 != 2] <- 0
table(CleanData3$ses_gai)

CleanData3$ses_bisex <- NA 
CleanData3$ses_bisex[DD$SES7 == 3] <- 1
CleanData3$ses_bisex[DD$SES7 != 3] <- 0
table(CleanData3$ses_bisex)

CleanData3$ses_sexOri_other <- NA 
CleanData3$ses_sexOri_other[DD$SES7 == 4] <- 1
CleanData3$ses_sexOri_other[DD$SES7 != 4] <- 0
table(CleanData3$ses_sexOri_other)

# Est-ce qu’au moins l'un de vos parents est né à l'extérieur du Canada?

# Êtes-vous né au Canada? 

table(DD$SES9)
CleanData3$immigrant <- NA 
CleanData3$immigrant[DD$SES9 == 1] <- 0 #oui (NON PAS NÉ(E) AU CANADA)
CleanData3$immigrant[DD$SES9 == 2] <- 1 #non
table(CleanData3$immigrant)

# Parmi les catégories suivantes, laquelle décrit le mieux votre type d'habitation?



table(DD$SES10)

# appartement dans un immeuble de moins de 5 étages
CleanData3$ses_dwelling_app <- NA 
CleanData3$ses_dwelling_app[DD$SES10 == 1] <- 1
CleanData3$ses_dwelling_app[DD$SES10 != 1] <- 0
table(CleanData3$ses_dwelling_app)

# loft 
CleanData3$ses_dwelling_loft <- NA 
CleanData3$ses_dwelling_loft[DD$SES10 == 2] <- 1
CleanData3$ses_dwelling_loft[DD$SES10 != 2] <- 0
table(CleanData3$ses_dwelling_loft)

# condo
CleanData3$ses_dwelling_condo <- NA 
CleanData3$ses_dwelling_condo[DD$SES10 == 3] <- 1
CleanData3$ses_dwelling_condo[DD$SES10 != 3] <- 0
table(CleanData3$ses_dwelling_condo)

# tour d'habitation
CleanData3$ses_dwelling_tour <- NA 
CleanData3$ses_dwelling_tour[DD$SES10 == 4] <- 1
CleanData3$ses_dwelling_tour[DD$SES10 != 4] <- 0
table(CleanData3$ses_dwelling_tour)

# maison individuelle 
CleanData3$ses_dwelling_detachedHouse <- NA 
CleanData3$ses_dwelling_detachedHouse[DD$SES10 == 5] <- 1
CleanData3$ses_dwelling_detachedHouse[DD$SES10 != 5] <- 0
table(CleanData3$ses_dwelling_detachedHouse)

# maison de ville
CleanData3$ses_dwelling_townHouse <- NA 
CleanData3$ses_dwelling_townHouse[DD$SES10 == 6] <- 1
CleanData3$ses_dwelling_townHouse[DD$SES10 != 6] <- 0
table(CleanData3$ses_dwelling_townHouse)

# maison semi-détachée
CleanData3$ses_dwelling_semiDetached <- NA 
CleanData3$ses_dwelling_semiDetached[DD$SES10 == 7] <- 1
CleanData3$ses_dwelling_semiDetached[DD$SES10 != 7] <- 0
table(CleanData3$ses_dwelling_semiDetached)

# cooperative 
CleanData3$ses_dwelling_coop <- NA 
CleanData3$ses_dwelling_coop[DD$SES10 == 8] <- 1
CleanData3$ses_dwelling_coop[DD$SES10 != 8] <- 0
table(CleanData3$ses_dwelling_coop)

# HLM
CleanData3$ses_dwelling_HLM <- NA 
CleanData3$ses_dwelling_HLM[DD$SES10 == 9] <- 1
CleanData3$ses_dwelling_HLM[DD$SES10 != 9] <- 0
table(CleanData3$ses_dwelling_HLM)

# maison mobile
CleanData3$ses_dwelling_mobile <- NA 
CleanData3$ses_dwelling_mobile[DD$SES10 == 10] <- 1
CleanData3$ses_dwelling_mobile[DD$SES10 != 10] <- 0
table(CleanData3$ses_dwelling_mobile)

# autre
CleanData3$ses_dwelling_other <- NA 
CleanData3$ses_dwelling_other[DD$SES10 == 11] <- 1
CleanData3$ses_dwelling_other[DD$SES10 != 11] <- 0
table(CleanData3$ses_dwelling_other)


# maison individuelle, maison semi-détachée et maison de ville mises ensemble 
CleanData3$ses_dwelling_house <- NA 
CleanData3$ses_dwelling_house[DD$SES10 == 5 | DD$SES10 == 6 | DD$SES10 == 7] <- 1
CleanData3$ses_dwelling_house[DD$SES10 != 5 & DD$SES10 != 6 & DD$SES10 != 7] <- 0
table(CleanData3$ses_dwelling_house)


# Quelle est votre religion, si vous en avez une?|

# Selon vous, les Québécois sont-ils plus ou moins religieux que les autres Canadiens?
DDDD <- subset(DD, PROV == 11)

table(DDDD$SE12)


CleanData3$op_QcMoreReligious <- NA
CleanData3$op_QcMoreReligious[DD$SE12==5] <- 0    #Beaucoup moins religieux
CleanData3$op_QcMoreReligious[DD$SE12==4] <- 0.25  
CleanData3$op_QcMoreReligious[DD$SE12==3] <- 0.5
CleanData3$op_QcMoreReligious[DD$SE12==2] <- 0.75    
CleanData3$op_QcMoreReligious[DD$SE12==1] <- 1    #Beaucoup plus religieux
table(CleanData3$op_QcMoreReligious)

# Dans quelle mesure faites-vous confiance à l'Église?|

# À quelle fréquence priez-vous en dehors des services religieux? Diriez-vous

# Laquelle des deux the`ses refle`te le plus l'explication de la vie humaine sur Terre?

# La connaissance, la perspicacité et la raison devraient guider les comportements des gens plus que les normes religieuses et morales


# Dans le passé, la religion a fait plus de mal que de bien à l'humanité.



write_csv(CleanData3, "../bav-2021/_SharedFolder_bav-2021/Data/Clean/CleanData-Lifestyle2.csv")



