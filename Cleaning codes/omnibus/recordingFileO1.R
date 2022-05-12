#### BEGINNING OF THE DOCUMENT ####

## Library used ##
library(foreign) # For reading Stata files
library(tidyverse)
library(haven)
library(readr)
library(labelled)

#********************************#
####_____ 0. LOADING DATA_____####
#********************************#  

## Loading/Reading a Datafile ##

Data  <- read_sav("_SharedFolder-omnibus/1-January2022/data/Raw/OMN01-ALL.Sav")
Data2 <- read.spss("_SharedFolder-omnibus/1-January2022/data/Raw/OMN01-ALL.Sav",to.data.frame = T,
                   reencode="utf-8")
Post <- readxl::read_excel("_SharedFolder-omnibus/1-January2022/data/Raw/omn01-postalcodes.xlsx")
pathToMonth <- "_SharedFolder-omnibus/1-January2022/"

source("codeR/cleaningSource.R")

#write_excel_csv(CleanData,"_SharedFolder-omnibus/questionnaires/January2022/data/Clean/omnibusJan22Partial.csv")

#***************************************#
####_____ 1. Cleanning Questions_____####
#***************************************# 

#### 1.1 Vote int Prov ####

# S’il y avait une élection générale provinciale au Québec aujourd’hui, 
# pour quel parti voteriez-vous ou seriez-vous tenté(e) de voter pour 
# vous représenter à l'Assemblée nationale du Québec?
var_label(Data$Q2)
table(Data$Q2)
table(Data2$Q2)
CleanData$voteIntCAQ <- NA
CleanData$voteIntCAQ[Data$Q2==1] <- 1
CleanData$voteIntCAQ[Data$Q2!=1 & !is.na(Data$Q2)] <- 0
CleanData$voteIntCAQ[Data$Q2 %in% c(7,6,5)] <- NA
table(CleanData$voteIntCAQ)

CleanData$voteIntPQ <- NA
CleanData$voteIntPQ[Data$Q2==2] <- 1
CleanData$voteIntPQ[Data$Q2!=2 & !is.na(Data$Q2)] <- 0
CleanData$voteIntPQ[Data$Q2 %in% c(7,6,5)] <- NA
table(CleanData$voteIntPQ)

CleanData$voteIntPLQ <- NA
CleanData$voteIntPLQ[Data$Q2==3] <- 1
CleanData$voteIntPLQ[Data$Q2!=3 & !is.na(Data$Q2)] <- 0
CleanData$voteIntPLQ[Data$Q2 %in% c(7,6,5)] <- NA
table(CleanData$voteIntPLQ)

CleanData$voteIntQS <- NA
CleanData$voteIntQS[Data$Q2==4] <- 1
CleanData$voteIntQS[Data$Q2!=4 & !is.na(Data$Q2)] <- 0
CleanData$voteIntQS[Data$Q2 %in% c(7,6,5)] <- NA
table(CleanData$voteIntQS)

CleanData$voteIntPCQ <- NA
CleanData$voteIntPCQ[Data$Q2==8] <- 1
CleanData$voteIntPCQ[Data$Q2!=8 & !is.na(Data$Q2)] <- 0
CleanData$voteIntPCQ[Data$Q2 %in% c(7,6,5)] <- NA
table(CleanData$voteIntPCQ)

# Peut-être que votre choix n'est pas définitif et que 
# vous pourriez changer d'avis d'ici l’élection, mais aujourd'hui, 
# quel parti ou chef seriez-vous tenté(e) d'appuyer?
var_label(Data$Q3)
table(Data$Q3)
table(Data2$Q3)
CleanData$voteIntCAQ[Data$Q3==1] <- 1
CleanData$voteIntPQ[Data$Q3==2] <- 1
CleanData$voteIntPLQ[Data$Q3==3] <- 1
CleanData$voteIntQS[Data$Q3==4] <- 1
CleanData$voteIntPCQ[Data$Q3==8] <- 1

# Quel serait votre second choix?
var_label(Data$Q3B)
table(Data$Q3B)
table(Data2$Q3B)
CleanData$voteIntCAQ2 <- NA
CleanData$voteIntCAQ2[Data$Q3B==1] <- 1
CleanData$voteIntCAQ2[Data$Q3B!=1 & !is.na(Data$Q3B)] <- 0
CleanData$voteIntCAQ2[Data$Q3B %in% c(7,6,5)] <- NA
table(CleanData$voteIntCAQ2)

CleanData$voteIntPQ2 <- NA
CleanData$voteIntPQ2[Data$Q3B==2] <- 1
CleanData$voteIntPQ2[Data$Q3B!=2 & !is.na(Data$Q3B)] <- 0
CleanData$voteIntPQ2[Data$Q3B %in% c(7,6,5)] <- NA
table(CleanData$voteIntPQ2)

CleanData$voteIntPLQ2 <- NA
CleanData$voteIntPLQ2[Data$Q3B==3] <- 1
CleanData$voteIntPLQ2[Data$Q3B!=3 & !is.na(Data$Q3B)] <- 0
CleanData$voteIntPLQ2[Data$Q3B %in% c(7,6,5)] <- NA
table(CleanData$voteIntPLQ2)

CleanData$voteIntQS2 <- NA
CleanData$voteIntQS2[Data$Q2==4] <- 1
CleanData$voteIntQS2[Data$Q2!=4 & !is.na(Data$Q3B)] <- 0
CleanData$voteIntQS2[Data$Q2 %in% c(7,6,5)] <- NA
table(CleanData$voteIntQS2)

CleanData$voteIntPCQ2 <- NA
CleanData$voteIntPCQ2[Data$Q3B==8] <- 1
CleanData$voteIntPCQ2[Data$Q3B!=8 & !is.na(Data$Q3B)] <- 0
CleanData$voteIntPCQ2[Data$Q3B %in% c(7,6,5)] <- NA
table(CleanData$voteIntPCQ2)

# Est-il possible que vous appuyiez ce parti ou êtes-vous certain(e) 
# que vous ne voterez pas pour lui? 
# La CAQ, la Coalition pour l'avenir du Québec de François Legault
var_label(Data$Q3C_A1)
table(Data$Q3C_A1)
table(Data2$Q3C_A1)
CleanData$couldVoteCAQ <- NA
CleanData$couldVoteCAQ[Data$Q3C_A1==1] <- 1
CleanData$couldVoteCAQ[Data$Q3C_A1!=1 & !is.na(Data$Q3C_A1)] <- 0
table(CleanData$couldVoteCAQ)

# Le PQ, le Parti québécois de Paul St-Pierre Plamondon
var_label(Data$Q3C_A2)
table(Data$Q3C_A2)
table(Data2$Q3C_A2)
CleanData$couldVotePQ <- NA
CleanData$couldVotePQ[Data$Q3C_A2==1] <- 1
CleanData$couldVotePQ[Data$Q3C_A2!=1 & !is.na(Data$Q3C_A2)] <- 0
table(CleanData$couldVotePQ)

# Le PLQ, le Parti libéral du Québec de Dominique Anglade
var_label(Data$Q3C_A3)
table(Data$Q3C_A3)
table(Data2$Q3C_A3)
CleanData$couldVotePLQ <- NA
CleanData$couldVotePLQ[Data$Q3C_A3==1] <- 1
CleanData$couldVotePLQ[Data$Q3C_A3!=1 & !is.na(Data$Q3C_A3)] <- 0
table(CleanData$couldVotePLQ)

# QS, Québec solidaire de Gabriel Nadeau-Dubois
var_label(Data$Q3C_A4)
table(Data$Q3C_A4)
table(Data2$Q3C_A4)
CleanData$couldVoteQS <- NA
CleanData$couldVoteQS[Data$Q3C_A4==1] <- 1
CleanData$couldVoteQS[Data$Q3C_A4!=1 & !is.na(Data$Q3C_A4)] <- 0
table(CleanData$couldVoteQS)

# Le PCQ, le Parti Conservateur du Québec de Éric Duhaime
var_label(Data$Q3C_A5)
table(Data$Q3C_A5)
table(Data2$Q3C_A5)
CleanData$couldVotePCQ <- NA
CleanData$couldVotePCQ[Data$Q3C_A5==1] <- 1
CleanData$couldVotePCQ[Data$Q3C_A5!=1 & !is.na(Data$Q3C_A5)] <- 0
table(CleanData$couldVotePCQ)

#### 1.2 Best PM Prov ####
# Quel chef, parmi les suivants, ferait le meilleur premier ministre pour le Québec?
var_label(Data$Q4)
table(Data$Q4)
table(Data2$Q4)
CleanData$bestPmLegault <- NA
CleanData$bestPmLegault[Data$Q4==1] <- 1
CleanData$bestPmLegault[Data$Q4!=1 & !is.na(Data$Q4)] <- 0
CleanData$bestPmLegault[Data$Q4 %in% c(7,6)] <- NA
table(CleanData$bestPmLegault)

CleanData$bestPmPSPP <- NA
CleanData$bestPmPSPP[Data$Q4==2] <- 1
CleanData$bestPmPSPP[Data$Q4!=2 & !is.na(Data$Q4)] <- 0
CleanData$bestPmPSPP[Data$Q4 %in% c(7,6)] <- NA
table(CleanData$bestPmPSPP)

CleanData$bestPmAnglade <- NA
CleanData$bestPmAnglade[Data$Q4==3] <- 1
CleanData$bestPmAnglade[Data$Q4!=3 & !is.na(Data$Q4)] <- 0
CleanData$bestPmAnglade[Data$Q4 %in% c(7,6)] <- NA
table(CleanData$bestPmAnglade)

CleanData$bestPmGND <- NA
CleanData$bestPmGND[Data$Q4==4] <- 1
CleanData$bestPmGND[Data$Q4!=4 & !is.na(Data$Q4)] <- 0
CleanData$bestPmGND[Data$Q4 %in% c(7,6)] <- NA
table(CleanData$bestPmGND)

CleanData$bestPmDuhaime <- NA
CleanData$bestPmDuhaime[Data$Q4==5] <- 1
CleanData$bestPmDuhaime[Data$Q4!=5 & !is.na(Data$Q4)] <- 0
CleanData$bestPmDuhaime[Data$Q4 %in% c(7,6)] <- NA
table(CleanData$bestPmDuhaime)

#### 1.3 Pot Croi Prov ####
# Quel que soit le parti pour lequel vous avez l'intention de voter 
# à l'occasion de la prochaine élection provinciale québécoise, en général, 
# quelle est la probabilité que vous appuyiez :
# la Coalition avenir Québec
var_label(Data$C3_A1)
table(Data$C3_A1)
CleanData$potGrowthCAQ <- Data$C3_A1/10
table(CleanData$potGrowthCAQ)

# le Parti libéral du Québec
var_label(Data$C3_A2)
table(Data$C3_A2)
CleanData$potGrowthPLQ <- Data$C3_A2/10
table(CleanData$potGrowthPLQ)

# le Parti Québecois
var_label(Data$C3_A3)
table(Data$C3_A3)
CleanData$potGrowthPQ <- Data$C3_A3/10
table(CleanData$potGrowthPQ)

# Québec solidaire
var_label(Data$C3_A4)
table(Data$C3_A4)
CleanData$potGrowthQS <- Data$C3_A4/10
table(CleanData$potGrowthQS)

# le Parti conservateur du Québec
var_label(Data$C3_A5)
table(Data$C3_A5)
CleanData$potGrowthPCQ <- Data$C3_A5/10
table(CleanData$potGrowthPCQ)

#### 1.4 Vote Int Fed ####

# Parlons maintenant du gouvernement fédéral à Ottawa
# Si les élections fédérales avaient lieu aujourd’hui, pour lequel des partis suivants 
# voteriez-vous ou seriez-vous tenté de voter?
var_label(Data$QFED1)
table(Data$QFED1)
table(Data2$QFED1)
CleanData$voteIntPLC <- NA
CleanData$voteIntPLC[Data$QFED1==1] <- 1
CleanData$voteIntPLC[Data$QFED1!=1 & !is.na(Data$QFED1)] <- 0
CleanData$voteIntPLC[Data$QFED1 %in% c(9,8,7)] <- NA
table(CleanData$voteIntPLC)

CleanData$voteIntPCC <- NA
CleanData$voteIntPCC[Data$QFED1==2] <- 1
CleanData$voteIntPCC[Data$QFED1!=2 & !is.na(Data$QFED1)] <- 0
CleanData$voteIntPCC[Data$QFED1 %in% c(9,8,7)] <- NA
table(CleanData$voteIntPCC)

CleanData$voteIntNPD <- NA
CleanData$voteIntNPD[Data$QFED1==3] <- 1
CleanData$voteIntNPD[Data$QFED1!=3 & !is.na(Data$QFED1)] <- 0
CleanData$voteIntNPD[Data$QFED1 %in% c(9,8,7)] <- NA
table(CleanData$voteIntNPD)

CleanData$voteIntBQ <- NA
CleanData$voteIntBQ[Data$QFED1==4] <- 1
CleanData$voteIntBQ[Data$QFED1!=4 & !is.na(Data$QFED1)] <- 0
CleanData$voteIntBQ[Data$QFED1 %in% c(9,8,7)] <- NA
table(CleanData$voteIntBQ)

CleanData$voteIntPPC <- NA
CleanData$voteIntPPC[Data$QFED1==5] <- 1
CleanData$voteIntPPC[Data$QFED1!=5 & !is.na(Data$QFED1)] <- 0
CleanData$voteIntPPC[Data$QFED1 %in% c(9,8,7)] <- NA
table(CleanData$voteIntPPC)

CleanData$voteIntPVC <- NA
CleanData$voteIntPVC[Data$QFED1==6] <- 1
CleanData$voteIntPVC[Data$QFED1!=6 & !is.na(Data$QFED1)] <- 0
CleanData$voteIntPVC[Data$QFED1 %in% c(9,8,7)] <- NA
table(CleanData$voteIntPVC)

# Peut-être que votre choix n’est pas définitif, mais y a-t-il tout de même 
# un parti ou chef que vous seriez tenté(e) d’appuyer?
var_label(Data$QFED2)
table(Data$QFED2)
table(Data2$QFED2)
CleanData$voteIntPLC[Data$QFED2==1] <- 1
CleanData$voteIntPCC[Data$QFED2==2] <- 1
CleanData$voteIntNPD[Data$QFED2==3] <- 1
CleanData$voteIntBQ[Data$QFED2==4] <- 1
CleanData$voteIntPPC[Data$QFED2==5] <- 1
CleanData$voteIntPVC[Data$QFED2==6] <- 1

#### 1.5 Pot Croi Fed ####

# Quel que soit le parti pour lequel vous avez l'intention de voter 
# à l'occasion de la prochaine élection fédérale canadienne, en général, 
# quelle est la probabilité que vous appuyiez :
# le Parti libéral du Canada
var_label(Data$C2_A1)
table(Data$C2_A1)
CleanData$potGrowthPLC <- Data$C2_A1/10
table(CleanData$potGrowthPLC)
# le Parti conservateur du Canada
var_label(Data$C2_A2)
table(Data$C2_A2)
CleanData$potGrowthPCC <- Data$C2_A2/10
table(CleanData$potGrowthPCC)
# le Nouveau parti démocratique
var_label(Data$C2_A3)
table(Data$C2_A3)
CleanData$potGrowthNPD <- Data$C2_A3/10
table(CleanData$potGrowthNPD)
# le Bloc Québécois
var_label(Data$C2_A4)
table(Data$C2_A4)
CleanData$potGrowthBQ <- Data$C2_A4/10
table(CleanData$potGrowthBQ)
# le Parti populaire du Canada
var_label(Data$C2_A5)
table(Data$C2_A5)
CleanData$potGrowthPPC <- Data$C2_A5/10
table(CleanData$potGrowthPPC)
# le Parti vert du Canada
var_label(Data$C2_A6)
table(Data$C2_A6)
CleanData$potGrowthPVC <- Data$C2_A6/10
table(CleanData$potGrowthPVC)

#### 1.6 Voted Fed 2021 ####
# Pour quel parti avez-vous voté lors de l’élection fédérale canadienne de 2021?
var_label(Data$C12)
table(Data$C12)
table(Data2$C12)
CleanData$votedFedPLC21 <- NA
CleanData$votedFedPLC21[Data$C12==1] <- 1
CleanData$votedFedPLC21[Data$C12!=1] <- 0
CleanData$votedFedPLC21[Data$C12 %in% c(10,9,8,7) | is.na(Data$C12)] <- NA

CleanData$votedFedPCC21 <- NA
CleanData$votedFedPCC21[Data$C12==2] <- 1
CleanData$votedFedPCC21[Data$C12!=2] <- 0
CleanData$votedFedPCC21[Data$C12 %in% c(10,9,8,7) | is.na(Data$C12)] <- NA

CleanData$votedFedNPD21 <- NA
CleanData$votedFedNPD21[Data$C12==3] <- 1
CleanData$votedFedNPD21[Data$C12!=3] <- 0
CleanData$votedFedNPD21[Data$C12 %in% c(10,9,8,7) | is.na(Data$C12)] <- NA

CleanData$votedFedBQ21 <- NA
CleanData$votedFedBQ21[Data$C12==4] <- 1
CleanData$votedFedBQ21[Data$C12!=4] <- 0
CleanData$votedFedBQ21[Data$C12 %in% c(10,9,8,7) | is.na(Data$C12)] <- NA

CleanData$votedFedPVC21 <- NA
CleanData$votedFedPVC21[Data$C12==5] <- 1
CleanData$votedFedPVC21[Data$C12!=5] <- 0
CleanData$votedFedPVC21[Data$C12 %in% c(10,9,8,7) | is.na(Data$C12)] <- NA

CleanData$votedFedPPC21 <- NA
CleanData$votedFedPPC21[Data$C12==6] <- 1
CleanData$votedFedPPC21[Data$C12!=6] <- 0
CleanData$votedFedPPC21[Data$C12 %in% c(10,9,8,7) | is.na(Data$C12)] <- NA


# Pour débuter, de façon générale, diriez-vous qu’au Québec les choses vont dans la bonne direction ou dans la mauvaise direction?
var_label(Data$Q1)
table(Data$Q1)
table(Data2$Q1)
CleanData$iss_quebRightDirect <- NA
CleanData$iss_quebRightDirect[Data$Q1==1] <- 1
CleanData$iss_quebRightDirect[Data$Q1!=1 & !is.na(Data$Q1)] <- 0
table(CleanData$iss_quebRightDirect)

# Dans quelle mesure êtes-vous satisfait(e) du gouvernement provincial de la CAQ de François Legault?
var_label(Data$Q5)
table(Data$Q5)
table(Data2$Q5)
CleanData$iss_satisfiedGouvCAQ <- NA
CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Tr.s satisfait(e)"] <- 1
CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Assez satisfait(e)"] <- 0.66
CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Assez insatisfait(e)"] <- 0.33
CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Tr.s insatisfait(e)"] <- 0
CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Je ne sais pas"] <- NA
table(CleanData$iss_satisfiedGouvCAQ)
# Dans quelle mesure êtes-vous satisfait(e) du gouvernement fédéral du PLC de Justin Trudeau?
var_label(Data$QFED3)
table(Data$QFED3)
table(Data2$QFED3)
CleanData$iss_satisfiedGouvPLC <- NA
CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Tr.s satisfait(e)"] <- 1
CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Assez satisfait(e)"] <- 0.66
CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Assez insatisfait(e)"] <- 0.33
CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Tr.s insatisfait(e)"] <- 0
CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Je ne sais pas"] <- NA
table(CleanData$iss_satisfiedGouvPLC)

# Au cours du dernier mois, avez-vous lu, vu ou entendu quoique ce soit dans les médias à propos du gouvernement 
# de la CAQ de François Legault?
var_label(Data$Q6)
table(Data$Q6)
table(Data2$Q6)

# Ce que vous avez lu, vu ou entendu vous a-t-il donné une impression plus ou moins favorable 
# du gouvernement de la CAQ de François Legault?
var_label(Data$Q7)
table(Data$Q7)
table(Data2$Q7)
CleanData$heardCAQGood <- NA
CleanData$heardCAQGood[Data2$Q7 == "Beaucoup plus favorable"] <- 1
CleanData$heardCAQGood[Data2$Q7 == "Un peu plus favorable"] <- 0.75
CleanData$heardCAQGood[Data2$Q7 == "N.a pas eu d.impact"] <- 0.5
CleanData$heardCAQGood[Data2$Q7 == "Un peu moins favorable"] <- 0.25
CleanData$heardCAQGood[Data2$Q7 == "Beaucoup moins favorable"] <- 0
table(CleanData$heardCAQGood)
# Personnellement, comment vous décrivez-vous? Diriez-vous que vous êtes
var_label(Data$Q21)
table(Data$Q21)
table(Data2$Q21)
CleanData$isQcBeforeCan <- NA
CleanData$isQcBeforeCan[Data2$Q21=="Avant tout Qu.b.cois"] <- 1
CleanData$isQcBeforeCan[Data2$Q21=="Avant tout Canadien"] <- 0
table(CleanData$isQcBeforeCan)

CleanData$isCanBeforeQc <- NA
CleanData$isCanBeforeQc[Data2$Q21=="Avant tout Qu.b.cois"] <- 0
CleanData$isCanBeforeQc[Data2$Q21=="Avant tout Canadien"] <- 1
table(CleanData$isCanBeforeQc)

# Lequel des énoncés décrit le mieux votre préférence sur le statut politique du Québec ?
var_label(Data$Q22)
table(Data$Q22)
table(Data2$Q22)
CleanData$isSouverainiste <- NA
CleanData$isSouverainiste[Data2$Q22 == "Fortement en faveur que le Qu.bec devienne un pays souverain"] <- 1
CleanData$isSouverainiste[Data2$Q22 == "Mod.r.ment en faveur que le Qu.bec devienne un pays souverai"] <- 0.66
CleanData$isSouverainiste[Data2$Q22 == "Mod.r.ment en faveur que le Qu.bec reste dans le Canada"] <- 0.33
CleanData$isSouverainiste[Data2$Q22 == "Fortement en faveur que le Qu.bec reste dans le Canada"] <- 0
table(CleanData$isSouverainiste)

# Êtes-vous inquiet(e) de la situation de la langue française au Québec?
var_label(Data$Q10)
table(Data$Q10)
table(Data2$Q10)
CleanData$iss_worriedFrInProvQc <- NA
CleanData$iss_worriedFrInProvQc[Data2$Q10 == "Tr.s inquiet(e)"] <- 1
CleanData$iss_worriedFrInProvQc[Data2$Q10 == "Inquiet(e)"] <- 0.66
CleanData$iss_worriedFrInProvQc[Data2$Q10 == "Peu inquiet(e)"] <- 0.33
CleanData$iss_worriedFrInProvQc[Data2$Q10 == "Pas du tout inquiet(e)"] <- 0
table(CleanData$iss_worriedFrInProvQc)

# Êtes-vous inquiet(e) de la situation de la langue française à Montréal?
var_label(Data$Q11)
table(Data$Q11)
table(Data2$Q11)
CleanData$iss_worriedFrInMtl <- NA
CleanData$iss_worriedFrInMtl[Data2$Q11 == "Tr.s inquiet(e)"] <- 1
CleanData$iss_worriedFrInMtl[Data2$Q11 == "Inquiet(e)"] <- 0.66
CleanData$iss_worriedFrInMtl[Data2$Q11 == "Peu inquiet(e)"] <- 0.33
CleanData$iss_worriedFrInMtl[Data2$Q11 == "Pas du tout inquiet(e)"] <- 0
table(CleanData$iss_worriedFrInMtl)

# Dans quelle mesure êtes-vous sécure, ou insécure, à l’égard de votre situation financière?
var_label(Data$Q12)
table(Data$Q12)
table(Data2$Q12)
CleanData$iss_isFinanceSecure <- NA
CleanData$iss_isFinanceSecure[Data2$Q12 == "Tr.s s.cure"] <- 1
CleanData$iss_isFinanceSecure[Data2$Q12 == "Assez s.cure"] <- 0.66
CleanData$iss_isFinanceSecure[Data2$Q12 == "Assez ins.cure"] <- 0.33
CleanData$iss_isFinanceSecure[Data2$Q12 == "Tr.s ins.cure"] <- 0
table(CleanData$iss_isFinanceSecure)

# Avez-vous l’impression que la vie vous coûte plus cher aujourd’hui qu’il y a 12 mois?
var_label(Data$Q13)
table(Data$Q13)
table(Data2$Q13)
CleanData$iss_findsLifeMoExp <- NA
CleanData$iss_findsLifeMoExp[Data2$Q13 == "Oui, beaucoup plus cher"] <- 1
CleanData$iss_findsLifeMoExp[Data2$Q13 == "Oui, un peu plus cher"] <- 0.66
CleanData$iss_findsLifeMoExp[Data2$Q13 == "Non, pas plus cher"] <- 0.33
CleanData$iss_findsLifeMoExp[Data2$Q13 == "Je ne sais pas"] <- 0
table(CleanData$iss_findsLifeMoExp)

# Avez-vous l’impression que la vie vous coûtera plus cher dans les 12 prochains mois?
var_label(Data$Q14)
table(Data$Q14)
table(Data2$Q14)
CleanData$iss_moExpInNext12 <- NA
CleanData$iss_moExpInNext12[Data2$Q14 == "Oui, beaucoup plus cher"] <- 1
CleanData$iss_moExpInNext12[Data2$Q14 == "Oui, un peu plus cher"] <- 0.66
CleanData$iss_moExpInNext12[Data2$Q14 == "Non, pas plus cher"] <- 0.33
CleanData$iss_moExpInNext12[Data2$Q14 == "Je ne sais pas"] <- 0
table(CleanData$iss_moExpInNext12)

# Avez-vous changé certaines habitudes pour faire face à l’augmentation des prix?
var_label(Data$Q15)
table(Data$Q15)
table(Data2$Q15)
CleanData$iss_changedHabitsAugPrice <- NA
CleanData$iss_changedHabitsAugPrice[Data2$Q15 == "Oui j.ai chang. mes habitudes"] <- 1
CleanData$iss_changedHabitsAugPrice[Data2$Q15 == "Non je n.ai pas chang. mes habitudes"] <- 0
table(CleanData$iss_changedHabitsAugPrice)

# Quel aspect de votre consommation coûte plus cher aujourd’hui qu’il y a 12 mois?
var_label(Data$Q16)
table(Data$Q16)
table(Data2$Q16)

# Êtes-vous en accord ou en désaccord avec les énoncés suivants?
# Toutes les personnes blanches ont un biais raciste, qu'il soit conscient ou non.
var_label(Data$QWOKE_A1)
table(Data$QWOKE_A1)
table(Data2$QWOKE_A1)
CleanData$woke_whiteAreRacists <- NA
CleanData$woke_whiteAreRacists[Data2$QWOKE_A1 == "Tout . fait d.accord"] <- 1
CleanData$woke_whiteAreRacists[Data2$QWOKE_A1 == "Plut.t d.accord"] <- 0.66
CleanData$woke_whiteAreRacists[Data2$QWOKE_A1 == "Plut.t en d.saccord"] <- 0.33
CleanData$woke_whiteAreRacists[Data2$QWOKE_A1 == "Tout . fait en d.saccord"] <- 0
CleanData$woke_whiteAreRacists[Data2$QWOKE_A1 == "Je ne sais pas"] <- NA
table(CleanData$woke_whiteAreRacists)

# Il existe une réalité objective. Tout n'est pas une question de point de vue.
var_label(Data$QWOKE_A2)
table(Data$QWOKE_A2)
table(Data2$QWOKE_A2)
CleanData$woke_objRealityExists <- NA
CleanData$woke_objRealityExists[Data2$QWOKE_A2 == "Tout . fait d.accord"] <- 1
CleanData$woke_objRealityExists[Data2$QWOKE_A2 == "Plut.t d.accord"] <- 0.66
CleanData$woke_objRealityExists[Data2$QWOKE_A2 == "Plut.t en d.saccord"] <- 0.33
CleanData$woke_objRealityExists[Data2$QWOKE_A2 == "Tout . fait en d.saccord"] <- 0
CleanData$woke_objRealityExists[Data2$QWOKE_A2 == "Je ne sais pas"] <- NA
table(CleanData$woke_objRealityExists)

# Les personnes blanches devraient laisser toute la place aux personnes racisées 
# lorsqu'il est question d'enjeux qui touchent ces dernières.
var_label(Data$QWOKE_A3)
table(Data$QWOKE_A3)
table(Data2$QWOKE_A3)
CleanData$woke_noWhitesRaceQuestions <- NA
CleanData$woke_noWhitesRaceQuestions[Data2$QWOKE_A3 == "Tout . fait d.accord"] <- 1
CleanData$woke_noWhitesRaceQuestions[Data2$QWOKE_A3 == "Plut.t d.accord"] <- 0.66
CleanData$woke_noWhitesRaceQuestions[Data2$QWOKE_A3 == "Plut.t en d.saccord"] <- 0.33
CleanData$woke_noWhitesRaceQuestions[Data2$QWOKE_A3 == "Tout . fait en d.saccord"] <- 0
CleanData$woke_noWhitesRaceQuestions[Data2$QWOKE_A3 == "Je ne sais pas"] <- NA
table(CleanData$woke_noWhitesRaceQuestions)

# On devrait censurer les oeuvres qui contiennent des mots qui offensent certaines minorités.
var_label(Data$QWOKE_A4)
table(Data$QWOKE_A4)
table(Data2$QWOKE_A4)
CleanData$woke_censorRacistWorks <- NA
CleanData$woke_censorRacistWorks[Data2$QWOKE_A4 == "Tout . fait d.accord"] <- 1
CleanData$woke_censorRacistWorks[Data2$QWOKE_A4 == "Plut.t d.accord"] <- 0.66
CleanData$woke_censorRacistWorks[Data2$QWOKE_A4 == "Plut.t en d.saccord"] <- 0.33
CleanData$woke_censorRacistWorks[Data2$QWOKE_A4 == "Tout . fait en d.saccord"] <- 0
CleanData$woke_censorRacistWorks[Data2$QWOKE_A4 == "Je ne sais pas"] <- NA
table(CleanData$woke_censorRacistWorks)

# La société est sous contrôle d'un pouvoir patriarcal blanc 
# qui domine les femmes et les personnes racisées.
var_label(Data$QWOKE_A5)
table(Data$QWOKE_A5)
table(Data2$QWOKE_A5)
CleanData$woke_socCtrlByWhiteMen <- NA
CleanData$woke_socCtrlByWhiteMen[Data2$QWOKE_A5 == "Tout . fait d.accord"] <- 1
CleanData$woke_socCtrlByWhiteMen[Data2$QWOKE_A5 == "Plut.t d.accord"] <- 0.66
CleanData$woke_socCtrlByWhiteMen[Data2$QWOKE_A5 == "Plut.t en d.saccord"] <- 0.33
CleanData$woke_socCtrlByWhiteMen[Data2$QWOKE_A5 == "Tout . fait en d.saccord"] <- 0
CleanData$woke_socCtrlByWhiteMen[Data2$QWOKE_A5 == "Je ne sais pas"] <- NA
table(CleanData$woke_socCtrlByWhiteMen)

# Pour réussir dans la vie, il faut venir des groupes sociaux privilégiés. 
# Le travail, l'effort et le talent ne comptent presque pas.
var_label(Data$QWOKE_A6)
table(Data$QWOKE_A6)
table(Data2$QWOKE_A6)
CleanData$woke_richToSucceed <- NA
CleanData$woke_richToSucceed[Data2$QWOKE_A6 == "Tout . fait d.accord"] <- 1
CleanData$woke_richToSucceed[Data2$QWOKE_A6 == "Plut.t d.accord"] <- 0.66
CleanData$woke_richToSucceed[Data2$QWOKE_A6 == "Plut.t en d.saccord"] <- 0.33
CleanData$woke_richToSucceed[Data2$QWOKE_A6 == "Tout . fait en d.saccord"] <- 0
CleanData$woke_richToSucceed[Data2$QWOKE_A6 == "Je ne sais pas"] <- NA
table(CleanData$woke_richToSucceed)

# Au Québec, les citoyens manquent de liberté.
var_label(Data$QWOKE_A7)
table(Data$QWOKE_A7)
table(Data2$QWOKE_A7)
CleanData$woke_qcNeedsMoreLiberty <- NA
CleanData$woke_qcNeedsMoreLiberty[Data2$QWOKE_A7 == "Tout . fait d.accord"] <- 1
CleanData$woke_qcNeedsMoreLiberty[Data2$QWOKE_A7 == "Plut.t d.accord"] <- 0.66
CleanData$woke_qcNeedsMoreLiberty[Data2$QWOKE_A7 == "Plut.t en d.saccord"] <- 0.33
CleanData$woke_qcNeedsMoreLiberty[Data2$QWOKE_A7 == "Tout . fait en d.saccord"] <- 0
CleanData$woke_qcNeedsMoreLiberty[Data2$QWOKE_A7 == "Je ne sais pas"] <- NA
table(CleanData$woke_qcNeedsMoreLiberty)

# La pandémie a révélé le côté autoritaire du gouvernement du Québec.
var_label(Data$QWOKE_A8)
table(Data$QWOKE_A8)
table(Data2$QWOKE_A8)
CleanData$woke_covidShowAutoGouv <- NA
CleanData$woke_covidShowAutoGouv[Data2$QWOKE_A8 == "Tout . fait d.accord"] <- 1
CleanData$woke_covidShowAutoGouv[Data2$QWOKE_A8 == "Plut.t d.accord"] <- 0.66
CleanData$woke_covidShowAutoGouv[Data2$QWOKE_A8 == "Plut.t en d.saccord"] <- 0.33
CleanData$woke_covidShowAutoGouv[Data2$QWOKE_A8 == "Tout . fait en d.saccord"] <- 0
CleanData$woke_covidShowAutoGouv[Data2$QWOKE_A8 == "Je ne sais pas"] <- NA
table(CleanData$woke_covidShowAutoGouv)

# Quand on regarde toutes les mesures sanitaires qui ont trait à la COVID-19, on se croirait en dictature.
var_label(Data$QWOKE_A9)
table(Data$QWOKE_A9)
table(Data2$QWOKE_A9)
CleanData$woke_covidTimeIsDictatorship <- NA
CleanData$woke_covidTimeIsDictatorship[Data2$QWOKE_A9 == "Tout . fait d.accord"] <- 1
CleanData$woke_covidTimeIsDictatorship[Data2$QWOKE_A9 == "Plut.t d.accord"] <- 0.66
CleanData$woke_covidTimeIsDictatorship[Data2$QWOKE_A9 == "Plut.t en d.saccord"] <- 0.33
CleanData$woke_covidTimeIsDictatorship[Data2$QWOKE_A9 == "Tout . fait en d.saccord"] <- 0
CleanData$woke_covidTimeIsDictatorship[Data2$QWOKE_A9 == "Je ne sais pas"] <- NA
table(CleanData$woke_covidTimeIsDictatorship)

# Comme vous le savez peut-être, il y aura une élection provinciale en octobre 2022. 
# Laquelle des deux positions se rapproche le plus de la vôtre?"
var_label(Data$Q19)
table(Data$Q19)
table(Data2$Q19)
CleanData$iss_shouldChangeQcGov22 <- NA 
CleanData$iss_shouldChangeQcGov22[Data2$Q19 == "Il devrait y avoir un changement de gouvernement . Qu.bec"] <- 1
CleanData$iss_shouldChangeQcGov22[Data2$Q19 == "On devrait r..lire le m.me gouvernement . Qu.bec"] <- 0
table(CleanData$iss_shouldChangeQcGov22)

# Un groupe d'investisseurs montréalais, mené par Stephen Bronfman, 
# désire participer au redéveloppement du bassin Peel en y construisant 
# un nouveau stade de baseball et en y attirant une équipe qui y jouerait 
# la moitié de la saison.Êtes-vous en faveur ?
var_label(Data$FCCQ1)
table(Data$FCCQ1)
table(Data2$FCCQ1)
CleanData$wantsNewBallStadium <- NA
CleanData$wantsNewBallStadium[Data2$FCCQ1 == "Tr.s en faveur"] <- 1
CleanData$wantsNewBallStadium[Data2$FCCQ1 == "Assez en faveur"] <- 0.66
CleanData$wantsNewBallStadium[Data2$FCCQ1 == "Assez oppos.(e)"] <- 0.33
CleanData$wantsNewBallStadium[Data2$FCCQ1 == "Tr.s oppos.(e)"] <- 0
CleanData$wantsNewBallStadium[Data2$FCCQ1 == "Je ne sais pas"] <- NA
table(CleanData$wantsNewBallStadium)

# Quel est l'enjeu le plus important pour vous personnellement?
var_label(Data$C7)
table(Data$C7)
table(Data2$C7)

# Selon vous, les Québécois sont-ils plus ou moins racistes que les autres Canadiens?
var_label(Data$C8)
table(Data$C8)
table(Data2$C8)
CleanData$iss_qcMoreRacistCan <- NA
CleanData$iss_qcMoreRacistCan[Data2$C8 == "Beaucoup plus racistes"] <- 1
CleanData$iss_qcMoreRacistCan[Data2$C8 == "Un peu plus racistes"] <- 0.75
CleanData$iss_qcMoreRacistCan[Data2$C8 == "Ni plus ni moins racistes"] <- 0.5
CleanData$iss_qcMoreRacistCan[Data2$C8 == "Un peu moins racistes"] <- 0.25
CleanData$iss_qcMoreRacistCan[Data2$C8 == "Beaucoup moins racistes"] <- 0
table(CleanData$iss_qcMoreRacistCan)

# Selon vous, à quel point les Québécois sont-ils appréciés des autres Canadiens?
var_label(Data$C9)
table(Data$C9)
table(Data2$C9)
CleanData$iss_canAppreciateQc <- NA
CleanData$iss_canAppreciateQc[Data2$C9 == "Tr.s appr.ci.s"] <- 1
CleanData$iss_canAppreciateQc[Data2$C9 == "Assez appr.ci.s"] <- 0.75
CleanData$iss_canAppreciateQc[Data2$C9 == "Plus ou moins appr.ci.s"] <- 0.5
CleanData$iss_canAppreciateQc[Data2$C9 == "Peu appr.ci.s"] <- 0.25
CleanData$iss_canAppreciateQc[Data2$C9 == "Pas du tout appr.ci.s"] <- 0
table(CleanData$iss_canAppreciateQc)

# En politique, on parle souvent de gauche et de droite.
var_label(Data$C10_A1)
table(Data$C10_A1)
table(Data2$C10_A1)
CleanData$rightist <- Data$C10_A1/10

# Veuillez indiquer à quel point vous êtes en accord ou en désaccord avec 
# l'énoncé suivant : La plupart du temps, nous pouvons avoir confiance que 
# les gens du gouvernement feront ce qui est juste.
var_label(Data$C11)
table(Data$C11)
table(Data2$C11)
CleanData$trustGovPplJust <- NA
CleanData$trustGovPplJust[Data2$C11 == "Fortement en accord"] <- 1
CleanData$trustGovPplJust[Data2$C11 == "Plut.t en accord"] <- 0.75
CleanData$trustGovPplJust[Data2$C11 == "Neutre"] <- 0.5
CleanData$trustGovPplJust[Data2$C11 == "Plut.t en d.saccord"] <- 0.25
CleanData$trustGovPplJust[Data2$C11 == "Fortement d.saccord"] <- 0
table(CleanData$trustGovPplJust)

# En général, diriez-vous que votre santé mentale est
var_label(Data$SC1)
table(Data$SC1)
table(Data2$SC1)
CleanData$mentalHealthGood <- NA
CleanData$mentalHealthGood[Data2$SC1 == "Excellente"] <- 1
CleanData$mentalHealthGood[Data2$SC1 == "Tr.s bonne"] <- 0.75
CleanData$mentalHealthGood[Data2$SC1 == "Bonne"] <- 0.5
CleanData$mentalHealthGood[Data2$SC1 == "Passable"] <- 0.25
CleanData$mentalHealthGood[Data2$SC1 == "Mauvaise"] <- 0
table(CleanData$mentalHealthGood)

# Votre travail affecte-t-il votre santé psychologique?
var_label(Data$SC2)
table(Data$SC2)
table(Data2$SC2)
CleanData$workNegImpactMental <- NA
CleanData$workNegImpactMental[Data2$SC2 == "Oui, de fa.on tr.s n.gative"] <- 1
CleanData$workNegImpactMental[Data2$SC2 == "Oui, de fa.on n.gative"] <- 0.75
CleanData$workNegImpactMental[Data2$SC2 == "Non, mon travail n'affecte pas ma sant. psychologique"] <- 0.5
CleanData$workNegImpactMental[Data2$SC2 == "Oui, de fa.on positive"] <- 0.25
CleanData$workNegImpactMental[Data2$SC2 == "Oui, de fa.on tr.s positive"] <- 0
table(CleanData$workNegImpactMental)

#  Veuillez indiquer à quel point vous êtes en accord ou en désaccord avec les énoncés suivants. / 
# Le gouvernement devrait légiférer pour encadrer le droit à la déconnexion 
# (c.-à-d., le droit de ne pas être joignable en permanence par son employeur)
var_label(Data$SC3_A1)
table(Data$SC3_A1)
table(Data2$SC3_A1)
CleanData$gvtShouldDeconnect <- NA
CleanData$gvtShouldDeconnect[Data2$SC3_A1 == "Fortement en accord"] <- 1
CleanData$gvtShouldDeconnect[Data2$SC3_A1 == "Plut.t en accord"] <- 0.75
CleanData$gvtShouldDeconnect[Data2$SC3_A1 == "Ni en accord ni en d.saccord"] <- 0.5
CleanData$gvtShouldDeconnect[Data2$SC3_A1 == "Plut.t en d.saccord"] <- 0.25
CleanData$gvtShouldDeconnect[Data2$SC3_A1 == "Fortement en d.saccord"] <- 0
table(CleanData$gvtShouldDeconnect)

# Si je vivais un problème de santé mentale, je saurais quoi faire pour aller mieux.
var_label(Data$SC3_A2)
table(Data$SC3_A2)
table(Data2$SC3_A2)
CleanData$knowDoMtlHlthBad <- NA
CleanData$knowDoMtlHlthBad[Data2$SC3_A2 == "Fortement en accord"] <- 1
CleanData$knowDoMtlHlthBad[Data2$SC3_A2 == "Plut.t en accord"] <- 0.75
CleanData$knowDoMtlHlthBad[Data2$SC3_A2 == "Ni en accord ni en d.saccord"] <- 0.5
CleanData$knowDoMtlHlthBad[Data2$SC3_A2 == "Plut.t en d.saccord"] <- 0.25
CleanData$knowDoMtlHlthBad[Data2$SC3_A2 == "Fortement en d.saccord"] <- 0
table(CleanData$knowDoMtlHlthBad)

# Mon employeur offre suffisamment de services et d'avantages pour soutenir adéquatement ma santé mentale.
var_label(Data$SC4)
table(Data$SC4)
table(Data2$SC4)
CleanData$bossGiveSupportMtlHlth <- NA
CleanData$bossGiveSupportMtlHlth[Data2$SC3_A2 == "Fortement en accord"] <- 1
CleanData$bossGiveSupportMtlHlth[Data2$SC3_A2 == "Plut.t en accord"] <- 0.75
CleanData$bossGiveSupportMtlHlth[Data2$SC3_A2 == "Ni en accord ni en d.saccord"] <- 0.5
CleanData$bossGiveSupportMtlHlth[Data2$SC3_A2 == "Plut.t en d.saccord"] <- 0.25
CleanData$bossGiveSupportMtlHlth[Data2$SC3_A2 == "Fortement en d.saccord"] <- 0
table(CleanData$bossGiveSupportMtlHlth)

# Récemment, des influenceurs web en route vers le Mexique à partir du Québec 
# ont fait la fête à bord d'un avion. Des médias rapportent que les règles de 
# sécurité aérienne et les précautions liées à la COVID-19 n'auraient pas été 
# respectées par les pas... 
var_label(Data$SC5)
table(Data$SC5)
table(Data2$SC5)
CleanData$influencersCovidPunition <- Data2$SC5

# La langue française est en danger
var_label(Data$SEG_4)
table(Data$SEG_4)
table(Data2$SEG_4)
CleanData$iss_frenchInDanger <- NA 
CleanData$iss_frenchInDanger[Data2$SEG_4 == "Tout . fait d.accord"] <- 1
CleanData$iss_frenchInDanger[Data2$SEG_4 == "Plut.t d.accord"] <- 0.66
CleanData$iss_frenchInDanger[Data2$SEG_4 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_frenchInDanger[Data2$SEG_4 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_frenchInDanger)

# Les immigrants font bénéficier le Québec par leur travail, leur talent et leur culture.
var_label(Data$SEG_5)
table(Data$SEG_5)
table(Data2$SEG_5)
CleanData$iss_immigBenefitQc <- NA 
CleanData$iss_immigBenefitQc[Data2$SEG_5 == "Tout . fait d.accord"] <- 1
CleanData$iss_immigBenefitQc[Data2$SEG_5 == "Plut.t d.accord"] <- 0.66
CleanData$iss_immigBenefitQc[Data2$SEG_5 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_immigBenefitQc[Data2$SEG_5 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_immigBenefitQc)

#Les immigrants devraient adopter les valeurs et coutumes du Québec
var_label(Data$SEG_6)
table(Data$SEG_6)
table(Data2$SEG_6)
CleanData$iss_immigShouldAdapt <- NA 
CleanData$iss_immigShouldAdapt[Data2$SEG_6 == "Tout . fait d.accord"] <- 1
CleanData$iss_immigShouldAdapt[Data2$SEG_6 == "Plut.t d.accord"] <- 0.66
CleanData$iss_immigShouldAdapt[Data2$SEG_6 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_immigShouldAdapt[Data2$SEG_6 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_immigShouldAdapt)

# L’État québécois fait de manière générale un bon travail
var_label(Data$SEG_8)
table(Data$SEG_8)
table(Data2$SEG_8)
CleanData$iss_qcStateGoodJob <- NA 
CleanData$iss_qcStateGoodJob[Data2$SEG_8 == "Tout . fait d.accord"] <- 1
CleanData$iss_qcStateGoodJob[Data2$SEG_8 == "Plut.t d.accord"] <- 0.66
CleanData$iss_qcStateGoodJob[Data2$SEG_8 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_qcStateGoodJob[Data2$SEG_8 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_qcStateGoodJob)

# Il y a du racisme systémique au Québec
var_label(Data$SEG_9)
table(Data$SEG_9)
table(Data2$SEG_9)
CleanData$iss_sysRacismQc <- NA 
CleanData$iss_sysRacismQc[Data2$SEG_9 == "Tout . fait d.accord"] <- 1
CleanData$iss_sysRacismQc[Data2$SEG_9 == "Plut.t d.accord"] <- 0.66
CleanData$iss_sysRacismQc[Data2$SEG_9 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_sysRacismQc[Data2$SEG_9 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_sysRacismQc)

# Le gouvernement doit en faire plus pour protéger l’environnement, quitte à prôner la décroissance économique.
var_label(Data$SEG_10)
table(Data$SEG_10)
table(Data2$SEG_10)
CleanData$iss_gvtMoreEnv <- NA 
CleanData$iss_gvtMoreEnv[Data2$SEG_10 == "Tout . fait d.accord"] <- 1
CleanData$iss_gvtMoreEnv[Data2$SEG_10 == "Plut.t d.accord"] <- 0.66
CleanData$iss_gvtMoreEnv[Data2$SEG_10 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_gvtMoreEnv[Data2$SEG_10 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_gvtMoreEnv)

# Le gouvernement du Québec doit travailler à nous rendre autosuffisants
var_label(Data$SEG_11)
table(Data$SEG_11)
table(Data2$SEG_11)
CleanData$iss_gvtMoreEnv <- NA 
CleanData$iss_gvtMoreEnv[Data2$SEG_11 == "Tout . fait d.accord"] <- 1
CleanData$iss_gvtMoreEnv[Data2$SEG_11 == "Plut.t d.accord"] <- 0.66
CleanData$iss_gvtMoreEnv[Data2$SEG_11 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_gvtMoreEnv[Data2$SEG_11 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_gvtMoreEnv)

# Depuis le début de la pandémie, le gouvernement du Québec n’a pas respecté pas les 
# libertés individuelles des Québécois.
var_label(Data$SEG_14)
table(Data$SEG_14)
table(Data2$SEG_14)
CleanData$iss_covidGvtNoRespctLib <- NA 
CleanData$iss_covidGvtNoRespctLib[Data2$SEG_14 == "Tout . fait d.accord"] <- 1
CleanData$iss_covidGvtNoRespctLib[Data2$SEG_14 == "Plut.t d.accord"] <- 0.66
CleanData$iss_covidGvtNoRespctLib[Data2$SEG_14 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_covidGvtNoRespctLib[Data2$SEG_14 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_covidGvtNoRespctLib)

# Il y a des dynamiques de pouvoir au sein de la société québécoise qui favorise 
# les hommes blancs au détriment des femmes et des personnes racisées
var_label(Data$SEG_18)
table(Data$SEG_18)
table(Data2$SEG_18)
CleanData$iss_qcWhiteMenFav <- NA 
CleanData$iss_qcWhiteMenFav[Data2$SEG_18 == "Tout . fait d.accord"] <- 1
CleanData$iss_qcWhiteMenFav[Data2$SEG_18 == "Plut.t d.accord"] <- 0.66
CleanData$iss_qcWhiteMenFav[Data2$SEG_18 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_qcWhiteMenFav[Data2$SEG_18 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_qcWhiteMenFav)

# Au Québec, les citoyens manquent de liberté.
var_label(Data$SEG_20)
table(Data$SEG_20)
table(Data2$SEG_20)
CleanData$iss_qcCitNoLib <- NA 
CleanData$iss_qcCitNoLib[Data2$SEG_20 == "Tout . fait d.accord"] <- 1
CleanData$iss_qcCitNoLib[Data2$SEG_20 == "Plut.t d.accord"] <- 0.66
CleanData$iss_qcCitNoLib[Data2$SEG_20 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_qcCitNoLib[Data2$SEG_20 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_qcCitNoLib)

# Le gouvernement a profité de la situation sanitaire pour se prévaloir 
# de pouvoirs extraordinaires sans considérer l'opposition.
var_label(Data$SEG_21)
table(Data$SEG_21)
table(Data2$SEG_21)
CleanData$iss_covidGvtPowerNoOppo <- NA 
CleanData$iss_covidGvtPowerNoOppo[Data2$SEG_21 == "Tout . fait d.accord"] <- 1
CleanData$iss_covidGvtPowerNoOppo[Data2$SEG_21 == "Plut.t d.accord"] <- 0.66
CleanData$iss_covidGvtPowerNoOppo[Data2$SEG_21 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_covidGvtPowerNoOppo[Data2$SEG_21 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_covidGvtPowerNoOppo)

# Les excès de certains groupes, à gauche comme à droite, sont une menace à la paix sociale au Québec
var_label(Data$SEG_22)
table(Data$SEG_22)
table(Data2$SEG_22)
CleanData$iss_extremeIsDanger <- NA 
CleanData$iss_extremeIsDanger[Data2$SEG_22 == "Tout . fait d.accord"] <- 1
CleanData$iss_extremeIsDanger[Data2$SEG_22 == "Plut.t d.accord"] <- 0.66
CleanData$iss_extremeIsDanger[Data2$SEG_22 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_extremeIsDanger[Data2$SEG_22 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_extremeIsDanger)

# Au Québec, on ne valorise pas assez le travail, le respect et la discipline
var_label(Data$SEG_25)
table(Data$SEG_25)
table(Data2$SEG_25)
CleanData$iss_qcNoValueWork <- NA 
CleanData$iss_qcNoValueWork[Data2$SEG_25 == "Tout . fait d.accord"] <- 1
CleanData$iss_qcNoValueWork[Data2$SEG_25 == "Plut.t d.accord"] <- 0.66
CleanData$iss_qcNoValueWork[Data2$SEG_25 == "Plut.t en d.saccord"] <- 0.33
CleanData$iss_qcNoValueWork[Data2$SEG_25 == "Tout . fait en d.saccord"] <- 0
table(CleanData$iss_qcNoValueWork)


#***********************************#
####_____2. Weights  ____________####
#***********************************# 

CleanData$weight <- Data$POND

#***********************************#
####_____3. Exporting Data ______####
#***********************************#  

# Fast check on the data frame
fastT19 = sapply(CleanData, table)
fastT20 = sapply(CleanData, class)

# Setting today's date
today = Sys.Date()

# Exporting CleanData for future use
write.csv(CleanData, paste0("_SharedFolder-omnibus/questionnaires/January2022/data/Clean/omnibus1_", today, ".csv"))

#### END OF THE DOCUMENT ####






