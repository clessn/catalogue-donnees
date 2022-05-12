############## Source code for cleanning recurring variables ###############

create_CodeBook <- function(Data,DataLabel,path) {
  CodebookClean <- data.frame(names=names(Data))
  labelVec <- c()
  for (i in 1:ncol(Data)) {
    label <- attributes(Data[[i]])$label
    if (length(label)!=1){
      label <- "NA"
    }
    labelVec <- c(labelVec,label)
    print(i)
  }
  CodebookClean$label <- labelVec
  vec <- which(apply(Data,2,class)=="character")
  if (length(vec)>0) {
    Data[,vec] <- apply(Data[,vec],2, as.character)
    Data[,vec] <- apply(Data[,vec],2, as.numeric)
  }
  CodebookClean$class <- sapply(Data, class)
  CodebookClean$class <- as.character(CodebookClean$class)
  CodebookClean$values <- sapply(Data, table)
  CodebookClean$values <- as.character(CodebookClean$values)
  CodebookClean$valLabel <- sapply(DataLabel, table)
  CodebookClean$valLabel <- as.character(CodebookClean$valLabel)
  CodebookClean$max <- sapply(Data, max)
  CodebookClean$min <- sapply(Data, min)
  CodebookClean$mean <- round(sapply(Data, mean,na.rm=T),2)
  CodebookClean$sd <- round(sapply(Data, sd, na.rm=T),2)
  write_excel_csv(CodebookClean,path)
  return(CodebookClean)
}

thisMonthYear <- format(Sys.Date(),"%m_%y")
thisYear <- as.numeric(format(Sys.Date(),"%Y"))

#### 0.0 Codebook ####
CodeBook <- create_CodeBook(Data,Data2,path=paste0(pathToMonth,"codebook/codebookRaw",thisMonthYear,".csv"))

#### 0.1 Creating a clean empty dataframe ####
CleanData <- data.frame(year=seq(thisYear, thisYear, length=nrow(Data)))

#### 0.2 Year ####
CleanData$year <- seq(thisYear, thisYear,length=nrow(Data))

#### 0.3 Country ####
CleanData$country <- paste0("CAN")

#### 0.4 respondent id ####
CleanData$id <- NA
CleanData$id <- as.numeric(Data$QUEST)
if("{ID de fiche}" %in% names(Post)) {
  Post <- Post %>% rename("quest"='{ID de fiche}')
}
if("QUEST" %in% names(Post)){
  Post <- Post %>% rename("quest"='QUEST')
}
Post <- Post[which(Post$quest %in% CleanData$id),] %>%
  rename(id=quest,
         postalCode=A5) 
Post$id <- as.numeric(Post$id)

CleanData <- left_join(CleanData,Post)
CleanData$postalCode <- tolower(CleanData$postalCode)

toBeRemoved <- c("QUEST")

### Get ID in the original Data ###

Data$id <- CleanData$id

#**********************************************#
####_____1. SOCIO-DEMOGRAPHIC VARIABLES _____####
#**********************************************#

#### ~1.1 AGE ####
normAGE1 <- c("2","3","4","5","6","7","8")
normAGE2 <- c("18-24 ans","25-34 ans","35-44 ans","45-54 ans","55-64 ans","65-74","75 ans et plus")
if(all(names(table(Data$AGE))!=normAGE1) |
   all(names(table(Data2$AGE))!=normAGE2)){
  warning("Problem with variable AGE's composition!")
} else {
  CleanData$ses_age34m <- NA
  CleanData$ses_age34m[Data$AGE<=3] <- 1
  CleanData$ses_age34m[Data$AGE>3] <- 0
  
  CleanData$ses_age35p54 <- NA
  CleanData$ses_age35p54[Data$AGE==4|Data$AGE==5] <- 1
  CleanData$ses_age35p54[Data$AGE!=4&Data$AGE!=5&!is.na(Data$AGE)] <- 0
  
  CleanData$ses_age55p <- NA
  CleanData$ses_age55p[Data$AGE>=6] <- 1
  CleanData$ses_age55p[Data$AGE<6] <- 0
  
  CleanData$ses_ageAll <- Data2$AGE
  
  toBeRemoved <- c(toBeRemoved,"AGE")
}

#### ~1.2 SEXE ####
normSEXE1 <- c("1","2")
normSEXE2 <- c("Un homme","Une femme")
if(all(names(table(Data$SEXE))!=normSEXE1) |
   all(names(table(Data2$SEXE))!=normSEXE2)){
  warning("Problem with variable SEXE's composition!")
} else {
  CleanData$ses_female <- NA
  CleanData$ses_female[Data$SEXE==1]<- 0
  CleanData$ses_female[Data$SEXE==2]<- 1
  toBeRemoved <- c(toBeRemoved,"SEXE")
}


#### ~1.3 REGION ####
normREGION_1 <- c("1","2","3","4")
normREGION_2 <- c("L..le de Montr.al","La grande r.gion de Montr.al (en dehors de l..le)","La grande r.gion de Qu.bec",                     
                  "Hors des grandes r.gions de Montr.al ou de Qu.bec")
if(all(names(table(Data$REGION))!=normREGION_1) |
   all(names(table(Data2$REGION))!=normREGION_2)){
  warning("Problem with variable REGION's composition!")
} else {
  CleanData$ses_mtlIsle <- NA
  CleanData$ses_mtlIsle[Data$REGION==1] <- 1
  CleanData$ses_mtlIsle[Data$REGION!=1&!is.na(Data$REGION)] <- 0
  
  CleanData$ses_mtl <- NA
  CleanData$ses_mtl[Data$REGION==2] <- 1
  CleanData$ses_mtl[Data$REGION!=2&!is.na(Data$REGION)] <- 0
  
  CleanData$ses_qcCity <- NA
  CleanData$ses_qcCity[Data$REGION==3] <- 1
  CleanData$ses_qcCity[Data$REGION!=3&!is.na(Data$REGION)] <- 0
  
  CleanData$ses_noMtlQc <- NA
  CleanData$ses_noMtlQc[Data$REGION==4] <- 1
  CleanData$ses_noMtlQc[Data$REGION!=4] <- 0
  toBeRemoved <- c(toBeRemoved,"REGION")
}


#### ~1.4 LANGUE ####
normLANGUE_1 <- c("1","2","3")
normLANGUE_2 <- c("Le fran.ais","L'anglais","Autre")
if(all(names(table(Data$LANGUE))!=normLANGUE_1) |
   all(names(table(Data2$LANGUE))!=normLANGUE_2)){
  warning("Problem with variable LANGUE's composition!")
} else {
  CleanData$ses_langFr <- NA
  CleanData$ses_langFr[Data$LANGUE==1] <- 1
  CleanData$ses_langFr[Data$LANGUE!=1&!is.na(Data$LANGUE)] <- 0
  
  CleanData$ses_langEn <- NA
  CleanData$ses_langEn[Data$LANGUE==2] <- 1
  CleanData$ses_langEn[Data$LANGUE!=2&!is.na(Data$LANGUE)] <- 0
  
  CleanData$ses_langOthr <- NA
  CleanData$ses_langOthr[Data$LANGUE==3] <- 1
  CleanData$ses_langOthr[Data$LANGUE!=3&!is.na(Data$LANGUE)] <- 0
  toBeRemoved <- c(toBeRemoved,"LANGUE")
}

#### ~1.5 S2 EDUC ####
normS2_1 <- c("1","2","3")
normS2_2 <- c(".cole primaire ou secondaire","Formation technique/coll.giale/C.GEP","Dipl.me universitaire")
if(all(names(table(Data$S2))!=normS2_1) |
   all(names(table(Data2$S2))!=normS2_2)){
  warning("Problem with variable S2's composition!")
} else {
  CleanData$ses_educHsOrBelow <- NA
  CleanData$ses_educHsOrBelow[Data$S2==1] <- 1
  CleanData$ses_educHsOrBelow[Data$S2!=1 & !is.na(Data$S2)] <- 0
  
  CleanData$ses_educColl <- NA
  CleanData$ses_educColl[Data$S2==2] <- 1
  CleanData$ses_educColl[Data$S2!=2 & !is.na(Data$S2)] <- 0
  
  CleanData$ses_educUniv <- NA
  CleanData$ses_educUniv[Data$S2==3] <- 1
  CleanData$ses_educUniv[Data$S2!=3 & !is.na(Data$S2)] <- 0
  toBeRemoved <- c(toBeRemoved,"S2")
}


#### ~1.6 S3 EMPLOYEMENT ####

normS3_1 <- c("1","2","3","4","5","6","7")
normS3_2 <- c("Emploi r.mun.r.","Travailleur autonome",".tudiant","Retrait.",                      
              "Ch.meur . la recherche d'emploi","Sans emploi","Autre")
if(all(names(table(Data$S3))!=normS3_1) |
   all(names(table(Data2$S3))!=normS3_2)){
  warning("Problem with variable S3's composition!")
} else {
  CleanData$ses_isEmployed <- NA
  CleanData$ses_isEmployed[Data$S3==1] <- 1
  CleanData$ses_isEmployed[Data$S3!=1 & !is.na(Data$S3)] <- 0
  CleanData$ses_isEmployed[Data$S3==7] <- NA
  
  CleanData$ses_isSelfEmployed <- NA
  CleanData$ses_isSelfEmployed[Data$S3==2] <- 1
  CleanData$ses_isSelfEmployed[Data$S3!=2 & !is.na(Data$S3)] <- 0
  CleanData$ses_isSelfEmployed[Data$S3==7] <- NA
  
  CleanData$ses_isStudent <- NA
  CleanData$ses_isStudent[Data$S3==3] <- 1
  CleanData$ses_isStudent[Data$S3!=3 & !is.na(Data$S3)] <- 0
  CleanData$ses_isStudent[Data$S3==7] <- NA
  
  CleanData$ses_isRetired <- NA
  CleanData$ses_isRetired[Data$S3==4] <- 1
  CleanData$ses_isRetired[Data$S3!=4 & !is.na(Data$S3)] <- 0
  CleanData$ses_isRetired[Data$S3==7] <- NA
  
  CleanData$ses_isUnemployed <- NA
  CleanData$ses_isUnemployed[Data$S3 %in% c(5,6)] <- 1
  CleanData$ses_isUnemployed[Data$S3!=5 & Data$S3!=6 & !is.na(Data$S3)] <- 0
  CleanData$ses_isUnemployed[Data$S3==7] <- NA
  
  CleanData$ses_workingStatus <- NA
  CleanData$ses_workingStatus[Data$S3==1] <- "Employed"
  CleanData$ses_workingStatus[Data$S3==2] <- "Self-employed"
  CleanData$ses_workingStatus[Data$S3==3] <- "Student"
  CleanData$ses_workingStatus[Data$S3==4] <- "Retired"
  CleanData$ses_workingStatus[Data$S3==5] <- "Unemployed, trying to find work"
  CleanData$ses_workingStatus[Data$S3==6] <- "Unemployed"
  CleanData$ses_workingStatus[Data$S3==7] <- "Other"
  toBeRemoved <- c(toBeRemoved,"S3")
}

#### ~1.7 S4A SYNDICATED ####
normS4A_1 <- c("1","2")
normS4A_2 <- c("Syndiqu.","Non syndiqu.")
if(all(names(table(Data$S4A))!=normS4A_1) |
   all(names(table(Data2$S4A))!=normS4A_2)){
  warning("Problem with variable S4A's composition!")
} else {
  CleanData$ses_isSyndicated <- NA
  CleanData$ses_isSyndicated[Data$S4A==1] <- 1
  CleanData$ses_isSyndicated[Data$S4A!=1 & !is.na(Data$S4A)] <- 0
  toBeRemoved <- c(toBeRemoved,"S4A")
}


#### ~1.8 S5 PROPRIO ####
normS5_1 <- c("1","2")
normS5_2 <- c("Propri.taire","Locataire")
if(all(names(table(Data$S5))!=normS5_1) |
   all(names(table(Data2$S5))!=normS5_2)){
  warning("Problem with variable S5's composition!")
} else {
  CleanData$ses_isOwner <- NA
  CleanData$ses_isOwner[Data$S5==1] <- 1
  CleanData$ses_isOwner[Data$S5!=1 & !is.na(Data$S5)] <- 0
  toBeRemoved <- c(toBeRemoved,"S5")
}


#### ~1.9 S6 MATRIMONIAL STATUS ####
normS6_1 <- c("1","2","3","4","5")
normS6_2 <- c("En couple sans enfants","En couple avec des enfants","Seul sans enfants","Seul avec des enfants","Autre")
if(all(names(table(Data$S6))!=normS6_1) |
   all(names(table(Data2$S6))!=normS6_2)){
  warning("Problem with variable S6's composition!")
} else {
  CleanData$ses_inRelatnship <- NA
  CleanData$ses_inRelatnship[Data$S6 %in% c(1,2)] <- 1
  CleanData$ses_inRelatnship[Data$S6!=1 & Data$S6!=2 & !is.na(Data$S6)] <- 0
  CleanData$ses_inRelatnship[Data$S6==5] <- NA
  
  CleanData$ses_hasKids <- NA
  CleanData$ses_hasKids[Data$S6 %in% c(2,4)] <- 1
  CleanData$ses_hasKids[Data$S6!=2 & Data$S6!=4 & !is.na(Data$S6)] <- 0
  CleanData$ses_hasKids[Data$S6==5] <- NA
  toBeRemoved <- c(toBeRemoved,"S6")
}

#### ~1.10 S7 INCOME ####
normS7_1 <- c("1","2","3","4","5","6","7","8","9")
normS7_2 <- c("Moins de 20 000 $","20 000 $ . 39 999 $","40 000 $ . 59 999 $","60 000 $ . 79 999 $","80 000 $ . 99 999$",      
              "100 000 $ . 119 999 $","120 000 $ . 139 999 $","140 000 $ et plus","Je pr.f.re ne pas r.pondre")
if(all(names(table(Data$S7))!=normS7_1) |
   all(names(table(Data2$S7))!=normS7_2)){
  warning("Problem with variable S7's composition!")
} else {
  CleanData$ses_incomeLow <- NA
  CleanData$ses_incomeLow[Data$S7<=3] <- 1
  CleanData$ses_incomeLow[Data$S7>3] <- 0
  CleanData$ses_incomeLow[Data$S7==9] <- NA
  
  CleanData$ses_incomeMid <- NA
  CleanData$ses_incomeMid[Data$S7>3 & Data$S7<=6] <- 1
  CleanData$ses_incomeMid[Data$S7<=3 | Data$S7>6] <- 0
  CleanData$ses_incomeMid[Data$S7==9] <- NA
  
  CleanData$ses_incomeHigh <- NA
  CleanData$ses_incomeHigh[Data$S7>6] <- 1
  CleanData$ses_incomeHigh[Data$S7<=6] <- 0
  CleanData$ses_incomeHigh[Data$S7==9] <- NA
  
  CleanData$ses_incomeAll <- Data2$S7 
  toBeRemoved <- c(toBeRemoved,"S7")
}

#### ~1.11 C1 RELIGION ####
normREL1 <- c("1","2","3","4","5","6","7","8","9","10")
normREL2 <- c("Aucune/Ath.isme","Agnosticisme","Bouddhisme","Hindouisme",
              "Juda.sme","Islam","Sikhisme","Catholicisme","Protestantisme",
              "Orthodoxe","Autre")
if(all(names(table(Data$C1))!=normREL1) |
   all(names(table(Data2$C1))!=normREL2)){
  warning("Problem with variable C1's composition!")
} else {
  CleanData$ses_isReligious <- NA
  CleanData$ses_isReligious[Data$C1 != 1 & !is.na(Data$C1)] <- 1
  CleanData$ses_isReligious[Data$C1 == 1] <- 0
  
  CleanData$ses_isCatho <- NA
  CleanData$ses_isCatho[Data$C1 == 8] <- 1
  CleanData$ses_isCatho[Data$C1 != 8 & !is.na(Data$C1)] <- 0
  
  CleanData$ses_religion <- NA
  CleanData$ses_religion[Data$C1 == 1] <- "Athéisme/Aucune"
  CleanData$ses_religion[Data$C1 == 2] <- "Agnosticisme"
  CleanData$ses_religion[Data$C1 == 3] <- "Bouddhisme"
  CleanData$ses_religion[Data$C1 == 4] <- "Hindouisme"
  CleanData$ses_religion[Data$C1 == 5] <- "Judaïsme"
  CleanData$ses_religion[Data$C1 == 6] <- "Islam"
  CleanData$ses_religion[Data$C1 == 7] <- "Sikhisme"
  CleanData$ses_religion[Data$C1 == 8] <- "Catholicisme"
  CleanData$ses_religion[Data$C1 == 9] <- "Protestantisme"
  CleanData$ses_religion[Data$C1 == 10] <- "Orthodoxe"
  CleanData$ses_religion[Data$C1 == 11] <- "Autre"
  toBeRemoved <- c(toBeRemoved,"C1")
}

normRELIGIOSITY1 <- c("0","1","2","3","4","5","6","7","8","9","10")
normRELIGIOSITY2 <- c("0 - Aucune importance","1","2","3","4","5","6","7","8","9","10 - Grande importance")
if(all(names(table(Data$C4_A1))!=normRELIGIOSITY1) |
   all(names(table(Data2$C4_A1))!=normRELIGIOSITY1)){
  warning("Problem with variable C4_A1's composition!")
} else {
  CleanData$ses_religiosity <- Data$C4_A1/10
  toBeRemoved <- c(toBeRemoved,"C4_A1")
}

#**********************************************#
####_____2. Pessimism Scale _____####
#**********************************************#

#### ~2.1 C5_A1  Sur une échelle de 0 à 10, comment est votre humeur ces temps-ci? ####
normC5_A1_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
normC5_A1_2 <- c("0 - pire humeur","1","2","3","4","5","6","7","8","9","10 - meilleure humeur")
if(all(names(table(Data$C5_A1))!=normC5_A1_1) |
   all(names(table(Data2$C5_A1))!=normC5_A1_2)){
  warning("Problem with variable C5_A1's composition!")
} else {
  CleanData$pess_goodMood <- NA
  CleanData$pess_goodMood <- Data$C5_A1/10
  
  CleanData$pess_badMood <- NA
  CleanData$pess_badMood <- 1-(Data$C5_A1/10)
  toBeRemoved <- c(toBeRemoved,"C5_A1")
}


#### ~2.2 C6_A1  L'avenir s'annonce sombre ####
normC6_A1_1 <- c("1","2","3","4","5")
normC6_A1_2 <- c("Fortement en accord","Plut.t en accord","Neutre","Plut.t en d.saccord","Fortement en d.saccord")
if(all(names(table(Data$C6_A1))!=normC6_A1_1) |
   all(names(table(Data2$C6_A1))!=normC6_A1_2)){
  warning("Problem with variable C6_A1's composition!")
} else {
  CleanData$pess_futureLooksBleak <- NA
  CleanData$pess_futureLooksBleak[Data$C6_A1==1] <- 1
  CleanData$pess_futureLooksBleak[Data$C6_A1==2] <- 0.75
  CleanData$pess_futureLooksBleak[Data$C6_A1==3] <- 0.5
  CleanData$pess_futureLooksBleak[Data$C6_A1==4] <- 0.25
  CleanData$pess_futureLooksBleak[Data$C6_A1==5] <- 0
  toBeRemoved <- c(toBeRemoved,"C6_A1")
}


#### ~2.3 C6_A2 L'humanité est mise en danger par des catastrophes à venir ####
normC6_A2_1 <- c("1","2","3","4","5")
normC6_A2_2 <- c("Fortement en accord","Plut.t en accord","Neutre","Plut.t en d.saccord","Fortement en d.saccord")
if(all(names(table(Data$C6_A2))!=normC6_A2_1) |
   all(names(table(Data2$C6_A2))!=normC6_A2_2)){
  warning("Problem with variable C6_A2's composition!")
} else {
  CleanData$pess_humanInDanger <- NA
  CleanData$pess_humanInDanger[Data$C6_A2==1] <- 1
  CleanData$pess_humanInDanger[Data$C6_A2==2] <- 0.75
  CleanData$pess_humanInDanger[Data$C6_A2==3] <- 0.5
  CleanData$pess_humanInDanger[Data$C6_A2==4] <- 0.25
  CleanData$pess_humanInDanger[Data$C6_A2==5] <- 0
  toBeRemoved <- c(toBeRemoved,"C6_A2")
}















#normVOTE1_1 <- c("1","2","3","4","5","6","7")
#normVOTE1_2 <- c("La CAQ, la Coalition pour l.avenir du Qu.bec de Fran.ois Leg","Le PQ, le Parti qu.b.cois de Paul St-Pierre Plamondon",      
#                 "Le PLQ, le Parti lib.ral du Qu.bec de Dominique Anglade","QS, Qu.bec solidaire de Gabriel Nadeau-Dubois",               
#                 "Le PCQ, le Parti Conservateur du Qu.bec d'.ric Duhaime","Un autre parti","IND.CIS DISCRET")
#if(all(names(table(Data$VOTE1))!=normVOTE1_1) |
#   all(names(table(Data2$VOTE1))!=normVOTE1_2)){
#  stop("Problem with variable VOTE1's composition!\nScript did NOT end!")
#}
#normVOTE2_1 <- c("1","2","3","4","5","6")
#normVOTE2_2 <- c("La CAQ, la Coalition pour l.avenir du Qu.bec de Fran.ois Leg","Le PQ, le Parti qu.b.cois de Paul St-Pierre Plamondon",      
#                 "Le PLQ, le Parti lib.ral du Qu.bec de Dominique Anglade","QS, Qu.bec solidaire de Gabriel Nadeau-Dubois",               
#                 "Le PCQ, le Parti Conservateur du Qu.bec d'.ric Duhaime","Un autre parti")
#if(all(names(table(Data$VOTE2))!=normVOTE2_1) |
#   all(names(table(Data2$VOTE2))!=normVOTE2_2)){
#  stop("Problem with variable VOTE2's composition!\nScript did NOT end!")
#}
#normVOTE3_1 <- c("1","2","3","4","5","6","7","8")
#normVOTE3_2 <- c("Parti lib.ral du Canada (PLC) de Justin Trudeau"             ,"Parti conservateur du Canda (PCC) de Erin O.Toole",
#                 "Nouveau Parti d.mocratique du Canada (NPD) de Jagmeet Singh" ,"Bloc qu.b.cois (BQ) d'Yves-Fran.ois Blanchet",
#                 "Parti populaire du Canada (PPC) de Maxime Bernier"           ,"Parti vert du Canada" ,        
#                 "Un autre parti"                                              ,"IND.CIS DISCRET")
#if(all(names(table(Data$VOTE3))!=normVOTE3_1) |
#   all(names(table(Data2$VOTE3))!=normVOTE3_2)){
#  stop("Problem with variable VOTE3's composition!\nScript did NOT end!")
#}
#normVOTE4_1 <- c("1","2","3","4","5","6","7")
#normVOTE4_2 <- c("Parti lib.ral du Canada (PLC) de Justin Trudeau"             ,"Parti conservateur du Canda (PCC) de Erin O.Toole",
#                 "Nouveau Parti d.mocratique du Canada (NPD) de Jagmeet Singh" ,"Bloc qu.b.cois (BQ) d'Yves-Fran.ois Blanchet",
#                 "Parti populaire du Canada (PPC) de Maxime Bernier"           ,"Parti vert du Canada" ,        
#                 "Un autre parti")
#if(all(names(table(Data$VOTE4))!=normVOTE4_1) |
#   all(names(table(Data2$VOTE4))!=normVOTE4_2)){
#  stop("Problem with variable VOTE4's composition!\nScript did NOT end!")
#}

## Q1 Pour débuter, de façon générale, diriez-vous qu’au Québec les choses vont dans la bonne direction ou dans la mauvaise direction?
normQ1_1 <- c("1","2")
normQ1_2 <- c("Dans la bonne direction","Dans la mauvaise direction")
if(all(names(table(Data$Q1))!=normQ1_1) |
   all(names(table(Data2$Q1))!=normQ1_2)){
  warning("Problem with variable Q1's composition!")
} else {
CleanData$iss_quebRightDirect <- NA
CleanData$iss_quebRightDirect[Data$Q1==1] <- 1
CleanData$iss_quebRightDirect[Data$Q1!=1 & !is.na(Data$Q1)] <- 0
toBeRemoved <- c(toBeRemoved, "Q1")
}



# Hubert DONT FUCKING TOUCH ####
## Q2 S’il y avait une élection générale provinciale au Québec aujourd’hui, pour quel parti voteriez-vous ou seriez-vous tenté(e) de voter pour vous représenter à l'Assemblée nationale du Québec?"
normQ2_1 <- c("1","2", "3", "4", "5", "6", "7", "8")
normQ2_2 <- c("La CAQ, la Coalition pour l.avenir du Qu.bec de Fran.ois Leg",
              "Le PQ, le Parti qu.b.cois de Paul St-Pierre Plamondon",
              "Le PLQ, le Parti lib.ral du Qu.bec de Dominique Anglade",
              "QS, Qu.bec solidaire de Gabriel Nadeau-Dubois",
              "Un autre parti",
              "Je ne sais pas",
              "Je n.irais pas voter",
              "Le PCQ, le Parti Conservateur du Qu.bec d'.ric Duhaime"
)
if(all(names(table(Data$Q2))!=normQ2_1) |
   all(names(table(Data2$Q2))!=normQ2_2)){
  warning("Problem with variable Q2's composition!")
} else {
  CleanData$voteIntCAQ <- NA
  CleanData$voteIntCAQ[Data$Q2==1] <- 1
  CleanData$voteIntCAQ[Data$Q2!=1 & !is.na(Data$Q2)] <- 0
  CleanData$voteIntCAQ[Data$Q2 %in% c(7,6,5)] <- NA
  CleanData$voteIntPQ <- NA
  CleanData$voteIntPQ[Data$Q2==2] <- 1
  CleanData$voteIntPQ[Data$Q2!=2 & !is.na(Data$Q2)] <- 0
  CleanData$voteIntPQ[Data$Q2 %in% c(7,6,5)] <- NA
  CleanData$voteIntPLQ <- NA
  CleanData$voteIntPLQ[Data$Q2==3] <- 1
  CleanData$voteIntPLQ[Data$Q2!=3 & !is.na(Data$Q2)] <- 0
  CleanData$voteIntPLQ[Data$Q2 %in% c(7,6,5)] <- NA
  CleanData$voteIntQS <- NA
  CleanData$voteIntQS[Data$Q2==4] <- 1
  CleanData$voteIntQS[Data$Q2!=4 & !is.na(Data$Q2)] <- 0
  CleanData$voteIntQS[Data$Q2 %in% c(7,6,5)] <- NA
  CleanData$voteIntPCQ <- NA
  CleanData$voteIntPCQ[Data$Q2==8] <- 1
  CleanData$voteIntPCQ[Data$Q2!=8 & !is.na(Data$Q2)] <- 0
  CleanData$voteIntPCQ[Data$Q2 %in% c(7,6,5)] <- NA
  toBeRemoved <- c(toBeRemoved, "Q2")
}

## "Q3)  Peut-être que votre choix n'est pas définitif et que vous pourriez changer d'avis d'ici l’élection, mais aujourd'hui, quel parti ou chef seriez-vous tenté(e) d'appuyer? / Je ne sais pas + Je n’irais pas voter
normQ3_1 <- c("1","2", "3", "4", "5", "6", "7", "8")
normQ3_2 <- c("La CAQ, la Coalition pour l.avenir du Qu.bec de Fran.ois Leg",
              "Le PQ, le Parti qu.b.cois de Paul St-Pierre Plamondon",
              "Le PLQ, le Parti lib.ral du Qu.bec de Dominique Anglade",
              "QS, Qu.bec solidaire de Gabriel Nadeau-Dubois",
              "Un autre parti",
              "Je ne sais pas",
              "Je n.irais pas voter",
              "Le PCQ, le Parti Conservateur du Qu.bec d'.ric Duhaime"
)
if(all(names(table(Data$Q3))!=normQ3_1) |
   all(names(table(Data2$Q3))!=normQ3_2)){
  warning("Problem with variable Q3's composition!")
} else {
  CleanData$voteIntCAQ[Data$Q3==1] <- 1
  CleanData$voteIntPQ[Data$Q3==2] <- 1
  CleanData$voteIntPLQ[Data$Q3==3] <- 1
  CleanData$voteIntQS[Data$Q3==4] <- 1
  CleanData$voteIntPCQ[Data$Q3==8] <- 1
  toBeRemoved <- c(toBeRemoved, "Q3")
}


## "Q3B)  Quel serait votre second choix?"
normQ3B_1 <- c("1","2", "3", "4", "5", "6", "7", "8")
normQ3B_2 <- c("La CAQ, la Coalition pour l.avenir du Qu.bec de Fran.ois Leg",
              "Le PQ, le Parti qu.b.cois de Paul St-Pierre Plamondon",
              "Le PLQ, le Parti lib.ral du Qu.bec de Dominique Anglade",
              "QS, Qu.bec solidaire de Gabriel Nadeau-Dubois",
              "Un autre parti",
              "Je ne sais pas",
              "Je n.irais pas voter",
              "Le PCQ, le Parti Conservateur du Qu.bec d'.ric Duhaime"
)
if(all(names(table(Data$Q3B))!=normQ3B_1) |
   all(names(table(Data2$Q3B))!=normQ3B_2)){
  warning("Problem with variable Q3B's composition!")
} else {
  CleanData$voteIntCAQ2 <- NA
  CleanData$voteIntCAQ2[Data$Q3B==1] <- 1
  CleanData$voteIntCAQ2[Data$Q3B!=1 & !is.na(Data$Q3B)] <- 0
  CleanData$voteIntCAQ2[Data$Q3B %in% c(7,6,5)] <- NA
  CleanData$voteIntPQ2 <- NA
  CleanData$voteIntPQ2[Data$Q3B==2] <- 1
  CleanData$voteIntPQ2[Data$Q3B!=2 & !is.na(Data$Q3B)] <- 0
  CleanData$voteIntPQ2[Data$Q3B %in% c(7,6,5)] <- NA
  CleanData$voteIntPLQ2 <- NA
  CleanData$voteIntPLQ2[Data$Q3B==3] <- 1
  CleanData$voteIntPLQ2[Data$Q3B!=3 & !is.na(Data$Q3B)] <- 0
  CleanData$voteIntPLQ2[Data$Q3B %in% c(7,6,5)] <- NA
  CleanData$voteIntQS2 <- NA
  CleanData$voteIntQS2[Data$Q2==4] <- 1
  CleanData$voteIntQS2[Data$Q2!=4 & !is.na(Data$Q3B)] <- 0
  CleanData$voteIntQS2[Data$Q2 %in% c(7,6,5)] <- NA
  CleanData$voteIntPCQ2 <- NA
  CleanData$voteIntPCQ2[Data$Q3B==8] <- 1
  CleanData$voteIntPCQ2[Data$Q3B!=8 & !is.na(Data$Q3B)] <- 0
  CleanData$voteIntPCQ2[Data$Q3B %in% c(7,6,5)] <- NA
  toBeRemoved <- c(toBeRemoved, "Q3B")
}

## "Q3C_A1)  Est-il possible que vous appuyiez ce parti ou êtes-vous certain(e) que vous ne voterez pas pour lui? / La CAQ, la Coalition pour l'avenir du Québec de François Legault"
normQ3C_A1_1 <- c("1","2")
normQ3C_A1_2 <- c("Pourrais appuyer","Ne voterai pas pour ce parti")
if(all(names(table(Data$Q3C_A1))!=normQ3C_A1_1) |
   all(names(table(Data2$Q3C_A1))!=normQ3C_A1_2)){
  warning("Problem with variable Q3C_A1's composition!")
} else {
  CleanData$couldVoteCAQ <- NA
  CleanData$couldVoteCAQ[Data$Q3C_A1==1] <- 1
  CleanData$couldVoteCAQ[Data$Q3C_A1!=1 & !is.na(Data$Q3C_A1)] <- 0
  toBeRemoved <- c(toBeRemoved, "Q3C_A1")
}

## Q3C_A2)  Est-il possible que vous appuyiez ce parti ou êtes-vous certain(e) que vous ne voterez pas pour lui? / Le PQ, le Parti québécois de Paul St-Pierre Plamondon
normQ3C_A2_1 <- c("1","2")
normQ3C_A2_2 <- c("Pourrais appuyer","Ne voterai pas pour ce parti")
if(all(names(table(Data$Q3C_A2))!=normQ3C_A2_1) |
   all(names(table(Data2$Q3C_A2))!=normQ3C_A2_2)){
  warning("Problem with variable Q3C_A2's composition!")
} else {
  CleanData$couldVotePQ <- NA
  CleanData$couldVotePQ[Data$Q3C_A2==1] <- 1
  CleanData$couldVotePQ[Data$Q3C_A2!=1 & !is.na(Data$Q3C_A2)] <- 0
  toBeRemoved <- c(toBeRemoved, "Q3C_A2")
}

## Q3C_A3)  Est-il possible que vous appuyiez ce parti ou êtes-vous certain(e) que vous ne voterez pas pour lui? / Le PLQ, le Parti libéral du Québec de Dominique Anglade
normQ3C_A3_1 <- c("1","2")
normQ3C_A3_2 <- c("Pourrais appuyer","Ne voterai pas pour ce parti")
if(all(names(table(Data$Q3C_A3))!=normQ3C_A3_1) |
   all(names(table(Data2$Q3C_A3))!=normQ3C_A3_2)){
  warning("Problem with variable Q3C_A3's composition!")
} else {
  CleanData$couldVotePLQ <- NA
  CleanData$couldVotePLQ[Data$Q3C_A3==1] <- 1
  CleanData$couldVotePLQ[Data$Q3C_A3!=1 & !is.na(Data$Q3C_A3)] <- 0
  toBeRemoved <- c(toBeRemoved, "Q3C_A3")
}

## Q3C_A4)  Est-il possible que vous appuyiez ce parti ou êtes-vous certain(e) que vous ne voterez pas pour lui? / QS, Québec solidaire de Gabriel Nadeau-Dubois
normQ3C_A4_1 <- c("1","2")
normQ3C_A4_2 <- c("Pourrais appuyer","Ne voterai pas pour ce parti")
if(all(names(table(Data$Q3C_A4))!=normQ3C_A4_1) |
   all(names(table(Data2$Q3C_A4))!=normQ3C_A4_2)){
  warning("Problem with variable Q3C_A4's composition!")
} else {
  CleanData$couldVoteQS <- NA
  CleanData$couldVoteQS[Data$Q3C_A4==1] <- 1
  CleanData$couldVoteQS[Data$Q3C_A4!=1 & !is.na(Data$Q3C_A4)] <- 0
  toBeRemoved <- c(toBeRemoved, "Q3C_A4")
}

## Q3C_A5)  Est-il possible que vous appuyiez ce parti ou êtes-vous certain(e) que vous ne voterez pas pour lui? / Le PCQ, le Parti Conservateur du Québec de Éric Duhaime
normQ3C_A5_1 <- c("1","2")
normQ3C_A5_2 <- c("Pourrais appuyer","Ne voterai pas pour ce parti")
if(all(names(table(Data$Q3C_A5))!=normQ3C_A5_1) |
   all(names(table(Data2$Q3C_A5))!=normQ3C_A5_2)){
  warning("Problem with variable Q3C_A5's composition!")
} else {
  CleanData$couldVotePCQ <- NA
  CleanData$couldVotePCQ[Data$Q3C_A5==1] <- 1
  CleanData$couldVotePCQ[Data$Q3C_A5!=1 & !is.na(Data$Q3C_A5)] <- 0
  toBeRemoved <- c(toBeRemoved, "Q3C_A5")
}


## Q4)  Quel chef, parmi les suivants, ferait le meilleur premier ministre pour le Québec?
normQ4_1 <- c("1","2", "3", "4", "5", "6", "7")
normQ4_2 <- c("Fran.ois Legault, chef de la Coalition avenir Qu.bec (CAQ)",
              "Paul St-Pierre Plamondon, chef du Parti qu.b.cois (PQ)",
              "Dominique Anglade, cheffe du Parti lib.ral du Qu.bec (PLQ)",
              "Gabriel Nadeau-Dubois, porte-parole de Qu.bec solidaire (QS)",
              ".ric Duhaime, chef du parti Conservateur du Qu.bec (PCQ)",
              "Autre",
              "Aucun")
if(all(names(table(Data$Q4))!=normQ4_1) |
   all(names(table(Data2$Q4))!=normQ4_2)){
  warning("Problem with variable Q4's composition!")
} else {
  CleanData$bestPmLegault <- NA
  CleanData$bestPmLegault[Data$Q4==1] <- 1
  CleanData$bestPmLegault[Data$Q4!=1 & !is.na(Data$Q4)] <- 0
  CleanData$bestPmLegault[Data$Q4 %in% c(7,6)] <- NA
  CleanData$bestPmPSPP <- NA
  CleanData$bestPmPSPP[Data$Q4==2] <- 1
  CleanData$bestPmPSPP[Data$Q4!=2 & !is.na(Data$Q4)] <- 0
  CleanData$bestPmPSPP[Data$Q4 %in% c(7,6)] <- NA
  CleanData$bestPmAnglade <- NA
  CleanData$bestPmAnglade[Data$Q4==3] <- 1
  CleanData$bestPmAnglade[Data$Q4!=3 & !is.na(Data$Q4)] <- 0
  CleanData$bestPmAnglade[Data$Q4 %in% c(7,6)] <- NA
  CleanData$bestPmGND <- NA
  CleanData$bestPmGND[Data$Q4==4] <- 1
  CleanData$bestPmGND[Data$Q4!=4 & !is.na(Data$Q4)] <- 0
  CleanData$bestPmGND[Data$Q4 %in% c(7,6)] <- NA
  CleanData$bestPmDuhaime <- NA
  CleanData$bestPmDuhaime[Data$Q4==5] <- 1
  CleanData$bestPmDuhaime[Data$Q4!=5 & !is.na(Data$Q4)] <- 0
  CleanData$bestPmDuhaime[Data$Q4 %in% c(7,6)] <- NA
  toBeRemoved <- c(toBeRemoved, "Q4")
}


## Q5)  Dans quelle mesure êtes-vous satisfait(e) du gouvernement provincial de la CAQ de François Legault?
normQ5_1 <- c("1","2", "3", "4", "5")
normQ5_2 <- c("Tr.s insatisfait(e)", "Assez insatisfait(e)", "Assez satisfait(e)",
              "Tr.s satisfait(e)", "Je ne sais pas")
if(all(names(table(Data$Q5))!=normQ5_1) |
   all(names(table(Data2$Q5))!=normQ5_2)){
  warning("Problem with variable Q5's composition!")
} else {
  CleanData$iss_satisfiedGouvCAQ <- NA
  CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Tr.s satisfait(e)"] <- 1
  CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Assez satisfait(e)"] <- 0.66
  CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Assez insatisfait(e)"] <- 0.33
  CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Tr.s insatisfait(e)"] <- 0
  CleanData$iss_satisfiedGouvCAQ[Data2$Q5 == "Je ne sais pas"] <- NA
  toBeRemoved <- c(toBeRemoved, "Q5")
}


## Q6)  Au cours du dernier mois, qu'est-ce que vous avez lu, vu ou entendu dans les médias à propos du gouvernement de la CAQ de François Legault?
normQ6_1 <- c("96", "99")
normQ6_2 <- c("Code de r.ponse pour r.ponses ouvertes", "Je n.ai rien lu, vu ni entendu")
if(all(names(table(Data$Q6))!=normQ6_1) |
   all(names(table(Data2$Q6))!=normQ6_2)){
  warning("Problem with variable Q6's composition!")
} else {
  CleanData$heardLegault_open <- Data$Q6
  toBeRemoved <- c(toBeRemoved, "Q6")
}


## Q7)  Ce que vous avez lu, vu ou entendu vous a-t-il donné une impression plus ou moins favorable du gouvernement de la CAQ de François Legault?
normQ7_1 <- c("1","2", "3", "4", "5", "6")
normQ7_2 <- c("Beaucoup plus favorable", "Un peu plus favorable", "N.a pas eu d.impact",
              "Un peu moins favorable", "Beaucoup moins favorable", "Je ne sais pas")
if(all(names(table(Data$Q7))!=normQ7_1) |
   all(names(table(Data2$Q7))!=normQ7_2)){
  warning("Problem with variable Q7's composition!")
} else {
  CleanData$heardCAQGood <- NA
  CleanData$heardCAQGood[Data2$Q7 == "Beaucoup plus favorable"] <- 1
  CleanData$heardCAQGood[Data2$Q7 == "Un peu plus favorable"] <- 0.75
  CleanData$heardCAQGood[Data2$Q7 == "N.a pas eu d.impact"] <- 0.5
  CleanData$heardCAQGood[Data2$Q7 == "Un peu moins favorable"] <- 0.25
  CleanData$heardCAQGood[Data2$Q7 == "Beaucoup moins favorable"] <- 0
  toBeRemoved <- c(toBeRemoved, "Q7")
}

## "QFED1)  Si les élections fédérales avaient lieu aujourd’hui, pour lequel des partis suivants voteriez-vous ou seriez-vous tenté de voter?
normQFED1_1 <- c("1","2", "3", "4", "5", "6", "7", "8", "9")
normQFED1_2 <- c("Parti lib.ral du Canada (PLC) de Justin Trudeau",
                 "Parti conservateur du Canda (PCC) d'Erin O.Toole",
                 "Nouveau Parti d.mocratique du Canada (NPD) de Jagmeet Singh",
                 "Bloc qu.b.cois (BQ) d'Yves-Fran.ois Blanchet", 
                 "Parti populaire du Canada (PPC) de Maxime Bernier",
                 "Parti vert du Canada (PVC) d'Amita Kuttner (Chef par int.rim",
                 "Un autre parti",
                 "Je ne sais pas",
                 "Je n.irais pas voter")
if(all(names(table(Data$QFED1))!=normQFED1_1) |
   all(names(table(Data2$QFED1))!=normQFED1_2)){
  warning("Problem with variable QFED1's composition!")
} else {
  CleanData$voteIntPLC <- NA
  CleanData$voteIntPLC[Data$QFED1==1] <- 1
  CleanData$voteIntPLC[Data$QFED1!=1 & !is.na(Data$QFED1)] <- 0
  CleanData$voteIntPLC[Data$QFED1 %in% c(9,8,7)] <- NA
  CleanData$voteIntPCC <- NA
  CleanData$voteIntPCC[Data$QFED1==2] <- 1
  CleanData$voteIntPCC[Data$QFED1!=2 & !is.na(Data$QFED1)] <- 0
  CleanData$voteIntPCC[Data$QFED1 %in% c(9,8,7)] <- NA
  CleanData$voteIntNPD <- NA
  CleanData$voteIntNPD[Data$QFED1==3] <- 1
  CleanData$voteIntNPD[Data$QFED1!=3 & !is.na(Data$QFED1)] <- 0
  CleanData$voteIntNPD[Data$QFED1 %in% c(9,8,7)] <- NA
  CleanData$voteIntBQ <- NA
  CleanData$voteIntBQ[Data$QFED1==4] <- 1
  CleanData$voteIntBQ[Data$QFED1!=4 & !is.na(Data$QFED1)] <- 0
  CleanData$voteIntBQ[Data$QFED1 %in% c(9,8,7)] <- NA
  CleanData$voteIntPPC <- NA
  CleanData$voteIntPPC[Data$QFED1==5] <- 1
  CleanData$voteIntPPC[Data$QFED1!=5 & !is.na(Data$QFED1)] <- 0
  CleanData$voteIntPPC[Data$QFED1 %in% c(9,8,7)] <- NA
  CleanData$voteIntPVC <- NA
  CleanData$voteIntPVC[Data$QFED1==6] <- 1
  CleanData$voteIntPVC[Data$QFED1!=6 & !is.na(Data$QFED1)] <- 0
  CleanData$voteIntPVC[Data$QFED1 %in% c(9,8,7)] <- NA
  toBeRemoved <- c(toBeRemoved, "QFED1")
}


## QFED2)  Peut-être que votre choix n’est pas définitif, mais y a-t-il tout de même un parti ou chef que vous seriez tenté(e) d’appuyer?
normQFED2_1 <- c("1","2", "3", "4", "5", "6", "7", "8", "9")
normQFED2_2 <- c("Parti lib.ral du Canada (PLC) de Justin Trudeau",
                 "Parti conservateur du Canda (PCC) d'Erin O.Toole",
                 "Nouveau Parti d.mocratique du Canada (NPD) de Jagmeet Singh",
                 "Bloc qu.b.cois (BQ) d'Yves-Fran.ois Blanchet", 
                 "Parti populaire du Canada (PPC) de Maxime Bernier",
                 "Parti vert du Canada (PVC) d'Amita Kuttner (Chef par int.rim",
                 "Un autre parti",
                 "Je ne sais pas",
                 "Je n.irais pas voter")
if(all(names(table(Data$QFED2))!=normQFED2_1) |
   all(names(table(Data2$QFED2))!=normQFED2_2)){
  warning("Problem with variable QFED2's composition!")
} else {
  CleanData$voteIntPLC[Data$QFED2==1] <- 1
  CleanData$voteIntPCC[Data$QFED2==2] <- 1
  CleanData$voteIntNPD[Data$QFED2==3] <- 1
  CleanData$voteIntBQ[Data$QFED2==4] <- 1
  CleanData$voteIntPPC[Data$QFED2==5] <- 1
  CleanData$voteIntPVC[Data$QFED2==6] <- 1
  toBeRemoved <- c(toBeRemoved, "QFED2")
}


## QFED3)  Dans quelle mesure êtes-vous satisfait(e) du gouvernement fédéral du PLC de Justin Trudeau?
normQFED3_1 <- c("1","2", "3", "4", "5")
normQFED3_2 <- c("Tr.s insatisfait(e)", "Assez insatisfait(e)", "Assez satisfait(e)",
              "Tr.s satisfait(e)", "Je ne sais pas")
if(all(names(table(Data$QFED3))!=normQFED3_1) |
   all(names(table(Data2$QFED3))!=normQFED3_2)){
  warning("Problem with variable QFED3's composition!")
} else {
  CleanData$iss_satisfiedGouvPLC <- NA
  CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Tr.s satisfait(e)"] <- 1
  CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Assez satisfait(e)"] <- 0.66
  CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Assez insatisfait(e)"] <- 0.33
  CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Tr.s insatisfait(e)"] <- 0
  CleanData$iss_satisfiedGouvPLC[Data2$QFED3 == "Je ne sais pas"] <- NA
  toBeRemoved <- c(toBeRemoved, "QFED3")
}


## Q21)  Personnellement, comment vous décrivez-vous? Diriez-vous que vous êtes..."
normQ21_1 <- c("1", "2")
normQ21_2 <- c("Avant tout Qu.b.cois", "Avant tout Canadien")
if(all(names(table(Data$Q21))!=normQ21_1) |
   all(names(table(Data2$Q21))!=normQ21_2)){
  warning("Problem with variable Q21's composition!")
} else {
  CleanData$isQcBeforeCan <- NA
  CleanData$isQcBeforeCan[Data2$Q21=="Avant tout Qu.b.cois"] <- 1
  CleanData$isQcBeforeCan[Data2$Q21=="Avant tout Canadien"] <- 0
  toBeRemoved <- c(toBeRemoved, "Q21")
}


## N1)  A quels médias sociaux êtes-vous abonné(e)?
normN1_1 <- c("1","2", "3", "4", "5", "6", "7", "8", "9", "10", "99")
normN1_2 <- c("Facebook",
              "Twitter",
              "Instagram",
              "LinkedIn",
              "TikTok",
              "YouTube",
              "Rumble",
              "GETTR",
              "Odysse",
              "Autre",
              "Aucun")
if(all(names(table(Data$N1_M1))!=normN1_1) |
   all(names(table(Data2$N1_M1))!=normN1_2)){
  warning("Problem with variable N1_M's composition!")
} else {
  CleanData$subscribe_Fb <- 0
  CleanData$subscribe_Fb[   Data$N1_M1 == 1 |
                            Data$N1_M2 == 1 |
                            Data$N1_M3 == 1 |
                            Data$N1_M4 == 1 |
                            Data$N1_M5 == 1 |
                            Data$N1_M6 == 1 |
                            Data$N1_M7 == 1 |
                            Data$N1_M8 == 1 |
                            Data$N1_M9 == 1 |
                           Data$N1_M10 == 1] <- 1
  CleanData$subscribe_Tw <- 0
  CleanData$subscribe_Tw[      Data$N1_M1 == 2 |
                               Data$N1_M2 == 2 |
                               Data$N1_M3 == 2 |
                               Data$N1_M4 == 2 |
                               Data$N1_M5 == 2 |
                               Data$N1_M6 == 2 |
                               Data$N1_M7 == 2 |
                               Data$N1_M8 == 2 |
                               Data$N1_M9 == 2 |
                              Data$N1_M10 == 2] <- 1
  CleanData$subscribe_Insta <- 0
  CleanData$subscribe_Insta[   Data$N1_M1 == 3 |
                               Data$N1_M2 == 3 |
                               Data$N1_M3 == 3 |
                               Data$N1_M4 == 3 |
                               Data$N1_M5 == 3 |
                               Data$N1_M6 == 3 |
                               Data$N1_M7 == 3 |
                               Data$N1_M8 == 3 |
                               Data$N1_M9 == 3 |
                              Data$N1_M10 == 3] <- 1
  CleanData$subscribe_LinkedIn <- 0
  CleanData$subscribe_LinkedIn[   Data$N1_M1 == 4 |
                                  Data$N1_M2 == 4 |
                                  Data$N1_M3 == 4 |
                                  Data$N1_M4 == 4 |
                                  Data$N1_M5 == 4 |
                                  Data$N1_M6 == 4 |
                                  Data$N1_M7 == 4 |
                                  Data$N1_M8 == 4 |
                                  Data$N1_M9 == 4 |
                                 Data$N1_M10 == 4] <- 1
  CleanData$subscribe_TikTok <- 0
  CleanData$subscribe_TikTok[   Data$N1_M1 == 5 |
                                Data$N1_M2 == 5 |
                                Data$N1_M3 == 5 |
                                Data$N1_M4 == 5 |
                                Data$N1_M5 == 5 |
                                Data$N1_M6 == 5 |
                                Data$N1_M7 == 5 |
                                Data$N1_M8 == 5 |
                                Data$N1_M9 == 5 |
                               Data$N1_M10 == 5] <- 1
  CleanData$subscribe_Yt <- 0
  CleanData$subscribe_Yt[      Data$N1_M1 == 6 |
                               Data$N1_M2 == 6 |
                               Data$N1_M3 == 6 |
                               Data$N1_M4 == 6 |
                               Data$N1_M5 == 6 |
                               Data$N1_M6 == 6 |
                               Data$N1_M7 == 6 |
                               Data$N1_M8 == 6 |
                               Data$N1_M9 == 6 |
                              Data$N1_M10 == 6] <- 1
  CleanData$subscribe_Rumble <- 0
  CleanData$subscribe_Rumble[   Data$N1_M1 == 7 |
                                Data$N1_M2 == 7 |
                                Data$N1_M3 == 7 |
                                Data$N1_M4 == 7 |
                                Data$N1_M5 == 7 |
                                Data$N1_M6 == 7 |
                                Data$N1_M7 == 7 |
                                Data$N1_M8 == 7 |
                                Data$N1_M9 == 7 |
                               Data$N1_M10 == 7] <- 1
  CleanData$subscribe_GETTR <- 0
  CleanData$subscribe_GETTR[   Data$N1_M1 == 8 |
                               Data$N1_M2 == 8 |
                               Data$N1_M3 == 8 |
                               Data$N1_M4 == 8 |
                               Data$N1_M5 == 8 |
                               Data$N1_M6 == 8 |
                               Data$N1_M7 == 8 |
                               Data$N1_M8 == 8 |
                               Data$N1_M9 == 8 |
                              Data$N1_M10 == 8] <- 1
  CleanData$subscribe_Odysse <- 0
  CleanData$subscribe_Odysse[   Data$N1_M1 == 9 |
                                Data$N1_M2 == 9 |
                                Data$N1_M3 == 9 |
                                Data$N1_M4 == 9 |
                                Data$N1_M5 == 9 |
                                Data$N1_M6 == 9 |
                                Data$N1_M7 == 9 |
                                Data$N1_M8 == 9 |
                                Data$N1_M9 == 9 |
                               Data$N1_M10 == 9] <- 1
  CleanData$subscribe_otherSocMed <- 0
  CleanData$subscribe_otherSocMed[    Data$N1_M1 == 10 |
                                      Data$N1_M2 == 10 |
                                      Data$N1_M3 == 10 |
                                      Data$N1_M4 == 10 |
                                      Data$N1_M5 == 10 |
                                      Data$N1_M6 == 10 |
                                      Data$N1_M7 == 10 |
                                      Data$N1_M8 == 10 |
                                      Data$N1_M9 == 10 |
                                     Data$N1_M10 == 10] <- 1
  toBeRemoved <- c(toBeRemoved, paste0("N1_M", 1:10))
}

## C9) Pour quel parti avez-vous voté lors de l’élection provinciale québécoise de 2018?
normC9 <- c("1","2", "3", "4", "5", "6", "7", "8", "9")
normC9 <- c("Coalition avenir Qu.bec (CAQ)",
                  "Parti lib.ral du Qu.bec (PLQ)",     
                   "Parti Qu.b.cois (PQ)",              
                   "Qu.bec solidaire (QS)",             
                   "Parti vert du Qu.bec",              
                   "Parti conservateur du Qu.bec (PCQ)",
                   "Un autre parti",                    
                   "Je n.ai pas vot.",                  
                   "J.ai annul. mon vote")
if(all(names(table(Data$C9))!=normC9) |
   all(names(table(Data2$C9))!=normC9)){
  warning("Problem with variable C9's composition!")
} else {
  CleanData$votedProvCAQ2018 <- 0
  CleanData$votedProvCAQ2018[Data$C9 == 1] <- 1
  CleanData$votedProvPLQ2018 <- 0
  CleanData$votedProvPLQ2018[Data$C9 == 2] <- 1
  CleanData$votedProvPQ2018 <- 0
  CleanData$votedProvPQ2018[Data$C9 == 3] <- 1
  CleanData$votedProvQS2018 <- 0
  CleanData$votedProvQS2018[Data$C9 == 4] <- 1
  CleanData$votedProvPVQ2018 <- 0
  CleanData$votedProvPVQ2018[Data$C9 == 5] <- 1
  CleanData$votedProvPCQ2018 <- 0
  CleanData$votedProvPCQ2018[Data$C9 == 6] <- 1
  CleanData$votedProvOther2018 <- 0
  CleanData$votedProvOther2018[Data$C9 == 7] <- 1
  CleanData$votedProvNoVote2018 <- 0
  CleanData$votedProvNoVote2018[Data$C9 == 8 |
                                  Data$C9 == 9] <- 1
  toBeRemoved <- c(toBeRemoved, "C9")
}


##### AXEL ####
   
 ## Q22:  Lequel des énoncés décrit le mieux votre préférence sur le statut politique du Québec ?"
   normQ22_1 <- c("1", "2", "3", "4")
   normQ22_2 <- c("Fortement en faveur que le Qu.bec reste dans le Canada", "Mod.r.ment en faveur que le Qu.bec reste dans le Canada", 
                  "Mod.r.ment en faveur que le Qu.bec devienne un pays souverai", "Fortement en faveur que le Qu.bec devienne un pays souverain")
   if(all(names(table(Data$Q22))!=normQ22_1) |
      all(names(table(Data2$Q22))!=normQ22_2)){
     warning("Problem with variable Q22's composition!")
   } else {
   CleanData$isSouverainiste <- NA
   CleanData$isSouverainiste[Data2$Q22 == "Fortement en faveur que le Qu.bec devienne un pays souverain"] <- 1
   CleanData$isSouverainiste[Data2$Q22 == "Mod.r.ment en faveur que le Qu.bec devienne un pays souverai"] <- 0.66
   CleanData$isSouverainiste[Data2$Q22 == "Mod.r.ment en faveur que le Qu.bec reste dans le Canada"]      <- 0.33
   CleanData$isSouverainiste[Data2$Q22 == "Fortement en faveur que le Qu.bec reste dans le Canada"]       <- 0
   toBeRemoved <- c(toBeRemoved, "Q22") 
   }
   
## Q10)  Êtes-vous inquiet(e) de la situation de la langue française au Québec?
   normQ10_1 <- c("1", "2", "3", "4")
   normQ10_2 <- c("Tr.s inquiet(e)", "Inquiet(e)", "Peu inquiet(e)", "Pas du tout inquiet(e)")
   if(all(names(table(Data$Q10))!=normQ10_1) |
      all(names(table(Data2$Q10))!=normQ10_2)){
     warning("Problem with variable Q12's composition!")
   } else {
     CleanData$iss_worriedFrInProvQc <- NA
     CleanData$iss_worriedFrInProvQc[Data2$Q10 == "Tr.s inquiet(e)"] <- 1
     CleanData$iss_worriedFrInProvQc[Data2$Q10 == "Inquiet(e)"] <- 0.66
     CleanData$iss_worriedFrInProvQc[Data2$Q10 == "Peu inquiet(e)"] <- 0.33
     CleanData$iss_worriedFrInProvQc[Data2$Q10 == "Pas du tout inquiet(e)"] <- 0
     toBeRemoved <- c(toBeRemoved, "Q10") 
   }
   
 ## Q12: Dans quelle mesure êtes-vous sécure, ou insécure, à l’égard de votre situation financière?
   normQ12_1 <- c("1", "2", "3", "4")
   normQ12_2 <- c("Tr.s s.cure", "Assez s.cure", "Assez ins.cure", "Tr.s ins.cure")
   if(all(names(table(Data$Q12))!=normQ12_1) |
      all(names(table(Data2$Q12))!=normQ12_2)){
     warning("Problem with variable Q12's composition!")
   } else {
   CleanData$iss_isFinanceSecure <- NA
   CleanData$iss_isFinanceSecure[Data2$Q12 == "Tr.s s.cure"]    <- 1
   CleanData$iss_isFinanceSecure[Data2$Q12 == "Assez s.cure"]   <- 0.66
   CleanData$iss_isFinanceSecure[Data2$Q12 == "Assez ins.cure"] <- 0.33
   CleanData$iss_isFinanceSecure[Data2$Q12 == "Tr.s ins.cure"]  <- 0
   toBeRemoved <- c(toBeRemoved, "Q12") 
   }
   
 #### Q13: Avez-vous l’impression que la vie vous coûte plus cher aujourd’hui qu’il y a 12 mois? ####
   normQ13_1 <- c("1", "2", "3", "4")
   normQ13_2 <- c("Oui, beaucoup plus cher", "Oui, un peu plus cher", "Non, pas plus cher", "Je ne sais pas")
   if(all(names(table(Data$Q13))!=normQ13_1) |
      all(names(table(Data2$Q13))!=normQ13_2)){
     warning("Problem with variable Q13's composition!")
   } else {
     CleanData$iss_findsLifeMoExp <- NA
     CleanData$iss_findsLifeMoExp[Data2$Q13 == "Oui, beaucoup plus cher"] <- 1
     CleanData$iss_findsLifeMoExp[Data2$Q13 == "Oui, un peu plus cher"]   <- 0.66
     CleanData$iss_findsLifeMoExp[Data2$Q13 == "Non, pas plus cher"]      <- 0.33
     CleanData$iss_findsLifeMoExp[Data2$Q13 == "Je ne sais pas"]          <- 0
     toBeRemoved <- c(toBeRemoved, "Q13") 
   }
   
#### Q14: Avez-vous l’impression que la vie vous coûtera plus cher dans les 12 prochains mois?"  ##### 
   normQ14_1 <- c("1", "2", "3", "4")
   normQ14_2 <- c("Oui, beaucoup plus cher", "Oui, un peu plus cher", "Non, pas plus cher", "Je ne sais pas")
   if(all(names(table(Data$Q14))!=normQ14_1) |
      all(names(table(Data2$Q14))!=normQ14_2)){
     warning("Problem with variable Q14's composition!")
   } else {
     CleanData$iss_moExpInNext12 <- NA
     CleanData$iss_moExpInNext12[Data2$Q14 == "Oui, beaucoup plus cher"] <- 1
     CleanData$iss_moExpInNext12[Data2$Q14 == "Oui, un peu plus cher"]   <- 0.66
     CleanData$iss_moExpInNext12[Data2$Q14 == "Non, pas plus cher"]      <- 0.33
     CleanData$iss_moExpInNext12[Data2$Q14 == "Je ne sais pas"]          <- 0
     toBeRemoved <- c(toBeRemoved, "Q14") 
   }
   
  ## Q15: Avez-vous changé certaines habitudes pour faire face à l’augmentation des prix?
   normQ15_1 <- c("1", "2")
   normQ15_2 <- c("Oui j.ai chang. mes habitudes", "Non je n.ai pas chang. mes habitudes")
   if(all(names(table(Data$Q15))!=normQ15_1) |
      all(names(table(Data2$Q15))!=normQ15_2)){
     warning("Problem with variable Q15's composition!")
   } else {
   CleanData$iss_changedHabitsAugPrice <- NA
   CleanData$iss_changedHabitsAugPrice[Data2$Q15 == "Oui j.ai chang. mes habitudes"] <- 1
   CleanData$iss_changedHabitsAugPrice[Data2$Q15 == "Non je n.ai pas chang. mes habitudes"] <- 0
   toBeRemoved <- c(toBeRemoved, "Q15") 
   }
  
  ## Q16: Quel aspect de votre consommation coûte plus cher aujourd’hui qu’il y a 12 mois?
   CleanData$iss_consoMoExpLast12 <- NA
   
     
  ## L1: Le gouvernement du Québec a lancé son projet de 3e lien, un tunnel qui reliera Québec et Lévis. Ce projet, dont les coûts sont estimés entre 6 et 10 milliards de dollars, 
  #     devrait voir le jour sur un horizon de dix ans. Êtes-vous en accord avec ce proj  
   normL1_1 <- c("1", "2", "3", "4")
   normL1_2 <- c("Fortement en accord", "En accord", "En d.saccord", "Fortement en d.saccord")
   if(all(names(table(Data$L1))!=normL1_1) |
      all(names(table(Data2$L1))!=normL1_2)){
     warning("Problem with variable L1's composition!")
   } else {
     CleanData$iss_accordProj3Lien <- NA 
     CleanData$iss_accordProj3Lien[Data2$L1 == "Fortement en accord"]     <- 1
     CleanData$iss_accordProj3Lien[Data2$L1 == "En accord"]               <- 0.66
     CleanData$iss_accordProj3Lien[Data2$L1 == "En d.saccord"]            <- 0.33
     CleanData$iss_accordProj3Lien[Data2$L1 == "Fortement en d.saccord"]  <- 0
     toBeRemoved <- c(toBeRemoved, "L1") 
   }
   
  ## L2: Si le projet de 3e lien était révisé pour en diminuer l’ampleur et la facture, seriez-vous en accord avec ce projet?"
   normL2_1 <- c("1", "2", "3", "4")
   normL2_2 <- c("Fortement en accord", "En accord", "En d.saccord", "Fortement en d.saccord")
   if(all(names(table(Data$L2))!=normL2_1) |
      all(names(table(Data2$L2))!=normL2_2)){
     warning("Problem with variable L2's composition!")
   } else {
   CleanData$iss_dimFact3eLien <- NA 
   CleanData$iss_dimFact3eLien[Data2$L2 == "Fortement en accord"]     <- 1
   CleanData$iss_dimFact3eLien[Data2$L2 == "En accord"]               <- 0.66
   CleanData$iss_dimFact3eLien[Data2$L2 == "En d.saccord"]            <- 0.33
   CleanData$iss_dimFact3eLien[Data2$L2 == "Fortement en d.saccord"]  <- 0
   toBeRemoved <- c(toBeRemoved, "L2") 
   }
  
  ## L3_A1: La configuration actuelle du 3e lien prévoit 3 voies dans chaque direction, dont une sera dédiée aux autobus électriques. 
  #        Afin de réduire l’ampleur et la facture du 3e lien, plusieurs options sont possibles. Êtes-vous en accord ou en désaccord avec"
   normL3_A1_1 <- c("1","2", "3", "4")
   normL3_A1_2 <- c("Fortement en accord","En accord", "En d.saccord", "Fortement en d.saccord")
   if(all(names(table(Data$L3_A1))!=normL3_A1_1) |
      all(names(table(Data2$L3_A1))!=normL3_A1_2)){
     warning("Problem with variable L3_A1's composition!")
   } else {
     CleanData$iss_3eLienA1 <- NA
     CleanData$iss_3eLienA1[Data2$L3_A1 == 1] <- 1
     CleanData$iss_3eLienA1[Data2$L3_A1 == 2] <- 0.67
     CleanData$iss_3eLienA1[Data2$L3_A1 == 3] <- 0.33
     CleanData$iss_3eLienA1[Data2$L3_A1 == 4] <- 0
     toBeRemoved <- c(toBeRemoved, "L3_A1") 
   }
   
   ## L3_A2: La configuration actuelle du 3e lien prévoit 3 voies dans chaque direction, dont une sera dédiée aux autobus électriques. 
   #        Afin de réduire l'ampleur et la facture du 3e lien, plusieurs options sont possibles. Êtes-vous en accord ou en désaccord avec"
   normL3_A2_1 <- c("1","2", "3", "4")
   normL3_A2_2 <- c("Fortement en accord","En accord", "En d.saccord", "Fortement en d.saccord")
   if(all(names(table(Data$L3_A2))!=normL3_A2_1) |
      all(names(table(Data2$L3_A2))!=normL3_A2_2)){
     warning("Problem with variable L3_A2's composition!")
   } else {
     CleanData$iss_3eLienA2 <- NA
     CleanData$iss_3eLienA2[Data2$L3_A2 == 1] <- 1
     CleanData$iss_3eLienA2[Data2$L3_A2 == 2] <- 0.67
     CleanData$iss_3eLienA2[Data2$L3_A2 == 3] <- 0.33
     CleanData$iss_3eLienA2[Data2$L3_A2 == 4] <- 0
     toBeRemoved <- c(toBeRemoved, "L3_A2") 
   }
   
  ## L3_A3: La configuration actuelle du 3e lien prévoit 3 voies dans chaque direction, dont une sera dédiée aux autobus électriques. 
  #        Afin de réduire l'ampleur et la facture du 3e lien, plusieurs options sont possibles. Êtes-vous en accord ou en désaccord avec"
   normL3_A3_1 <- c("1","2", "3", "4")
   normL3_A3_2 <- c("Fortement en accord","En accord", "En d.saccord", "Fortement en d.saccord")
   if(all(names(table(Data$L3_A3))!=normL3_A3_1) |
      all(names(table(Data2$L3_A3))!=normL3_A3_2)){
     warning("Problem with variable L3_A3's composition!")
   } else {
     CleanData$iss_3eLienA3 <- NA
     CleanData$iss_3eLienA3[Data2$L3_A3 == 1] <- 1
     CleanData$iss_3eLienA3[Data2$L3_A3 == 2] <- 0.67
     CleanData$iss_3eLienA3[Data2$L3_A3 == 3] <- 0.33
     CleanData$iss_3eLienA3[Data2$L3_A3 == 4] <- 0
     toBeRemoved <- c(toBeRemoved, "L3_A3") 
   }
   
   
  ## N2_A1: Dans quelle mesure utilisez ces sources d’information pour vous renseigner sur la politique? / Journaux"
   normN2_A1_1 <- c("1","2", "3", "4")
   normN2_A1_2 <- c("R.guli.rement", "Occasionnellement", "Rarement", "Jamais")
   if(all(names(table(Data$N2_A1))!=normN2_A1_1) |
      all(names(table(Data2$N2_A1))!=normN2_A1_2)){
     warning("Problem with variable N2_A1's composition!")
   } else {
     CleanData$sourceInfoPol_Journaux <- NA
     CleanData$sourceInfoPol_Journaux[Data2$N2_A1 == "R.guli.rement"]     <- 1
     CleanData$sourceInfoPol_Journaux[Data2$N2_A1 == "Occasionnellement"] <- 0.67
     CleanData$sourceInfoPol_Journaux[Data2$N2_A1 == "Rarement"]          <- 0.33
     CleanData$sourceInfoPol_Journaux[Data2$N2_A1 == "Jamais"]            <- 0
     toBeRemoved <- c(toBeRemoved, "N2_A1") 
   }
   
  ## N2_A2: Dans quelle mesure utilisez ces sources d'information pour vous renseigner sur la politique? / Radio"
   normN2_A2_1 <- c("1","2", "3", "4")
   normN2_A2_2 <- c("R.guli.rement", "Occasionnellement", "Rarement", "Jamais")
   if(all(names(table(Data$N2_A2))!=normN2_A2_1) |
      all(names(table(Data2$N2_A2))!=normN2_A2_2)){
     warning("Problem with variable N2_A2's composition!")
   } else {
     CleanData$sourceInfoPol_Radio <- NA
     CleanData$sourceInfoPol_Radio[Data2$N2_A2 == "R.guli.rement"]     <- 1
     CleanData$sourceInfoPol_Radio[Data2$N2_A2 == "Occasionnellement"] <- 0.67
     CleanData$sourceInfoPol_Radio[Data2$N2_A2 == "Rarement"]          <- 0.33
     CleanData$sourceInfoPol_Radio[Data2$N2_A2 == "Jamais"]            <- 0
     toBeRemoved <- c(toBeRemoved, "N2_A2") 
   }
   
  ## N2_A3: Dans quelle mesure utilisez ces sources d'information pour vous renseigner sur la politique? / Télévision"
   normN2_A3_1 <- c("1","2", "3", "4")
   normN2_A3_2 <- c("R.guli.rement", "Occasionnellement", "Rarement", "Jamais")
   if(all(names(table(Data$N2_A3))!=normN2_A3_1) |
      all(names(table(Data2$N2_A3))!=normN2_A3_2)){
     warning("Problem with variable N2_A3's composition!")
   } else {
     CleanData$sourceInfoPol_Tv <- NA
     CleanData$sourceInfoPol_Tv[Data2$N2_A3 == "R.guli.rement"]     <- 1
     CleanData$sourceInfoPol_Tv[Data2$N2_A3 == "Occasionnellement"] <- 0.67
     CleanData$sourceInfoPol_Tv[Data2$N2_A3 == "Rarement"]          <- 0.33
     CleanData$sourceInfoPol_Tv[Data2$N2_A3 == "Jamais"]            <- 0
     toBeRemoved <- c(toBeRemoved, "N2_A3") 
   }
   
  ## N2_A4: Dans quelle mesure utilisez ces sources d'information pour vous renseigner sur la politique? / Médias sociaux"
   normN2_A4_1 <- c("1","2", "3", "4")
   normN2_A4_2 <- c("R.guli.rement", "Occasionnellement", "Rarement", "Jamais")
   if(all(names(table(Data$N2_A4))!=normN2_A4_1) |
      all(names(table(Data2$N2_A4))!=normN2_A4_2)){
     warning("Problem with variable N2_A4's composition!")
   } else {
     CleanData$sourceInfoPol_MedSoc <- NA
     CleanData$sourceInfoPol_MedSoc[Data2$N2_A4 == "R.guli.rement"]     <- 1
     CleanData$sourceInfoPol_MedSoc[Data2$N2_A4 == "Occasionnellement"] <- 0.67
     CleanData$sourceInfoPol_MedSoc[Data2$N2_A4 == "Rarement"]          <- 0.33
     CleanData$sourceInfoPol_MedSoc[Data2$N2_A4 == "Jamais"]            <- 0
     toBeRemoved <- c(toBeRemoved, "N2_A4") 
   }
  
  ## N2_A5: Dans quelle mesure utilisez ces sources d'information pour vous renseigner sur la politique? / ami"
   normN2_A5_1 <- c("1","2", "3", "4")
   normN2_A5_2 <- c("R.guli.rement", "Occasionnellement", "Rarement", "Jamais")
   if(all(names(table(Data$N2_A5))!=normN2_A5_1) |
      all(names(table(Data2$N2_A5))!=normN2_A5_2)){
     warning("Problem with variable N2_A5's composition!")
   } else {
     CleanData$sourceInfoPol_Ami <- NA
     CleanData$sourceInfoPol_Ami[Data2$N2_A5 == "R.guli.rement"]     <- 1
     CleanData$sourceInfoPol_Ami[Data2$N2_A5 == "Occasionnellement"] <- 0.67
     CleanData$sourceInfoPol_Ami[Data2$N2_A5 == "Rarement"]          <- 0.33
     CleanData$sourceInfoPol_Ami[Data2$N2_A5 == "Jamais"]            <- 0
     toBeRemoved <- c(toBeRemoved, "N2_A5") 
   }
   
  ## N2_A6:  Dans quelle mesure utilisez ces sources d'information pour vous renseigner sur la politique? / Un membre de la famille
   normN2_A6_1 <- c("1","2", "3", "4")
   normN2_A6_2 <- c("R.guli.rement", "Occasionnellement", "Rarement", "Jamais")
   if(all(names(table(Data$N2_A6))!=normN2_A6_1) |
      all(names(table(Data2$N2_A6))!=normN2_A6_2)){
     warning("Problem with variable N2_A6's composition!")
   } else {
     CleanData$sourceInfoPol_Fam <- NA
     CleanData$sourceInfoPol_Fam[Data2$N2_A6 == "R.guli.rement"]     <- 1
     CleanData$sourceInfoPol_Fam[Data2$N2_A6 == "Occasionnellement"] <- 0.67
     CleanData$sourceInfoPol_Fam[Data2$N2_A6 == "Rarement"]          <- 0.33
     CleanData$sourceInfoPol_Fam[Data2$N2_A6 == "Jamais"]            <- 0
     toBeRemoved <- c(toBeRemoved, "N2_A6") 
   }
   
  ## N2_A7: Dans quelle mesure utilisez ces sources d'information pour vous renseigner sur la politique? / Un enseignant/professeur
   normN2_A7_1 <- c("1","2", "3", "4")
   normN2_A7_2 <- c("R.guli.rement", "Occasionnellement", "Rarement", "Jamais")
   if(all(names(table(Data$N2_A7))!=normN2_A7_1) |
      all(names(table(Data2$N2_A7))!=normN2_A7_2)){
     warning("Problem with variable N2_A7's composition!")
   } else {
     CleanData$sourceInfoPol_Prof <- NA
     CleanData$sourceInfoPol_Prof[Data2$N2_A7 == "R.guli.rement"]     <- 1
     CleanData$sourceInfoPol_Prof[Data2$N2_A7 == "Occasionnellement"] <- 0.67
     CleanData$sourceInfoPol_Prof[Data2$N2_A7 == "Rarement"]          <- 0.33
     CleanData$sourceInfoPol_Prof[Data2$N2_A7 == "Jamais"]            <- 0
     toBeRemoved <- c(toBeRemoved, "N2_A7") 
   }
   
  ## N2_A8: Dans quelle mesure utilisez ces sources d'information pour vous renseigner sur la politique? / Un collègue de travail
   normN2_A8_1 <- c("1","2", "3", "4")
   normN2_A8_2 <- c("R.guli.rement", "Occasionnellement", "Rarement", "Jamais")
   if(all(names(table(Data$N2_A8))!=normN2_A8_1) |
      all(names(table(Data2$N2_A8))!=normN2_A8_2)){
     warning("Problem with variable N2_A8's composition!")
   } else {
     CleanData$sourceInfoPol_Coll <- NA
     CleanData$sourceInfoPol_Coll[Data2$N2_A8 == "R.guli.rement"]     <- 1
     CleanData$sourceInfoPol_Coll[Data2$N2_A8 == "Occasionnellement"] <- 0.67
     CleanData$sourceInfoPol_Coll[Data2$N2_A8 == "Rarement"]          <- 0.33
     CleanData$sourceInfoPol_Coll[Data2$N2_A8 == "Jamais"]            <- 0
     toBeRemoved <- c(toBeRemoved, "N2_A8") 
   }
   
  ##  N2B: Quelle est, parmi les sources suivantes, celle que vous utilisez le plus souvent? 
   normN2B_1 <- c("1", "2", "3", "4", "5", "6", "7", "8")
   normN2B_2 <- c("Journaux", "Radio", "T.l.vision", "M.dias sociaux", "Un ami", "Un membre de la famille", 
                  "Un enseignant/professeur", "Un coll.gue de travail")
   if(all(names(table(Data$N2B))!=normN2B_1) |
      all(names(table(Data2$N2B))!=normN2B_2)){
     warning("Problem with variable N2B's composition!")
   } else {
     CleanData$sourceUsed_journal <- NA
     CleanData$sourceUsed_journal[Data$N2B == 1] <- 1 # Journaux
     CleanData$sourceUsed_journal[Data$N2B != 1] <- 0 
     table(CleanData$sourceUsed_journal)
     
     CleanData$sourceUsed_radio <- NA
     CleanData$sourceUsed_radio[Data$N2B == 2] <- 1 # Radio  
     CleanData$sourceUsed_radio[Data$N2B != 2] <- 0 
     table(CleanData$sourceUsed_radio)
     
     CleanData$sourceUsed_tv <- NA
     CleanData$sourceUsed_tv[Data$N2B == 3] <- 1 # Télévision
     CleanData$sourceUsed_tv[Data$N2B != 3] <- 0 
     table(CleanData$sourceUsed_tv)
     
     CleanData$sourceUsed_medSoc <- NA
     CleanData$sourceUsed_medSoc[Data$N2B == 4] <- 1 # Médias sociaux  
     CleanData$sourceUsed_medSoc[Data$N2B != 4] <- 0 
     table(CleanData$sourceUsed_medSoc)
     
     CleanData$sourceUsed_ami <- NA
     CleanData$sourceUsed_ami[Data$N2B == 5] <- 1 # Un ami 
     CleanData$sourceUsed_ami[Data$N2B != 5] <- 0 
     table(CleanData$sourceUsed_ami)
     
     CleanData$sourceUsed_famille <- NA
     CleanData$sourceUsed_famille[Data$N2B == 6] <- 1 # Un membre de la famille
     CleanData$sourceUsed_famille[Data$N2B != 6] <- 0 
     table(CleanData$sourceUsed_famille)
     
     CleanData$sourceUsed_prof <- NA
     CleanData$sourceUsed_prof[Data$N2B == 7] <- 1 #  Un enseignant/professeur
     CleanData$sourceUsed_prof[Data$N2B != 7] <- 0 
     table(CleanData$sourceUsed_prof)
     
     CleanData$sourceUsed_coll <- NA
     CleanData$sourceUsed_coll[Data$N2B == 8] <- 1 #  Un collègue de travail
     CleanData$sourceUsed_coll[Data$N2B != 8] <- 0 
     table(CleanData$sourceUsed_coll)
     
     toBeRemoved <- c(toBeRemoved, "N2B")
   }

  ## N3: Sur les médias sociaux, en qui ou en quoi faites-vous le plus confiance pour forger votre opinion sur un enjeu politique?
   normN3_1 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
   normN3_2 <- c("Un ami", "Un membre de la famille", "Un journaliste", "Une publication partag.e un grand nombre de fois", 
                 "Un politicien", "Un scientifique", "Un influenceur", " Un collègue de travail", "Un collègue de travail",
                   "Ma propre évaluation de la qualité de l’information", "Autre")
   if(all(names(table(Data$N3))!=normN3_1) |
      all(names(table(Data2$N3))!=normN3_2)){
     warning("Problem with variable N3's composition!")
   } else {
     CleanData$confMedSoc_ami <- NA
     CleanData$confMedSoc_ami[Data$N3 == 1] <- 1 #  Un ami 
     CleanData$confMedSoc_ami[Data$N3 != 1] <- 0 
     table(CleanData$confMedSoc_ami)
     
     CleanData$confMedSoc_fam <- NA
     CleanData$confMedSoc_fam[Data$N3 == 2] <- 1 #  Un membre de la famille  
     CleanData$confMedSoc_fam[Data$N3 != 2] <- 0 
     table(CleanData$confMedSoc_fam)
     
     CleanData$confMedSoc_journal <- NA
     CleanData$confMedSoc_journal[Data$N3 == 3] <- 1 # Un journaliste
     CleanData$confMedSoc_journal[Data$N3 != 3] <- 0 
     table(CleanData$confMedSoc_journal)
     
     CleanData$sourceUsed_medSoc <- NA
     CleanData$sourceUsed_medSoc[Data$N3 == 4] <- 1 # Une publication partagée un grand nombre de fois 
     CleanData$sourceUsed_medSoc[Data$N3 != 4] <- 0 
     table(CleanData$sourceUsed_medSoc)
     
     CleanData$confMedSoc_pol <- NA
     CleanData$confMedSoc_pol[Data$N3 == 5] <- 1 # Un politicien 
     CleanData$confMedSoc_pol[Data$N3 != 5] <- 0 
     table(CleanData$confMedSoc_pol)
     
     CleanData$confMedSoc_sci <- NA
     CleanData$confMedSoc_sci[Data$N3 == 6] <- 1 # Un scientifique
     CleanData$confMedSoc_sci[Data$N3 != 6] <- 0 
     table(CleanData$confMedSoc_sci)
     
     CleanData$confMedSoc_influ <- NA
     CleanData$confMedSoc_influ[Data$N3 == 7] <- 1 #  Un influenceur
     CleanData$confMedSoc_influ[Data$N3 != 7] <- 0 
     table(CleanData$confMedSoc_influ)
     
     CleanData$confMedSoc_coll <- NA
     CleanData$confMedSoc_coll[Data$N3 == 8] <- 1 #  Un collègue de travail
     CleanData$confMedSoc_coll[Data$N3 != 8] <- 0 
     table(CleanData$confMedSoc_coll)
     
     CleanData$confMedSoc_ownEvQualInfo <- NA
     CleanData$confMedSoc_ownEvQualInfo[Data$N3 == 9] <- 1 #  Ma propre évaluation de la qualité de l’information
     CleanData$confMedSoc_ownEvQualInfo[Data$N3 != 9] <- 0 
     table(CleanData$confMedSoc_ownEvQualInfo)
     
     CleanData$confMedSoc_autre <- NA
     CleanData$confMedSoc_autre[Data$N3 == 10] <- 1 #  Autre
     CleanData$confMedSoc_autre[Data$N3 != 10] <- 0 
     table(CleanData$confMedSoc_autre)
     
     toBeRemoved <- c(toBeRemoved, "N3")
   }
   
  ## N4_A1: Dans quelle mesure êtes-vous en accord avec les énoncés suivants? / L'information sur les médias sociaux est plus 
  #        fiable celle des médias traditionnels car je contrôle mes abonnements sur mon fil.
   normN4_A1_1 <- c("1","2", "3", "4")
   normN4_A1_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord")
   if(all(names(table(Data$N4_A1))!=normN4_A1_1) |
      all(names(table(Data2$N4_A1))!=normN4_A1_2)){
     warning("Problem with variable N4_A1's composition!")
   } else {
     CleanData$medSocMoRelMedTrad <- NA
     CleanData$medSocMoRelMedTrad[Data2$N4_A1 == "Tr.s en accord"]      <- 1
     CleanData$medSocMoRelMedTrad[Data2$N4_A1 == "Plut.t en accord"]    <- 0.67
     CleanData$medSocMoRelMedTrad[Data2$N4_A1 == "Plut.t en d.saccord"] <- 0.33
     CleanData$medSocMoRelMedTrad[Data2$N4_A1 == "Tr.s en d.saccord"]   <- 0
     toBeRemoved <- c(toBeRemoved, "N4_A1") 
   }
   
  ## N4_A2: Dans quelle mesure êtes-vous en accord avec les énoncés suivants? / Les politiciens sont généralement bienveillants.
   normN4_A2_1 <- c("1","2", "3", "4")
   normN4_A2_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord")
   if(all(names(table(Data$N4_A2))!=normN4_A2_1) |
      all(names(table(Data2$N4_A2))!=normN4_A2_2)){
     warning("Problem with variable N4_A2's composition!")
   } else {
     CleanData$polBienveil <- NA
     CleanData$polBienveil[Data2$N4_A2 == "Tr.s en accord"]      <- 1
     CleanData$polBienveil[Data2$N4_A2 == "Plut.t en accord"]    <- 0.67
     CleanData$polBienveil[Data2$N4_A2 == "Plut.t en d.saccord"] <- 0.33
     CleanData$polBienveil[Data2$N4_A2 == "Tr.s en d.saccord"]   <- 0
     toBeRemoved <- c(toBeRemoved, "N4_A2") 
   }
   
  ## N4_A3: Dans quelle mesure êtes-vous en accord avec les énoncés suivants? /Les politiciens ne se préoccupent pas des 
   #        problèmes des citoyens
   normN4_A3_1 <- c("1","2", "3", "4")
   normN4_A3_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord")
   if(all(names(table(Data$N4_A3))!=normN4_A3_1) |
      all(names(table(Data2$N4_A3))!=normN4_A3_2)){
     warning("Problem with variable N4_A3's composition!")
   } else {
     CleanData$polProbCit <- NA
     CleanData$polProbCit[Data2$N4_A3 == "Tr.s en accord"]      <- 1
     CleanData$polProbCit[Data2$N4_A3 == "Plut.t en accord"]    <- 0.67
     CleanData$polProbCit[Data2$N4_A3 == "Plut.t en d.saccord"] <- 0.33
     CleanData$polProbCit[Data2$N4_A3 == "Tr.s en d.saccord"]   <- 0
     toBeRemoved <- c(toBeRemoved, "N4_A3") 
   }
   
  ## N4_A4: Dans quelle mesure êtes-vous en accord avec les énoncés suivants? / Il est bon pour la démocratie que les partis politiques 
   #        collectent des données sur moi, car cela leur permet de proposer des idées avec lesquelles je suis plus d'accord
   normN4_A4_1 <- c("1","2", "3", "4")
   normN4_A4_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord")
   if(all(names(table(Data$N4_A4))!=normN4_A4_1) |
      all(names(table(Data2$N4_A4))!=normN4_A4_2)){
     warning("Problem with variable N4_A4's composition!")
   } else {
     CleanData$DemPartiPolCollDon <- NA
     CleanData$DemPartiPolCollDon[Data2$N4_A4 == "Tr.s en accord"]      <- 1
     CleanData$DemPartiPolCollDon[Data2$N4_A4 == "Plut.t en accord"]    <- 0.67
     CleanData$DemPartiPolCollDon[Data2$N4_A4 == "Plut.t en d.saccord"] <- 0.33
     CleanData$DemPartiPolCollDon[Data2$N4_A4 == "Tr.s en d.saccord"]   <- 0
     toBeRemoved <- c(toBeRemoved, "N4_A4") 
   }
   
  ## N5_A1:  Faites-vous ce qui suit sur les médias sociaux? / Je ne m'abonne qu'aux gens qui pensent comme moi
   normN5_A1_1 <- c("1","2")
   normN5_A1_2 <- c("Oui", "Non")
   if(all(names(table(Data$N5_A1))!=normN5_A1_1) |
      all(names(table(Data2$N5_A1))!=normN5_A1_2)){
     warning("Problem with variable N5_A1's composition!")
   } else {
     CleanData$faitMedSoc_AboGensPenCoMoi <- NA
     CleanData$faitMedSoc_AboGensPenCoMoi[Data2$N5_A1 == "Oui"] <- 1
     CleanData$faitMedSoc_AboGensPenCoMoi[Data2$N5_A1 == "Non"] <- 0
     toBeRemoved <- c(toBeRemoved, "N5_A1") 
   }
   
  ## N5_A2: Faites-vous ce qui suit sur les médias sociaux? / Je partage des informations personnelles"
   normN5_A2_1 <- c("1","2")
   normN5_A2_2 <- c("Oui", "Non")
   if(all(names(table(Data$N5_A2))!=normN5_A2_1) |
      all(names(table(Data2$N5_A2))!=normN5_A2_2)){
     warning("Problem with variable N5_A2's composition!")
   } else {
     CleanData$FaitMedSoc_partInfoPers <- NA
     CleanData$FaitMedSoc_partInfoPers[Data2$N5_A2 == "Oui"] <- 1
     CleanData$FaitMedSoc_partInfoPers[Data2$N5_A2 == "Non"] <- 0
     toBeRemoved <- c(toBeRemoved, "N5_A2") 
   }
   
  ## N5_A3: Faites-vous ce qui suit sur les médias sociaux? / Je partage des opinions politiques"
   normN5_A3_1 <- c("1","2")
   normN5_A3_2 <- c("Oui", "Non")
   if(all(names(table(Data$N5_A3))!=normN5_A3_1) |
      all(names(table(Data2$N5_A3))!=normN5_A3_2)){
     warning("Problem with variable N5_A3's composition!")
   } else {
     CleanData$FaitMedSoc_partOpPol <- NA
     CleanData$FaitMedSoc_partOpPol[Data2$N5_A3 == "Oui"] <- 1
     CleanData$FaitMedSoc_partOpPol[Data2$N5_A3 == "Non"] <- 0
     toBeRemoved <- c(toBeRemoved, "N5_A3") 
   }
   
  ## N5_A4: Faites-vous ce qui suit sur les médias sociaux? / J'interviens plusieurs fois par semaine" 
   normN5_A4_1 <- c("1","2")
   normN5_A4_2 <- c("Oui", "Non")
   if(all(names(table(Data$N5_A4))!=normN5_A4_1) |
      all(names(table(Data2$N5_A4))!=normN5_A4_2)){
     warning("Problem with variable N5_A4's composition!")
   } else {
     CleanData$FaitMedSoc_IntPlusFoSem <- NA
     CleanData$FaitMedSoc_IntPlusFoSem[Data2$N5_A4 == "Oui"] <- 1
     CleanData$FaitMedSoc_IntPlusFoSem[Data2$N5_A4 == "Non"] <- 0
     toBeRemoved <- c(toBeRemoved, "N5_A4") 
   }
   

    
   
   
#### Alexandre ####
   # N5_A5)  Faites-vous ce qui suit sur les médias sociaux? / Je consulte mon fil à chaque jour
   normN5_A5_1 <- c("1", "2")
   normN5_A5_2 <- c("Oui","Non")
   if(all(names(table(Data$N5_A5))!=normN5_A5_1) |
      all(names(table(Data2$N5_A5))!=normN5_A5_2)){
     warning("Problem with variable N5_A5's composition!")
   } else {
     CleanData$everyday_socMedia <- NA
     CleanData$everyday_socMedia[Data$N5_A5 == 1] <- 1
     CleanData$everyday_socMedia[Data$N5_A5 == 2] <- 0  
     toBeRemoved <- c(toBeRemoved, "N5_A5")
   }
   
   # N8)  À chaque élection, plusieurs personnes sont incapables de voter parce qu'elles ne sont pas inscrites sur la liste électorale, 
   # elles sont malades ou elles n'ont pas le temps. Laquelle des réponses suivantes décrit le mieux ce que vous avez fait pendant
   normN8_1 <- c("1", "2", "3", "4", "5", "6")
   normN8_2 <- c("Je n'ai pas vot.", "J'ai pens. voter mais je ne l'ai pas fait", "Je vote g.n.ralement mais je ne suis pas all. cette fois-ci", "Je suis certain d'avoir vot. . cette .lection", "Je ne me souviens plus", "Je n'.tais pas .ligible pour voter")
   if(all(names(table(Data$N8))!=normN8_1) |
      all(names(table(Data2$N8))!=normN8_2)){
     warning("Problem with variable N8's composition!")
   } else {
     CleanData$turnOut_Desc <- NA
     CleanData$turnOut_Desc[Data$N8 == 1] <- 0
     CleanData$turnOut_Desc[Data$N8 == 2] <- 0.2
     CleanData$turnOut_Desc[Data$N8 == 3] <- 0.4
     CleanData$turnOut_Desc[Data$N8 == 4] <- 0.6
     CleanData$turnOut_Desc[Data$N8 == 5] <- 0.8
     CleanData$turnOut_Desc[Data$N8 == 6] <- 1
     toBeRemoved <- c(toBeRemoved, "N8")
   }
   
   
   # N9)  Les gens ont des points de vue différents sur le vote. Pour certains, voter est un DEVOIR. 
   # Ils estiment qu'ils devraient voter à chaque élection. Pour d’autres, voter est un CHOIX. Ils ne votent que lorsqu'ils se sentent fortement interpellés par cett
   normN9_1 <- c("1", "2")
   normN9_2 <- c("Devoir", "Choix")
   if(all(names(table(Data$N9))!=normN9_1) |
      all(names(table(Data2$N9))!=normN9_2)){
     warning("Problem with variable N9's composition!")
   } else {
     CleanData$turnOut_Choix <- NA
     CleanData$turnOut_Choix[Data$N9 == 2] <- 1 # Voter est un choix 
     CleanData$turnOut_Choix[Data$N9 != 2] <- 0 
     table(CleanData$voteChoix)
     
     CleanData$turnOut_Devoir <- NA
     CleanData$turnOut_Devoir[Data$N9 == 1] <- 1 # Voter est un devoir
     CleanData$turnOut_Devoir[Data$N9 != 1] <- 0 
       toBeRemoved <- c(toBeRemoved, "N9")
     }
  
     
     # N10)  Dans quelle mesure seriez-vous susceptible de prendre le temps de voter aux élections générales québécoises de 2022 si vous pouviez voter en ligne?
     normN10_1 <- c("1", "2", "3", "4")
     normN10_2 <- c("Certainement", "Probablement", "Probablement pas", "Certainement pas")
     if(all(names(table(Data$N10))!=normN10_1) |
        all(names(table(Data2$N10))!=normN10_2)) {
       warning("Problem with variable N10's composition!")
     } else {
       CleanData$turnOutPot <- NA
       CleanData$turnOutPot[Data$N10 == 1] <- 0
       CleanData$turnOutPot[Data$N10 == 2] <- 0.33
       CleanData$turnOutPot[Data$N10 == 3] <- 0.67
       CleanData$turnOutPot[Data$N10 == 4] <- 1
       toBeRemoved <- c(toBeRemoved, "N10")
     }
       
      # N11_A1)  Dans quelle mesure chacune de ces formes d’engagement politique est-elle|nbsp;importante pour vous? / Manifester dans la rue
     normN11_A1_1 <- c("1", "2", "3", "4")
     normN11_A1_2 <- c("Tr.s important", "Assez important", "Peu important", "Pas important du tout")
     if(all(names(table(Data$N11_A1))!=normN11_A1_1) |
        all(names(table(Data2$N11_A1))!=normN11_A1_2)) {
       warning("Problem with variable N11_A1's composition!")
     } else {
       CleanData$partManif <- NA
       CleanData$partManif[Data$N11_A1 == 1] <- 1
       CleanData$partManif[Data$N11_A1 == 2] <- 0.67
       CleanData$partManif[Data$N11_A1 == 3] <- 0.33
       CleanData$partManif[Data$N11_A1 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N11_A1")
     }
       
   # N11_A2)  Dans quelle mesure chacune de ces formes d'engagement politique est-elle|nbsp;importante pour vous? 
     # / Partager du contenu militant sur les médias sociaux
     normN11_A2_1 <- c("1", "2", "3", "4")
     normN11_A2_2 <- c("Tr.s important", "Assez important", "Peu important", "Pas important du tout")
     if(all(names(table(Data$N11_A2))!=normN11_A2_1) |
        all(names(table(Data2$N11_A2))!=normN11_A2_2)) {
       warning("Problem with variable N11_A2's composition!")
     } else {
       CleanData$partSocMedia <- NA
       CleanData$partSocMedia[Data$N11_A2 == 1] <- 1
       CleanData$partSocMedia[Data$N11_A2 == 2] <- 0.67
       CleanData$partSocMedia[Data$N11_A2 == 3] <- 0.33
       CleanData$partSocMedia[Data$N11_A2 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N11_A2")
     }
   
   # N11_A3)  Dans quelle mesure chacune de ces formes d'engagement politique est-elle|nbsp;importante pour vous?
    # / Signer une pétition en ligne
     normN11_A3_1 <- c("1", "2", "3", "4")
     normN11_A3_2 <- c("Tr.s important", "Assez important", "Peu important", "Pas important du tout")
     if(all(names(table(Data$N11_A3))!=normN11_A3_1) |
        all(names(table(Data2$N11_A3))!=normN11_A3_2)) {
       warning("Problem with variable N11_A3's composition!")
     } else {
       CleanData$partSignPet <- NA
       CleanData$partSignPet[Data$N11_A3 == 1] <- 1
       CleanData$partSignPet[Data$N11_A3 == 2] <- 0.67
       CleanData$partSignPet[Data$N11_A3 == 3] <- 0.33
       CleanData$partSignPet[Data$N11_A3 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N11_A3")
     }
     
   # N11_A4)  Dans quelle mesure chacune de ces formes d'engagement politique est-elle|nbsp;importante pour vous? 
    # / Voter
     normN11_A4_1 <- c("1", "2", "3", "4")
     normN11_A4_2 <- c("Tr.s important", "Assez important", "Peu important", "Pas important du tout")
     if(all(names(table(Data$N11_A4))!=normN11_A4_1) |
        all(names(table(Data2$N11_A4))!=normN11_A4_2)) {
       warning("Problem with variable N11_A4's composition!")
     } else {
       CleanData$partVote <- NA
       CleanData$partVote[Data$N11_A4 == 1] <- 1
       CleanData$partVote[Data$N11_A4 == 2] <- 0.67
       CleanData$partVote[Data$N11_A4 == 3] <- 0.33
       CleanData$partVote[Data$N11_A4 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N11_A4")
     }
     
     
    #  N11_A5)  Dans quelle mesure chacune de ces formes d'engagement politique est-elle|nbsp;importante pour vous? 
     # / Devenir membre d'un parti politique
     normN11_A5_1 <- c("1", "2", "3", "4")
     normN11_A5_2 <- c("Tr.s important", "Assez important", "Peu important", "Pas important du tout")
     if(all(names(table(Data$N11_A5))!=normN11_A5_1) |
        all(names(table(Data2$N11_A5))!=normN11_A5_2)) {
       warning("Problem with variable N11_A5's composition!")
     } else {
       CleanData$partMember <- NA
       CleanData$partMember[Data$N11_A5 == 1] <- 1
       CleanData$partMember[Data$N11_A5 == 2] <- 0.67
       CleanData$partMember[Data$N11_A5 == 3] <- 0.33
       CleanData$partMember[Data$N11_A5 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N11_A5")
     }
     
    # N11_A6)  Dans quelle mesure chacune de ces formes d'engagement politique est-elle|nbsp;importante pour vous? 
     # / Devenir membre d'un groupe ou d'une association
     normN11_A6_1 <- c("1", "2", "3", "4")
     normN11_A6_2 <- c("Tr.s important", "Assez important", "Peu important", "Pas important du tout")
     if(all(names(table(Data$N11_A6))!=normN11_A6_1) |
        all(names(table(Data2$N11_A6))!=normN11_A6_2)) {
       warning("Problem with variable N11_A6's composition!")
     } else {
       CleanData$partGroup <- NA
       CleanData$partGroup[Data$N11_A6 == 1] <- 1
       CleanData$partGroup[Data$N11_A6 == 2] <- 0.67
       CleanData$partGroup[Data$N11_A6 == 3] <- 0.33
       CleanData$partGroup[Data$N11_A6 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N11_A6")
     }
     
     # N11_A7)  Dans quelle mesure chacune de ces formes d'engagement politique est-elle|nbsp;importante pour vous? 
     # / Contacter mon député
     normN11_A7_1 <- c("1", "2", "3", "4")
     normN11_A7_2 <- c("Tr.s important", "Assez important", "Peu important", "Pas important du tout")
     if(all(names(table(Data$N11_A7))!=normN11_A7_1) |
        all(names(table(Data2$N11_A7))!=normN11_A7_2)) {
       warning("Problem with variable N11_A7's composition!")
     } else {
       CleanData$partContactDep <- NA
       CleanData$partContactDep[Data$N11_A7 == 1] <- 1
       CleanData$partContactDep[Data$N11_A7 == 2] <- 0.67
       CleanData$partContactDep[Data$N11_A7 == 3] <- 0.33
       CleanData$partContactDep[Data$N11_A7 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N11_A7")
     }
     
     # N11_A8)  Dans quelle mesure chacune de ces formes d'engagement politique est-elle|nbsp;importante pour vous? 
     # / Se porter candidat lors d'une élection
     normN11_A8_1 <- c("1", "2", "3", "4")
     normN11_A8_2 <- c("Tr.s important", "Assez important", "Peu important", "Pas important du tout")
     if(all(names(table(Data$N11_A8))!=normN11_A8_1) |
        all(names(table(Data2$N11_A8))!=normN11_A8_2)) {
       warning("Problem with variable N11_A8's composition!")
     } else {
       CleanData$partCandidat <- NA
       CleanData$partCandidat[Data$N11_A8 == 1] <- 1
       CleanData$partCandidat[Data$N11_A8 == 2] <- 0.67
       CleanData$partCandidat[Data$N11_A8 == 3] <- 0.33
       CleanData$partCandidat[Data$N11_A8 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N11_A8")
     }
     
     # N12)  Quel est le moyen le plus efficace pour vous rejoindre lorsqu’un parti politique veut vous informer sur ses idées?
     normN12_1 <- c("1", "2", "3", "4", "5", "6", "7", "8")
     normN12_2 <- c("Au t.l.phone", ". votre porte en personne", "Par la poste", ". la t.l.vision", "Par courriel", "Sur les m.dias sociaux", ". la radio",  "Dans les journaux")
     if(all(names(table(Data$N12))!=normN12_1) |
        all(names(table(Data2$N12))!=normN12_2)){
       warning("Problem with variable N12's composition!")
     } else {
       CleanData$meetParty_tel <- NA
       CleanData$meetParty_tel[Data$N12 == 1] <- 1 # Au téléphone 
       CleanData$meetParty_tel[Data$N12 != 1] <- 0 
       table(CleanData$meetParty_tel)
       
       CleanData$meetParty_door <- NA
       CleanData$meetParty_door[Data$N12 == 2] <- 1 # À votre porte en personne
       CleanData$meetParty_door[Data$N12 != 2] <- 0 
       table(CleanData$meetParty_door) 
       
       CleanData$meetParty_post <- NA
       CleanData$meetParty_post[Data$N12 == 3] <- 1 # Par la poste
       CleanData$meetParty_post[Data$N12 != 3] <- 0 
       table(CleanData$meetParty_post)
       
       CleanData$meetParty_tv <- NA
       CleanData$meetParty_tv[Data$N12 == 4] <- 1 # À la télévision
       CleanData$meetParty_tv[Data$N12 != 4] <- 0 
       table(CleanData$meetParty_tv)
       
       CleanData$meetParty_email <- NA
       CleanData$meetParty_email[Data$N12 == 5] <- 1 # Par courriel
       CleanData$meetParty_email[Data$N12 != 5] <- 0 
       table(CleanData$meetParty_email)
       
       CleanData$meetParty_media <- NA
       CleanData$meetParty_media[Data$N12 == 6] <- 1 # Sur les médias sociaux
       CleanData$meetParty_media[Data$N12 != 6] <- 0 
       table(CleanData$meetParty_media)
       
       CleanData$meetParty_radio <- NA
       CleanData$meetParty_radio[Data$N12 == 7] <- 1 # À la radio
       CleanData$meetParty_radio[Data$N12 != 7] <- 0 
       table(CleanData$meetParty_radio)
       
       CleanData$meetParty_journal <- NA
       CleanData$meetParty_journal[Data$N12 == 8] <- 1 # Dans les journaux
       CleanData$meetParty_journal[Data$N12 != 8] <- 0 
       table(CleanData$meetParty_journal)
       
       toBeRemoved <- c(toBeRemoved, "N12")
     }
     
     
     # N14)  Dans quel but principal les partis politiques récoltent-ils  des données sur les électeurs?
     normN14_1 <- c("1", "2", "3", "4", "5", "6")
     normN14_2 <- c("Pour transmettre des messages personnalis.s aux .lecteurs", "Pour tenter de modifier vos opinions", "Pour inciter les .lecteurs . aller voter pour eux", "Pour vous manipuler", "Pour conna.tre les id.es les plus populaires", "Je ne sais pas" )
     if(all(names(table(Data$N14))!=normN14_1) |
        all(names(table(Data2$N14))!=normN14_2)){
       warning("Problem with variable N14's composition!")
     } else {
       CleanData$recolParty_messPerso <- NA
       CleanData$recolParty_messPerso[Data$N14 == 1] <- 1 # Pour transmettre des messages personnalisés aux électeurs 
       CleanData$recolParty_messPerso[Data$N14 != 1] <- 0 
       CleanData$recolParty_messPerso[Data$N14 == 6] <- NA
       table(CleanData$recolParty_messPerso)
     
       CleanData$recolParty_modifOp <- NA
       CleanData$recolParty_modifOp[Data$N14 == 2] <- 1 # Pour tenter de modifier vos opinions  
       CleanData$recolParty_modifOp[Data$N14 != 2] <- 0 
       CleanData$recolParty_modifOp[Data$N14 == 6] <- NA
       table(CleanData$recolParty_modifOp)
       
       CleanData$recolParty_incitElect <- NA
       CleanData$recolParty_incitElect[Data$N14 == 3] <- 1 # Pour inciter les électeurs à aller voter pour eux  
       CleanData$recolParty_incitElect[Data$N14 != 3] <- 0 
       CleanData$recolParty_incitElect[Data$N14 == 6] <- NA
       table(CleanData$recolParty_incitElect)
       
       CleanData$recolParty_manipule <- NA
       CleanData$recolParty_manipule[Data$N14 == 4] <- 1 # Pour vous manipuler  
       CleanData$recolParty_manipule[Data$N14 != 4] <- 0 
       CleanData$recolParty_manipule[Data$N14 == 6] <- NA
       table(CleanData$recolParty_manipule)
       
       CleanData$recolParty_knowPop <- NA
       CleanData$recolParty_knowPop[Data$N14 == 5] <- 1 # Pour connaître les idées les plus populaires  
       CleanData$recolParty_knowPop[Data$N14 != 5] <- 0 
       CleanData$recolParty_knowPop[Data$N14 == 6] <- NA
       table(CleanData$recolParty_knowPop)
       
       toBeRemoved <- c(toBeRemoved, "N14")
     }
     
     # N15)  Faites-vous confiance aux partis politiques pour représenter vos intérêts?
     normN15_1 <- c("1", "2", "3", "4", "5")
     normN15_2 <- c("1 - le moins confiance", "2", "3","4", "5 - le plus confiance")
     if(all(names(table(Data$N15))!=normN15_1) |
        all(names(table(Data2$N15))!=normN15_2)){
       warning("Problem with variable N15's composition!")
     } else {
       CleanData$trustParty_interest <- NA
       CleanData$trustParty_interest[Data$N15 == 1] <- 0
       CleanData$trustParty_interest[Data$N15 == 2] <- 0.25
       CleanData$trustParty_interest[Data$N15 == 3] <- 0.50
       CleanData$trustParty_interest[Data$N15 == 4] <- 0.75
       CleanData$trustParty_interest[Data$N15 == 5] <- 1
       toBeRemoved <- c(toBeRemoved, "N15")
     }
     
     # N16)  Faites-vous confiance aux partis politiques pour protéger les données personnelles qu’ils récoltent sur les électeurs?
     normN16_1 <- c("1", "2", "3", "4", "5")
     normN16_2 <- c("1 - le moins confiance", "2", "3","4", "5 - le plus confiance")
     if(all(names(table(Data$N16))!=normN16_1) |
        all(names(table(Data2$N16))!=normN16_2)){
       warning("Problem with variable N16's composition!")
     } else {
       CleanData$trustParty_protectData <- NA
       CleanData$trustParty_protectData[Data$N16 == 1] <- 0
       CleanData$trustParty_protectData[Data$N16 == 2] <- 0.25
       CleanData$trustParty_protectData[Data$N16 == 3] <- 0.50
       CleanData$trustParty_protectData[Data$N16 == 4] <- 0.75
       CleanData$trustParty_protectData[Data$N16 == 5] <- 1
       toBeRemoved <- c(toBeRemoved, "N16")
     }
    
     
     #  N17)  L’État a développé différentes plateformes pour permettre aux citoyens d’accéder à des données gouvernementales sur Internet. 
     # C’est ce qu’on appelle des données ouvertes. 
     # \r\nDans quelle mesure êtes-vous en accord avec l’énoncé suivant : / L'accès à un
     normN17_1 <- c("1", "2", "3", "4")
     normN17_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord" )
     if(all(names(table(Data$N17))!=normN17_1) |
        all(names(table(Data2$N17))!=normN17_2)){
       warning("Problem with variable N17's composition!")
     } else {
       CleanData$agreeState_publicData <- NA
       CleanData$agreeState_publicData[Data$N17 == 1] <- 1
       CleanData$agreeState_publicData[Data$N17 == 2] <- 0.67
       CleanData$agreeState_publicData[Data$N17 == 3] <- 0.33
       CleanData$agreeState_publicData[Data$N17 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N17")
     }
     
     # N18_A1)  Dans quelle mesure êtes-vous en accord avec les énoncés suivants? 
     # / L'intégration du numérique dans l'administration publique permet de développer des services publics plus rapides et plus personnalisés.
     normN18_A1_1 <- c("1", "2", "3", "4")
     normN18_A1_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord" )
     if(all(names(table(Data$N18_A1))!=normN18_A1_1) |
        all(names(table(Data2$N18_A1))!=normN18_A1_2)){
       warning("Problem with variable N18_A1's composition!")
     } else {
       CleanData$percepNumeric_admin <- NA
       CleanData$percepNumeric_admin[Data$N18_A1 == 1] <- 1
       CleanData$percepNumeric_admin[Data$N18_A1 == 2] <- 0.67
       CleanData$percepNumeric_admin[Data$N18_A1 == 3] <- 0.33
       CleanData$percepNumeric_admin[Data$N18_A1 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N18_A1")
     }
     
     # N18_A2)  Dans quelle mesure êtes-vous en accord avec les énoncés suivants? 
     # / L'État doit jouer un rôle plus important dans la protection des données personnelles.
     normN18_A2_1 <- c("1", "2", "3", "4")
     normN18_A2_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord" )
     if(all(names(table(Data$N18_A2))!=normN18_A2_1) |
        all(names(table(Data2$N18_A2))!=normN18_A2_2)){
       warning("Problem with variable N18_A2's composition!")
     } else {
       CleanData$state_protectData <- NA
       CleanData$state_protectData[Data$N18_A2 == 1] <- 1
       CleanData$state_protectData[Data$N18_A2 == 2] <- 0.67
       CleanData$state_protectData[Data$N18_A2 == 3] <- 0.33
       CleanData$state_protectData[Data$N18_A2 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N18_A2")
     }
     
     # "N18_A3)  Dans quelle mesure êtes-vous en accord avec les énoncés suivants? 
     # / Il serait plus simple d'avoir une seule identité numérique au gouvernement que plusieurs documents (une carte d'assurance sociale, une carte d'assurance maladie, un permis de cond"
     normN18_A3_1 <- c("1", "2", "3", "4")
     normN18_A3_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord")
     if(all(names(table(Data$N18_A3))!=normN18_A3_1) |
        all(names(table(Data2$N18_A3))!=normN18_A3_2)){
       warning("Problem with variable N18_A3's composition!")
     } else {
       CleanData$numId <- NA
       CleanData$numId[Data$N18_A3 == 1] <- 1
       CleanData$numId[Data$N18_A3 == 2] <- 0.67
       CleanData$numId[Data$N18_A3 == 3] <- 0.33
       CleanData$numId[Data$N18_A3 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N18_A3")
     }
     
     # N18_A4)  Dans quelle mesure êtes-vous en accord avec les énoncés suivants? 
     # / Je fais confiance à l'état pour bien gérer les données qu'il récolte.
     normN18_A4_1 <- c("1", "2", "3", "4")
     normN18_A4_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord")
     if(all(names(table(Data$N18_A4))!=normN18_A4_1) |
        all(names(table(Data2$N18_A4))!=normN18_A4_2)){
       warning("Problem with variable N18_A4's composition!")
     } else {
       CleanData$trustStateGest_data <- NA
       CleanData$trustStateGest_data[Data$N18_A4 == 1] <- 1
       CleanData$trustStateGest_data[Data$N18_A4 == 2] <- 0.67
       CleanData$trustStateGest_data[Data$N18_A4 == 3] <- 0.33
       CleanData$trustStateGest_data[Data$N18_A4 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N18_A4")
     }
     # N18_A5)  Dans quelle mesure êtes-vous en accord avec les énoncés suivants? 
     # / Les médias sociaux minent la démocratie car ils favorisent la désinformation.
     normN18_A5_1 <- c("1", "2", "3", "4")
     normN18_A5_2 <- c("Tr.s en accord", "Plut.t en accord", "Plut.t en d.saccord", "Tr.s en d.saccord")
     if(all(names(table(Data$N18_A5))!=normN18_A5_1) |
        all(names(table(Data2$N18_A5))!=normN18_A5_2)){
       warning("Problem with variable N18_A5's composition!")
     } else {
       CleanData$socialMedias_breakDem <- NA
       CleanData$socialMedias_breakDem[Data$N18_A5 == 1] <- 1
       CleanData$socialMedias_breakDem[Data$N18_A5 == 2] <- 0.67
       CleanData$socialMedias_breakDem[Data$N18_A5 == 3] <- 0.33
       CleanData$socialMedias_breakDem[Data$N18_A5 == 4] <- 0
       toBeRemoved <- c(toBeRemoved, "N18_A5")
     }
     # N19)  Votre niveau de confiance envers la démocratie québécoise est :
     normN19_1 <- c("1", "2", "3")
     normN19_2 <- c("Plus .lev. qu.il y a cinq ans", "Le m.me qu.il y a cinq ans", "Moins .lev. qu.il y a cinq ans")
     if(all(names(table(Data$N19))!=normN19_1) |
        all(names(table(Data2$N19))!=normN19_2)){
       warning("Problem with variable N19's composition!")
     } else {
       CleanData$trustDemocratyQc <- NA
       CleanData$trustDemocratyQc[Data$N19 == 1] <- 1
       CleanData$trustDemocratyQc[Data$N19 == 2] <- 0.5
       CleanData$trustDemocratyQc[Data$N19 == 3] <- 0
       toBeRemoved <- c(toBeRemoved, "N19")
     }
     
     # N21)  Quelle serait votre réaction face à une fuite de données personnelles récoltées par un parti politique?
     normN21_1 <- c("1", "2", "3")
     normN21_2 <- c("Je ne voterais plus pour ce parti.", "Cela me laisserait indiff.rent.", "Cela diminuerait ma confiance envers tous les partis.")
     if(all(names(table(Data$N21))!=normN21_1) |
        all(names(table(Data2$N21))!=normN21_2)){
       warning("Problem with variable N21's composition!")
     } else {
       CleanData$trustParty_DataLeakEndvote <- NA
       CleanData$trustParty_DataLeakEndvote[Data$N21 == 1] <- 1 # Je ne voterais plus pour ce parti.
       CleanData$trustParty_DataLeakEndvote[Data$N21 != 1] <- 0
       table(CleanData$trustParty_DataLeakEndvote)
       
       CleanData$trustParty_DataLeakIndif <- NA
       CleanData$trustParty_DataLeakIndif[Data$N21 == 2] <- 1 # Cela me laisserait indifférent.
       CleanData$trustParty_DataLeakIndif[Data$N21 != 2] <- 0 
       table(CleanData$trustParty_DataLeakIndif)
       
       CleanData$trustParty_DataLeakDimTrust <- NA
       CleanData$trustParty_DataLeakDimTrust[Data$N21 == 3] <- 1 # Cela diminuerait ma confiance envers tous les partis.
       CleanData$trustParty_DataLeakDimTrust[Data$N21 != 3] <- 0 
       table(CleanData$trustParty_DataLeakDimTrust)
       
       toBeRemoved <- c(toBeRemoved, "N21")
     } 
     
     # N22)  Selon vous, le risque que des données personnelles récoltées 
     # par un parti politique soient utilisées lorsque ce parti formera le gouvernement est...
     normN22_1 <- c("1", "2", "3", "4", "5")
     normN22_2 <- c("Nul", "Faible", "Moyen", ".lev.", "Je ne sais pas")
     if(all(names(table(Data$N22))!=normN22_1) |
        all(names(table(Data2$N22))!=normN22_2)){
       warning("Problem with variable N22's composition!")
     } else {
       CleanData$riskData_usedWinner <- NA
       CleanData$riskData_usedWinner[Data$N22 == 4] <- 1
       CleanData$riskData_usedWinner[Data$N22 == 3] <- 0.67
       CleanData$riskData_usedWinner[Data$N22 == 2] <- 0.33
       CleanData$riskData_usedWinner[Data$N22 == 1] <- 0
       CleanData$riskData_usedWinner[Data$N22 == 5] <- NA
       toBeRemoved <- c(toBeRemoved, "N22")
     }
     
     # N23)  Selon vous, le risque que des données personnelles récoltées par des gouvernements 
     # soient utilisés par des partis politiques à des fins partisanes est...
     normN23_1 <- c("1", "2", "3", "4", "5")
     normN23_2 <- c("Nul", "Faible", "Moyen", ".lev.", "Je ne sais pas")
     if(all(names(table(Data$N23))!=normN23_1) |
        all(names(table(Data2$N23))!=normN23_2)){
       warning("Problem with variable N23's composition!")
     } else {
       CleanData$riskData_usedPartisan <- NA
       CleanData$riskData_usedPartisan[Data$N23 == 4] <- 1
       CleanData$riskData_usedPartisan[Data$N23 == 3] <- 0.67
       CleanData$riskData_usedPartisan[Data$N23 == 2] <- 0.33
       CleanData$riskData_usedPartisan[Data$N23 == 1] <- 0
       CleanData$riskData_usedPartisan[Data$N23 == 5] <- NA
       toBeRemoved <- c(toBeRemoved, "N23")
     }
     
     # N24)  Avec la pandémie, le Parlement a intégré davantage d’activités en ligne dans ses travaux. 
     # Quel est l’effet de ce changement sur votre niveau de confiance envers cette institution?
     normN24_1 <- c("1", "2", "3", "4")
     normN24_2 <- c("Positif", "Aucun", "N.gatif", "Je ne savais pas que le Parlement avait des activit.s en lig")
     if(all(names(table(Data$N24))!=normN24_1) |
        all(names(table(Data2$N24))!=normN24_2)){
       warning("Problem with variable N24's composition!")
     } else { 
       CleanData$changeTrustOnlineParlement <- NA
       CleanData$changeTrustOnlineParlement[Data$N24 == 1] <- 1
       CleanData$changeTrustOnlineParlement[Data$N24 == 2] <- 0.5
       CleanData$changeTrustOnlineParlement[Data$N24 == 3] <- 0
       CleanData$changeTrustOnlineParlement[Data$N24 == 4] <- NA
       toBeRemoved <- c(toBeRemoved, "N24")
     }
     
     # N25)  Quel est votre niveau de confiance envers la sécurité du vote en ligne? 
     normN25_1 <- c("1", "2", "3")
     normN25_2 <- c(".lev.", "Moyen", "Faible")
     if(all(names(table(Data$N25))!=normN25_1) |
        all(names(table(Data2$N25))!=normN25_2)){
       warning("Problem with variable N25's composition!")
     } else { 
       CleanData$trustOnlineVote <- NA
       CleanData$trustOnlineVote[Data$N25 == 1] <- 1
       CleanData$trustOnlineVote[Data$N25 == 2] <- 0.50
       CleanData$trustOnlineVote[Data$N25 == 3] <- 0
       toBeRemoved <- c(toBeRemoved, "N25")
     }
     
     
     
#### Nadjim ####     
     
# C2_A1)  Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne, en général, quelle est la probabilité que vous appuyiez... / ...le Parti conservateur du Canada
     normC2_A1_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC2_A1_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C2_A1))!=normC2_A1_1) |
        all(names(table(Data2$C2_A1))!=normC2_A1_2)){
       warning("Problem with variable ", CodeBook[92,1], "'s composition!")
     } else {
       CleanData$potGrowthPLC <- Data$C2_A1/10
       toBeRemoved <- c(toBeRemoved, "C2_A1")
     }
     
# C2_A2) Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne, en général, quelle est la probabilité que vous appuyiez... / ...le Parti conservateur du Canada
     normC2_A2_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC2_A2_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C2_A2))!=normC2_A2_1) |
        all(names(table(Data2$C2_A2))!=normC2_A2_2)){
       warning("Problem with variable ",  CodeBook[93,1], "'s composition!")
     } else {
       CleanData$potGrowthPCC <- Data$C2_A2/10
       toBeRemoved <- c(toBeRemoved,  "C2_A2")
     }
     
# C2_A3) Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne, en général, quelle est la probabilité que vous appuyiez... / ...le Nouveau parti démocratique
     normC2_A3_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC2_A3_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C2_A3))!=normC2_A3_1) |
        all(names(table(Data2$C2_A3))!=normC2_A3_2)){
       warning("Problem with variable ", CodeBook[94,1], "'s composition!")
     } else {
       CleanData$potGrowthNPD <- Data$C2_A3/10
       toBeRemoved <- c(toBeRemoved, "C2_A3")
     }
# C2_A4) Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne, en général, quelle est la probabilité que vous appuyiez... / ...le Bloc Québécois
     normC2_A4_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC2_A4_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C2_A4))!=normC2_A4_1) |
        all(names(table(Data2$C2_A4))!=normC2_A4_2)){
       warning("Problem with variable ", CodeBook[95,1], "'s composition!")
     } else {
       CleanData$potGrowthBQ <- Data$C2_A4/10
       toBeRemoved <- c(toBeRemoved, "C2_A4")
     }

# C2_A5) Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne, en général, quelle est la probabilité que vous appuyiez... / ...le Parti populaire du Canada
     normC2_A5_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC2_A5_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C2_A5))!=normC2_A5_1) |
        all(names(table(Data2$C2_A5))!=normC2_A5_2)){
       warning("Problem with variable ", CodeBook[96,1], "'s composition!")
     } else {
       CleanData$potGrowthPPC <- Data$C2_A5/10
       toBeRemoved <- c(toBeRemoved, "C2_A5")
     }

# C2_A6 Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne, en général, quelle est la probabilité que vous appuyiez... / ...le Parti vert du Canada
     normC2_A6_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC2_A6_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C2_A6))!=normC2_A6_1) |
        all(names(table(Data2$C2_A6))!=normC2_A6_2)){
       warning("Problem with variable ", CodeBook[97,1], "'s composition!")
     } else {
       CleanData$potGrowthPVC <- Data$C2_A6/10
       toBeRemoved <- c(toBeRemoved, "C2_A6") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
## dplyr essai pour all C2  (Ca marche, je t'expliquerai Will)
#      colnameOldC2 <- colnames(select(Data, starts_with("C2")))
#      colnameNewC2 <- c("potGrowthPLC", "potGrowthPCC", "potGrowthNPD", "potGrowthBQ", "potGrowthPPC", "potGrowthPVC")
#      normC2_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
#      normC2_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
#      if(all(names(table(Data[,92]))  !=normC2_1) |
#         all(names(table(Data2[,92])) !=normC2_2) |
#         all(names(table(Data[,93]))  !=normC2_1) |
#         all(names(table(Data2[,93])) !=normC2_2) |
#         all(names(table(Data[,94]))  !=normC2_1) |
#         all(names(table(Data2[,94])) !=normC2_2) |
#         all(names(table(Data[,95]))  !=normC2_1) |
#         all(names(table(Data2[,95])) !=normC2_2) |
#         all(names(table(Data[,96]))  !=normC2_1) |
#         all(names(table(Data2[,96])) !=normC2_2)){
#         warning("Problem with variables' composition!")
#         } else {
# Data_c2 <- Data %>%
#   select(mergeId, starts_with("C2")) %>%
#   mutate(across(c(C2_A1:C2_A6), ~ .x/10)) %>%
#   rename_at(vars(colnameOldC2), ~ colnameNewC2)
# ## Set variable in new dataset ##
# # CleanData <- Data %>%
# #   left_join(Data_c2) # Il faudrait un merging ID
# }
     
# C3_A1 Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise, en général, quelle est la probabilité que vous appuyiez... / la Coalition avenir Québec   
     normC3_A1_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC3_A1_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C3_A1))!=normC3_A1_1) |
        all(names(table(Data2$C3_A1))!=normC3_A1_2)){
       warning("Problem with variable ", CodeBook[98,1], "'s composition!")
     } else {
       CleanData$potGrowthCAQ <- Data$C3_A1/10
       toBeRemoved <- c(toBeRemoved, "C3_A1") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C3_A2 Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise, en général, quelle est la probabilité que vous appuyiez... / le Parti libéral du Québec
     normC3_A2_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC3_A2_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C3_A2))!=normC3_A2_1) |
        all(names(table(Data2$C3_A2))!=normC3_A2_2)){
       warning("Problem with variable ", CodeBook[99,1], "'s composition!")
     } else {
       CleanData$potGrowthPLQ <- Data$C3_A2/10
       toBeRemoved <- c(toBeRemoved, "C3_A2") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
    
# C3_A3 Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise, en général, quelle est la probabilité que vous appuyiez... / le Parti Québecois
     normC3_A3_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC3_A3_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C3_A3))!=normC3_A3_1) |
        all(names(table(Data2$C3_A3))!=normC3_A3_2)){
       warning("Problem with variable ", CodeBook[100,1], "'s composition!")
     } else {
       CleanData$potGrowthPQ <- Data$C3_A3/10
       toBeRemoved <- c(toBeRemoved, "C3_A3") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C3_A4 Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise, en général, quelle est la probabilité que vous appuyiez... / Québec solidaire
     normC3_A4_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC3_A4_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C3_A4))!=normC3_A4_1) |
        all(names(table(Data2$C3_A4))!=normC3_A4_2)){
       warning("Problem with variable ", CodeBook[101,1], "'s composition!")
     } else {
       CleanData$potGrowthQS <- Data$C3_A4/10
       toBeRemoved <- c(toBeRemoved, "C3_A4") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C3_A5 Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise, en général, quelle est la probabilité que vous appuyiez... / ...le Parti conservateur du Québec
     normC3_A5_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC3_A5_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C3_A5))!=normC3_A5_1) |
        all(names(table(Data2$C3_A5))!=normC3_A5_2)){
       warning("Problem with variable ", CodeBook[102,1], "'s composition!")
     } else {
       CleanData$potGrowthPCQ <- Data$C3_A5/10
       toBeRemoved <- c(toBeRemoved, "C3_A5") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C5_A1 Sur une échelle de 0 à 10, comment est votre humeur ces temps-ci?"
     normC5_A1_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC5_A1_2 <- c("0 - pire humeur","1","2","3","4","5","6","7","8","9","10 - meilleure humeur")
     if(all(names(table(Data$C5_A1))!=normC5_A1_1) |
        all(names(table(Data2$C5_A1))!=normC5_A1_2)){
       warning("Problem with variable ", CodeBook[103,1], "'s composition!")
     } else {
       CleanData_pess_badMood <- Data %>%
         select(id, C5_A1) %>%
         mutate(pess_badMood = case_when(C5_A1 == 0  ~ 1,
                                         C5_A1 == 1  ~ 0.9,
                                         C5_A1 == 2  ~ 0.8,
                                         C5_A1 == 3  ~ 0.7,
                                         C5_A1 == 4  ~ 0.6,
                                         C5_A1 == 5  ~ 0.5,
                                         C5_A1 == 6  ~ 0.4,
                                         C5_A1 == 7  ~ 0.3,
                                         C5_A1 == 8  ~ 0.2,
                                         C5_A1 == 9  ~ 0.1,
                                         C5_A1 == 10 ~ 0)) %>%
         select(-C5_A1)
       CleanData <- CleanData %>%
         left_join(CleanData_pess_badMood)
       toBeRemoved <- c(toBeRemoved, "C5_A1") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C6_A1 Veuillez indiquer à quel point vous êtes en accord ou en désaccord avec les énoncés suivants.Veuillez répondre en fonction de ce que vous ressentez maintenant, c'est-à-dire au moment présent. / L'avenir s'annonce sombre
     normC6_A1_1 <- c("1","2","3","4","5")
     normC6_A1_2 <- c("Fortement en accord","Plut.t en accord","Neutre","Plut.t en d.saccord","Fortement en d.saccord")
     if(all(names(table(Data$C6_A1))!=normC6_A1_1) |
         all(names(table(Data2$C6_A1))!=normC6_A1_2)){
       warning("Problem with variable ", CodeBook[104,1], "'s composition!")
     } else {
       CleanData_pess_futureLooksBleak <- Data %>%
         select(id, C6_A1) %>%
         mutate(pess_futureLooksBleak = case_when(C6_A1 == 1  ~ 1,
                                                  C6_A1 == 2  ~ 0.75,
                                                  C6_A1 == 3  ~ 0.5,
                                                  C6_A1 == 4  ~ 0.25,
                                                  C6_A1 == 5  ~ 0)) %>%
         select(-C6_A1)
       CleanData <- CleanData %>%
         left_join(CleanData_pess_futureLooksBleak)
       toBeRemoved <- c(toBeRemoved, "C6_A1") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }

# C6_A2 Veuillez indiquer à quel point vous êtes en accord ou en désaccord avec les énoncés suivants.Veuillez répondre en fonction de ce que vous ressentez maintenant, c'est-à-dire au moment présent. / L'humanité est mise en danger par des catastrophes à v
     normC6_A2_1 <- c("1","2","3","4","5")
     normC6_A2_2 <- c("Fortement en accord","Plut.t en accord","Neutre","Plut.t en d.saccord","Fortement en d.saccord")
     if(all(names(table(Data$C6_A2))!=normC6_A2_1) |
        all(names(table(Data2$C6_A2))!=normC6_A2_2)){
       warning("Problem with variable ", CodeBook[105,1], "'s composition!")
     } else {
       CleanData_pess_humanInDanger <- Data %>%
         select(id, C6_A2) %>% 
         mutate(pess_humanInDanger = case_when(C6_A2 == 1  ~ 1,
                                               C6_A2 == 2  ~ 0.75,
                                               C6_A2 == 3  ~ 0.5,
                                               C6_A2 == 4  ~ 0.25,
                                               C6_A2 == 5  ~ 0)) %>%
         select(-C6_A2)
       CleanData <- CleanData %>%
         left_join(CleanData_pess_humanInDanger)
       toBeRemoved <- c(toBeRemoved, "C6_A2") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C6_A3  Veuillez indiquer à quel point vous êtes en accord ou en désaccord avec les énoncés suivants.Veuillez répondre en fonction de ce que vous ressentez maintenant, c'est-à-dire au moment présent. / Je crains qu'une nouvelle vague d'infections soit auss
     normC6_A3_1 <- c("1","2","3","4","5")
     normC6_A3_2 <- c("Fortement en accord","Plut.t en accord","Neutre","Plut.t en d.saccord","Fortement en d.saccord")
     if(all(names(table(Data$C6_A3))!=normC6_A3_1) |
        all(names(table(Data2$C6_A3))!=normC6_A3_2)){
       warning("Problem with variable ", CodeBook[106,1], "'s composition!")
     } else {
       CleanData_pess_secWaveWorse <- Data %>%
         select(id, C6_A3) %>%
         mutate(pess_secWaveWorse = case_when(C6_A3 == 1  ~ 1,
                                              C6_A3 == 2  ~ 0.75,
                                              C6_A3 == 3  ~ 0.5,
                                              C6_A3 == 4  ~ 0.25,
                                              C6_A3 == 5  ~ 0)) %>%
         select(-C6_A3)
       CleanData <- CleanData %>%
         left_join(CleanData_pess_secWaveWorse)
       toBeRemoved <- c(toBeRemoved, "C6_A3") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C6_A4  Veuillez indiquer à quel point vous êtes en accord ou en désaccord avec les énoncés suivants.Veuillez répondre en fonction de ce que vous ressentez maintenant, c'est-à-dire au moment présent. / Le pire de la crise est derrière nous
     normC6_A4_1 <- c("1","2","3","4","5")
     normC6_A4_2 <- c("Fortement en accord","Plut.t en accord","Neutre","Plut.t en d.saccord","Fortement en d.saccord")
     if(all(names(table(Data$C6_A4))!=normC6_A4_1) |
        all(names(table(Data2$C6_A4))!=normC6_A4_2)){
       warning("Problem with variable ", CodeBook[107,1], "'s composition!")
     } else {
       CleanData_pess_worstBehind <- Data %>%
         select(id, C6_A4) %>%
         mutate(pess_worstBehind = case_when(C6_A4 == 5  ~ 1,
                                             C6_A4 == 4  ~ 0.75,
                                             C6_A4 == 3  ~ 0.5,
                                             C6_A4 == 2  ~ 0.25,
                                             C6_A4 == 1  ~ 0)) %>%
         select(-C6_A4)
       CleanData <- CleanData %>%
         left_join(CleanData_pess_worstBehind)
       toBeRemoved <- c(toBeRemoved, "C6_A4") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C6_A5  Veuillez indiquer à quel point vous êtes en accord ou en désaccord avec les énoncés suivants.Veuillez répondre en fonction de ce que vous ressentez maintenant, c'est-à-dire au moment présent. / La COVID-19 menace la survie de l'humanité
     normC6_A5_1 <- c("1","2","3","4","5")
     normC6_A5_2 <- c("Fortement en accord","Plut.t en accord","Neutre","Plut.t en d.saccord","Fortement en d.saccord")
     if(all(names(table(Data$C6_A5))!=normC6_A5_1) |
        all(names(table(Data2$C6_A5))!=normC6_A5_2)){
       warning("Problem with variable ", CodeBook[108,1], "'s composition!")
     } else {
       CleanData_pess_futureLooksBleak <- Data %>%
         select(id, C6_A5) %>%
         mutate(pess_futureLooksBleak = case_when(C6_A5 == 1  ~ 1,
                                                  C6_A5 == 2  ~ 0.75,
                                                  C6_A5 == 3  ~ 0.5,
                                                  C6_A5 == 4  ~ 0.25,
                                                  C6_A5 == 5  ~ 0)) %>%
         select(-C6_A5)
       CleanData <- CleanData %>%
         left_join(CleanData_pess_futureLooksBleak)
       toBeRemoved <- c(toBeRemoved, "C6_A5") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C10_A1  En politique, on parle souvent de gauche et de droite. Sur une échelle de 0 à 10, avec 0 signifiant que vous êtes de gauche et 10 que vous êtes de droite, où vous placeriez-vous?
     normC10_A1_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC10_A1_2 <- c("0 - de gauche","1","2","3","4","5","6","7","8","9","10 - de droite")
     if(all(names(table(Data$C10_A1))!=normC10_A1_1) |
        all(names(table(Data2$C10_A1))!=normC10_A1_2)){
       warning("Problem with variable ", CodeBook[110,1], "'s composition!")
     } else {
       CleanData$rightist <- Data$C10_A1/10
       toBeRemoved <- c(toBeRemoved, "C10_A1") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# C12 Pour quel parti avez-vous voté lors de l’élection fédérale canadienne de 2021?
     normC12_1 <- c("1","2","3","4","5","6","7","8","9","10")
     normC12_2 <- c("Parti lib.ral du Canada","Parti conservateur du Canada","Nouveau Parti d.mocratique","Bloc Qu.b.cois","Parti vert du Canada","Parti populaire du Canada","Un autre parti","e n.ai pas vot.","J.ai annul. mon vote","Je ne me souviens plus")
     if(all(names(table(Data$C12))!=normC12_1) |
        all(names(table(Data2$C12))!=normC12_2)){
       warning("Problem with variable ", CodeBook[111,1], "'s composition!")
     } else {
       CleanData_vote <- Data %>% 
         select(id, C12) %>%
         mutate(votedFedPLC21 = case_when(C12 == 1 ~ 1,
                                          C12 %in% c(2:6) ~ 0),
                voteFedPCC21  = case_when(C12 == 2 ~ 1,
                                          C12 %in% c(1,3:6) ~ 0),
                votedFedNPD21 = case_when(C12 == 3 ~ 1,
                                          C12 %in% c(1:2,4:6) ~ 0),
                votedFedBQ21  = case_when(C12 == 4 ~ 1,
                                          C12 %in% c(1:3,5:6) ~ 0),
                votedFedPVC21 = case_when(C12 == 5 ~ 1,
                                          C12 %in% c(1:4,6) ~ 0),
                votedFedPPC21 = case_when(C12 == 6 ~ 1,
                                          C12 %in% c(1:5) ~ 0)) %>%
         select(-C12)
       CleanData <- CleanData %>%
         left_join(CleanData_vote)
       toBeRemoved <- c(toBeRemoved, "C12") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }

# C13_A1 Selon vous, quelles sont les probabilités que chacun des partis suivants remporte les prochaines élections provinciales québécoises? / ...la Coalition avenir Québec
     normC13_A1_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC13_A1_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C13_A1))!=normC13_A1_1) |
        all(names(table(Data2$C13_A1))!=normC13_A1_2)){
       warning("Problem with variable ", CodeBook[112,1], "'s composition!")
     } else {
       CleanData$potGrowthCAQ <- Data$C13_A1/10
       toBeRemoved <- c(toBeRemoved, "C13_A1")
     }
     
# C13_A2  Selon vous, quelles sont les probabilités que chacun des partis suivants remporte les prochaines élections provinciales québécoises? / ...le Parti libéral du Québec
     normC13_A2_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC13_A2_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C13_A2))!=normC13_A2_1) |
        all(names(table(Data2$C13_A2))!=normC13_A2_2)){
       warning("Problem with variable ", CodeBook[113,1], "'s composition!")
     } else {
       CleanData$potGrowthPLQ <- Data$C13_A2/10
       toBeRemoved <- c(toBeRemoved, "C13_A2")
     }
     
# C13_A3  Selon vous, quelles sont les probabilités que chacun des partis suivants remporte les prochaines élections provinciales québécoises? / ...le Parti Québecois
     normC13_A3_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC13_A3_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C13_A3))!=normC13_A3_1) |
        all(names(table(Data2$C13_A3))!=normC13_A3_2)){
       warning("Problem with variable ", CodeBook[114,1], "'s composition!")
     } else {
       CleanData$potGrowthPQ <- Data$C13_A3/10
       toBeRemoved <- c(toBeRemoved, "C13_A3")
     }
     
# C13_A4  Selon vous, quelles sont les probabilités que chacun des partis suivants remporte les prochaines élections provinciales québécoises? / ...Québec solidaire
     normC13_A4_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC13_A4_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C13_A4))!=normC13_A4_1) |
        all(names(table(Data2$C13_A4))!=normC13_A4_2)){
       warning("Problem with variable ", CodeBook[115,1], "'s composition!")
     } else {
       CleanData$potGrowthQS <- Data$C13_A4/10
       toBeRemoved <- c(toBeRemoved, "C13_A4")
     }
     
# C13_A5  Selon vous, quelles sont les probabilités que chacun des partis suivants remporte les prochaines élections provinciales québécoises? / ...le Parti conservateur du Québec
     normC13_A5_1 <- c("0","1","2","3","4","5","6","7","8","9","10")
     normC13_A5_2 <- c("0 - Tr.s peu probable","1","2","3","4","5","6","7","8","9","10 - Tr.s probable")
     if(all(names(table(Data$C13_A5))!=normC13_A5_1) |
        all(names(table(Data2$C13_A5))!=normC13_A5_2)){
       warning("Problem with variable ", CodeBook[116,1], "'s composition!")
     } else {
       CleanData$potGrowthPCQ <- Data$C13_A5/10
       toBeRemoved <- c(toBeRemoved, "C13_A5")
     }

# SC1 En général, diriez-vous que votre santé mentale est...?
     normSC1_1 <- c("1","2","3","4","5")
     normSC1_2 <- c("Excellente","Tr.s bonne","Bonne","Passable", "Mauvaise")
     if(all(names(table(Data$SC1))!=normSC1_1) |
        all(names(table(Data2$SC1))!=normSC1_2)){
       warning("Problem with variable ", CodeBook[118,1], "'s composition!")
     } else {
       CleanData_mentalHealthGood <- Data %>%
         select(id, SC1) %>%
         mutate(mentalHealthGood = case_when(SC1 == 1  ~ 1,
                                             SC1 == 2  ~ 0.75,
                                             SC1 == 3  ~ 0.5,
                                             SC1 == 4  ~ 0.25,
                                             SC1 == 5  ~ 0)) %>%
         select(-SC1)
       CleanData <- CleanData %>%
         left_join(CleanData_mentalHealthGood)
       toBeRemoved <- c(toBeRemoved, "SC1") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
     
# SC2 otre travail affecte-t-il votre santé psychologique?
     normSC2_1 <- c("1","2","3","4","5", "6")
     normSC2_2 <- c("Oui, de fa.on tr.s n.gative","Oui, de fa.on n.gative","Non, mon travail n'affecte pas ma sant. psychologique","Oui, de fa.on positive", "Oui, de fa.on tr.s positive", "Je n'ai pas d'emploi/d'employeur")
     if(all(names(table(Data$SC2))!=normSC2_1) |
        all(names(table(Data2$SC2))!=normSC2_2)){
       warning("Problem with variable ", CodeBook[119,1], "'s composition!")
     } else {
       CleanData_workNegImpactMental <- Data %>%
         select(id, SC2) %>%
         mutate(workNegImpactMental = case_when(SC2 == 1 ~ 1,
                                                SC2 == 2 ~ 0.75,
                                                SC2 == 3 ~ 0.5,
                                                SC2 == 4 ~ 0.25,
                                                SC2 == 5 ~ 0)) %>%
         select(-SC2)
       CleanData <- CleanData %>%
         left_join(CleanData_workNegImpactMental)
       toBeRemoved <- c(toBeRemoved, "SC2") # Facon d'automatiser ça trop de place à l'erreur (notamment en callant la colonne)
     }
    
#### 3. Removing variables that have already been cleanned ####
rm(list=grep('norm', ls(all.names = TRUE), value=TRUE))
Data <- Data %>% 
  dplyr::select(-toBeRemoved)
Data2<- Data2 %>%
  dplyr::select(-toBeRemoved)



