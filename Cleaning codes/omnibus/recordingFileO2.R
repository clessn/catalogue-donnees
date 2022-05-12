
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

Data  <- read_sav("_SharedFolder-omnibus/2-February2022/data/raw/OMN0222.Sav")
Data2 <- read.spss("_SharedFolder-omnibus/2-February2022/data/raw/OMN0222.Sav",to.data.frame = T,
                   reencode="utf-8")
Post <- readxl::read_excel("_SharedFolder-omnibus/2-February2022/data/raw/omn02-postal_codes.xlsx")
pathToMonth <- "_SharedFolder-omnibus/2-February2022/"

source("codeR/cleaningSource.R")
names(Data)


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
write.csv(CleanData, paste0("_SharedFolder-omnibus/2-February2022/data/Clean/omnibus2_", today, ".csv"))

#### END OF THE DOCUMENT ####

#CleanData$pess_secWaveWorse <- NA
#CleanData$pess_secWaveWorse[Data$C6_A3==1] <- 1
#CleanData$pess_secWaveWorse[Data$C6_A3==2] <- 0.75
#CleanData$pess_secWaveWorse[Data$C6_A3==3] <- 0.5
#CleanData$pess_secWaveWorse[Data$C6_A3==4] <- 0.25
#CleanData$pess_secWaveWorse[Data$C6_A3==5] <- 0
#
#CleanData$pess_worstBehind <- NA
#CleanData$pess_worstBehind[Data$C6_A4==5] <- 1
#CleanData$pess_worstBehind[Data$C6_A4==4] <- 0.75
#CleanData$pess_worstBehind[Data$C6_A4==3] <- 0.5
#CleanData$pess_worstBehind[Data$C6_A4==2] <- 0.25
#CleanData$pess_worstBehind[Data$C6_A4==1] <- 0
#
#CleanData$pess_covidThreatHumans <- NA
#CleanData$pess_covidThreatHumans[Data$C6_A5==1] <- 1
#CleanData$pess_covidThreatHumans[Data$C6_A5==2] <- 0.75
#CleanData$pess_covidThreatHumans[Data$C6_A5==3] <- 0.5
#CleanData$pess_covidThreatHumans[Data$C6_A5==4] <- 0.25
#CleanData$pess_covidThreatHumans[Data$C6_A5==5] <- 0
#
#FactAnal <- CleanData[, c(#"pess_badMood", 
#                          "pess_futureLooksBleak",
#                          #"pess_humanInDanger",
#                          "pess_secWaveWorse","pess_worstBehind","pess_covidThreatHumans")]
#FactAnal <- na.omit(FactAnal) # Getting rid of the missing values
#
## Cronbach's alpha (Test 1)
#cronbachAlpha <- round(psych::alpha(FactAnal)$total$raw_alpha, 2)
#
## Analyse factorielle (Test 2)
#
#factAnalysis <- factanal(FactAnal, factors=1) # Analyse factorielle
#factorVarNames <- c(#"pess_badMood", 
#                    "pess_futureLooksBleak",
#                    #"pess_humanInDanger",
#                    "*pess_secWaveWorse","*pess_worstBehind","*pess_covidThreatHumans")
#
#factorLoadings <- as.numeric(factAnalysis$loadings[,1]) 
#factor1stEigen <- round(eigen(cor(FactAnal))$values[1], digit=2)
#
#
#ggplot(data.frame(factorVarNames,factorLoadings), 
#       aes(x=factorVarNames, y=factorLoadings)) + 
#  coord_flip() +
#  geom_bar(stat="identity", colour="black", fill="black", size=1, width=0.5) +
#  geom_text(aes(label=as.character(round(factorLoadings, 
#                                         digits = 2))), vjust=0.35, hjust=-0.3, size = 8) +
#  geom_hline(yintercept=0.3, colour="gray", linetype = "longdash") +
#  annotate("text", label=paste("Alpha de Cronbach =", as.character(cronbachAlpha)), 
#           x=1.1, y=1.31, size=6.8) +
#  annotate("text", label=paste("Première valeur propre =", as.character(factor1stEigen)), 
#           x=0.75, y=1.31, size=6.8) +
#  annotate("segment", x = 0.4, xend = 1.45, 
#           y = 1, yend = 1, colour = "black") +
#  annotate("segment", x = 1.45, xend = 1.45, 
#           y = 1, yend = Inf, colour = "black") +
#  scale_y_continuous(name="Coefficients de saturation", 
#                     limits=c(0, 1.55), breaks=seq(0, 1, by=0.1)) +
#  xlab("\n") + 
#  theme_linedraw() +
#  theme(axis.text.y = element_text(size=20), 
#        axis.title.y = element_text(size = 20), 
#        axis.text.x = element_text(size = 17),
#        axis.title.x = element_text(hjust=0.3, vjust=-0.17, size=20), 
#        panel.grid=element_blank()) 
#
