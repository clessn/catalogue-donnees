################################# Code book formation ###########################################
# To be used with codeR_sondageCCSA.R
CodeBook <- data.frame(names(data))
CodeBook$variables<- CodeBook$names.data.
CodeBook <- CodeBook %>% dplyr::select(variables)
CodeBook$type <- "SES"
CodeBook$type[72:nrow(CodeBook)]<- "Q"
CodeBook$nbScale<-NA
x<-0
for (i in 72:ncol(data)){
    CodeBook$nbScale[i]<-sum(length(na.omit(unique(data[,i]))))
  x<-x+1
  print(x)
}
CodeBook <- CodeBook %>% dplyr::filter(nbScale > 0) %>% na.omit()
CodeBook$qName <- NA
CodeBook <- CodeBook %>% dplyr::select(variables,nbScale,qName)

write.csv(CodeBook,"_SharedFolder_sondage-ccsa/CodeBook.csv")
