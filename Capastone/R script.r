#rm(list=ls())
#install.packages('car')


#Single Detached Housing 
#graph Data vs Index
attach(Single_Detached_Houseing)
View(Single_Detached_Houseing)
#Analyse of Index 
summary(Index)
#Analyse of Growth (%Y-0-Y)
summary(`Growth (%Y-O-Y)`) 
#graph DatE vs Index
xS<- c(Single_Detached_Houseing$Date)
SingleyS<- c(Single_Detached_Houseing$Index)
  plot(xS, SingleyS,type = "o", col = "red", xlab = "date", ylab = "Index", main = "Single Detached Housing")
#Graph Date vs Growth (%Y-O-Y)
  singley1<- c(`Growth (%Y-O-Y)`)
  plot(x, singley1,type = "o", col = "blue", xlab = "date", ylab = "Growth (%Y-O-Y) ", main = "Single Detached Housing")
 #variance and std for Single Detached Housing
  var(Index)
  sd(Index)
  var(`Growth (%Y-O-Y)`)
  sd(`Growth (%Y-O-Y)`)
#Creating A Table for Index  
  SummarySI<- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','Var','std')
  ValueSI<-c('100.4','108.3','121.7','119.6','129.2','138.1','123.5621','11.11585')
  TableSI<- data.frame(SummarySI, ValueSI)
  View(TableSI)
  
#Creating A Table for Growth
  SummarySG<- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','Var','std')
  ValueSG<-c('-3.200','1.100','3.600','3.568','5.800','10.600','9.455322','3.074951')
  TableSG<- data.frame(SummarySG, ValueSG)
  View(TableSG)
  
  #Addng regression line for Single_Detached_Houseing
  plot(Single_Detached_Houseing$Index,Single_Detached_Houseing$`Growth (%Y-O-Y)`, main = 'Regression for Single_Detached_Houseing', xlab = 'index',ylab = 'growth')
  abline(lm(`Growth (%Y-O-Y)`~ Index,data = land), col = 'red')  
  
#Town House Data
  attach(Town_House_including_land_)
  View(Town_House_including_land_)
  #Analyse of Index 
  summary(Index) 
  #Analyse of Growth (%Y-0-Y)
  summary(`Growth (%Y-O-Y)`) 
  #Town House
  #graph Data vs Index
  x<- c(Date)
 Towny<- c(Index)
  plot(x, Towny,type = "o", col = "red", xlab = "date", ylab = "Index", main = "Town House (including land)")
  #Graph Data vs Growth (%Y-O-Y)
  Towny1<- c(`Growth (%Y-O-Y)`)
  plot(x, Towny1,type = "o", col = "blue", xlab = "date", ylab = "Growth (%Y-O-Y) ", main = "Town House (including land)")
  #variance and std 
  var(Index)
  sd(Index)
  var(`Growth (%Y-O-Y)`)
  sd(`Growth (%Y-O-Y)`)
  
  #Creating A Table for Index  
  SummaryTI<- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','Var','std')
  ValueTI<-c('97.7','105.8','124.9','121.9','129.2','135.9','239.5359','15.47695')
  TableTI<- data.frame(SummaryTI, ValueTI)
  View(TableTI)
  
  #Creating A Table for Growth
  SummaryTG<- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','Var','std')
  ValueTG<-c('-1.400 ','2.300','3.800','4.659','7.200','14.000','13.43828','3.665826')
  TableTG<- data.frame(SummaryTG, ValueTG)
  View(TableTG)
  
 #Addng regression line for Town_House_including_land_
  plot(Town_House_including_land_$Index,Town_House_including_land_$`Growth (%Y-O-Y)`, main = 'Regression for Town_House_including_land_', xlab = 'index',ylab = 'growth')
  abline(lm(`Growth (%Y-O-Y)`~ Index,data = land), col = 'red')  

#Condo
  attach(Condominium)
  View(Condominium)
  #Analyse of Index 
  summary(Index)
  #Condominium
  #Analyse of Growth (%Y-0-Y)
  summary(`Growth (%Y-O-Y)`) 
  #Condominium
  #graph Data vs Index
  x<- c(Date)
 Condoy<- c(Index)
  plot(x,  Condoy,type = "o", col = "red", xlab = "date", ylab = "Index", main = "Condominium")
  #Graph Data vs Growth (%Y-O-Y)
 Condoy1<- c(`Growth (%Y-O-Y)`)
  plot(x, Condoy1,type = "o", col = "blue", xlab = "date", ylab = "Growth (%Y-O-Y) ", main = "Condominium")
 #variance and std 
  var(Index)
  sd(Index)
  var(`Growth (%Y-O-Y)`)
  sd(`Growth (%Y-O-Y)`)
  
  #Creating A Table for Index  
  SummaryCI<- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','Var','std')
  ValueCI<-c('112.4','126.0','139.8','141.8','160.8','181.6','427.0431','20.66502')
  TableCI<- data.frame(SummaryCI, ValueCI)
  View(TableCI)
  
  #Creating A Table for Growth
  SummaryCG<- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','Var','std')
  ValueCG<-c('-3.600 ','2.900','6.100','5.903','8.400','14.400','15.42572','3.927559')
  TableCG<- data.frame(SummaryCG, ValueCG)
  View(TableCG)
  
  #Addng regression line for Condominium
  plot(Condominium$Index,Condominium$`Growth (%Y-O-Y)`, main = 'Regression for Condominium', xlab = 'index',ylab = 'growth')
  abline(lm(`Growth (%Y-O-Y)`~ Index,data = land), col = 'red')  
  
#land
  attach(land)
  View(land)
  #Analyse of Index 
  summary(Index)
  #land
  #Analyse of Growth (%Y-0-Y)
  summary(`Growth (%Y-O-Y)`) 
  #land
  #graph Data vs Index
  x<- c(Date)
  Landy<- c(Index)
  plot(x, Landy,type = "o", col = "red", xlab = "date", ylab = "Index", main = "land")
  #Graph Data vs Growth (%Y-O-Y)
  landy1<- c(`Growth (%Y-O-Y)`)
  plot(x,  landy1,type = "o", col = "blue", xlab = "date", ylab = "Growth (%Y-O-Y) ", main = "land")
  #variance and std 
  var(Index)
  sd(Index)
  var(`Growth (%Y-O-Y)`)
  sd(`Growth (%Y-O-Y)`)
  
  #Creating A Table for Index  
  SummaryLI<- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','Var','std')
  ValueLI<-c('102.8','116.7','137.7','139.3','167.1','178.3','594.0412','24.37296')
  TableLI<- data.frame(SummaryLI, ValueLI)
  View(TableLI)
  
  #Creating A Table for Growth
  SummaryLG<- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','Var','std')
  ValueLG<-c('-5.500 ','1.900','6.293','9.900','16.900','14.400','24.97963','4.997963')
  TableLG<- data.frame(SummaryLG, ValueLG)
  View(TableLG)

#Addng regression line for land
plot(land$Index,land$`Growth (%Y-O-Y)`, main = 'Regression for Land', xlab = 'index',ylab = 'growth')
abline(lm(`Growth (%Y-O-Y)`~ Index,data = land), col = 'red')  







