
# Analiza regresji  -------------------------------------------------------
library(neuralnet)
library(rpart.plot)
library(RODBC)
library (data.table)
library(arules)
library(ggplot2)
library(rpart)
library(MASS)

library(plyr)
# odczytanie danych  ------------------------------------------------------


dwh<-odbcConnect("SADT", uid="mazurekma")

rep_sprzedaz_stacji<-data.table(sqlQuery(dwh, "
SELECT      [GfK_Segment gastronomii_KOD] [GfK_Segment_gastronomii_KOD]
                                        ,[GfK_Segment gastronomii_OPIS] [GfK_Segment_gastronomii_OPIS]
                                         ,[LU_STACJA_ID]
                                         ,[STACJA_STACJA_OPIS]
                                         ,[SZEROKOSC_GEOGRAFICZNA]
                                         ,[DLUGOSC_GEOGRAFICZNA]
                                         ,[STACJA_Punkty gastronomiczn] [STACJA_Punkty_gastronomiczn]
                                         ,[STACJA_Punkty gastronomiczn_NAZWA] [STACJA_Punkty_gastronomiczn_NAZWA]
                                         ,[STACJA_Typ stacji] [STACJA_Typ_stacji]
                                         ,[STACJA_Typ stacji_NAZWA] [STACJA_Typ_stacji_NAZWA]
                                         ,[GfK_TYP_STACJI_KOD]
                                         ,[GfK_TYP_STACJI_OPIS]                                   
                                         ,[STACJA_Myjnia  automatyczna] [STACJA_Myjnia_automatyczna]
                                         ,[STACJA_Powierzchnia sklepu] [STACJA_Powierzchnia_sklepu]
                                         ,[STACJA_Typ miejscowoœci] [STACJA_Typ_miejscowosci]
                                         ,[STACJA_Typ miejscowoœci_NAZWA] [STACJA_Typ_miejscowosci_NAZWA]
                                         ,[RODZAJ_PROBY]
                                         ,[GfK_Segment sklepów_KOD] [GfK_Segment_sklepow_KOD]
                                         ,[GfK_Segment sklepów_OPIS] [GfK_Segment_sklepow_OPIS]
                                         ,[STACJA_Brand]
                                         ,[STACJA_Brand_NAZWA]
                                         ,[STACJA_Powierzchnia stacji]  [STACJA_Powierzchnia_stacji]
                                         ,[STACJA_Liczba etatów]   [STACJA_Liczba_etatow]
                                        ,[STACJA_Stacja ca³odobowa] AS STACJA_Stacja_calodobowa
                                        ,[STACJA_Parking dla TIRów] As STACJA_Parking_dla_TIROw
                                        ,[STACJA_Nr drogi] [STACJA_Nr_drogi]
                                        ,[STACJA_Myjnia rêczna] STACJA_Myjnia_reczna 
                                        ,[STACJA_Karta Benzina] [STACJA_Karta_Benzina]
                                        ,[STACJA_Karta Open Drive] [STACJA_Karta_OpenDrive]
                                        ,[STACJA_Karta RED] [STACJA_Karta_RED]
                                        ,[STACJA_Karta WOG] [STACJA_Karta_WOG]
                                         ,[WJXBFS1]
                                         ,[WJXBFS2]
                                         ,[WJXBFS3]
                                         ,[WJXBFS4]
                                         ,[WJXBFS5]
                                         ,[WJXBFS6]
                                         ,[WJXBFS7]
                                         ,[WJXBFS8]
                                         ,[WJXBFS9]
                                         ,[WJXBFSa]
                                         ,[WJXBFSb]
                                         FROM [SADT].[dbo].[REP_SPRZEDAZ_STACJI]
"))

rep_sprzedaz_stacji_produkty<-data.table(
  sqlQuery(dwh, "
SELECT [LU_STACJA_ID]
      ,[GRUPA_MATERIALOW_L3_OPIS]
      ,[WJXBFS1] AS SPRZEDAZ_PROD
  FROM [SADT].[dbo].[REP_SPRZEDAZ_STACJI_PRODUKTY]
  ")
)


setkey(rep_sprzedaz_stacji_produkty,"LU_STACJA_ID")
setkey(rep_sprzedaz_stacji,"LU_STACJA_ID")


sprzedaz_prod<-rep_sprzedaz_stacji_produkty[rep_sprzedaz_stacji]
sprzedaz_prod<-sprzedaz_prod[GRUPA_MATERIALOW_L3_OPIS!='OPAKOWANIA']


#PALIWA 
sprzedaz_paliwa<-sprzedaz_prod[GRUPA_MATERIALOW_L3_OPIS=='PALIWA',]







rep_sprzedaz_stacji_estymacja<-data.table(
  sqlQuery(dwh, "
     SELECT
     [GfK_Segment gastronomii_KOD] [GfK_Segment_gastronomii_KOD]
                                        ,[GfK_Segment gastronomii_OPIS] [GfK_Segment_gastronomii_OPIS]
                                         ,[LU_STACJA_ID]
           ,[STACJA_STACJA_OPIS]
           ,[SZEROKOSC_GEOGRAFICZNA]
           ,[DLUGOSC_GEOGRAFICZNA]
           ,[STACJA_Punkty gastronomiczn] [STACJA_Punkty_gastronomiczn]
           ,[STACJA_Punkty gastronomiczn_NAZWA] [STACJA_Punkty_gastronomiczn_NAZWA]
           ,[STACJA_Typ stacji] [STACJA_Typ_stacji]
           ,[STACJA_Typ stacji_NAZWA] [STACJA_Typ_stacji_NAZWA]
           ,[GfK_TYP_STACJI_KOD]
           ,[GfK_TYP_STACJI_OPIS]           
           ,[STACJA_Myjnia  automatyczna] [STACJA_Myjnia_automatyczna]
           ,[STACJA_Powierzchnia sklepu] [STACJA_Powierzchnia_sklepu]
           ,[STACJA_Typ miejscowoœci] [STACJA_Typ_miejscowosci]
           ,[STACJA_Typ miejscowoœci_NAZWA] [STACJA_Typ_miejscowosci_NAZWA]
           ,[RODZAJ_PROBY]
           ,[GfK_Segment sklepów_KOD] [GfK_Segment_sklepow_KOD]
           ,[GfK_Segment sklepów_OPIS] [GfK_Segment_sklepow_OPIS]
           ,[STACJA_Brand]
           ,[STACJA_Brand_NAZWA]
           ,[STACJA_Powierzchnia stacji]  [STACJA_Powierzchnia_stacji]
           ,[STACJA_Liczba etatów]   [STACJA_Liczba_etatow]
           ,[STACJA_Stacja ca³odobowa] AS STACJA_Stacja_calodobowa
           ,[STACJA_Parking dla TIRów] As STACJA_Parking_dla_TIROw
           ,[STACJA_Nr drogi] [STACJA_Nr_drogi]
           ,[STACJA_Myjnia rêczna] STACJA_Myjnia_reczna 
           ,[STACJA_Karta Benzina] [STACJA_Karta_Benzina]
           ,[STACJA_Karta Open Drive] [STACJA_Karta_OpenDrive]
           ,[STACJA_Karta RED] [STACJA_Karta_RED]
           ,[STACJA_Karta WOG] [STACJA_Karta_WOG]
           ,[WJXBFS1]
           ,[WJXBFS2]
           ,[WJXBFS3]
           ,[WJXBFS4]
           ,[WJXBFS5]
           ,[WJXBFS6]
           ,[WJXBFS7]
           ,[WJXBFS8]
           ,[WJXBFS9]
           ,[WJXBFSa]
           FROM [SADT].[dbo].[REP_SPRZEDAZ_STACJI_ESTYMACJA]
  ")
)




# model dla sprzedazy ca³kowitej ------------------------------------------




model<-lm( WJXBFSb~
           STACJA_Typ_stacji
           +STACJA_Powierzchnia_sklepu 
           +STACJA_Typ_miejscowosci
           +STACJA_Brand
           +STACJA_Punkty_gastronomiczne
           +STACJA_Myjnia_automatyczna
           +GfK_TYP_STACJI_KOD
           +WJXBFSa
           , data.frame(rep_sprzedaz_stacji))

pairs(WJXBFSb~STACJA_Typ_stacji+STACJA_Powierzchnia_sklepu 
      +STACJA_Typ_miejscowosci
      +STACJA_Brand
      +RODZAJ_PROBY
      +STACJA_Punkty_gastronomiczne
      +STACJA_Myjnia_automatyczna
      +GfK_TYP_STACJI_KOD
      +WJXBFSa, data.frame(rep_sprzedaz_stacji) )



summary(model)

# model dla sprzedazy grup produktow PALIWA  --------------------------------------


#ggplot(sprzedaz_prod,(aes(x=SPRZEDAZ_PROD))) + geom_histogram()+facet_grid(.~ GRUPA_MATERIALOW_L3_OPIS)

#ggplot(sprzedaz_prod,(aes(x=STACJA_Typ_stacji, y=SPRZEDAZ_PROD))) + geom_bar()



model<-lm( SPRZEDAZ_PROD~STACJA_Typ_stacji
           +STACJA_Powierzchnia_sklepu 
           +STACJA_Typ_miejscowosci
           +STACJA_Brand
           #+RODZAJ_PROBY
           #+STACJA_Punkty_gastronomiczne
           +STACJA_Myjnia_automatyczna
           # +GfK_TYP_STACJI_KOD              
           + STACJA_Karta_RED
           + STACJA_Karta_WOG
           # +WJXBFSa       
           # +WJXBFS1
           # +WJXBFS2
           # +WJXBFS3
           # +WJXBFS4
           # +WJXBFS5
           #+WJXBFS6   indeksy obecnych obrotow sklepu - data leakage
           #+WJXBFS7   indeksy obecnych obrotow sklepu - data leakage
           + STACJA_Stacja_calodobowa
           + STACJA_Parking_dla_TIROw           
           + STACJA_Myjnia_reczna                     
           + STACJA_Powierzchnia_stacji
           + STACJA_Liczba_etatow |GRUPA_MATERIALOW_L3_OPIS
           , data.frame(sprzedaz_prod))

summary(model)
levels(sprzedaz_prod$GRUPA_MATERIALOW_L3_OPIS)

models <- dlply(data.frame(sprzedaz_prod), "GRUPA_MATERIALOW_L3_OPIS", function(df) 
   lm( SPRZEDAZ_PROD~STACJA_Typ_stacji
             +STACJA_Powierzchnia_sklepu 
             +STACJA_Typ_miejscowosci
             +STACJA_Brand
             #+RODZAJ_PROBY
             #+STACJA_Punkty_gastronomiczne
             +STACJA_Myjnia_automatyczna
             # +GfK_TYP_STACJI_KOD              
             + STACJA_Karta_RED
             + STACJA_Karta_WOG
             # +WJXBFSa       
             # +WJXBFS1
             # +WJXBFS2
             # +WJXBFS3
             # +WJXBFS4
             # +WJXBFS5
             #+WJXBFS6   indeksy obecnych obrotow sklepu - data leakage
             #+WJXBFS7   indeksy obecnych obrotow sklepu - data leakage
             + STACJA_Stacja_calodobowa
             + STACJA_Parking_dla_TIROw           
             + STACJA_Myjnia_reczna                     
             + STACJA_Powierzchnia_stacji
             + STACJA_Liczba_etatow 
             , df)
)

# Apply coef to each model and return a data frame
ldply(models, coef)



predictions<-lapply(models, predict, wszystkie_stacje)
dc<-do.call(rbind, predictions)
dc[dc<0]<-0
total<-colSums(dc, na.rm = TRUE) 
sum(total)




update_levels<-function(train, score, attr){
  #liczba rekordow  
  expr<-parse(paste0('train$',attr))
  train_levels<-levels(eval(expr))
  #print(train_levels)
  #n<-nrow(score[!score$get(attr)  %in% train_levels]   )
  #score[!score$get(attr)  %in% train_levels]$attr<- sample(traing$attr, n, replace=TRUE )    
  #score 
  train_levels
}

rep_sprzedaz_stacji_e<-update_levels(sprzedaz_prod,rep_sprzedaz_stacji_estymacja,"STACJA_Typ_stacji")
  

#STACJA_Typ_stacji

n<-nrow(rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Typ_stacji  %in% levels(sprzedaz_prod$STACJA_Typ_stacji)]   )
rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Typ_stacji  %in% levels(sprzedaz_prod$STACJA_Typ_stacji)]$STACJA_Typ_stacji<- sample(sprzedaz_prod$STACJA_Typ_stacji, n, replace=TRUE )    


#STACJA_Brand


n<-nrow(rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Brand  %in% levels(sprzedaz_prod$STACJA_Brand)]   )
rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Brand  %in% levels(sprzedaz_prod$STACJA_Brand)]$STACJA_Brand<- sample(sprzedaz_prod$STACJA_Brand, n, replace=TRUE )    




#STACJA_Myjnia_automatyczna

n<-nrow(rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Myjnia_automatyczna  %in% levels(sprzedaz_prod$STACJA_Myjnia_automatyczna)]   )
rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Myjnia_automatyczna  %in% levels(sprzedaz_prod$STACJA_Myjnia_automatyczna)]$STACJA_Myjnia_automatyczna<- sample(sprzedaz_prod$STACJA_Myjnia_automatyczna, n, replace=TRUE )    


#STACJA_Karta_RED
n<-nrow(rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Karta_RED  %in% levels(sprzedaz_prod$STACJA_Karta_RED)]   )
rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Karta_RED  %in% levels(sprzedaz_prod$STACJA_Karta_RED)]$STACJA_Karta_RED<- sample(sprzedaz_prod$STACJA_Karta_RED, n, replace=TRUE )    

#STACJA_Karta_WOG
n<-nrow(rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Karta_WOG  %in% levels(sprzedaz_prod$STACJA_Karta_WOG)]   )
rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Karta_WOG  %in% levels(sprzedaz_prod$STACJA_Karta_WOG)]$STACJA_Karta_WOG<- sample(sprzedaz_prod$STACJA_Karta_WOG, n, replace=TRUE )    


#STACJA_Parking_dla_TIROw
n<-nrow(rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Parking_dla_TIROw  %in% levels(sprzedaz_prod$STACJA_Parking_dla_TIROw)]   )
rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Parking_dla_TIROw  %in% levels(sprzedaz_prod$STACJA_Parking_dla_TIROw)]$STACJA_Parking_dla_TIROw<- sample(sprzedaz_prod$STACJA_Parking_dla_TIROw, n, replace=TRUE )    




#STACJA_Myjnia_automatyczna

n<-nrow(rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Myjnia_reczna  %in% levels(sprzedaz_prod$STACJA_Myjnia_reczna)]   )
rep_sprzedaz_stacji_estymacja[!rep_sprzedaz_stacji_estymacja$STACJA_Myjnia_reczna  %in% levels(sprzedaz_prod$STACJA_Myjnia_reczna)]$STACJA_Myjnia_reczna<- sample(sprzedaz_prod$STACJA_Myjnia_reczna, n, replace=TRUE )    





wszystkie_stacje<-rbind(sprzedaz_paliwa[, colnames(rep_sprzedaz_stacji_estymacja), with=FALSE] ,(rep_sprzedaz_stacji_estymacja) )





#Liczba etatow 

avg<-wszystkie_stacje[wszystkie_stacje$STACJA_Liczba_etatow>0, mean(wszystkie_stacje$STACJA_Liczba_etatow), ]
rep_sprzedaz_stacji_estymacja$STACJA_Liczba_etatow<-avg



avg<-wszystkie_stacje[wszystkie_stacje$STACJA_Powierzchnia_stacji>0, mean(wszystkie_stacje$STACJA_Powierzchnia_stacji), ]
rep_sprzedaz_stacji_estymacja$STACJA_Powierzchnia_stacji<-avg



avg<-wszystkie_stacje[wszystkie_stacje$STACJA_Powierzchnia_sklepu>0, mean(wszystkie_stacje$STACJA_Powierzchnia_sklepu), ]
rep_sprzedaz_stacji_estymacja$STACJA_Powierzchnia_sklepu<-avg

#Powieerzchnia sklepow 


wszystkie_stacje<-rbind(sprzedaz_paliwa[, colnames(rep_sprzedaz_stacji_estymacja), with=FALSE] ,(rep_sprzedaz_stacji_estymacja) )






nrow(rep_sprzedaz_stacji_estymacja)
nrow(sprzedaz_paliwa)

train_sum<-sprzedaz_paliwa[,sum(SPRZEDAZ_PROD),]
train_sum<-sum(sprzedaz_paliwa$SPRZEDAZ_PROD)
score_sum1<-train_sum*nrow(rep_sprzedaz_stacji_estymacja)/nrow(sprzedaz_paliwa)
pred<-predict(model, data.frame(rep_sprzedaz_stacji_estymacja))
pred[pred<0]<-0
score_sum_predict1<-sum(pred)


sprzedaz_paliwa[, colnames(rep_sprzedaz_stacji_estymacja), with=FALSE]

wszystkie_stacje<-rbind(sprzedaz_paliwa[, colnames(rep_sprzedaz_stacji_estymacja), with=FALSE] ,(rep_sprzedaz_stacji_estymacja) )

pred<-predict(model, data.frame(wszystkie_stacje))


prediction<-cbind(wszystkie_stacje[,LU_STACJA_ID],pred )
close(dwh)

sqlSave(dwh, tablename="REP_SPRZEDAZ_STACJI_SCORE", dat=data.frame(prediction),  append = FALSE) 


# CART --------------------------------------------------------------------

cart<-rpart(  SPRZEDAZ_PROD~STACJA_Typ_stacji
  +STACJA_Powierzchnia_sklepu 
  +STACJA_Typ_miejscowosci
  +STACJA_Brand
  #+RODZAJ_PROBY
  #+STACJA_Punkty_gastronomiczne
  +STACJA_Myjnia_automatyczna
  # +GfK_TYP_STACJI_KOD              
  + STACJA_Karta_RED
  + STACJA_Karta_WOG
  # +WJXBFSa       
  # +WJXBFS1
  # +WJXBFS2
  # +WJXBFS3
  # +WJXBFS4
  # +WJXBFS5
  #+WJXBFS6   indeksy obecnych obrotow sklepu - data leakage
  #+WJXBFS7   indeksy obecnych obrotow sklepu - data leakage
  + STACJA_Stacja_calodobowa
  + STACJA_Parking_dla_TIROw           
  + STACJA_Myjnia_reczna                     
  + STACJA_Powierzchnia_stacji
  + STACJA_Liczba_etatow
  , data.frame(sprzedaz_paliwa))

summary(cart)  
pred<-predict(model, data.frame(wszystkie_stacje))
score_sum_predict1<-sum(pred)

rpart.plot(cart)






models <- dlply(data.frame(sprzedaz_prod), "GRUPA_MATERIALOW_L3_OPIS", function(df) 
  rpart( SPRZEDAZ_PROD~STACJA_Typ_stacji
      +STACJA_Powierzchnia_sklepu 
      +STACJA_Typ_miejscowosci
      +STACJA_Brand
      #+RODZAJ_PROBY
      #+STACJA_Punkty_gastronomiczne
      +STACJA_Myjnia_automatyczna
      # +GfK_TYP_STACJI_KOD              
      + STACJA_Karta_RED
      + STACJA_Karta_WOG
      # +WJXBFSa       
      # +WJXBFS1
      # +WJXBFS2
      # +WJXBFS3
      # +WJXBFS4
      # +WJXBFS5
      #+WJXBFS6   indeksy obecnych obrotow sklepu - data leakage
      #+WJXBFS7   indeksy obecnych obrotow sklepu - data leakage
      + STACJA_Stacja_calodobowa
      + STACJA_Parking_dla_TIROw           
      + STACJA_Myjnia_reczna                     
      + STACJA_Powierzchnia_stacji
      + STACJA_Liczba_etatow 
      , df)
)


lpredictions<-lapply(models, predict, wszystkie_stacje)
dc<-do.call(rbind, lpredictions)
dc[dc<0]<-0
pred<-colSums(dc, na.rm = TRUE) 
sum(pred)






prediction<-cbind(wszystkie_stacje[,LU_STACJA_ID],pred )

sqlDrop(dwh, "REP_SPRZEDAZ_STACJI_SCORE")
sqlSave(dwh, tablename="REP_SPRZEDAZ_STACJI_SCORE", dat=data.frame(prediction),  append = FALSE) 


