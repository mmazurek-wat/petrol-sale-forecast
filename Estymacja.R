
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

load("D://Pentacomp//Orlen/Dane/Estymacja.dat")

stacje_train<-data.table(stacje_train)
stacje_train[,sum(SPRZEDAZ_PROD), by=GRUPA_MATERIALOW_L3_OPIS]
stacje_train[,sum(SPRZEDAZ_PROD)]



colnames(stacje_score)

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





stacje_score[, GfK_Segment_sklepow_OPIS]

models <- dlply(data.frame(sprzedaz_prod), "GRUPA_MATERIALOW_L3_OPIS", function(df) 
  rpart( SPRZEDAZ_PROD~
      ## LU_STACJA_ID       
      ##STACJA_STACJA_OPIS               
    +  SZEROKOSC_GEOGRAFICZNA         
    +   DLUGOSC_GEOGRAFICZNA
      ##STACJA_Punkty_gastronomiczn
    +  STACJA_Punkty_gastronomiczn_NAZWA
    + STACJA_Typ_stacji_NAZWA          
    + GfK_TYP_STACJI_OPIS              
    + STACJA_Myjnia_automatyczna
    + STACJA_Powierzchnia_sklepu
     ##STACJA_Typ_miejscowosci    
    + STACJA_Typ_miejscowosci_NAZWA    
     #STACJA_Brand                     
    + STACJA_Brand_NAZWA
    + STACJA_Powierzchnia_stacji
    + STACJA_Liczba_etatow 
    + STACJA_Stacja_calodobowa         
    + STACJA_Parking_dla_TIROw   
    + STACJA_Nr_drogi              
    + STACJA_Myjnia_reczna         
    + STACJA_Karta_Benzina             
    + STACJA_Karta_OpenDrive        
    + STACJA_Karta_RED
    + STACJA_Karta_WOG   
    + WJXBFS1                          
    + WJXBFS2                          
    + WJXBFS3    
    + WJXBFS4                    
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


