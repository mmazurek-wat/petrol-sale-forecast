
# Analiza regresji  -------------------------------------------------------
library(neuralnet)
library(rpart.plot)
library(RODBC)
library (data.table)
library(arules)
library(ggplot2)
library(rpart)
library(MASS)

library(reshape)

library(plyr)
# odczytanie danych  ------------------------------------------------------

load( "D://Pentacomp//Orlen/Dane/Estymacja.dat" )

#stacje_train - identyfikator stacji z   rozbiciem na produkty przypisanymi wartosciam 
#stacje_score - wszystkie stacje z atrybutami 

# przeglad danych uczacyh 

boxplot(SPRZEDAZ_PROD~GRUPA_MATERIALOW_L3_OPIS, 
        stacje_train[!stacje_train$GRUPA_MATERIALOW_L3_OPIS %in% 'PALIWA', ],
        notch = TRUE, add = TRUE, col = "blue")
stacje_train.dt<-data.table(stacje_train)
stacje_train_agg<-stacje_train.dt[, list(sum(SPRZEDAZ_PROD), sd(SPRZEDAZ_PROD), .N), by='GRUPA_MATERIALOW_L3_OPIS']
pie(stacje_train_agg$V1, labels = stacje_train_agg$GRUPA_MATERIALOW_L3_OPIS )

#PALIWA< ARTYKULY SPOZYWCZE<ARTYKULY_POZOSTALE. 


sum(stacje_train[!stacje_train$GRUPA_MATERIALOW_L3_OPIS %in% c('PALIWA','ARTYKULY POZOSTALE', 'ARTYKULY SPOZYWCZE') , SPRZEDAZ_PROD])/sum(stacje_train$SPRZEDAZ_PROD)

setkey(stacje_train,  LU_STACJA_ID ,  GRUPA_MATERIALOW_L3_OPIS )
setkey(stacje_score, LU_STACJA_ID)
df.train<-stacje_score[stacje_train]
sum(df.train$SPRZEDAZ_PROD)


stacje_train_3gr<-stacje_train[stacje_train$GRUPA_MATERIALOW_L3_OPIS %in% c('PALIWA','ARTYKULY POZOSTALE', 'ARTYKULY SPOZYWCZE') ,]
stacje_train_3gr_wide<-cast(stacje_train_3gr, LU_STACJA_ID~GRUPA_MATERIALOW_L3_OPIS, mean)

stacje_train_3gr_wide[is.nan(stacje_train_3gr_wide$PALIWA),]

stacje_train_3gr_wide[is.nan(stacje_train_3gr_wide$SPOZYWCZE),]

##296,329, 356

stacje_train_3gr_wide[is.nan(stacje_train_3gr_wide$POZOSTALE),]


colnames(stacje_train_3gr_wide)<-c("LU_STACJA_ID","POZOSTALE", "SPOZYWCZE", "PALIWA")

##486 nie sprzedaje paliwa ?  Warszawa, ul. Bielanska (siedziba Orlenu)

stacje_score[stacje_score$LU_STACJA_ID==486,]
stacje_score[stacje_score$LU_STACJA_ID==356,]



#paliwa 


# estymacja sprzedazy  paliwa  --------------------------------------------



stacje_train_paliwa<-stacje_train_3gr_wide[!is.nan(stacje_train_3gr_wide$PALIWA),c("LU_STACJA_ID", "PALIWA")]
stacje_train_paliwa<-




stacje.list<-as.vector(stacje_score[,LU_STACJA_ID])


stacje.dedup<-stacje.list[!duplicated(stacje.list)]

stacje.list.train<-as.vector(stacje_train[,LU_STACJA_ID])
stacje.dedup.train<-stacje.list.train[!duplicated(stacje.list.train)]
length(stacje.dedup.train)  #395 stacji w ciagu uczacym 
length(stacje.dedup)  #2166 w ciagu score 

length(stacje.dedup[! stacje.dedup %in% stacje.dedup.train]) #1852  stacje  do wyznaczenia proby

stacje_train[,LU_STACJA_ID]

train<-sample(stacje.dedup.train, 300)
test<-stacje.dedup.train[!(stacje.dedup.train %in% train)]
length(train)
length(test) 

stacje_train_train<-stacje_train[LU_STACJA_ID %in% train]
stacje_train_test<-stacje_train[LU_STACJA_ID %in% test]


setkey(stacje_train_train,  LU_STACJA_ID ,  GRUPA_MATERIALOW_L3_OPIS )
setkey(stacje_score, LU_STACJA_ID)
df.train<-stacje_score[stacje_train_train]
df.test<-stacje_score[stacje_train_test]
sum(df.train$SPRZEDAZ_PROD)





# CART --------------------------------------------------------------------

cart<-rpart(SPRZEDAZ_PROD~STACJA_Typ_stacji
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
  , df.train)

summary(cart)  
pred<-predict(model, data.frame(wszystkie_stacje))
score_sum_predict1<-sum(pred)

rpart.plot(cart)





stacje_score[, GfK_Segment_sklepow_OPIS]

models <- dlply(data.frame(df.train),  "GRUPA_MATERIALOW_L3_OPIS" , function(df) 
  rpart( SPRZEDAZ_PROD~
          SZEROKOSC_GEOGRAFICZNA
          +DLUGOSC_GEOGRAFICZNA            
          +STACJA_Punkty_gastronomiczn_NAZWA 
         + STACJA_Typ_stacji_NAZWA         
          +  GfK_TYP_STACJI_OPIS             
          + STACJA_Myjnia_automatyczna     +   STACJA_Powierzchnia_sklepu      
          + STACJA_Typ_miejscowosci    +       STACJA_Typ_miejscowosci_NAZWA   
          + RODZAJ_PROBY              +        GfK_Segment_sklepow_KOD         
          + GfK_Segment_sklepow_OPIS   +       STACJA_Brand                    
          + STACJA_Brand_NAZWA        +        STACJA_Powierzchnia_stacji      
          +  STACJA_Liczba_etatow     +         STACJA_Stacja_calodobowa        
          + STACJA_Parking_dla_TIROw   +       STACJA_Nr_drogi                 
          + STACJA_Myjnia_reczna     +         STACJA_Karta_Benzina            
          + STACJA_Karta_OpenDrive        +     STACJA_Karta_RED                
          + STACJA_Karta_WOG         +          WJXBFS1                         
          + WJXBFS2                +            WJXBFS3                               
         + WJXBFS4                                           
    ,df)
)

lpredictions<-lapply(models, predict, df.test)
dc<-do.call(rbind, lpredictions)
dc[dc<0]<-0
pred<-colSums(dc, na.rm = TRUE) 

sum(pred)   # dla tych 100 stacji

sum(df.test$SPRZEDAZ_PROD)
344 013 598

sum(df.train$SPRZEDAZ_PROD)
1 132 173 571

sum(df.test$SPRZEDAZ_PROD)+ sum(df.train$SPRZEDAZ_PROD)



prediction<-cbind(stacje_score[,LU_STACJA_ID],pred )
prediction[1:10,]
stacje_train[1:10, LU_STACJA_ID]

sqlDrop(dwh,  REP_SPRZEDAZ_STACJI_SCORE )
sqlSave(dwh, tablename= REP_SPRZEDAZ_STACJI_SCORE , dat=data.frame(prediction),  append = FALSE) 


