#Zadanie 2.
#Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.

#Drugą połowę przejechał ze średnią prędkością 90 km/h.

#Jaka była średnia prędkość pociągu.

#mean

lw<-c(120,90)
s=mean(lw)
print('srednia predkosc pociagu wynosi:')
print(s)

#Zadanie 3.
#Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#sWczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

w1 <-c(5:15)
w2 <-c(30:40)

suma_ab<-0
suma_a<-0
suma_b<-0
r<-0

r_Pearsona <-function(w1,w2){
  if (length(w1)==length(w2)){
    w1_s=mean(w1)
    w2_s=mean(w2)
    for(i in 1:length(w1)){
      suma_ab<-suma_ab+(w2[i]-w1_s)*(w2[i]-w2_s)
      suma_a<-suma_a+(w1[i]-w1_s)**2
      suma_b<-suma_b+(w2[i]-w2_s)**2
    }
    r<-suma_ab/(sqrt(suma_a)*sqrt(suma_b))
    print(r)
  }else{
    print("długość wektorów się nie zgadza")
  }
}
ws<-r_Pearsona(w1,w2)
print("współczynnik korelacki Pearsona wynosi:")
print(ws)


#s
dane<-read.csv("dane.csv",sep=";")
w1<-c(dane[,2])
w2<-c(dane[,1])
r_Pearsona(w1,w2)

#wynik r_Pearsona = 0,973521,  jest to korelacja silna, zbliża się do idelanej korelacji

#Zadanie 4.
#Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)


stworzDataFrame <- function(ile=1){
  kol <- readline(prompt="Podaj nazwy kolumn odseparowane przecinkiem: ")
  kol_podzial <- strsplit(kol, ",")
  kol_dl <- length(kol_podzial[[1]])
  df <- data.frame(matrix(ncol=kol_dl, nrow=0))
  colnames(df) <- kol_podzial[[1]]
  for (wiersz_id in 1:ile){
    wiersz_dl <- 0
    while (wiersz_dl != kol_dl) {
      wiersz <- readline(prompt=sprintf("Podaj %i wartosci dla %i wiersza odseparowane przecinkiem: ", kol_dl, wiersz_id))
      wiersz_podzial <- strsplit(wiersz, ",")
      wiersz_dl <- length(wiersz_podzial[[1]])
    }
    df[wiersz_id,] <- wiersz_podzial[[1]]
  }
  
  print(df)	
}
stworzDataFrame(3)




#Zadanie 5.
# Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
# UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.  Wykonując obliczenia pomiń brakujące wartości.

# liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
#   
#   #...
#   
# }
# 
# Lista plików w katalogu: 
#   
#   list.files(sciezka)
# 
# Omijanie na : na.omit(myDataFrame[[nazwaKolumny]])
# Do złączania stringów: 
#   
#   paste("string1","string2",sep="TU WSTAW SEPARATOR")
# Gdy mamy, rózne oznaczenia NA w plikach możemy wykorzystać ( w tym wypadku pusty znak i NA: na.strings=c("","NA")

liczZPlikow <- function(sciezka, nazwaKolumny, jakaFunkcja="mean", DlaIluPlikow=1){
  lista_plikow <- list(list.files(sciezka))
  lista_kolumn = list()
  for (i in 1:DlaIluPlikow){
    na.strings=c("","NA")
    plik <- lista_plikow[[1]][i]
    pelna_sciezka <- paste(sciezka, "/", plik, sep="")
    dane_z_pliku <- read.csv(pelna_sciezka)
    kolumna <- dane_z_pliku[nazwaKolumny]
    kolumna <- na.omit(kolumna[[nazwaKolumny]])
    lista_kolumn[[i]] <- kolumna
  }
  kolumna = do.call(rbind, lista_kolumn)
  if (jakaFunkcja == "mean"){
    if (is.numeric(kolumna)){
      print(mean(kolumna))
    } else{
      print("Kolumna zawiera wartosci nienumeryczne")
    }
  }
  if (jakaFunkcja == "median"){
    if (is.numeric(kolumna)){
      print(median(kolumna))
    } else{
      print("Kolumna zawiera wartosci nienumeryczne")
    }
  }
  if (jakaFunkcja == "min"){
    if (is.numeric(kolumna)){
      print(min(kolumna))
    } else{
      print("Kolumna zawiera wartosci nienumeryczne")
    }
  }
  if (jakaFunkcja == "max"){
    if (is.numeric(kolumna)){
      print(max(kolumna))
    } else{
      print("Kolumna zawiera wartosci nienumeryczne")
    }
  }
  
}

sciezka <- "/home/user/"
nazwaKolumny <- ""
jakaFunkcja <- "mean"
DlaIluPlikow <- 1
liczZPlikow(sciezka, nazwaKolumny, jakaFunkcja, DlaIluPlikow)
