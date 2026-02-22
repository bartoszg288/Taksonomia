# Instalacja potrzebnych pakietów
#install.packages(c("readxl", "dplyr", "irr", "cluster", "factoextra", "sf", "tmap", "maptools", "mapview", "labeling"))
library(readxl)
library(dplyr)
library(irr)
library(cluster)
library(factoextra)
library(sf)
library(tmap)
library(maptools)
library(labeling)

# Wczytanie danych z Excela
setwd("D:/OneDrive/Desktop/Taksonomia/")  # Ustaw folder, w którym jest projekt.xlsx
kraj.dane <- read_excel("D:/OneDrive/Desktop/Taksonomia/projekt.xlsx")

#Początkowo (analogicznie jak wyżej) wyznaczone zostaną wartości TMR Hellwiga z odległości euklidesowej
x1<-kraj.dane$X1
x2<-kraj.dane$X2
x3<-kraj.dane$X3
x4<-kraj.dane$X4
x5<-kraj.dane$X5
dane_TMR<-cbind(x1, x2, x3, x4, x5)
dane_TMR<-as.data.frame(dane_TMR)
n<-ncol(dane_TMR)
m<-nrow(dane_TMR)
stim<-c(1,0,1,1,1)
dane_TMR.stand<-scale(dane_TMR)

#Zdefniowanie wektora wartości wzorcowych dla każdej ze zmiennych.
wzorzec<-rep(0, n)

# Zdefniowanie macierzy odległości od wzorca.
odl<-matrix(0,m,n)

# Pętla ustalająca wartości wzorcowe (wartość minimalna dla destymulant oraz maksymalna dla stymulant oraz wypełniająca macierz odległości
for(i in 1:n){
  if (stim[i] == 0) {
    wzorzec[i] <- min(dane_TMR.stand[,i])
  } else {
    wzorzec[i] <- max(dane_TMR.stand[,i])
  }
  odl[,i] <- (dane_TMR.stand[,i] - wzorzec[i])^2
}
# Wyznaczenie odległości od wektora wzorcowego (odległość euklidesowa).
odl.wektor<-sqrt(rowSums(odl))

# Wyznaczenie wartości TMR Hellwiga.
tmr=1-odl.wektor/(mean(odl.wektor)+2*sd(odl.wektor))

# Utworzenie macierzy o nazwie TMR z wartościami TMR Hellwiga z odległości euklidesowej.
TMR<-as.matrix(tmr)

rank_e<-rank(-TMR)

#Dalej dokonane zostaje uporządkowanie liniowe obiektów.
Wyniki<-cbind(kraj.dane$Województwo, TMR, rank_e)
colnames(Wyniki)<-c("Wojewodztwo", "TMR Hellwiga (e)", "Ranking")
Wyniki_sort<-Wyniki[order(Wyniki[,2], decreasing=TRUE),]
print(Wyniki_sort)

#Wyznaczymy ranking krajów stosując w procedurze odległość miejską.
odl_m<-matrix(0,m,n)
for(i in 1:n){
  ifelse(stim[i]==0, wzorzec[i]<-min(dane_TMR.stand[,i]), wzorzec[i]<-max(dane_TMR.stand[,i]))
  odl_m[,i]<-abs(dane_TMR.stand[,i]-wzorzec[i])
}
# Moduł różnicy pomiędzy wartością zmiennej dla obiektu a wartością wzorcową jest niezbędny do wyznaczenia odległości miejskiej.
odl_m.wektor<-rowSums(odl_m)
tmr_m=1-odl_m.wektor/(mean(odl_m.wektor)+2*sd(odl_m.wektor))

# Utworzenie macierzy o nazwie TMR_m z wartościami TMR Hellwiga z odległością miejską.
TMR_m<-as.matrix(tmr_m)
rank_m<-rank(-TMR_m)
#Dalej dokonane zostaje uporządkowanie liniowe rozważanych obiektów.
Wyniki<-cbind(kraj.dane$Województwo, TMR_m, rank_m)
colnames(Wyniki)<-c("Wojewodztwo", "TMR Hellwiga (m)", "Ranking")
Wyniki_sort<-Wyniki[order(Wyniki[,2], decreasing=TRUE),]
print(Wyniki_sort)

#I podsumowanie wyników wg zastosowanych metod
Wyniki<-cbind(kraj.dane$Województwo, TMR, rank_e, TMR_m, rank_m)
colnames(Wyniki)<-c("Wojewodztwo", "TMR Hellwiga (e)", "Ranking", "TMR Hellwiga (m)", "Ranking")
Wyniki_sort<-Wyniki[order(Wyniki[,2], decreasing=TRUE),]
print(Wyniki_sort)

#Ocena zgodności rankingów
library(irr)
a<-data.frame(rank_e, rank_m)
kendall(a, correct=FALSE)

#Prezentacja rozkładów przestrzennych TMR na mapach
#install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
#install.packages("car", repos = "https://packagemanager.posit.co/cran/2023-10-13")
library(maptools)
library(car)
library(sf)

woj.poly<-readShapePoly("Wojewodztwa.shp")   
colors<-c("yellow","orange1","red2","red4")
par(mfrow=c(1,2))


###


#odległość euklidesowa
od<-(quantile(TMR, 0.75)-quantile(TMR, 0.25))/2
p1<-c(min(TMR), median(TMR)-od, median(TMR), median(TMR)+od)
p1 <- sort(p1)
plot(woj.poly, col=colors[findInterval(TMR,p1)], main="TMR Hellwiga - odległość euklidesowa", cex.main=1)
legend("bottomleft", fill=colors, legend=c("mniej niż Mediana-Q", "od Mediana-Q do Mediana", "od Mediana do Mediana+Q","więcej niż Mediana+Q"), bty="n", cex=0.6)
pointLabel(coords,as.character(kraj.dane$Województwo),cex=0.4,offset=0)



#odległość miejska
od_m<-(quantile(TMR_m, 0.75)-quantile(TMR_m, 0.25))/2
p2<-c(min(TMR_m), median(TMR_m)-od_m, median(TMR_m), median(TMR_m)+od_m)
plot(woj.poly, col=colors[findInterval(TMR_m,p2)], main="TMR Hellwiga - odległość miejska", cex.main=1)
legend("bottomleft", fill=colors, legend=c("mniej niż Mediana-Q", "od Mediana-Q do Mediana", "od Mediana do Mediana+Q","więcej niż Mediana+Q"), bty="n", cex=0.6)
pointLabel(coords,as.character(kraj.dane$Województwo),cex=0.4,offset=0)

#Zaczytanie potrzebnych pakietów
library(cluster)
library(factoextra)
library(maptools)

#Ustalić „optymalną” liczbę klastrów w grupowaniu analizowanych wcześniej krajów, stosując odpowiednie metody.
kraj.dane=read_excel("D:/OneDrive/Desktop/Taksonomia/projekt.xlsx")

# Ustalenie zmiennych diagnostycznych, scalenie ich i standaryzacja
x1<-kraj.dane$X1
x2<-kraj.dane$X2
x3<-kraj.dane$X3
x4<-kraj.dane$X4
x5<-kraj.dane$X5

dane_clust<-cbind(x1, x2, x3, x4, x5)
dane_clust<-as.data.frame(dane_clust)
library(cluster)


# Standaryzacja wartości zmiennych w celu ich porównywalności - jeden z koniecznych etapów analizy skupień.
dane_clust.stand<-scale(dane_clust)

#ustalenie nazw obiektów
row.names(dane_clust.stand)<-kraj.dane$Województwo

# Ustalenie liczby skupień (hierarchiczna metoda grupowania)
#Metoda łokcia
fviz_nbclust(dane_clust.stand, FUN=hcut, method="wss")
# metoda profile
fviz_nbclust(dane_clust.stand, FUN=hcut, method="silhouette")
# metoda luki
gap_stat <- clusGap(dane_clust.stand, FUN = hcut, nstart=25, K.max = 6, B = 500)
fviz_gap_stat(gap_stat)

# Wybór metody aglomeracji (współczynniki aglomeracji)
# Zdefiniowanie macierzy wartości współczynników aglomeracji
a_coeff<-matrix(0, 1, 5)

#Warda
a_coeff[1, 1]<-agnes(dane_clust.stand, method="ward")$ac
#najbliższego sąsiectwa
a_coeff[1, 2]<-agnes(dane_clust.stand, method="single")$ac
#?
a_coeff[1, 3]<-agnes(dane_clust.stand, method="complete")$ac
#średnia
a_coeff[1, 4]<-agnes(dane_clust.stand, method="average")$ac
#?
a_coeff[1, 5]<-agnes(dane_clust.stand, method="weighted")$ac

colnames(a_coeff)<-c("Ward", "Single", "Complete", "Average", "Weighted")
print(a_coeff)



library(factoextra)

# Konstrukcja macierzy odległości euklidesowej pomiędzy jednostkami
d<-dist(dane_clust.stand, method = "euclidean")
#dokonuje hierarchicznego grupowania jednostek na podstawie macierzy odległości  (metoda warda)
hc<-hclust(d, method="ward.D")
#podzielenie jednostki na konkretną liczbę grup - k=4 na podstawie wyników grupowania metodą Warda przechowywanych w obiekcie hc.
ward.clust<-cutree(hc, k=4)

fviz_cluster(list(data=dane_clust.stand, cluster=ward.clust), palette="Set1", main="Metoda Warda")

#Dendrogram
plot(hc, cex=0.6, hang=-1, main="Grupowanie metodą Warda")
rect.hclust(hc, k=4, border=1:2)

#Uwaga, Aby ocenić średni poziom rozwoju społeczno-gospodarczego podregionów warto wyznaczyć przeciętne wartości dla każdej ze zmiennych w otrzymanych grupach.
dane.clust.ward<-cbind(dane_clust, cluster=ward.clust)
aggar.ward<-aggregate(dane.clust.ward[,1:5], by=list(dane.clust.ward$cluster), mean)
colnames(aggar.ward)<-c("Grupa", "X1", "X2", "X3", "X4", "X5")
print(aggar.ward)

library(maptools)

#Mapa
kraj.dane<-cbind(kraj.dane, ward.clust)
kraj.poly<-readShapePoly("Wojewodztwa.shp")   
colors<-c('darkgreen',"green","red",'darkred')
p1<-c(1,2,3,4)
plot(woj.poly, col=colors[findInterval(ward.clust,p1)], main="Podział na skupienia – metoda Warda", cex.main=1)
legend("bottomleft", fill=colors, legend=c("Skupienie 1", "Skupienie 2", "Skupienie 3", "Skupienie 4"), bty="n", cex=0.8)


#taksonomia
#test
#dodanie danych
# czy git działa?