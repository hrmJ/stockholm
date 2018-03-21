#cluster analysis
#importirovat dannye v programmu
tablica<-readRDS("clusters.RDS")

#poschitat rasstojanije mezhdu raznymi elementami v tablice, ispolzuem funkciju dist s 308 LN
rasstojanije<-dist(tablica, method="canberra")
#ispolzuem metod ierarhicheskij cluster
kluster<-hclust(rasstojanije, method = "ward.D2")
#mozhno poluchit kartinku, ispolzuem funkciju plot
plot(kluster)
