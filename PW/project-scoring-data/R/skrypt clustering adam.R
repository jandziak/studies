library(cluster)
library(mlbench)

cassini <- mlbench.cassini(n=1000)
dane  <- cassini$x
klasa <- cassini$classes 

###################################################
### PAM 

pam3 <- pam( dane, k=3 )
pam3.etykietki <- pam3$clustering

par(mfrow=c(1,1))
plot(dane, col=pam3.etykietki, pch=16, main="PAM")

###################################################
### AGNES

agnes.single.all <- agnes( dane, method="single" )
agnes.single3 <- cutree( agnes.single.all, 3 )
agnes.complete.all <- agnes( dane, method="complete" )
agnes.complete3 <- cutree( agnes.complete.all, 3 )
agnes.average.all <- agnes( dane, method="average" )
agnes.average3 <- cutree( agnes.average.all, 3 )

par(mfrow=c(1,3))
plot(dane, col=agnes.single3, pch=16, main="SINGLE linkage")
plot(dane, col=agnes.complete3, pch=16, main="COMPLETE linkage")
plot(dane, col=agnes.average3, pch=16, main="AVERAGE linkage")
