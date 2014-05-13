### R code from vignette source 'jssarticle.Rnw'

###################################################
### code chunk number 1: jssarticle.Rnw:553-556
###################################################
library("TSdist")
data(example.series1)
data(example.series2)


###################################################
### code chunk number 2: jssarticle.Rnw:558-560
###################################################
crossCorrelationDistance(example.series1, example.series2)
correlationDistance(example.series1, example.series2)


###################################################
### code chunk number 3: jssarticle.Rnw:568-569
###################################################
manhattanDistance(example.series1, example.series2)


###################################################
### code chunk number 4: jssarticle.Rnw:573-574
###################################################
lpDistance(example.series1, example.series2, method="manhattan")


###################################################
### code chunk number 5: jssarticle.Rnw:583-585
###################################################
edrDistance(example.series1, example.series2, epsilon=0.1)
erpDistance(example.series1, example.series2, g=0)


###################################################
### code chunk number 6: jssarticle.Rnw:593-595
###################################################
data(example.series3)
data(example.series4)


###################################################
### code chunk number 7: jssarticle.Rnw:597-598
###################################################
 lcssDistance(example.series3, example.series4, epsilon=0.1)


###################################################
### code chunk number 8: jssarticle.Rnw:614-616
###################################################
length(example.series3)
length(example.series4)


###################################################
### code chunk number 9: jssarticle.Rnw:682-686
###################################################
data(zoo.series1)
data(zoo.series2)
library(zoo)
library(xts)


###################################################
### code chunk number 10: jssarticle.Rnw:688-691
###################################################
tsDistances(zoo.series1, zoo.series2, 
distance="pearsoncorrelation")
tsDistances(zoo.series1, zoo.series2, distance="dtw", sigma=10)


###################################################
### code chunk number 11: jssarticle.Rnw:750-752
###################################################
data(example.database)
library(proxy)


###################################################
### code chunk number 12: jssarticle.Rnw:754-757
###################################################
dist(example.database, method="tsDistances", 
distance="tquest", tau=mean(example.database), 
diag=TRUE, upper=TRUE)


###################################################
### code chunk number 13: jssarticle.Rnw:764-765
###################################################
data(zoo.database)


###################################################
### code chunk number 14: jssarticle.Rnw:767-769 (eval = FALSE)
###################################################
## tsDatabaseDistances(example.database, method="tquest", 
## tau=mean(example.database), diag=TRUE, upper=TRUE)


###################################################
### code chunk number 15: jssarticle.Rnw:776-778
###################################################
tsDatabaseDistances(zoo.database, method="tquest", 
tau=mean(zoo.database), diag=TRUE, upper=TRUE)


