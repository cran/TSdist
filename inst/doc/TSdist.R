### R code from vignette source 'TSdist.Rnw'

###################################################
### code chunk number 1: init-prompt
###################################################
options(prompt= "R> ", continue = "+  ")


###################################################
### code chunk number 2: TSdist.Rnw:547-550
###################################################
library("TSdist")
data(example.series1)
data(example.series2)


###################################################
### code chunk number 3: TSdist.Rnw:552-554
###################################################
crossCorrelationDistance(example.series1, example.series2)
correlationDistance(example.series1, example.series2)


###################################################
### code chunk number 4: TSdist.Rnw:561-562
###################################################
manhattanDistance(example.series1, example.series2)


###################################################
### code chunk number 5: TSdist.Rnw:566-567
###################################################
lpDistance(example.series1, example.series2, method="manhattan")


###################################################
### code chunk number 6: TSdist.Rnw:575-577
###################################################
edrDistance(example.series1, example.series2, epsilon=0.1)
erpDistance(example.series1, example.series2, g=0)


###################################################
### code chunk number 7: TSdist.Rnw:585-587
###################################################
data(example.series3)
data(example.series4)


###################################################
### code chunk number 8: TSdist.Rnw:589-590
###################################################
 lcssDistance(example.series3, example.series4, epsilon=0.1)


###################################################
### code chunk number 9: TSdist.Rnw:607-609
###################################################
length(example.series3)
length(example.series4)


###################################################
### code chunk number 10: TSdist.Rnw:675-677
###################################################
data(zoo.series1)
data(zoo.series2)


###################################################
### code chunk number 11: TSdist.Rnw:679-681
###################################################
tsDistances(zoo.series1, zoo.series2, distance="pearsoncorrelation")
tsDistances(zoo.series1, zoo.series2, distance="dtw", sigma=10)


###################################################
### code chunk number 12: TSdist.Rnw:738-739
###################################################
data(example.database)


###################################################
### code chunk number 13: TSdist.Rnw:741-744
###################################################
dist(example.database, method="tsDistances", 
distance="tquest", tau=mean(example.database), 
diag=TRUE, upper=TRUE)


###################################################
### code chunk number 14: TSdist.Rnw:749-750
###################################################
data(zoo.database)


###################################################
### code chunk number 15: TSdist.Rnw:752-754 (eval = FALSE)
###################################################
## tsDatabaseDistances(example.database, method="tquest", 
## tau=mean(example.database), diag=TRUE, upper=TRUE)


###################################################
### code chunk number 16: TSdist.Rnw:761-763
###################################################
tsDatabaseDistances(zoo.database, method="tquest", 
tau=mean(zoo.database), diag=TRUE, upper=TRUE)


