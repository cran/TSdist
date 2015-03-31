library(TSdist);
library(TSclust);
library(longitudinalData);
library(pdc);

#Synthetic toy examples that can be calculated easily by hand and are valuable 
#to test the correctness of the distances measures: 

x<-c(0,2)
y<-c(1,3)

euclideanDistance(x, y) # 1.414214 
manhattanDistance(x, y) # 2
minkowskiDistance(x, y, p=3) # 1.259921
inf.normDistance(x, y) # 1
corDistance(x, y) # 2.107342e-08 (correlation between x and y is 1)
corDistance(x, y, beta=1) # 1.053671e-08 (correlation between x and y is 1)
ccorDistance(x, y) # 0 (cross correlation at lag 0 is 1)
stsDistance(x, y) # 0 (series have the same slope)
dtwDistance(x, y) # 3 (no warping window and symmetric2 step pattern.)
lb.keoghDistance(x, y, window.size=1) #1.414214 is lower than dtwDistance
edrDistance(x, y, epsilon=1) # 0 (matching is done directly point by point)
erpDistance(x, y, g=0) # 2 (matching is done directly point by point)
lcssDistance(x, y, epsilon=1) # 2 (These two elements are equal to this distance)
fourierDistance(x, y) # 2 (Fourier coefficients are (2,-2) and (4,-2))
dissimDistance(x, y) # 1 (Euclidean distance between series is constant=1, so integral is directly obtained)
dissimapproxDistance(x, y) # 1 (D(t_1)=D(t_2)=1, so 1/2*(1+1)/1=1)
acfDistance(x, y) # 0 (autocorrelation of both series are equal.)
pacfDistance(x, y) # 0 (partial autocorrelation of both series are equal.)


#Other synthetic toy examples to verify the correctness of DTW, ERP, EDR and LCSS.
#Examples taken from: 

q<-c(0) 
r<-c(1,2)
s<-c(2,3,3)

dtwDistance(q, r) # 3
dtwDistance(r, s) # 3
dtwDistance(q, s) # 8

edrDistance(q, r, epsilon=1) # 1
edrDistance(r, s, epsilon=1) # 1
edrDistance(q, s, epsilon=1) # 3

erpDistance(q, r, g=0) # 3
erpDistance(r, s, g=0) # 5
erpDistance(q, s, g=0) # 8

lcssDistance(q, r, epsilon=1) # 1
lcssDistance(r, s, epsilon=1) # 2
lcssDistance(q, s, epsilon=1) # 0

#Use of time series objects: 
data(zoo.series1)
data(zoo.series2)

#zoo
tsDistances(zoo.series1, zoo.series2, distance="edr", epsilon=0)
tsDistances(zoo.series1, zoo.series2, distance="erp", g=0)

#ts
ts.series1<-as.ts(zoo.series1)
ts.series2<-as.ts(zoo.series2)
tsDistances(ts.series1, ts.series2, distance="edr", epsilon=0)
tsDistances(ts.series1, ts.series2, distance="erp", g=0)

#xts
xts.series1<-xts::as.xts(zoo.series1)
xts.series2<-xts::as.xts(zoo.series2)
tsDistances(xts.series1, xts.series2, distance="edr", epsilon=0)
tsDistances(xts.series1, xts.series2, distance="erp", g=0)


#Examples of distances matrix computations: 
data(example.database)
data(zoo.database)

#For numerical matrix, using proxy:
dist(example.database, distance="tsDistances", 
     distance="tquest", tau=mean(example.database), 
     diag=TRUE, upper=TRUE)

#For numerical matrix, using tsDatabaseDistances.
tsDatabaseDistances(example.database, distance="tquest", 
                    tau=mean(example.database), diag=TRUE, upper=TRUE)

#For multivariate zoo object:
tsDatabaseDistances(zoo.database, distance="tquest", 
                    tau=mean(zoo.database), diag=TRUE, upper=TRUE)

#For mts object: 
example.database3<-as.ts(zoo.database)
tsDatabaseDistances(example.database3, distance="tquest", 
                    tau=mean(example.database3), diag=TRUE, upper=TRUE)

#For multivariate xts object: 
example.database4<-xts::as.xts(zoo.database)
tsDatabaseDistances(example.database4, distance="tquest", 
                    tau=mean(example.database4), diag=TRUE, upper=TRUE)



#Try 1NN function for example.database2
data(example.database2)

#Create train/test by dividing the dataset 70%/30%
trainindex <- sample(1:100, 70, replace=FALSE)
train <- example.database2[[1]][trainindex,]
test <- example.database2[[1]][-trainindex,]
trainc <- example.database2[[2]][trainindex]
testc <- example.database2[[2]][-trainindex]


#Apply 1NN
oneNN(train, trainc, test, testc, "ccor")
oneNN(train, trainc, test, testc, "dtw")
oneNN(train, trainc, test, testc, "acf")
oneNN(train, trainc, test, testc, "cid")

#Try the k-medoids algorithm on the example.database3 database.
data(example.database3)
tsdata <- example.database3[[1]]
groundt <- example.database3[[2]]

#Apply K-medoids
k.medoids(data=tsdata, ground.truth=groundt, k=5, "lb_keogh", window.size=31)
k.medoids(data=tsdata, ground.truth=groundt, k=5, "edr", epsilon=0.1)
k.medoids(data=tsdata, ground.truth=groundt, k=5, "ar.mah.statistic")
k.medoids(data=tsdata, ground.truth=groundt, k=5, "ar.lpc.ceps")
