# Markov Regime Switchin Modelling

# import libraries
library(tseries)
library(MSwM)
library(forecast)

# set working directory
setwd ("/Users/katharinaenders/Documents")

#--------------------------------------Forecasting insample--------------------------------
#-----------------------------------------------------------------------------------------
#create object ifo
#ifo <- read.csv2(file="ifo.csv",header=T)
#ifo <- ts(as.numeric(ifo[,2]), start=c(1991,1), frequency = 12)

#plot(ifo, type="l", col="darkblue")

ifo <- read.csv2(file="ifo_index.csv",header=T) # ifo object as data.frame
ifo <- ts(as.numeric(ifo[,2]), start=c(1991,1), end=c(2017,7), frequency=12) #ifo as ts
ifo <- as.numeric(as.matrix(ifo[,2]))
summary(ifo) #descriptive stat
ifo
plot(ifo,  main="Ifo Business Climate Index", type="l", col="blue", sub="1991:1 - 2017:7", ylab="Ifo (2005 = 100)")

#Model Lags

ifo_1 <- c(NA, ifo[-length(ifo)])
#ifo_2 <- c(NA, ifo_1[-length(ifo_1)])
#ifo_3 <- c(NA, ifo_2[-length(ifo_2)])
#ifo_4 <- c(NA, ifo_3[-length(ifo_3)])


#model with 1 Lag
ifodata <- na.omit(cbind(ifo,ifo_1))

#LModel
model=lm(ifo ~ ., data=as.data.frame(ifodata))

summary (model)

#MSWM Model

MSmodel = msmFit(model, k=2, sw=rep(TRUE, 3)) 
summary(MSmodel)


#Run commands for graphical representation of MS:

#plotProb(MSmodel, which =1)
plotProb(MSmodel, which =2) #'Regime 1 Detection'
plotProb(MSmodel, which =3) #'Regime 2 Detection'

plotReg(MSmodel, regime = 2) #'Regime 1&2 Detection'

### Residual Analysis

# To get diagnostic tests for regime 1, run:
#plotDiag(MSmodel, regime=1, which=1)
#plotDiag(MSmodel, regime=1, which=2)
#plotDiag(MSmodel, regime=1, which=3)


# To get diagnostic tests for regime 2, run:
#plotDiag(MSmodel, regime=2, which=1)
#plotDiag(MSmodel, regime=2, which=2)
#plotDiag(MSmodel, regime=2, which=3)


#-----------------------------------Forecasting outsample -------------------------
#---------------------------------------------------------------------------------
#while loop
#Specifications: 33 values to forcast (out of sample)
#                286 values in sample


## inital values

#specify length of in-sample / outsample
In.sample <- window(ifo, end=c(2014, 10))
Out.sample <- window(ifo, start=c(2014, 11))

#counter var i
i <- 0

#Forcast 30 iterations

while (i < 33) {
  
  
  #MS Model on in-sample
  
  #1 lag on in sample
  ifo_1.In.sample <- c(NA, In.sample[-length(In.sample)])
  #2 lag on in sample
  #ifo_2.In.sample <- c(NA, ifo_1.In.sample[-length(ifo_1.In.sample)])
  
  
  ifodata.sample <- na.omit(cbind(In.sample ,ifo_1.In.sample))
  
  
  #LModel on sample
  model.sample = lm(In.sample ~ ., data=as.data.frame(ifodata.sample))
  
  summary (model.sample)
  
  #MSwM on in sample
  MSmodel.sample = msmFit(model.sample, k=2, sw=rep(TRUE, 3)) 
  
  summary(MSmodel.sample)
  
  
  #estimating Parameters
  #extract b and m for each state
  In.sample.intercept <- MSmodel.sample@Coef
  R1.m <- In.sample.intercept[1,1]
  R2.m <- In.sample.intercept[2,1]
  R1.b <- In.sample.intercept[1,2]
  R2.b <- In.sample.intercept[2,2]
  
  
  #extract transition probabilities values
  #In.sample.trans.prob <- MSmodel.sample@transMat
  
  # Forecasting Y(t+1)
  
  #State dependend
  Y.t <- In.sample[286+i]
  
  Y.t_1.s1 <- (Y.t*R1.b)+R1.b #in state 1
  Y.t_1.s2 <- (Y.t*R2.b)+R2.b #in state 2
  
  
  #extract smoothed probabilities 
  smoProb <- ts(MSmodel.sample@Fit@smoProb[,1])
  
  #value for yt
  yt.smoProb <- as.numeric(smoProb[286+i])
  
  #weightend with smoothed probabilities
  Y.t_1.w <- (Y.t_1.s1*yt.smoProb)+(Y.t_1.s2*(1-yt.smoProb))
  
  
  In.sample.New <- append(In.sample,as.numeric(Y.t_1.w), after = length(In.sample) )
  
  In.sample <- In.sample.New
  #counter
  i = i+1
  
}

forcast.row <- ts(In.sample, start=c(1991,1), end=c(2017,7), frequency=12)
data.row <- ifo



#plot forecast and ifo data
plot(forcast.row)
seqplot.ts(forcast.row, main='Forecast 3', colx="red", coly="darkblue", data.row)


#Quality Test forecast vs ifo data
accuracy(forcast.row, data.row, test=NULL, d=NULL, D=NULL)





