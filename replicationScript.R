# Figure 1
library(datasets)
library(tidyverse)
library(bayest)
data("sleep")
grp1 = as.vector(sleep %>% filter(group == 1) %>% select(extra))$extra
grp2 = as.vector(sleep %>% filter(group == 2) %>% select(extra))$extra
set.seed(4298)
bayes.t.test(n=50000, plot="all", firstComp = grp1, secondComp = grp2, hyperpars="narrow", ci=0.95, burnin = 25000, sd="sd")




#############################################################################

# Figure 2
### SMALL EFFECT SIZE ###
set.seed(917165)
newpar <- par(mfrow=c(3,4),mar=c(2,1.5,1.5,1.5))

# Small effect size, N = 100
smplsize=100
effSizePostExpVecSmallEffectN100<-numeric(it)
effSizeLowerCIVecSmallEffectN100<-numeric(it)
effSizeUpperCIVecSmallEffectN100<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a small true effect size
  firstComponent=rnorm(smplsize,mean=3.5,sd=1.56)
  secondComponent=rnorm(smplsize,mean=2.89,sd=1.84)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecSmallEffectN100[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecSmallEffectN100[i]=resDF$LowerCI[3]
  effSizeUpperCIVecSmallEffectN100[i]=resDF$UpperCI[3]
} 

effSz=(2.89-3.5)/(sqrt((1.56^2+1.84^2)/2))
par(mar=c(2,4,1.5,1.5))
plot(effSizePostExpVecSmallEffectN100,pch=16,col="blue",lwd=1,ylim=c(0,-0.8),xaxt='n',main="N=100",ylab=expression(delta),xlab="")
par(mar=c(2,2,1.5,1.5))
points(effSizeLowerCIVecSmallEffectN100,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecSmallEffectN100,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=-0.2,b=0,lwd=1,col="grey",lty="dotted")
abline(a=-0.5,b=0,lwd=1,col="grey",lty="dotted")
#lines(x=c(0,0),y=c(-0.2,-0.5),ty="l",lty="dotted",lwd=1)
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecSmallEffectN100[z],effSizeUpperCIVecSmallEffectN100[z]),ty="l",lty="dotted",lwd=1)
}






# Small effect size, N = 200
smplsize=200
effSizePostExpVecSmallEffectN200<-numeric(it)
effSizeLowerCIVecSmallEffectN200<-numeric(it)
effSizeUpperCIVecSmallEffectN200<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a small true effect size
  firstComponent=rnorm(smplsize,mean=3.5,sd=1.56)
  secondComponent=rnorm(smplsize,mean=2.89,sd=1.84)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecSmallEffectN200[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecSmallEffectN200[i]=resDF$LowerCI[3]
  effSizeUpperCIVecSmallEffectN200[i]=resDF$UpperCI[3]
} 

effSz=(2.89-3.5)/(sqrt((1.56^2+1.84^2)/2))
plot(effSizePostExpVecSmallEffectN200,pch=16,col="blue",lwd=1,ylim=c(0,-0.8),xaxt='n',main="N=200",ylab=expression(delta))
points(effSizeLowerCIVecSmallEffectN200,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecSmallEffectN200,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=-0.2,b=0,lwd=1,col="grey",lty="dotted")
abline(a=-0.5,b=0,lwd=1,col="grey",lty="dotted")
#lines(x=c(0,0),y=c(-0.2,-0.5),ty="l",lty="dotted",lwd=1)
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecSmallEffectN200[z],effSizeUpperCIVecSmallEffectN200[z]),ty="l",lty="dotted",lwd=1)
}



# Small effect size, N = 300
smplsize=300
effSizePostExpVecSmallEffectN300<-numeric(it)
effSizeLowerCIVecSmallEffectN300<-numeric(it)
effSizeUpperCIVecSmallEffectN300<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a small true effect size
  firstComponent=rnorm(smplsize,mean=3.5,sd=1.56)
  secondComponent=rnorm(smplsize,mean=2.89,sd=1.84)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecSmallEffectN300[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecSmallEffectN300[i]=resDF$LowerCI[3]
  effSizeUpperCIVecSmallEffectN300[i]=resDF$UpperCI[3]
} 

effSz=(2.89-3.5)/(sqrt((1.56^2+1.84^2)/2))
plot(effSizePostExpVecSmallEffectN300,pch=16,col="blue",lwd=1,ylim=c(0,-0.8),xaxt='n',main="N=300",ylab=expression(delta))
points(effSizeLowerCIVecSmallEffectN300,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecSmallEffectN300,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=-0.2,b=0,lwd=1,col="grey",lty="dotted")
abline(a=-0.5,b=0,lwd=1,col="grey",lty="dotted")
#lines(x=c(0,0),y=c(-0.2,-0.5),ty="l",lty="dotted",lwd=1)
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecSmallEffectN300[z],effSizeUpperCIVecSmallEffectN300[z]),ty="l",lty="dotted",lwd=1)
}



# Small effect size, N = 700
smplsize=700
effSizePostModeVecSmallEffectN700<-numeric(it)
effSizeLowerCIVecSmallEffectN700<-numeric(it)
effSizeUpperCIVecSmallEffectN700<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a small true effect size
  firstComponent=rnorm(smplsize,mean=3.5,sd=1.56)
  secondComponent=rnorm(smplsize,mean=2.89,sd=1.84)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostModeVecSmallEffectN700[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecSmallEffectN700[i]=resDF$LowerCI[3]
  effSizeUpperCIVecSmallEffectN700[i]=resDF$UpperCI[3]
} 

effSz=(2.89-3.5)/(sqrt((1.56^2+1.84^2)/2))
plot(effSizePostModeVecSmallEffectN700,pch=16,col="blue",lwd=1,ylim=c(0,-0.8),xaxt='n',main="N=700",ylab=expression(delta))
points(effSizeLowerCIVecSmallEffectN700,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecSmallEffectN700,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=-0.2,b=0,lwd=1,col="grey",lty="dotted")
abline(a=-0.5,b=0,lwd=1,col="grey",lty="dotted")
#lines(x=c(0,0),y=c(-0.2,-0.5),ty="l",lty="dotted",lwd=1)
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecSmallEffectN700[z],effSizeUpperCIVecSmallEffectN700[z]),ty="l",lty="dotted",lwd=1)
}




############ MEDIUM EFFECT SIZE ################
# Medium effect size, N = 100
smplsize=100
effSizePostExpVecMediumEffectN100<-numeric(it)
effSizeLowerCIVecMediumEffectN100<-numeric(it)
effSizeUpperCIVecMediumEffectN100<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a medium true effect size
  firstComponent=rnorm(smplsize,mean=254.08,sd=2.36)
  secondComponent=rnorm(smplsize,mean=255.84,sd=3.04)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecMediumEffectN100[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecMediumEffectN100[i]=resDF$LowerCI[3]
  effSizeUpperCIVecMediumEffectN100[i]=resDF$UpperCI[3]
} 

effSz=(255.84-254.08)/(sqrt((2.36^2+3.04^2)/2))
par(mar=c(2,4,1.5,1.5))
plot(effSizePostExpVecMediumEffectN100,pch=16,col="blue",lwd=1,ylim=c(0,1),xaxt='n',main="N=100",ylab=expression(delta))
par(mar=c(2,2,1.5,1.5))
points(effSizeLowerCIVecMediumEffectN100,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecMediumEffectN100,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=0.5,b=0,lwd=1,col="grey",lty="dotted")
abline(a=0.8,b=0,lwd=1,col="grey",lty="dotted")
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecMediumEffectN100[z],effSizeUpperCIVecMediumEffectN100[z]),ty="l",lty="dotted",lwd=1)
}



# Medium effect size, N = 200
smplsize=200
effSizePostExpVecMediumEffectN200<-numeric(it)
effSizeLowerCIVecMediumEffectN200<-numeric(it)
effSizeUpperCIVecMediumEffectN200<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a medium true effect size
  firstComponent=rnorm(smplsize,mean=254.08,sd=2.36)
  secondComponent=rnorm(smplsize,mean=255.84,sd=3.04)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecMediumEffectN200[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecMediumEffectN200[i]=resDF$LowerCI[3]
  effSizeUpperCIVecMediumEffectN200[i]=resDF$UpperCI[3]
} 

effSz=(255.84-254.08)/(sqrt((2.36^2+3.04^2)/2))
plot(effSizePostExpVecMediumEffectN200,pch=16,col="blue",lwd=1,ylim=c(0,1),xaxt='n',main="N=200",ylab=expression(delta))
points(effSizeLowerCIVecMediumEffectN200,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecMediumEffectN200,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=0.5,b=0,lwd=1,col="grey",lty="dotted")
abline(a=0.8,b=0,lwd=1,col="grey",lty="dotted")
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecMediumEffectN200[z],effSizeUpperCIVecMediumEffectN200[z]),ty="l",lty="dotted",lwd=1)
}



# Medium effect size, N = 400
smplsize=400
effSizePostExpVecMediumEffectN400<-numeric(it)
effSizeLowerCIVecMediumEffectN400<-numeric(it)
effSizeUpperCIVecMediumEffectN400<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a medium true effect size
  firstComponent=rnorm(smplsize,mean=254.08,sd=2.36)
  secondComponent=rnorm(smplsize,mean=255.84,sd=3.04)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecMediumEffectN400[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecMediumEffectN400[i]=resDF$LowerCI[3]
  effSizeUpperCIVecMediumEffectN400[i]=resDF$UpperCI[3]
} 

effSz=(255.84-254.08)/(sqrt((2.36^2+3.04^2)/2))
plot(effSizePostExpVecMediumEffectN400,pch=16,col="blue",lwd=1,ylim=c(0,1),xaxt='n',main="N=400",ylab=expression(delta))
points(effSizeLowerCIVecMediumEffectN400,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecMediumEffectN400,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=0.5,b=0,lwd=1,col="grey",lty="dotted")
abline(a=0.8,b=0,lwd=1,col="grey",lty="dotted")
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecMediumEffectN400[z],effSizeUpperCIVecMediumEffectN400[z]),ty="l",lty="dotted",lwd=1)
}



# Medium effect size, N = 600
smplsize=600
effSizePostExpVecMediumEffectN600<-numeric(it)
effSizeLowerCIVecMediumEffectN600<-numeric(it)
effSizeUpperCIVecMediumEffectN600<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a medium true effect size
  firstComponent=rnorm(smplsize,mean=254.08,sd=2.36)
  secondComponent=rnorm(smplsize,mean=255.84,sd=3.04)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecMediumEffectN600[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecMediumEffectN600[i]=resDF$LowerCI[3]
  effSizeUpperCIVecMediumEffectN600[i]=resDF$UpperCI[3]
} 

effSz=(255.84-254.08)/(sqrt((2.36^2+3.04^2)/2))
plot(effSizePostExpVecMediumEffectN600,pch=16,col="blue",lwd=1,ylim=c(0,1),xaxt='n',main="N=600",ylab=expression(delta))
points(effSizeLowerCIVecMediumEffectN600,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecMediumEffectN600,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=0.5,b=0,lwd=1,col="grey",lty="dotted")
abline(a=0.8,b=0,lwd=1,col="grey",lty="dotted")
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecMediumEffectN600[z],effSizeUpperCIVecMediumEffectN600[z]),ty="l",lty="dotted",lwd=1)
}



########### LARGE EFFECT SIZE ############
# Large effect size, N = 50
smplsize=50
effSizePostExpVecLargeEffectN50<-numeric(it)
effSizeLowerCIVecLargeEffectN50<-numeric(it)
effSizeUpperCIVecLargeEffectN50<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a large true effect size
  firstComponent=rnorm(smplsize,mean=15.01,sd=3.4)
  secondComponent=rnorm(smplsize,mean=19.91,sd=5.8)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecLargeEffectN50[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecLargeEffectN50[i]=resDF$LowerCI[3]
  effSizeUpperCIVecLargeEffectN50[i]=resDF$UpperCI[3]
} 

effSz=(19.91-15.01)/(sqrt((5.8^2+3.4^2)/2))
par(mar=c(1.5,4,1.5,1.5))
plot(effSizePostExpVecLargeEffectN50,pch=16,col="blue",lwd=1,ylim=c(0,2),xaxt='n',main="N=50",ylab=expression(delta))
par(mar=c(1.5,2,1.5,1.5))
points(effSizeLowerCIVecLargeEffectN50,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecLargeEffectN50,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=0.8,b=0,lwd=1,col="grey",lty="dotted")
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecLargeEffectN50[z],effSizeUpperCIVecLargeEffectN50[z]),ty="l",lty="dotted",lwd=1)
}



# Large effect size, N = 100
smplsize=100
effSizePostExpVecLargeEffectN100<-numeric(it)
effSizeLowerCIVecLargeEffectN100<-numeric(it)
effSizeUpperCIVecLargeEffectN100<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a large true effect size
  firstComponent=rnorm(smplsize,mean=15.01,sd=3.4)
  secondComponent=rnorm(smplsize,mean=19.91,sd=5.8)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecLargeEffectN100[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecLargeEffectN100[i]=resDF$LowerCI[3]
  effSizeUpperCIVecLargeEffectN100[i]=resDF$UpperCI[3]
} 

effSz=(19.91-15.01)/(sqrt((5.8^2+3.4^2)/2))
plot(effSizePostExpVecLargeEffectN100,pch=16,col="blue",lwd=1,ylim=c(0,2),xaxt='n',main="N=100",ylab=expression(delta))
points(effSizeLowerCIVecLargeEffectN100,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecLargeEffectN100,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=0.8,b=0,lwd=1,col="grey",lty="dotted")
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecLargeEffectN100[z],effSizeUpperCIVecLargeEffectN100[z]),ty="l",lty="dotted",lwd=1)
}



# Large effect size, N = 150
smplsize=150
effSizePostExpVecLargeEffectN150<-numeric(it)
effSizeLowerCIVecLargeEffectN150<-numeric(it)
effSizeUpperCIVecLargeEffectN150<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a large true effect size
  firstComponent=rnorm(smplsize,mean=15.01,sd=3.4)
  secondComponent=rnorm(smplsize,mean=19.91,sd=5.8)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecLargeEffectN150[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecLargeEffectN150[i]=resDF$LowerCI[3]
  effSizeUpperCIVecLargeEffectN150[i]=resDF$UpperCI[3]
} 

effSz=(19.91-15.01)/(sqrt((5.8^2+3.4^2)/2))
plot(effSizePostExpVecLargeEffectN150,pch=16,col="blue",lwd=1,ylim=c(0,2),xaxt='n',main="N=150",ylab=expression(delta))
points(effSizeLowerCIVecLargeEffectN150,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecLargeEffectN150,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=0.8,b=0,lwd=1,col="grey",lty="dotted")
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecLargeEffectN150[z],effSizeUpperCIVecLargeEffectN150[z]),ty="l",lty="dotted",lwd=1)
}




# Large effect size, N = 200
smplsize=200
effSizePostExpVecLargeEffectN200<-numeric(it)
effSizeLowerCIVecLargeEffectN200<-numeric(it)
effSizeUpperCIVecLargeEffectN200<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit a large true effect size
  firstComponent=rnorm(smplsize,mean=15.01,sd=3.4)
  secondComponent=rnorm(smplsize,mean=19.91,sd=5.8)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecLargeEffectN200[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecLargeEffectN200[i]=resDF$LowerCI[3]
  effSizeUpperCIVecLargeEffectN200[i]=resDF$UpperCI[3]
} 

effSz=(19.91-15.01)/(sqrt((5.8^2+3.4^2)/2))
plot(effSizePostExpVecLargeEffectN200,pch=16,col="blue",lwd=1,ylim=c(0,2),xaxt='n',main="N=200",ylab=expression(delta))
points(effSizeLowerCIVecLargeEffectN200,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecLargeEffectN200,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=0.8,b=0,lwd=1,col="grey",lty="dotted")
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecLargeEffectN200[z],effSizeUpperCIVecLargeEffectN200[z]),ty="l",lty="dotted",lwd=1)
}





end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




#############################################################
# Figure 3
library(bayest)
set.seed(59870)
start.time <- Sys.time()
it=100

newpar <- par(mfrow=c(1,4))

### NO EFFECT SIZE ###
# No effect size, N=50
smplsize=50
effSizePostExpVecNoEffectN50<-numeric(it)
effSizeLowerCIVecNoEffectN50<-numeric(it)
effSizeUpperCIVecNoEffectN50<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit no effect size
  firstComponent=rnorm(smplsize,mean=148.3,sd=1.34)
  secondComponent=rnorm(smplsize,mean=148.3,sd=2.04)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecNoEffectN50[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecNoEffectN50[i]=resDF$LowerCI[3]
  effSizeUpperCIVecNoEffectN50[i]=resDF$UpperCI[3]
} 

effSz=0
par(mar=c(2,4,1.5,1.5))
plot(effSizePostExpVecNoEffectN50,pch=16,col="blue",lwd=1,ylim=c(-0.5,0.5),xaxt='n',main="N=50",ylab="",xlab="")
par(mar=c(2,2,1.5,1.5))
points(effSizeLowerCIVecNoEffectN50,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecNoEffectN50,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=-0.2,b=0,lwd=1,col="grey",lty="dotted")
abline(a=0.2,b=0,lwd=1,col="grey",lty="dotted")
#lines(x=c(0,0),y=c(-0.2,-0.5),ty="l",lty="dotted",lwd=1)
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecNoEffectN50[z],effSizeUpperCIVecNoEffectN50[z]),ty="l",lty="dotted",lwd=1)
}



# No effect size, N=100
smplsize=100
effSizePostExpVecNoEffectN100<-numeric(it)
effSizeLowerCIVecNoEffectN100<-numeric(it)
effSizeUpperCIVecNoEffectN100<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit no effect size
  firstComponent=rnorm(smplsize,mean=148.3,sd=1.34)
  secondComponent=rnorm(smplsize,mean=148.3,sd=2.04)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecNoEffectN100[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecNoEffectN100[i]=resDF$LowerCI[3]
  effSizeUpperCIVecNoEffectN100[i]=resDF$UpperCI[3]
} 

effSz=0
par(mar=c(2,4,1.5,1.5))
plot(effSizePostExpVecNoEffectN100,pch=16,col="blue",lwd=1,ylim=c(-0.5,0.5),xaxt='n',main="N=100",ylab="",xlab="")
par(mar=c(2,2,1.5,1.5))
points(effSizeLowerCIVecNoEffectN100,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecNoEffectN100,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=-0.2,b=0,lwd=1,col="grey",lty="dotted")
abline(a=0.2,b=0,lwd=1,col="grey",lty="dotted")
#lines(x=c(0,0),y=c(-0.2,-0.5),ty="l",lty="dotted",lwd=1)
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecNoEffectN100[z],effSizeUpperCIVecNoEffectN100[z]),ty="l",lty="dotted",lwd=1)
}



# No effect size, N=200
smplsize=200
effSizePostExpVecNoEffectN200<-numeric(it)
effSizeLowerCIVecNoEffectN200<-numeric(it)
effSizeUpperCIVecNoEffectN200<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit no effect size
  firstComponent=rnorm(smplsize,mean=148.3,sd=1.34)
  secondComponent=rnorm(smplsize,mean=148.3,sd=2.04)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecNoEffectN200[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecNoEffectN200[i]=resDF$LowerCI[3]
  effSizeUpperCIVecNoEffectN200[i]=resDF$UpperCI[3]
} 

effSz=0
par(mar=c(2,4,1.5,1.5))
plot(effSizePostExpVecNoEffectN200,pch=16,col="blue",lwd=1,ylim=c(-0.5,0.5),ylab="",xaxt='n',main="N=200",xlab="")
par(mar=c(2,2,1.5,1.5))
points(effSizeLowerCIVecNoEffectN200,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecNoEffectN200,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=-0.2,b=0,lwd=1,col="grey",lty="dotted")
abline(a=0.2,b=0,lwd=1,col="grey",lty="dotted")
#lines(x=c(0,0),y=c(-0.2,-0.5),ty="l",lty="dotted",lwd=1)
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecNoEffectN200[z],effSizeUpperCIVecNoEffectN200[z]),ty="l",lty="dotted",lwd=1)
}



# No effect size, N=300
smplsize=300
effSizePostExpVecNoEffectN300<-numeric(it)
effSizeLowerCIVecNoEffectN300<-numeric(it)
effSizeUpperCIVecNoEffectN300<-numeric(it)
effSizeMAPE<-numeric(it)
for(i in 1:it){
  # Simulate two datasets which exhibit no effect size
  firstComponent=rnorm(smplsize,mean=148.3,sd=1.34)
  secondComponent=rnorm(smplsize,mean=148.3,sd=2.04)
  
  # conduct the Gibbs sampling and isolate the posterior expectation as well as Lower and Upper CIs for the effect size
  resDF=bayes.t.test(10000,plot="none",firstComp = firstComponent, secondComp = secondComponent,hyperpars="wide",ci=0.95,burnin=5000,sd="sd",q=0.1)
  effSizePostExpVecNoEffectN300[i]=resDF$PosteriorExpectation[3]
  effSizeLowerCIVecNoEffectN300[i]=resDF$LowerCI[3]
  effSizeUpperCIVecNoEffectN300[i]=resDF$UpperCI[3]
} 

effSz=0
par(mar=c(2,4,1.5,1.5))
plot(effSizePostExpVecNoEffectN300,pch=16,col="blue",lwd=1,ylim=c(-0.5,0.5),xaxt='n',main="N=300",ylab="",xlab="")
par(mar=c(2,2,1.5,1.5))
points(effSizeLowerCIVecNoEffectN300,pch=45,col="blue",lwd=1)
points(effSizeUpperCIVecNoEffectN300,pch=45,col="blue",lwd=1)
abline(a=effSz,b=0,lwd=1,col="black")
abline(a=-0.2,b=0,lwd=1,col="grey",lty="dotted")
abline(a=0.2,b=0,lwd=1,col="grey",lty="dotted")
#lines(x=c(0,0),y=c(-0.2,-0.5),ty="l",lty="dotted",lwd=1)
for(z in 1:it){
  lines(x=c(z,z),y=c(effSizeLowerCIVecNoEffectN300[z],effSizeUpperCIVecNoEffectN300[z]),ty="l",lty="dotted",lwd=1)
}
