Dat_yield <- read.csv(file="/Users/user/Downloads/APM466_template_final.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
library(xtable)
library(leaflet)
#Clean the csv file 
bonds<-as.data.frame(Dat_yield)
bonds$MATURITYDATE
mature_date <- as.Date(bonds$MATURITYDATE)
mature_date
bonds <- bonds[-c(1,2,4,5,6)]
bonds <- bonds[c(1:11)]
bonds[11,] <- bonds[10,]
r<-rep(1, 10)
R<-data.frame(r, r, r, r, r, r, r, r, r, r)
for (j in c(1:10)) {
  R[1,j]<-(log((bonds[1,j+1])/(((bonds[1,1])*100)/2+100)))*(-2)
  }
for ( j in c(1:10)) {
  for (i in c(2:10)) {
    R[i,j]<- -log((bonds[i,(j+1)]-
                sum(((bonds[i,1]*100)/2)
                    *exp(R[c(1:(i-1)),j]*(-c(1:(i-1))))))
                *((100+bonds[i,1]*100/2)^(-1)))*(2/i) 
  }
}
R# where the first ten entries are ten bonds with different matruity dates, they are all calculated by the price of the first day
A <- R
colnames(A)<- pricedate
rownames(A)<-c("r1/2", "r1", "r3/2", "r2", "r5/2", "r3", "r7/2", "r4",
               "r9/2", "r5")
xtable(A, digits = 5)
R1<-as.data.frame(unlist(R))
new <- R1[c(c(1:10)%%2==0),]
new <- as.vector(new)
new #is the vector contains ytm , the first five is the ytm at different maturity at 1.02)
R<-as.data.frame(unlist(R))
R
plot_colours <- c("blue", "red", "forestgreen", "yellow", rgb(0.3,0.3,.3),
                  rgb(0.6,0.3,0), rgb(.9,0,0), 
                  rgb(0.3,0.6,0), rgb(0.3,0,.6), rgb(0,0.3,.6))


Tms <- seq(0.5, 5, by = 0.5)
cs1 <- splinefun(Tms, 100*R[c(1:10),], method = "monoH.FC")
cs2 <- splinefun(Tms, 100*R[c(11:20),], method = "monoH.FC")
cs3 <- splinefun(Tms, 100*R[c(21:30),], method = "monoH.FC")
cs4 <- splinefun(Tms, 100*R[c(31:40),], method = "monoH.FC")
cs5 <- splinefun(Tms, 100*R[c(41:50),], method = "monoH.FC")
cs6 <- splinefun(Tms, 100*R[c(51:60),], method = "monoH.FC")
cs7 <- splinefun(Tms, 100*R[c(61:70),], method = "monoH.FC")
cs8 <- splinefun(Tms, 100*R[c(71:80),], method = "monoH.FC")
cs9 <- splinefun(Tms, 100*R[c(81:90),], method = "monoH.FC")
cs10 <- splinefun(Tms, 100*R[c(91:100),], method = "monoH.FC")
curve(cs1(x, deriv = 0), 0, 5, col=plot_colours[1], ylab = "Yield (in %)", xlab = "Time")
curve(cs2(x, deriv = 0), 0, 5, col = plot_colours[2], ylab = "Yield (in %)", xlab = "Time",add=TRUE)
curve(cs3(x, deriv = 0), 0, 5, col = plot_colours[3], ylab = "Yield (in %)", xlab = "Time",add=TRUE)
curve(cs4(x, deriv = 0), 0, 5, col = plot_colours[4], ylab = "Yield (in %)", xlab = "Time",add=TRUE)
curve(cs5(x, deriv = 0), 0, 5, col = plot_colours[5], ylab = "Yield (in %)", xlab = "Time",add=TRUE)
curve(cs6(x, deriv = 0), 0, 5, col = plot_colours[6], ylab = "Yield (in %)", xlab = "Time",add=TRUE)
curve(cs7(x, deriv = 0), 0, 5, col = plot_colours[7], ylab = "Yield (in %)", xlab = "Time",add=TRUE)
curve(cs8(x, deriv = 0), 0, 5, col = plot_colours[8], ylab = "Yield (in %)", xlab = "Time",add=TRUE)
curve(cs9(x, deriv = 0), 0, 5, col = plot_colours[9], ylab = "Yield (in %)", xlab = "Time",add=TRUE)
curve(cs10(x, deriv = 0), 0, 5, col = plot_colours[10], add=TRUE)
pricedate <- c('2020-01-02','2020-01-03','2020-01-06','2020-01-07','2020-01-08','2020-01-09','2020-01-10','2020-01-13','2020-01-14','2020-01-15')
title(main="All Spot Curves", 
      col.main="forestgreen", font.main=4)
legend(2,2.8,paste("Yield Curve for ",pricedate,sep=""), lty=c(1,1),lwd=c(2,2),cex=.7, bty = "n", col=plot_colours)

Ryrs<-as.data.frame(R[c(c(1:100)%%2==0),])
Ryrs #the even entry of r, which means they are r1, r2, r3, r4, r5, since there are 10 day's price, there are 50 rates we need to calculate forward rates in total)
Ryrs<-as.vector(t(Ryrs))
Ryrs
g<-rep(1,3)
f1<-data.frame(g,g,g,g,g,g,g,g,g,g)
for (j in c(1:10)) {
  for (i in c(1:3)) {
    f1[i,j]<- ((i+1)*Ryrs[j*5-4+i]-Ryrs[j*5-5+i])/i
  }
}
f1
r1 <- rep(1,10)
data.frame(r1)
for (j in c(1:10)) {
  r1[j] <- Ryrs[5*(j-1)+1]
}
r1
r1 <- 100*r1
as.vector(r1)
f1<-as.data.frame(t(f1))
f1<-100*f1
f1 <- data.frame(r1, f1)
f2<-as.vector(f1)
timef <- seq(0.5, 2, by = 0.5)
plot(timef, f2[1,], type="l", col=plot_colours[1],ann = F)
lines(timef, f2[2,], type="l", 
      lty=1, col=plot_colours[2])
lines(timef, f2[3,], type="l", 
      lty=1, col=plot_colours[3])
lines(timef, f2[4,], type="l", 
      lty=1, col=plot_colours[4])
lines(timef, f2[5,], type="l", 
      lty=1, col=plot_colours[5])
lines(timef, f2[6,], type="l", 
      lty=1, col=plot_colours[6])
lines(timef, f2[7,], type="l", 
      lty=1, col=plot_colours[7])
lines(timef, f2[8,], type="l", 
      lty=1, col=plot_colours[8])
lines(timef, f2[9,], type="l", 
      lty=1, col=plot_colours[9])
lines(timef, f2[10,], type="l", 
      lty=1, col=plot_colours[10])
legend(1,1.8,paste("Forward Curve for ",pricedate,sep=""), lty=c(1,1),lwd=c(2,2),cex=.6, bty = "n", col=plot_colours)
help(legend)
title(main="Forward Curves", 
      col.main="forestgreen", font.main=4)
title(xlab="Time", col.lab=rgb(0,0.6,.7))
title(ylab="f(t,T)" , col.lab=rgb(0,0.6,.7))


cs11 <- splinefun(timef, f2[1,], method = "monoH.FC")
cs12 <- splinefun(timef, f2[2,], method = "monoH.FC")
cs13 <- splinefun(timef, f2[3,], method = "monoH.FC")
cs14 <- splinefun(timef, f2[4,], method = "monoH.FC")
cs15 <- splinefun(timef, f2[5,], method = "monoH.FC")
cs16 <- splinefun(timef, f2[6,], method = "monoH.FC")
cs17 <- splinefun(timef, f2[7,], method = "monoH.FC")
cs18 <- splinefun(timef, f2[8,], method = "monoH.FC")
cs19 <- splinefun(timef, f2[9,], method = "monoH.FC")
cs110 <- splinefun(timef, f2[10,], method = "monoH.FC")
curve(cs11(x, deriv = 0), 0, 2, col=  plot_colours[1], ylab = "Foward rate (in %)", xlab = "Time")
curve(cs12(x, deriv = 0), 0, 2, col = plot_colours[2], ylab = "Foward rate (in %)", xlab = "Time",add=TRUE)
curve(cs13(x, deriv = 0), 0, 2, col = plot_colours[3], ylab = "Foward rate (in %)", xlab = "Time",add=TRUE)
curve(cs14(x, deriv = 0), 0, 2, col = plot_colours[4], ylab = "Foward rate (in %)", xlab = "Time",add=TRUE)
curve(cs15(x, deriv = 0), 0, 2, col = plot_colours[5], ylab = "Foward rate (in %)", xlab = "Time",add=TRUE)
curve(cs16(x, deriv = 0), 0, 2, col = plot_colours[6], ylab = "Foward rate (in %)", xlab = "Time",add=TRUE)
curve(cs17(x, deriv = 0), 0, 2, col = plot_colours[7], ylab = "Foward rate (in %)", xlab = "Time",add=TRUE)
curve(cs18(x, deriv = 0), 0, 2, col = plot_colours[8], ylab = "Foward rate (in %)", xlab = "Time",add=TRUE)
curve(cs19(x, deriv = 0), 0, 2, col = plot_colours[9], ylab = "Foward rate (in %)", xlab = "Time",add=TRUE)
curve(cs110(x, deriv = 0), 0, 2, col = plot_colours[10], add=TRUE)


#Ryrs is the yield for r1
#Covariance 
diff1<-c(1:9)
diff2<-c(1:9)
diff3<-c(1:9)
diff4<-c(1:9)
diff5<-c(1:9)
for (i in c(1:9)) {
  diff1[i]<-log(new[(5*i+1)]/new[5*(i-1)+1])
}
diff1
for (i in c(1:9)) {
  diff2[i]<-log(new[(5*i+2)]/new[5*(i-1)+2])
}
diff2
for (i in c(1:9)) {
  diff3[i]<-log(new[(5*i+3)]/new[5*(i-1)+3])
}
diff3
for (i in c(1:9)) {
  diff4[i]<-log(new[(5*i+4)]/new[5*(i-1)+4])
}
diff4
for (i in c(1:9)) {
  diff5[i]<-log(new[(5*i+5)]/new[5*(i-1)+5])
}
diff5
diff <- cbind(diff1,diff2,diff3,diff4,diff5)
diff
covdif<-cov(diff, diff)
covdif
xtable(diff,digits = 10)
xtable(10^4*covdif, digits = 4)
eeg<-eigen(cov(diff, diff), symmetric='T')
eeg$vectors

xtable(as.data.frame(eeg$vectors), digits = 4)
xtable(1000*as.data.frame(eeg$values), digits = 4)
#for foward rates
dif1<-c(1:9)
dif2<-c(1:9)
dif3<-c(1:9)
dif4<-c(1:9)
for (i in c(1:9)) {
  dif1[i]<-log(f1[i+1,1]/f1[i,1])
}
dif1
for (i in c(1:9)) {
  dif2[i]<-log(f1[i+1,2]/f1[i,2])
}
dif2
for (i in c(1:9)) {
  dif3[i]<-log(f1[i+1,3]/f1[i,3])
}
dif3
for (i in c(1:9)) {
  dif4[i]<-log(f1[i+1,4]/f1[i,4])
}
dif4
difff <- cbind(dif1,dif2,dif3,dif4)
covdiff<-cov(difff, difff)
covdiff
xtable(10^4*covdiff, digits = 3)
eegf<-eigen(cov(difff, difff), symmetric='T')
eegf

xtable(eegf$vectors, digits = 3)
xtable(1000*as.data.frame(eegf$values), digits = 4)



