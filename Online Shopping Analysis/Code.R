#  Loading the data
setwd("C:/Users/eriko/Desktop/Uni/MASTER DATA/Asignaturas/1st Semi/Statistics for data analysis/Project")
Shopping = read.csv("FinalDataset.csv",sep = ";", dec = ".")

#  Some pre-processing
names(Shopping)[1] <- 'Age'
names(Shopping)[2] <- 'Gender'
names(Shopping)[3] <- 'Employment'
names(Shopping)[4] <- 'Income'
names(Shopping)[5] <- 'Buys'
names(Shopping)[6] <- 'Cost'
names(Shopping)[7] <- 'Platforms'
names(Shopping)[8] <- 'UsualPlatform'
names(Shopping)[9] <- 'Products'
names(Shopping)[10] <- 'CovidInfluence'
names(Shopping)[11] <- 'Confidence'
names(Shopping)
attach(Shopping)

#  In case we want to export to excel
library("writexl")
write_xlsx(Shopping,"C:/Users/eriko/Desktop/Uni/MASTER DATA/Asignaturas/1st Semi/Statistics for data analysis/Project/Dataset.xlsx")

#  One-hot-encode categorical variables
test <- strsplit(Shopping['Platforms'][,], ";")
test2 <- strsplit(Shopping['Products'][,], ";")

df <- data.frame(0,0,0,0,0)
df[1:length(test),] <- c(0,0,0,0,0)
names(df) <- c('Amazon', 'Aliexpress', 'Zalando/Asos', '2nd hand', 'Others')

for (i in 1:length(test)){
  for (j in 1:length(test[[i]])){ 
    if (test[[i]][j] == 'Amazon'){df[i,] = df[i,] + c(1,0,0,0,0)}
    if (test[[i]][j] == 'Aliexpress'){df[i,] = df[i,] + c(0,1,0,0,0)}
    if (test[[i]][j] == 'Zalando/Asos'){df[i,] = df[i,] + c(0,0,1,0,0)}
    if (test[[i]][j] == 'Second-hand apps (eBay, Wish, Wallapop, Milanuncios, Vinted)'){df[i,] = df[i,] + c(0,0,0,1,0)}
    if (test[[i]][j] == 'Other'){df[i,] = df[i,] + c(0,0,0,0,1)}
}}

df2 <- data.frame(0,0,0,0,0,0)
df2[1:length(test2),] <- c(0,0,0,0,0,0)
names(df2) <- c('Clothing/Sport', 'Technology', 'Personal care', 'Books', 'Home/Kitchen', 'Others')

for (i in 1:length(test2)){
  for (j in 1:length(test2[[i]])){ 
    if (test2[[i]][j] == 'Clothing/Sport Material'){df2[i,] = df2[i,] + c(1,0,0,0,0,0)}
    if (test2[[i]][j] == 'Technology'){df2[i,] = df2[i,] + c(0,1,0,0,0,0)}
    if (test2[[i]][j] == 'Personal care'){df2[i,] = df2[i,] + c(0,0,1,0,0,0)}
    if (test2[[i]][j] == 'Books'){df2[i,] = df2[i,] + c(0,0,0,1,0,0)}
    if (test2[[i]][j] == 'Home/Kitchen items'){df2[i,] = df2[i,] + c(0,0,0,0,1,0)}
    if (test2[[i]][j] == 'Other'){df2[i,] = df2[i,] + c(0,0,0,0,0,1)}
  }}

Encode <- cbind(df, df2)


#  Start descriptive analysis
#  One variable analysys
n = length(Age)

#  Histogram of Age
library(e1071) 
hist(Age,  main = '', xlim = c(10, 60), ylim = c(0, 50), xlab ='Age (years)')
skewness(Age)

#  Pie Chart Gender
slices <- c(round(table(Gender)/n*100))
lbls <- c('Female', 'Male')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls, "%", sep="") 
pie(slices, col=gray(seq(0.25, 0.75, length = 2)), labels = lbls)

#  Pie Chart Employment
slices <- c(round(table(Employment)/n*100))
lbls <- c('None', 'Studying', 'Working')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls, "%", sep="") 
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Employment Status")

#  Pie Chart Income
slices <- c(round(table(Income)/n*100))
lbls <- c('No', 'Yes')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls, "%", sep="") 
pie(slices,labels = lbls, col=rainbow(length(lbls)), main = "Outside-family Income")

#  Histogram of Buys in the last three months
hist(Buys, main = '', ylim = c(0, 70), xlab = 'Number of Buys')
skewness(Buys)

#  Log?
LogB <- log(Buys)
LogB <- LogB[LogB != -Inf]
hist(LogB, main = '', ylim = c(0, 30), xlab = 'Log_e(Number of Buys)')
skewness(LogB)

#  Histogram of last purchase amount
hist(Cost, main = '', ylim = c(0, 100), xlab = 'Last purchase amount (???)')
skewness(Cost)

#  Makes sense to do the log transfor as the skewness is very high
LogC <- log(Cost)
LogC <- LogC[LogC != -Inf]
hist(LogC, main = '', ylim = c(0, 30), xlab = 'Log_e(Last purchase amount (???))')
skewness(LogC)

Log10C <- log10(Cost)
Log10C <- Log10C[Log10C != -Inf]
hist(Log10C, main = '', ylim = c(0, 40), xlab = 'Log_10(Last purchase amount (???))')
skewness(Log10C)

#  BarPlot of Platforms
Plat_count <- unlist(strsplit(Shopping['Platforms'][,], ";"))
y <- table(Plat_count)/n*100
bp <- barplot(y, ylim = c(0, 100), names.arg=c("Aliexpress", "Amazon", "Other", "Second-hand", "Zalando/Asos"),
        main = '', ylab = 'Percentage')
text(bp,y+4,labels=paste(as.character(round(y)), "%", sep=" "))

#  Pie Chart Usual Platform
slices <- c(round(table(UsualPlatform)/n*100))
lbls <- c("Aliexpress", "Amazon", "None", "Other", "Second-hand", "Zalando/Asos")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls, "%", sep="") 
pie(slices,labels = lbls, col=gray(seq(1, 0, length = 6)), main = "")

#  BarPlot of Usual Products Bought
Prod_count <- unlist(strsplit(Shopping['Products'][,], ";"))
y <- table(Prod_count)/n*100
bp <- barplot(y, ylim = c(0, 100), names.arg=c("Books", "Clothing/Sport", "Home/Kitchen", "Other", "Personal Care", "Technology"),
              main = '', ylab = 'Percentage')
text(bp,y+4,labels=paste(as.character(round(y)), "%", sep=" "))

#  Pie Chart Covid Influence
slices <- c(round(table(CovidInfluence)/n*100))
lbls <- c("Less", "More", "Same")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls, "%", sep="") 
pie(slices,labels = lbls, col=rainbow(length(lbls)), main = "Covid Positive Influence on Buys")
     
#  Histogram of confidence when online shopping
hist(Confidence, freq = F, main = '', xlim = c(0, 10), ylim = c(0, 0.3), xlab = 'Confidence when shopping online (-)')
skewness(Confidence)

#  Grouped  
#  BarPlot of Products Bought
testM <- Shopping[Gender == 'Male',]
testF <- Shopping[Gender == 'Female',]
nM = 85
nF = 21

Prod_countF <- unlist(strsplit(testF['Products'][,], ";"))
yF <- table(Prod_countF)/nF*100
bpF <- barplot(yF, names.arg=c("Books", "Clothing/Sport", "Home/Kitchen", "Other", "Personal Care", "Technology"),
              main = '', ylim = c(0,100), ylab = 'Percentage')
text(bpF,yF+4,labels=paste(as.character(round(yF)), "%", sep=" "))

Prod_count <- unlist(strsplit(testM['Products'][,], ";"))
yM <- table(Prod_count)/nM*100
bpM <- barplot(yM, names.arg=c("Books", "Clothing/Sport", "Home/Kitchen", "Other", "Personal Care", "Technology"),
              main = '', ylim = c(0,100), ylab = 'Percentage')
text(bpM,yM+4,labels=paste(as.character(round(yM)), "%", sep=" "))

Male <- yM
Female <- yF

both<- cbind(Male, Female)

pl <- barplot(t(both), beside = TRUE, names.arg=c("Books", "Clothing/Sport", "Home/Kitchen", "Other", "Personal Care", "Technology"),
               main = '', ylim = c(0,100), ylab = 'Percentage', legend = TRUE, args.legend = list(x = "topleft", bty = "n"))
text(pl,t(both)+4,labels=paste(as.character(round(t(both))), "%", sep=" "))

#  BarPlot of Platforms Used
Prod_countF <- unlist(strsplit(testF['Platforms'][,], ";"))
yF <- table(Prod_countF)/nF*100
bpF <- barplot(yF, names.arg=c("Aliexpress", "Amazon", "Other", "Second-hand", "Zalando/Asos"),
               main = '', ylim = c(0,100), ylab = 'Percentage')
text(bpF,yF+4,labels=paste(as.character(round(yF)), "%", sep=" "))

Prod_count <- unlist(strsplit(testM['Platforms'][,], ";"))
yM <- table(Prod_count)/nM*100
bpM <- barplot(yM, names.arg=c("Aliexpress", "Amazon", "Other", "Second-hand", "Zalando/Asos"),
               main = '', ylim = c(0,100), ylab = 'Percentage')
text(bpM,yM+4,labels=paste(as.character(round(yM)), "%", sep=" "))

Male <- yM
Female <- yF

both<- cbind(Male, Female)

pl <- barplot(t(both), beside = TRUE, names.arg=c("Aliexpress", "Amazon", "Other", "Second-hand", "Zalando/Asos"),
              main = '', ylim = c(0,100), ylab = 'Percentage', legend = TRUE, args.legend = list(x = "topleft", bty = "n"))

text(pl,t(both)+4,labels=paste(as.character(round(t(both))), "%", sep=" "))

boxplot(Cost)
boxplot(Buys)

#  Two varibles
boxplot(Cost ~ Income, ylim = c(0,300))
boxplot(Buys ~ Income)

boxplot(Cost ~ Confidence)
boxplot(Buys ~ Confidence)

boxplot(Cost ~ CovidInfluence)
boxplot(Buys ~ CovidInfluence)

plot(Buys, Cost, ylim = c(0,300))
cov(Buys, Cost)
cor(Buys, Cost)

plot(Confidence, Cost)
plot(Confidence, Buys)

#  Model Fitting
#Continuous variables. Age, Buys, Amountm, Confidence
library(fitdistrplus)

# Confidence
# graph distribution (right-skewed)
hist(Confidence, freq=F)

# fit to gamma, lognormal, and weibull...
s_normal <- fitdist(Confidence,"norm")
s_gamma <- fitdist(Confidence, 'gamma', method="mme")
s_gamma2 <- fitdist(Confidence, 'gamma', method="mle")
s_lognormal <- fitdist(Confidence, 'lnorm', method="mme")
s_lognormal2 <- fitdist(Confidence, 'lnorm', method="mle")
s_weibull <- fitdist(Confidence, 'weibull', method="mle")

# plot the fits of 3 options
plotlegend <- c('Normal', 'Gamma1', 'Gamma2', 'Lognormal1', 'Lognormal2', 'Weibull')
denscomp(list(s_normal, s_gamma, s_gamma2, s_lognormal, s_lognormal2, s_weibull), legendtext = plotlegend, xlegend = "topleft", lwd = 3, xlim = c(0,30), ylim = c(0,0.3))
dens <- density(Confidence)
lines(dens, lwd = 2)
s_normal$aic
s_gamma$aic
s_gamma2$aic
s_lognormal$aic
s_lognormal2$aic
s_weibull$aic

s_weibull$estimate

# Fit a Beta for the grades using the method of moments
fit2 = fitdist(Confidence/10,"beta", method="mme")
fit2$estimate
hist(Confidence/10, freq = F,ylim=c(0,4))
grid=seq(.1,1,0.01)
lines(grid,dbeta(grid,fit2$estimate[1],fit2$estimate[2]))

plot(ecdf(Confidence/10))
grid=seq(.1,1,0.01)
lines(grid,pbeta(grid,fit2$estimate[1],fit2$estimate[2]))

fit2$aic

plot(ecdf(Confidence))
grid=seq(1,10,0.01)
lines(grid,pweibull(grid,s_weibull$estimate[1],s_weibull$estimate[2]))
lines(grid,pnorm(grid,s_normal$estimate[1],s_normal$estimate[2]),col="red")

# Try binomial
hist(Confidence, freq=F)
fitBinom=fitdist(data=Confidence, dist="binom", fix.arg=list(size=10), start=list(prob=0.3))
grid=seq(1,10,0.01)
lines(grid,dbinom(grid, size =10, prob = fitBinom$estimate[1]), lw=4)

plot(ecdf(Confidence))
grid=seq(1,10,0.01)
lines(grid,pbinom(grid, size =10, prob = fitBinom$estimate[1]))

fitBinom$aic

# Buys
# graph distribution (right-skewed)
hist(Buys, freq=F)

# fit to gamma, lognormal, and weibull...
s_normal <- fitdist(Buys,"norm")
s_exp <- fitdist(Buys,"exp")
s_gamma <- fitdist(Buys, 'gamma', method="mme")
#s_gamma2 <- fitdist(Buys, 'gamma', method="mle")
#s_lognormal <- fitdist(Buys, 'lnorm', method="mme")
#s_lognormal2 <- fitdist(Buys, 'lnorm', method="mle")
#s_weibull <- fitdist(Buys, 'weibull', method="mle")

# plot the fits of 3 options
plotlegend <- c('Normal', 'Exp','Gamma1') #,'Gamma2', 'Lognormal1', 'Lognormal2', 'Weibull')
denscomp(list(s_normal, s_exp, s_gamma), legendtext = plotlegend, xlegend = "topright", lwd = 3,  ylim = c(0,0.15))
dens <- density(Buys)
lines(dens, lwd = 2)
s_normal$aic
s_exp$aic
s_gamma$aic

s_exp$estimate

LogB <- log(Buys)
LogB <- LogB[LogB > 0]
hist(LogB, main = '', freq = F)
s_normal <- fitdist(LogB,"norm")
s_gamma <- fitdist(LogB, 'gamma') #Both are equal
s_lognormal <- fitdist(LogB, 'lnorm', method="mme")
s_lognormal2 <- fitdist(LogB, 'lnorm', method="mle")
s_weibull <- fitdist(LogB, 'weibull', method="mle")

plotlegend <- c('Normal','Gamma', 'Lognormal1', 'Lognormal2', 'Weibull')
denscomp(list(s_normal, s_gamma, s_lognormal, s_lognormal2, s_weibull), legendtext = plotlegend, xlegend = "topright", lwd = 3, xlim = c(0,4),  ylim = c(0,0.8))

s_normal$aic
s_gamma$aic
s_lognormal$aic
s_lognormal2$aic
s_weibull$aic

s_weibull$estimate

# Cost
# graph distribution (right-skewed)
hist(Cost, freq=F)

# fit to gamma, lognormal, and weibull...
s_normal <- fitdist(Cost,"norm")
s_exp <- fitdist(Cost,"exp")
s_gamma <- fitdist(Cost, 'gamma', method="mme")
#s_gamma2 <- fitdist(Cost, 'gamma', method="mle")
#s_lognormal <- fitdist(Cost, 'lnorm', method="mme")
#s_lognormal2 <- fitdist(Cost, 'lnorm', method="mle")
#s_weibull <- fitdist(Cost, 'weibull', method="mle")

# plot the fits of 3 options
plotlegend <- c('Normal', 'Exp','Gamma1') #,'Gamma2', 'Lognormal1', 'Lognormal2', 'Weibull')
denscomp(list(s_normal, s_exp, s_gamma), legendtext = plotlegend, xlegend = "topright", lwd = 3,  ylim = c(0,0.005))
dens <- density(Cost)
lines(dens, lwd = 2)
s_normal$aic
s_exp$aic
s_gamma$aic

s_exp$estimate

Log10C <- log10(Cost)
Log10C <- Log10C[Log10C != -Inf]
hist(Log10C, main = '', freq = F)
s_normal <- fitdist(Log10C,"norm")
s_gamma <- fitdist(Log10C, 'gamma') #Both are equal
s_lognormal <- fitdist(Log10C, 'lnorm', method="mme")
s_lognormal2 <- fitdist(Log10C, 'lnorm', method="mle")
s_weibull <- fitdist(Log10C, 'weibull', method="mle")

plotlegend <- c('Normal','Gamma', 'Lognormal1', 'Lognormal2', 'Weibull')
denscomp(list(s_normal, s_gamma, s_lognormal, s_lognormal2, s_weibull), legendtext = plotlegend, xlegend = "topright", lwd = 3, xlim = c(0,4),  ylim = c(0,0.8))

s_normal$aic
s_gamma$aic
s_lognormal$aic
s_lognormal2$aic
s_weibull$aic

s_gamma$estimate

# INFERENCE FOR ONE VARIABLE
t.test(Buys)
t.test(Buys, mu = 3*3.5, alternative="l")

t.test(Cost)
t.test(Cost, mu = 68, alternative="g")

t.test(Confidence, mu = , alternative="l")

# INFERENCE FOR TWO VARIABLES
# Two qualitative
chisq.test(UsualPlatform,Gender)
chisq.test(UsualPlatform,Gender)$observed
chisq.test(UsualPlatform,Gender)$expected
Chi = chisq.test(UsualPlatform,Gender)$statistic
N = length(UsualPlatform)
V = sqrt(Chi/N)
V

# One qualitative, one quantitative
Y1 = log(Buys[Gender=="Male"])
Y1 <- Y1[Y1 != -Inf]

Y2 = log(Buys[Gender=="Female"])
Y2 <- Y2[Y2 != -Inf]

boxplot(Y1,Y2)
t.test(Y1,Y2,var.equal = T)
t.test(Y1,Y2,var.equal = F)
var.test(Y1,Y2)
length(Y1)
length(Y2)

Y1 = log(Cost[Gender=="Male"])
Y1 <- Y1[Y1 != -Inf]

Y2 = log(Cost[Gender=="Female"])
Y2 <- Y2[Y2 != -Inf]

boxplot(Y1,Y2)
t.test(Y1,Y2,var.equal = T)
t.test(Y1,Y2,var.equal = F)
var.test(Y1,Y2)
length(Y1)
length(Y2)

# Two quantitative
cor.test(Buys,Cost)
plot(Buys,Cost)
abline(lm(Buys~Cost))
lm(Buys~Cost)

#Log
library(tidyverse)
df <- select(Shopping, Buys, Cost)
df <- log(df)
df <- subset(df, Cost != -Inf | Buys != -Inf)

B <- df['Buys'][,]
C <- df['Cost'][,]

cor.test(B,C)
plot(B,C)
abline(lm(C~B))
lm(C~B)