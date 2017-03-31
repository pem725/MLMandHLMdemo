library(foreign)
library(lme4)

lmerICCest <- function(x,facet=1){
  tmp <- as.data.frame(VarCorr(x))
  out <- round(tmp$vcov[facet]/sum(tmp$vcov[c(1,nrow(tmp))]),3)
  return(out)
}

sang <- read.spss("./Sex Anger dataset for daily reliability 022517.sav",F,T)
lme.NA <- lmer(NA.~ 1 + (1|Item) + (1|subjno) + (1|day),data=sang)
lme.PA <- lmer(PA~ 1 + (1|Item) + (1|subjno) + (1|day),data=sang)

lme.NA2 <- lmer(dailyna~day+(day|subjno),data=sang)
lme.PA2 <- lmer(dailypa~day+(day|subjno),data=sang)

lme.NA3 <- lmer(dailyna~ Item + (Item|day),data=sang)
lme.PA3 <- lmer(dailypa~ Item + (Item|day),data=sang)

contrasts(sang$Item)

summary(lme.NA)
summary(lme.PA)

lmerICCest(lme.NA)
lmerICCest(lme.NA2)
lmerICCest(lme.PA)
lmerICCest(lme.PA2)

table(sang$Item)

dPA <- sang[,c(1,2,3,4)]
dNA <- sang[,c(1,2,3,5)]
dPA$Item <- as.numeric(dPA$Item)
dNA$Item <- as.numeric(dNA$Item)


tmp<-
str(tmp)

library(reshape)
dPA.w <- as.data.frame(cast(dPA,subjno+day~Item))
str(dPA.w)
dNA.w <- as.data.frame(cast(dNA,subjno+day~Item))
str(dNA.w)

library(psych)
psych::alpha(dPA.w[dPA.w$day < 17,3:6])
psych::alpha(dNA.w[dPA.w$day < 17,3:6])

outPA <- data.frame(day=NA,meas="PA",alpha=NA)
outNA <- data.frame(day=NA,meas="NA",alpha=NA)
for (i in 1:21){
  tmpPA <- dPA.w[dPA.w$day == i,3:6]
  tmpPA <- tmpPA[complete.cases(tmpPA),]
  tmpNA <- dNA.w[dNA.w$day == i,3:6]
  tmpNA <- tmpNA[complete.cases(tmpNA),]
  outPA <- rbind(outPA,data.frame(day=i,meas="PA",alpha=psych::alpha(tmpPA)$total$raw_alpha))
  outNA <- rbind(outNA,data.frame(day=i,meas="NA",alpha=psych::alpha(tmpNA)$total$raw_alpha))
}
outPA <- outPA[-1,]
outNA <- outNA[-1,]
outNAPA <- rbind(outPA,outNA)

library(ggplot2)
ggplot(outNAPA,aes(x=day,y=alpha)) + geom_smooth(aes(fill=meas))


source("https://raw.githubusercontent.com/pem725/MRES/master/R/gtheory.R")

#gt.PA <- gtheory(PA~as.factor(subjno)*as.factor(Item)*as.factor(day),data=dPA)

library(afex)
out <- aov_ez(PA~subjno*Item*day+Error(id/(Item*day)),data=dPA[dPA$day<17,])
names(dPA)

#ezANOVA(data=dPA,dv=PA,wid="subjno",)

## create some bogus data to begin our adventure...

df <- data.frame(idROW=1:120,id=as.factor(sort(rep(1:10,12))),f1=gl(2,6,120),f2=as.factor(sort(rep(1:6,20))),value=sort(rnorm(120)))
library(lme4)
lm0 <- lmer(value~1 + (1|id) + (1|f1) + (1|f2),data=df)
lmerICCest(lm0,1)
lmerICCest(lm0,2)
lmerICCest(lm0,3)




str(df)

out <- aov(value~f1*f2,data=df)
summary(out)

sum((tapply(df$value,list(df$f1),mean) - mean(df$value))^2)

tapply(df$value,list(df$id,df$f1,df$f2),sum)


AC <- aov_ez(idROW,value,id*f1*f2,data=df)
