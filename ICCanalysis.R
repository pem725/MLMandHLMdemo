library(foreign)
library(lme4)

lmerICCest <- function(x){
  tmp <- as.data.frame(VarCorr(x))
  out <- round(tmp$vcov[1]/sum(tmp$vcov[c(1,nrow(tmp))]),3)
  return(out)
}

sang <- read.spss("./Sex Anger dataset for daily reliability 022517.sav",F,T)
lme.NA <- lmer(dailyna~1 + (1|subjno) + (1|day),data=sang)
lme.PA <- lmer(dailypa~1 + (1|subjno) + (1|day),data=sang)

lme.NA2 <- lmer(dailyna~day+(day|subjno),data=sang)
lme.PA2 <- lmer(dailypa~day+(day|subjno),data=sang)

summary(lme.NA)
summary(lme.PA)

lmerICCest(lme.NA)
lmerICCest(lme.NA2)
lmerICCest(lme.PA)
lmerICCest(lme.PA2)

table(sang$Item)
