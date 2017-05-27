## see https://stat.utexas.edu/images/SSC/Site/hlm_comparison-1.pdf

library(foreign)
library(lme4)
## library(languageR) # obsolete - no support for MCMC
library(lmerTest)
library(merTools)

## functions you might find useful:

### cheat sheet:
## x:  an lmer object
## facet:  the level of generalization you wish to assess (ordered in the lmer object)

lmerICCest <- function(x,facet=NULL){
  tmp <- as.data.frame(VarCorr(x))[,c("grp","vcov")]
  out <- round(tmp$vcov[!is.na(match(tmp$grp,facet))]/sum(tmp$vcov),2)
  return(out)
}

## Data used for the entire demo:
dat <- read.spss("./popular2.sav",F,T)
## see URL above for the location:


################### Level-1 Model or the Null Model #####################
## Level-0 model (also known as the unconditional model)

lme0 <- lmer(popular ~ 1 + (1|class),data=dat)
summary(lme0)

# Linear mixed model fit by REML ['lmerMod']
# Formula: popular ~ 1 + (1 | class)
# Data: dat
# 
# REML criterion at convergence: 6330.5
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.5655 -0.6975  0.0020  0.6758  3.3175 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# class    (Intercept) 0.7021   0.8379  
# Residual             1.2218   1.1053  
# Number of obs: 2000, groups:  class, 100
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)  5.07786    0.08739    58.1

## brute force method to calculate ICC from the output:
ICCdat <- as.data.frame(VarCorr(lme0))
ICCdat$vcov[1]/sum(ICCdat$vcov)

## check by merTools - only once just so you can see alternative methods
ICC(outcome="popular",group="class",data=dat) ## the ICC for the level 1 model

### use my function (from here on out)
lmerICCest(lme0)

######################## 2-Level with fixed predictor ###########################
#### now a 2-level model with fixed effect predictor
lme1.a <- lmer(popular ~ 1 + Cextrav + (1|class),data=dat)
summary(lme1.a)

# Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of
# freedom [lmerMod]
# Formula: popular ~ 1 + Cextrav + (1 | class)
# Data: dat
# 
# REML criterion at convergence: 5832.6
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.0644 -0.7267  0.0165  0.7088  3.3587 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# class    (Intercept) 0.8406   0.9168  
# Residual             0.9304   0.9646  
# Number of obs: 2000, groups:  class, 100
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 5.078e+00  9.421e-02 9.830e+01   53.90   <2e-16 ***
#   Cextrav     4.863e-01  2.015e-02 1.965e+03   24.13   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# Cextrav 0.000 

## ICC for the model:
lmerICCest(lme1.a)


##################### Level-1 Model with Random effect predictor ###############
## another Level-1 factor that specifies the predictor as a random effect
lme1.b <- lmer(popular ~ Cextrav + (Cextrav|class), data=dat)
## note, you can drop the "1" from the formula as I did above - they are implicit
summary(lme1.b)

# Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of
# freedom [lmerMod]
# Formula: popular ~ Cextrav + (Cextrav | class)
# Data: dat
# 
# REML criterion at convergence: 5779.4
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.1961 -0.7291  0.0146  0.6816  3.2217 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev. Corr 
# class    (Intercept) 0.89182  0.9444        
# Cextrav     0.02599  0.1612   -0.88
# Residual             0.89492  0.9460        
# Number of obs: 2000, groups:  class, 100
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  5.03127    0.09702 97.07000   51.86   <2e-16 ***
#   Cextrav      0.49286    0.02546 89.70000   19.36   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# Cextrav -0.552

### ICC
lmerICCest(lme1.b)

###################### Level-1 model with 2 predictors (both random) ############
lme1.c <- lmer(popular~Cextrav+Csex+(Cextrav + Csex|class),data=dat)
summary(lme1.c)
lmerICCest(lme1.c) ## HLM and R agree on these values for ICC


##################### Level-2 model with one level 2 and 2 level 1 factors (no ints) #####
lme2.a <- lmer(popular~Cextrav + Csex + Ctexp + (Cextrav + Csex | class),data=dat)
summary(lme2.a)
lmerICCest(lme2.a)

#################### Level-2 model as above but with interaction ################
lme2.b <- lmer(popular~Cextrav + Csex + Ctexp + Cextrav:Ctexp + Csex:Ctexp + (Cextrav + Csex | class),data=dat)
summary(lme2.b)
lmerICCest(lme2.b)


################################ BEGIN HERE ON 3/31/17 ############################
## create some bogus data to begin our adventure...

df <- data.frame(id=as.factor(sort(rep(1:10,12))),f1=gl(2,6,120),f2=as.factor(sort(rep(1:6,20))))
df$y1 <- sort(rnorm(120))
df$y2 <- as.numeric(as.character(df$f1))+rnorm(120)
df$y3 <- as.numeric(as.character(df$f2))+rnorm(120)

library(lme4)
library(ggplot2)
lm1 <- lmer(y1~1 + (1|id) + (1|f1) + (1|f2),data=df)
summary(lm1)
lmerICCest(lm1,"id")
ggplot(df,aes(x=id,y=y1)) + geom_boxplot(aes(col=id))
lmerICCest(lm1,"f1")
ggplot(df,aes(x=f1,y=y1)) + geom_boxplot(aes(col=f1))
lmerICCest(lm1,"f2")
ggplot(df,aes(x=f2,y=y1)) + geom_boxplot(aes(col=f2))


lm2 <- lmer(y2~1 + (1|id) + (1|f1) + (1|f2),data=df)
lmerICCest(lm2,"id")
ggplot(df,aes(x=id,y=y2)) + geom_boxplot(aes(col=id))
lmerICCest(lm2,"f1")
ggplot(df,aes(x=f1,y=y2)) + geom_boxplot(aes(col=f1))
lmerICCest(lm2,"f2")
ggplot(df,aes(x=f2,y=y2)) + geom_boxplot(aes(col=f2))

lm3 <- lmer(y3~1 + (1|id) + (1|f1) + (1|f2),data=df)
lmerICCest(lm3,"id")
ggplot(df,aes(x=id,y=y3)) + geom_boxplot(aes(col=id))
lmerICCest(lm3,"f1")
ggplot(df,aes(x=f1,y=y3)) + geom_boxplot(aes(col=f1))
lmerICCest(lm3,"f2")
ggplot(df,aes(x=f2,y=y3)) + geom_boxplot(aes(col=f2))


#####################################################################
## New data to demonstrate a point about nesting.

## Suppose we have 100 subjects - 25 per group (say classroom), who also work in
## teams of 5 to solve problems.  I start with my id and dv variables first:

df2 <- data.frame(id=1:100,y=c(rnorm(25,0,1),rnorm(25,.25,.5),rnorm(25,.5,2),rnorm(25,1,4)))
str(df2)

## what did I do above?  I created 25 observations for 4 groups and made the dv 
## (y) differ between the groups by specifying a different mean and standard 
## deviation for each group.  The data were generated at random using those 
## different distributional parameters.  Group 1 has a mean of 0, SD of 1; group
## 2 has a .25 mean with .5 SD and so on.  We have the makings for a main effect
## between groups, right?  I have not created the grouping variable so I will do
## that now.

df2$classrm <- gl(4,25,100)

## check out the data now and make sure the groups are setup as I described
## above.  Remember, you can view the data.frame, check the structure, or run a
## summary on it to check.  What we want to ensure is that the first 25
## observations have a "1," the next 25 hav a "2" and so on.

## if the data check out, now we want to form the groups of 5.  Before doing so,
## we ought to devise a plan.  Suppose we have some groups that are really good 
## and others that are not so good.  We could arrange for those differences to
## come out by ranking the values observed by big group (1-4) and then assigning
## the top performers to the same group, the next to group 2, and so on.

URDR <- by(df2$y,df2$classrm,order)
groups <- list(1:25,26:50,51:75,76:100)
df2$grpRNK <- NA ## setup a vector of NA's

for (i in 1:4){
  df2$grpRNK[groups[[i]]] <- URDR[[i]]
}

library(car)
df2$wkgrp <- recode(df2$grpRNK,"1:5=1; 6:10=2; 11:15=3; 16:20=4; 21:25=5",F,T)
table(df2$classrm,df2$wkgrp) ## check design - fully crossed, right?

library(lme4)

lm00 <- lmer(y~1+(1|classrm),data=df2)
lm01 <- lmer(y~1+(1|classrm)+(1|wkgrp),data=df2)
lm02 <- lmer(y~1 + (1|wkgrp),data=df2)
anova(lm00,lm01) ## lower BIC wins - and not sign different.  Always keep the simplest, most parsimonious model

## don't forget to reload the lmerICCest() function from above
lmerICCest(lm00,"classrm")
lmerICCest(lm01,"classrm")
lmerICCest(lm01,"wkgrp")
lmerICCest(lm02,"wkgrp")

## NOTE:  Even though we have a group level effect for classroom, we still do
## not have a high ICC.  We see a small amount of variance by wkgrp but that
## comes after really pushing for a big difference between groups within
## classrom.

## I want you two to start manipulating data so you can see how difficult it is 
## to find these types of nesting effects.  It takes a ton of an effect to make 
## much difference.  Also, try chaning the number of groups within the 
## classroom.  Suppose you have groups of 2 (dyads) and you pair one strong 
## person with a weak or a "high" with a "low."  What happens?  The more
## comfortable you get with making these datasets, the more you will realize
## that these effects are hard to observe.

outdat <- data.frame(Xdiff=NA,Vdiff=NA,ICC=NA) ## storage container for results
for (i in seq(0,5,by=.1)){ # manipulate Xdiff
  for (j in seq(0,5,by=.1)){ # manipulate Vdiff
    tmp <- data.frame(id=gl(3,5),time=rep(1:5,3),y=c(rnorm(5,0,1),rnorm(5,0+i,j),rnorm(5,0+2*i,j*2))) # data for lmer
    outdat <- rbind(outdat,c(i,j,lmerICCest(lmer(y~1+(1|id),data=tmp),"id"))) # create output and merge with container
  }
}
outdat <- outdat[-1,]
ggplot(outdat,aes(x=Xdiff,y=ICC,colour=as.factor(Vdiff))) + geom_smooth()

devtools::install_github('hadley/ggplot2')
devtools::install_github("ropensci/plotly")
library(ggplot2)
library(plotly)

p <- ggplot(outdat,aes(x=as.factor(Xdiff),y=as.factor(Vdiff))) + geom_tile(aes(fill=ICC))
p
pp <- ggplotly(p)
pp

q <- ggplot(outdat,aes(x=Xdiff,y=ICC,colour=as.factor(Vdiff))) + geom_smooth()
pq <- ggplotly(q)
pq

lm1 <- lm(ICC~Xdiff*Vdiff,data=outdat)
summary(lm1)

### new discussion on 5/25/17 for our first foray into conditioning variables

base <- seq(2,10,by=2)+rnorm(5)
base

dat <- data.frame(id=gl(4,5),time=rep(1:5,4),y=c(base+1*rnorm(5),base+2*rnorm(5,1,1),base+3*rnorm(5,2,1),base+4*rnorm(5,3,1)))
dat$p1 <- sort(rnorm(20)) + rnorm(20)
dat
library(lme4)
library(lmerTest)
lmer0 <- lmer(y~1 + (1|id),data=dat)
summary(lmer0)
lmerICCest(lmer0,"id") ## we are done with ICC's - permission granted to move to the conditional model
lmer1 <- lmer(y~time + (1|id),data=dat) ## time is a fixed effect
summary(lmer1)
anova(lmer0,lmer1) # nested model comparison
lmer2a <- lmer(y~ time + (time|id),data=dat) ## time is both fixed and random
summary(lmer2a)
anova(lmer1,lmer2a)
lmer2b <- lmer(y~ 1 + (time|id),data=dat)
anova(lmer1,lmer2b)
lmer2c <- lmer(y~ (time|id),data=dat)
anova(lmer1,lmer2c)

## your homework:  Figure out the formulas above.

