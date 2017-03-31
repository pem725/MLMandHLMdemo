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

lmerICCest <- function(x,facet=1){
  tmp <- as.data.frame(VarCorr(x))
  out <- round(tmp$vcov[facet]/sum(tmp$vcov[c(1,nrow(tmp))]),3)
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

df <- data.frame(idROW=1:120,id=as.factor(sort(rep(1:10,12))),f1=gl(2,6,120),f2=as.factor(sort(rep(1:6,20))),value=sort(rnorm(120)))
library(lme4)
lm0 <- lmer(value~1 + (1|id) + (1|f1) + (1|f2),data=df)
lmerICCest(lm0,1)
lmerICCest(lm0,2)
lmerICCest(lm0,3)



