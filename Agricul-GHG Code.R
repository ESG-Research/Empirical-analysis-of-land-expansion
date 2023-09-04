---
title: "Agricul-GHG Code"
author: "Zhaojie"
date: "8/30/2023"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r cars}

library(blorr)
library(car)
library(dplyr)
library(DescTools)
library(effectsize)
library(ggplot2)
library(lmtest)
library(mctest)
library(olsrr)
library(orcutt)
library(prais)
library(plm)
library(stargazer)
library(skedastic)

Data_1 <- read.csv("1990-14WorldGHGLand")
View(Data_1)

m1 <- lm(lnGHG ~ Ergpc + Cleanerg_rate + Forest + Agland + GDPpc  + Land +Forest_rate + Agland_rate + GDPP, data=Data_1)
summary(m1)
m2 <- lm(GHG ~ Ergpc + Cleanerg_rate + GDPpc + Forest_rate + Agland_rate , data=Data_1)
summary(m2)

Data_1_1 <- Data_1 %>%
  mutate(lnGHG= log(GHG))

m3 <- lm(lnGHG ~ Ergpc + Cleanerg_rate + GDPpc + Forest_rate + Agland_rate , data=Data_1)
summary(m3)
m1 <- lm(lnGHG ~  Forest + Agland + GDPpc +Forest_rate + Agland_rate , data=Data_1)
summary(m1)

plm_ <- plm(lnGHG ~   Ergpc  + GDPpc + Agland_rate + Forest + Forest_rate, model = "within", index = c("Country", "Year"), data= Data_1_1)
summary(plm_)

stargazer(plm_,type = "text")

par(mfrow = c(2,2))
plot(plm_)

mctest(plm_)

plm_1 <- plm(lnGHG ~  Ergpc + Cleanerg_rate + GDPpc + Agland_rate + Forest_rate + Forest + Agland, model = "within", index = c("Country", "Year"), data= Data_1_1)
summary(plm_1)

head(Data_1_1)
describe(Data_1_1)

lm1 <- lm(lnGHG ~  Ergpc + Cleanerg_rate + lnGDPpc + Agland_rate, data= Data_1_1)
summary(lm1)

CoefVar(lm1)
standardize_parameters(lm1)
mctest(lm1,type = 'b',method = "VIF")
eigprop(lm1)

plm_0 <- plm(lnGHG ~  Ergpc + Cleanerg_rate + GDPpc + Agland_rate + Forest_rate + Forest, model = "pooling", index = c("Country", "Year"), data= Data_1_1)
summary(plm_0)

pFtest(plm_0 , plm_1)

plm_1t <- plm(lnGHG ~  Ergpc + Cleanerg_rate + GDPpc + Agland_rate + Forest_rate + Forest, model = "within",effect = "time", index = c("Country", "Year"), data= Data_1_1)
summary(plm_1t)
stargazer(plm_1,plm_1t,type = "text")
plm_1r <- plm(lnGHG ~  Ergpc + Cleanerg_rate + GDPpc + Agland_rate + Forest_rate + Forest, model = "random", index = c("Country", "Year"), data= Data_1_1)
summary(plm_1r)

phtest(plm_1 , plm_1r)

plm_1r <- plm(lnGHG ~  Ergpc + Cleanerg_rate + GDPpc + Agland_rate + Forest_rate + Forest, model = "random", index = c("Country", "Year"), data= Data_1_1)
summary(plm_1r)

plm_1twofx <- plm(lnGHG ~  Ergpc + Cleanerg_rate + GDPpc + Agland_rate + Forest_rate + Forest, model = "within",effect = "twoways", index = c("Country", "Year"), data= Data_1_1)
summary(plm_1twofx)

plm_1tword <- plm(lnGHG ~  Ergpc + Cleanerg_rate + GDPpc + Agland_rate + Forest_rate + Forest, model = "random",effect = "twoways", index = c("Country", "Year"), data= Data_1_1)
summary(plm_1tword)

fixef(plm_1) 

plm_pool <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + lnGDPpc + Agland_rate, model = "pooling", index = c("Country", "Year"), data= Data_1_1)
summary(plm_pool)

#I use ‘pFtest’ to campare Two-ways fixed effect model and pooled regression model, to comfirm the necessity of using One-way Fixed (country fixed) effect model”#
plm_1FX <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + lnGDPpc + Agland_rate, model = "within", index = c("Country", "Year"), data= Data_1_1)
summary(plm_1FX)
stargazer(plm_1FX,plm_pool,type = "text")
pFtest(plm_pool,plm_1FX)

#I use ‘pFtest’ to campare Two-ways fixed effect model and pooled regression model, to comfirm the necessity of using One-way Fixed (time fixed) effect model”#
plm_1T <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + lnGDPpc + Agland_rate, model = "within",effect = "time", index = c("Country", "Year"), data= Data_1_1)
summary(plm_1T)
stargazer(plm_1T,plm_pool,type = "text")
pFtest(plm_pool,plm_1T)


#‘phtest’ is used to compare the Two-ways fixed effect model and the Two-way random effect model#
phtest(plm_2RD,plm_2FX)

#I refine the model by logarithmically and compare the Two-ways fixed effect and Two-ways random effect again#
Data_1_1 <- Data_1 %>%
  mutate(lnGHG= log(GHG), lnErgpc=log(Ergpc),lnGDPpc=log(GDPpc),lnForest=log(Forest),lnAgland=log(Agland))

plm_2FX <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + GDPpc + Agland_rate, model = "within",effect = "twoways", index = c("Country", "Year"), data= Data_1_1)
summary(plm_2FX)

plm_2RD <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + lnGDPpc + Agland_rate, model = "random",effect = "twoways", index = c("Country", "Year"), data= Data_1_1)
summary(plm_2RD)

stargazer(plm_1FX,plm_1RD,type = "text")
phtest(plm_2RD,plm_2FX)

#Continue refine the model until all coefficients are significant and adj R^2 is improved#
plm_2FX <- plm(lnGHG ~  lnErgpc + Cleanerg_rate  + Agland_rate, model = "within",effect = "twoways", index = c("Country", "Year"), data= Data_1_1)
summary(plm_2FX)

plm_2RD <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + lnGDPpc + Agland_rate, model = "random",effect = "twoways", index = c("Country", "Year"), data= Data_1_1)
summary(plm_2RD)

phtest(plm_2FX , plm_2RD)

par(mfrow=c(2,2))
plot(plm_2FX)

bptest(plm_2FX)

plm_2FX <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + lnGDPpc + Agland_rate, model = "within",effect="twoways",index = c("Country", "Year"),robust="true", data= Data_1_1)
summary(plm_2FX)
bptest(plm_2FX)

plm_2RD <- plm(lnGHG ~  lnGDPpc +lnErgpc + Cleanerg_rate + Agland_rate, model = "random",index = c("Country", "Year"), data= Data_1_1)
summary(plm_2RD)
bptest(plm_2RD)

#Finally, I confirmed that the Two-ways Fixed Effect is the most appropriate.#
phtest(plm_2FX , plm_2RD)

fixef(plm_2FX)

# Then I adopt 'mctest' and 'Auxiliary Regression' to test Near Multi-Collinearity(NMC)#

mctest
Data_1_2 <- Data_1_1 %>%
  select(lnErgpc,Cleanerg_rate,lnGDPpc,Agland_rate)
cor(Data_1_2, use="complete.obs")
lm_2FX <- lm(lnGHG ~  lnErgpc , data= Data_1_1)
summary(lm_2FX)
mctest(lm_2FX,type = 'b',method = "VIF")
eigprop(lm_2FX)
bptest(lm_2FX)
white(lm_2FX , interactions= TRUE)
AUX_1 <- lm(lnGDPpc ~  lnErgpc, data= Data_1_1)
summary(AUX_1)

# Calculate Durbin-Watson Statistics#
plm_2FX <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + Agland_rate, model = "within",effect="twoways",index = c("Country", "Year"), data= Data_1_1)
summary(plm_2FX)
lm_2FX <- lm(lnGHG ~  lnErgpc + Cleanerg_rate + Agland_rate, data= Data_1_1)
summary(lm_2FX)
durbinWatsonTest(lm_2FX)

# Capture the residuals and lag them by one time period (needed to estimate rho!)
U1hat <- resid(lm_2FX)
Data_1_3 <- cbind(Data_1_1, U1hat)
U2hat <- resid(plm_2FX)
Data_1_3 <- cbind(Data_1_3, U2hat)
# Lagging the residuals by 1 time period #
Data_1_3 <- Data_1_3 %>%
  mutate(lag_U1hat= lag(U1hat,1),lag_U2hat=lag(U2hat,1))
# Estimate first round autocorrelation coefficient rho #
lmu1<- lm(U1hat~lag_U1hat -1,data=Data_1_3)
summary(lmu1)
lmu2<- lm(U2hat~lag_U2hat -1,data=Data_1_3)
summary(lmu2)

#Transform Variables using WLS(weight least square) #
rho <- lmu2$coefficients
rho
lnGHG2 <- Data_1_3$lnGHG -rho*lag(Data_1_3$lnGHG,1)
lnErgpc2 <- Data_1_3$lnErgpc -rho*lag(Data_1_3$lnErgpc,1) 
Cleanerg_rate2 <- Data_1_3$Cleanerg_rate -rho*lag(Data_1_3$Cleanerg_rate,1) 
Agland_rate2 <- Data_1_3$Agland_rate -rho*lag(Data_1_3$Agland_rate,1) 
Data_1_3 <- mutate(Data_17_3,lnGHG2, lnErgpc2, Cleanerg_rate2, Agland_rate2)
View(Data_1_3)

# Run WLS regression on transformed variables #
Nplm_2FX <- plm(lnGHG2 ~  lnErgpc2 + Cleanerg_rate2 + Agland_rate2, model = "within",effect="twoways",index = c("Country", "Year"), data= Data_1_3)
summary(Nplm_2FX)
Nplm_3FX <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + Agland_rate, model = "fd",index = c("Country", "Year"), data= Data_1_3)
summary(Nplm_3FX)
Nlm_2FX <- lm(lnGHG2 ~  lnErgpc2 + Cleanerg_rate2 + Agland_rate2, data= Data_1_3)
summary(Nlm_2FX)
durbinWatsonTest(Nlm_2FX)
Final <- (whiteauto <-white( lm_2FX,interactions=T))cochrane.orcutt(Nplm_FX,convergence = 8,max.iter = 100)
summary(Final)
mctest(Final)

Final2 <-prais_winsten(lnGHG ~  lnErgpc + Cleanerg_rate + Agland_rate, data=Data_1_3, index = Data_1_3$Year)
summary(Final2)

pwartest(lnGHG ~  lnErgpc + Cleanerg_rate + Agland_rate, data = Data_1_3)

Nplm_FX <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + Agland_rate, model = "within",index = c("Country", "Year"), data= Data_1_3)
summary(Nplm_FX)
plmtest(Nplm_FX, c("time"), type=("bp"))


plm_2FX <- plm(lnGHG ~  lnErgpc + Cleanerg_rate + Agland_rate, model = "within",effect="twoways",index = c("Country", "Year"), data= Data_1_3)
summary(plm_2FX)
fixef(plm_2FX)

Data_1_ <- Data_1_3 %>%
  select(lnGHG,lnErgpc,Cleanerg_rate,lnGDPpc,Agland_rate)
head(Data_1_)
describe(Data_1_)
CoefVar(plm_2FX)
whiteauto <-white( lm_2FX,interactions=T)

```
