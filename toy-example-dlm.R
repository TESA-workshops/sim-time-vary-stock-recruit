## load required packages  ----
library(tidyverse)

source("dlm-wrapper.R")


## toy example  ----

# simulate a brood table ----
alpha <- seq(2,0.5,length.out=50)
beta <- seq(0.01,0.01,length.out=50)
seq <- alpha/beta

spwn <- rep(NA,50)
rec <- spwn
spwn[1] <- seq[1]*0.5

for(i in 1:49){
  rec[i] <- spwn[i]*exp(alpha[i]-beta[i]*spwn[i])*exp(rnorm(1,0,0.25))
  spwn[i+1] <-rec[i]*0.8
}

rec<-rec[1:49]
spwn<-spwn[1:49]
byr<-seq(1:49)

bt<-as.data.frame(cbind(byr,spwn,rec, alpha[-50], beta[-50]))
colnames(bt)<-c("byr","spwn","rec","alpha_true","beta_true")

# fit DLM ----
dlm_model <- fitDLM(data = bt,
              alpha_vary = TRUE,
              beta_vary = FALSE)

dlm_model

# plot model estimates vs true values ----

plotDLM(dlm_model)
