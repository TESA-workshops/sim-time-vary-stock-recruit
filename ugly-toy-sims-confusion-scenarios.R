# number of MC trials
nMC<-100
lengthSr<-50

srSimsAr<-array(NA,dim=c(lengthSr,nMC, 6))

## stationary----
for(j in 1:nMC){
  
  alpha <- seq(1.5,1.5,length.out=lengthSr+1) # Ricker a parameter
  beta <- seq(0.01,0.01,length.out=lengthSr+1) # Ricker b parameter
  seq <- alpha/beta # equilibrium population size
  
  spwn <- rep(NA,lengthSr+1)
  rec <- spwn
  spwn[1] <- seq[1]*0.5
  
  for(i in 1:lengthSr){
    rec[i] <- spwn[i]*exp(alpha[i]-beta[i]*spwn[i])*exp(rnorm(1,0,0.25))
    spwn[i+1] <-rec[i]*0.5
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfstationary<-as.data.frame(cbind(rep("stationary",dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfstationary)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## downAlpha----
srSimsAr<-array(NA,dim=c(lengthSr,nMC, 6))

for(j in 1:nMC){
  
  alpha <- seq(2,0.5,length.out=lengthSr+1) # Ricker a parameter
  beta <- seq(0.01,0.01,length.out=lengthSr+1) # Ricker b parameter
  seq <- alpha/beta # equilibrium population size
  
  spwn <- rep(NA,lengthSr+1)
  rec <- spwn
  spwn[1] <- seq[1]*0.5
  
  for(i in 1:lengthSr){
    rec[i] <- spwn[i]*exp(alpha[i]-beta[i]*spwn[i])*exp(rnorm(1,0,0.25))
    spwn[i+1] <-rec[i]*0.5
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfdownAlpha<-as.data.frame(cbind(rep("downAlpha",dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfdownAlpha)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")



## downBeta----
srSimsAr<-array(NA,dim=c(lengthSr,nMC, 6))

for(j in 1:nMC){
  
  alpha <- seq(1.5,1.5,length.out=lengthSr+1) # Ricker a parameter
  beta <- seq(0.01,0.001,length.out=lengthSr+1) # Ricker b parameter
  seq <- alpha/beta # equilibrium population size
  
  spwn <- rep(NA,lengthSr+1)
  rec <- spwn
  spwn[1] <- seq[1]*0.5
  
  for(i in 1:lengthSr){
    rec[i] <- spwn[i]*exp(alpha[i]-beta[i]*spwn[i])*exp(rnorm(1,0,0.25))
    spwn[i+1] <-rec[i]*0.5
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfdownBeta<-as.data.frame(cbind(rep("downBeta",dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfdownBeta)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")


## downAlphaBeta----
srSimsAr<-array(NA,dim=c(lengthSr,nMC, 6))

for(j in 1:nMC){
  
  alpha <- seq(2,0.5,length.out=lengthSr+1) # Ricker a parameter
  beta <- seq(0.01,0.001,length.out=lengthSr+1) # Ricker b parameter
  seq <- alpha/beta # equilibrium population size
  
  spwn <- rep(NA,lengthSr+1)
  rec <- spwn
  spwn[1] <- seq[1]*0.5
  
  for(i in 1:lengthSr){
    rec[i] <- spwn[i]*exp(alpha[i]-beta[i]*spwn[i])*exp(rnorm(1,0,0.25))
    spwn[i+1] <-rec[i]*0.5
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfdownAlphaBeta<-as.data.frame(cbind(rep("downAlphaBeta",dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfdownAlphaBeta)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## COMBINE----

srSimsdfConf<-rbind(srSimsDfstationary,srSimsDfdownAlpha,srSimsDfdownBeta, srSimsDfdownAlphaBeta)

srSimsdfConf$MC <- as.numeric(as.character(srSimsdfConf$MC))
srSimsdfConf$byr<- as.numeric(as.character(srSimsdfConf$byr))
srSimsdfConf$spwn <- as.numeric(as.character(srSimsdfConf$spwn))
srSimsdfConf$rec <- as.numeric(as.character(srSimsdfConf$rec))
srSimsdfConf$alpha_true <- as.numeric(as.character(srSimsdfConf$alpha_true))
srSimsdfConf$beta_true <- as.numeric(as.character(srSimsdfConf$beta_true))
