# number of MC trials
nMC<-500
lengthSr<-50

srSimsAr<-array(NA,dim=c(lengthSr,nMC, 6))

#expR<-seq(0.1,0.8,length.out=10)

## er = 0.1----
for(j in 1:nMC){
  
  alpha <- seq(2,0.5,length.out=lengthSr+1) # Ricker a parameter
  beta <- seq(0.01,0.01,length.out=lengthSr+1) # Ricker b parameter
  seq <- alpha/beta # equilibrium population size
  
  spwn <- rep(NA,lengthSr+1)
  rec <- spwn
  spwn[1] <- seq[1]*0.5
  
  for(i in 1:lengthSr){
    rec[i] <- spwn[i]*exp(alpha[i]-beta[i]*spwn[i])*exp(rnorm(1,0,0.25))
    spwn[i+1] <-rec[i]*0.1
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfER.1<-as.data.frame(cbind(rep(0.1,dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfER.1)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## er = 0.2----
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
    spwn[i+1] <-rec[i]*0.2
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfER.2<-as.data.frame(cbind(rep(0.2,dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfER.2)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## er = 0.3----
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
    spwn[i+1] <-rec[i]*0.3
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfER.3<-as.data.frame(cbind(rep(0.3,dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfER.3)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## er = 0.4----
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
    spwn[i+1] <-rec[i]*0.4
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfER.4<-as.data.frame(cbind(rep(0.4,dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfER.4)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## er = 0.5----
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


srSimsDfER.5<-as.data.frame(cbind(rep(0.5,dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfER.5)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## er = 0.6----
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
    spwn[i+1] <-rec[i]*0.6
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfER.6<-as.data.frame(cbind(rep(0.6,dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfER.6)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## er = 0.7----
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
    spwn[i+1] <-rec[i]*0.7
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfER.7<-as.data.frame(cbind(rep(0.7,dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfER.7)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## er = 0.8----
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
    spwn[i+1] <-rec[i]*0.8
  }
  
  srSimsAr[,j,] <- cbind(rep(j,length.out=lengthSr),seq(1:lengthSr),spwn[1:lengthSr],rec[1:lengthSr], alpha[-lengthSr+1], beta[-lengthSr+1])
  
}

dim(srSimsAr)<-c(lengthSr*nMC,6)


srSimsDfER.8<-as.data.frame(cbind(rep(0.8,dim(srSimsAr)[1]),srSimsAr))
colnames(srSimsDfER.8)<-c("scen","MC","byr","spwn","rec","alpha_true","beta_true")

## COMBINE

srSimsdfERs<-rbind(srSimsDfER.1,srSimsDfER.2,srSimsDfER.3,srSimsDfER.4,srSimsDfER.5,srSimsDfER.6,srSimsDfER.7,srSimsDfER.8)

