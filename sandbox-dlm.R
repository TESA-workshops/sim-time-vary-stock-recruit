
# load required packages =============================================
library(dlm)
library(tidyverse)
library(ggpubr)

source("dlm-wrapper.R")


# toy example =============================================

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

# plot model estimates vs true values ---

spwn_range<-seq(0,max(dlm_model$results$spwn)*1.2,length.out=100)
r_pred <- matrix(NA,nrow=length(dlm_model$results$byr), ncol=length(spwn_range))


for(i in 1:length(byr)){
  r_pred[i,]<-exp(median(dlm_model$results$lnalpha[i]))*spwn_range*exp(dlm_model$results$beta[i]*spwn_range)
}

rownames(r_pred)<-dlm_model$results$byr
colnames(r_pred)<-spwn_range

r_pred<-cbind(dlm_model$results$byr,r_pred)
colnames(r_pred)[1]<-c("byr")
sr_pred<-pivot_longer(data =as.data.frame(r_pred), cols=!byr, names_to="spwn",values_to="rec" )
sr_pred$spwn<-as.numeric(sr_pred$spwn)

max_spawn <- max(dlm_model$result$spwn)
max_rec <- max(dlm_model$result$spwn)

# create each panel for plot starting with spawner-recruitment relationship
library(ggpubr)

a <- ggplot(data=dlm_model$results, aes(x = spwn, y = rec, colour=factor(byr)))+
  geom_point(size = 3) +
  coord_cartesian(xlim=c(0, max_spawn*1.2), ylim=c(0,max_rec*1.2)) +
  xlab("Spawners") +
  ylab("Recruits") +
  geom_line(data = sr_pred, aes(x = spwn, y = rec, colour=factor(byr)), size = 0.5) +
  scale_colour_viridis_d()+
  theme_bw() +
  theme(strip.text.x = element_text(size=8),
        axis.title = element_text(size=10),
        axis.text = element_text(size=7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0.5,0.5,0.5,0.5), units="lines"))

# next plot true and estimate alpha
b <- ggplot(data=dlm_model$results, aes(x = byr, y = lnalpha )) +
  geom_line( color="black", size = 1)+
  geom_ribbon(aes(ymin = lnalpha-lnalpha_se*2, ymax = lnalpha+lnalpha_se*2), 
              fill = "grey80", alpha=0.5, linetype=2, colour="gray46") +
  geom_line(aes(x = byr, y = alpha_true), color="red", size = 1)+
  ylab("alpha") +
  xlab("") +
  scale_y_continuous(position = "right", limits=c(0,3)) +
  theme_bw() +
  theme(strip.text.x = element_text(size=8),
        axis.title = element_text(size=10),
        axis.text = element_text(size=7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0.5,1.85,0.25,0.5), units="lines"))+
  annotate("text", x = c(30,30),
           y = c(2.7, 2.5),
           label = c("True", "Estimated"),
           color= c("red", "black"), 
           size=3,
           hjust=0) +
  annotate("segment", x = c(28,28),
           xend=c(29.5,29.5),
           y = c(2.7, 2.5),
           yend = c(2.7, 2.5),
           lty = c(1,1),
           color=c("red", "black"), 
           size=1)

# next plot true and estimate beta
c<- ggplot(data=dlm_model$results, aes(x = byr, y = beta)) +
  geom_line( color="black", size = 1)+
  geom_ribbon(aes(ymin = beta-beta_se*2, ymax = beta+beta_se*2), 
              fill = "grey80", alpha=0.5, linetype=2, colour="gray46") +
  geom_line(aes(x = byr, y = -beta_true), color="red", size = 1)+
  xlab("Brood year") +
  ylab("beta") +
  scale_y_continuous(position = "right", limits=c(-0.016,-0.008)) +
  theme_bw() +
  theme(strip.text.x = element_text(size=8),
        axis.title = element_text(size=10),
        axis.text = element_text(size=7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0.25,0.5,0.5,0.5), units="lines"))


g <- ggarrange(a, ggarrange(b,c, nrow =2),
  ncol=2)

jpeg("TV-alpha-example.jpeg", width = 10, height = 6, units = "in", res = 600)
print(g)
dev.off()

