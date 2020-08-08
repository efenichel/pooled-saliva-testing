###Created by: Anne Watkins
###Last Updated: August 7, 2020

library(reshape)
library(scales)
library(lmtest)
library(sandwich)
library(extrafont)
pooling <- read.csv('./Data/pooling_n1.csv')
pooling$ct1[which(pooling$Sample=='252 5/12')]=40
p5 <- pooling[,c(1,2,3,6)]
p5$ratio <- 5
p10 <- pooling[,c(1,2,4,7)]
p10$ratio <- 10
p20 <- pooling[,c(1,2,5,8)]
p20$ratio <- 20

names(p5) <- c('sample','ct1','ctp','dct','ratio')
names(p10) <- c('sample','ct1','ctp','dct','ratio')
names(p20) <- c('sample','ct1','ctp','dct','ratio')
# pooling.m <- melt(pooling,id.vars = 'Sample','ct1')
# pooling.c <- cast(pooling.m,)

pooling.c <- rbind(p5,p10,p20)
pooling.c$ctp[which(pooling.c$ctp>=40)]=40

mod1 <- glm(dct ~  ct1 +as.factor(ratio), data=pooling.c)
summary(mod1)

mod2 <- glm(dct ~  ctp +as.factor(ratio), data=pooling.c)
summary(mod2)
#ct pooled seems to significantly predict difference but starting ct doesn't

mod3 <- glm(dct ~  ct1 +ctp +as.factor(ratio), data=pooling.c)
summary(mod3)
#including both makes it highly significant for ct1 and ctp but makes ratio almost negligible

mod4 <- lm(ctp ~  ct1 +as.factor(ratio), data=pooling.c)
summary(mod4)
#this actually looks good

pooling.c$ctn <- 0
pooling.c$ctn[which(pooling.c$ctp>=38)] <- 1

mod5 <- glm(ctn ~ ct1 +as.factor(ratio),data=pooling.c, family='binomial')
summary(mod5)

#modified, final model
cor.test(pooling.c$dct,pooling.c$ct1)
mod.d.400<-lm(dct ~ as.factor(ratio), data=pooling.c)
summary(mod.d.400)
vcov(mod.d.400)

#restructure to create ratio 1 as level
p1 <- p20
p1$ratio <- 1
p1$ctp <- p1$ct1
p1$dct <- 0

pooling.c2 <- rbind(p1,p5,p10,p20)
pooling.c2$ctp[which(pooling.c2$ctp>=40)]=40

mod4.1.400 <- lm(ctp ~  ct1 +as.factor(ratio), data=pooling.c2)
summary(mod4.1.400)

mod4.2 <- lm(ctp ~ ct1 +as.factor(ratio), data=pooling.c2)
summary(mod4.2)

mod1.1 <- glm(dct ~  ct1 +as.factor(ratio), data=pooling.c2)
summary(mod1.1)

mod2.1 <- glm(dct ~  ctp +as.factor(ratio), data=pooling.c2)
summary(mod2.1)

pooling.c$ratio <- as.factor(pooling.c$ratio)
pcolors <- data.frame(ratio=levels(pooling.c$ratio),color=c('navyblue','blue','cornflowerblue'))
pcolors$color <- as.character(pcolors$color)
plot(pooling.c$ct1,pooling.c$ctp,col=pcolors$color[match(pooling.c$ratio,pcolors$ratio)],pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25)
abline(h=38,lty='dashed')
#abline(h=40,lty='dotted')
legend(x='topleft',legend=pcolors$ratio,col=pcolors$color,pch=16,bty = 'n',pt.cex = 1,cex=0.65)
abline(0,1)


#remove high CT values
pooling.c.high <- pooling2.c[which(pooling2.c$ct1<=35),]

mod4.h <- glm(ctp ~  ct1 +as.factor(ratio), data=pooling.c.high)
summary(mod4.h)

mod4.2h <- lm(ctp ~ ct1 +as.factor(ratio), data=pooling.c.high)
summary(mod4.2h)
mod1.1 <- glm(dct ~  ct1 +as.factor(ratio), data=pooling.c2)
summary(mod1.1)

mod2.1 <- glm(dct ~  ctp +as.factor(ratio), data=pooling.c2)
summary(mod2.1)

mod.d.high<-lm(dct ~ as.factor(ratio), data=pooling.c.high)
summary(mod.d.high)
lm.ci.func(mod.d.high)

pooling.c.high$ratio <- as.factor(pooling.c.high$ratio)
pcolors <- data.frame(ratio=levels(pooling.c$ratio),color=c('#ad143a','#ea8fa5','#e4c5ea'))
pcolors$color <- as.character(pcolors$color)
plot(pooling.c.high$ct1,pooling.c.high$ctp,col=pcolors$color[match(pooling.c.high$ratio,pcolors$ratio)],pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25)
abline(h=38,lty='dashed')
abline(h=40,lty='dotted')
legend(x='topleft',legend=pcolors$ratio,col=pcolors$color,pch=16,bty = 'n',pt.cex = 1,cex=0.65)
abline(0,1)


#do again for second set
pooling2 <- read.csv('./Data/pooling2.csv')
p2.5 <- pooling2[,c(1,2,3)]
p2.5$ratio <- 5
p2.10 <- pooling2[,c(1,2,4)]
p2.10$ratio <- 10
p2.20 <- pooling2[,c(1,2,5)]
p2.20$ratio <- 20

names(p2.5) <- c('sample','ct1','ctp','ratio')
names(p2.10) <- c('sample','ct1','ctp','ratio')
names(p2.20) <- c('sample','ct1','ctp','ratio')

pooling2.c <- rbind(p2.5,p2.10,p2.20)
pooling2.c$ctp[which(pooling2.c$ctp>=40)]=40

pooling2.c$dct <- pooling2.c$ctp - pooling2.c$ct1

mod4 <- glm(ctp ~  ct1 +as.factor(ratio), data=pooling2.c)
summary(mod4)

p2 <- p2.20
p2$ratio <- 1
p2$ctp <- p2$ct1


pooling2.c2 <- rbind(p2,p2.5,p2.10,p2.20)
pooling2.c2$ctp[which(pooling2.c2$ctp>=40)]=40

mod4.1.300 <- lm((ctp) ~  (ct1) +as.factor(ratio), data=pooling2.c2)
summary(mod4.1.300)

pooling2.c$ratio <- as.factor(pooling2.c$ratio)
p2colors <- data.frame(ratio=levels(pooling2.c$ratio),color=c('#27496a','#85c4b9','#afd88d'))
p2colors$color <- as.character(p2colors$color)

pcolors$ratio2<-c('1:5','1:10','1:20')
p2colors$ratio2<-c('1:5','1:10','1:20')

#figure 1
par(mfrow=c(1,2))


plot(pooling2.c$ct1,pooling2.c$ctp,col=alpha(p2colors$color[match(pooling2.c$ratio,p2colors$ratio)],0.9),
     pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25,
     xlim=c(20,41),ylim=c(20,41),title('300uL'))
abline(h=38,lty='dashed')
#abline(h=40,lty='dotted')
legend(x='topleft',legend=p2colors$ratio2,col=p2colors$color,pch=16,bty = 'n',
       pt.cex = 1,cex=0.85,y.intersp = 0.5,x.intersp = 0.25)
abline(0,1)
#points(pooling.c$ct1,pooling.c$ctp,col=alpha(pcolors$color[match(pooling.c$ratio,pcolors$ratio)],0.6),pch=16,cex=1.25,)

plot(pooling.c$ct1,pooling.c$ctp,col=alpha(pcolors$color[match(pooling.c$ratio,pcolors$ratio)],0.9),
     pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25,
     xlim=c(20,41),ylim=c(20,41),title('400uL'))
abline(h=38,lty='dashed')
#abline(h=40,lty='dotted')
legend(x='topleft',legend=pcolors$ratio2,col=pcolors$color,pch=16,bty = 'n',
       pt.cex = 1,cex=0.85,y.intersp = 0.5,x.intersp = 0.25,)
abline(0,1)

#pooling2.c$dct<- pooling2.c$ctp -pooling2.c$ct1

cor.test(pooling2.c$dct,pooling2.c$ct1)
mod.d.300<-lm(dct ~ as.factor(ratio), data=pooling2.c)
summary(mod.d.300)
vcov(mod.d.300)

#combine
pooling.c$input <- 400
pooling2.c$input <- 300

pooling.c2$input <- 400
pooling2.c2$input <- 300

p3 <- rbind(pooling.c[,-c(6)],pooling2.c)
p3.2 <- rbind(pooling.c2[,-c(4)],pooling2.c2)

mod6 <- glm(ctp ~ ct1 +as.factor(ratio) +as.factor(input), data=p3)
summary(mod6)

mod6.1 <- lm(ctp ~  ct1 +as.factor(ratio) +as.factor(input), data=p3.2)
summary(mod6.1)

p3.2$ratio2 <- p3.2$ratio
p3.2$ratio2 <- print(paste(p3.2$ratio,',',p3.2$input,'ul'))
p3.2$ratio2 <- as.factor(p3.2$ratio2)

p3$ratio2 <- print(paste(p3$ratio,',',p3$input,'ul'))
p3$ratio2 <- as.factor(p3$ratio2)
p3$ratio2 <- ordered(p3$ratio2,levels=c('5 , 300 ul','10 , 300 ul','20 , 300 ul','5 , 400 ul','10 , 400 ul','20 , 400 ul'))

p3colors <- data.frame(ratio=levels(p3$ratio2),color=c('darkred','red1','lightpink2','navyblue','blue','cornflowerblue'))
p3colors$color <- as.character(p3colors$color)
plot(p3$ct1,p3$ctp,col=alpha(p3colors$color[match(p3$ratio2,p3colors$ratio)],0.8),pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25,ylim=c(20,43),xlim=c(20,43))
abline(h=38,lty='dashed')
abline(h=40,lty='dotted')
legend(x='topleft',legend=p3colors$ratio,col=p3colors$color,pch=16,bty = 'n',pt.cex = 1,cex=0.65,y.intersp = 0.5)
abline(0,1)
#points(pooling.c$ct1,pooling.c$ctp,col=alpha(pcolors$color[match(pooling.c$ratio,pcolors$ratio)],0.6),pch=16,cex=1.25,)
library(car)
residualPlot(mod4.l.400)

plot(mod4.1.400)

mod4.l.300 <- lm(log(ctp) ~  log(ct1) +as.factor(ratio), data=pooling2.c2)
mod4.l.400 <- lm(log(ctp) ~  log(ct1) +as.factor(ratio), data=pooling.c2)
summary(mod4.l.300)
summary(mod4.l.400)

bptest(mod4.l.300)

coeftest(mod4.1.300, vcov. = vcovHC(mod4.1.300,type='HC1'))
coeftest(mod4.1.400, vcov. = vcovHC(mod4.1.400,type='HC1'))
coeftest(mod4.l.300, vcov. = vcovHC(mod4.l.300,type='HC1'))
coeftest(mod4.l.400, vcov. = vcovHC(mod4.l.400,type='HC1'))


#add coef vals to real data for sensitivity
impact <- read.csv('./Data/IMPACT_saliva.csv')
mod4.3.coef <- coef(mod.d.300)

impact$n1_p5 <- 0
impact$n1_p5 <- impact$cdc_n1_ct +mod4.3.coef[1]

impact$n1_p10 <- 0
impact$n1_p10 <- impact$cdc_n1_ct +mod4.3.coef[1]+mod4.3.coef[2]

impact$n1_p20 <- 0
impact$n1_p20 <- impact$cdc_n1_ct +mod4.3.coef[1]+mod4.3.coef[3]

impact.c <- impact[,c(1,4,10,11,12)]
names(impact.c) <- c('Sample.ID','Undiluted','1:5','1:10','1:20')
#names(impact.c) <- c('Sample.ID','1','5','10','20')
impact.m <- melt(impact.c,id.vars = c('Sample.ID'))
names(impact.m) <- c('Sample.ID','condition','ct')
impact.m$colorpal<-'#ff0000'
impact.m$colorpal[which(impact.m$condition=='1:5')]<-'#ff5448'
impact.m$colorpal[which(impact.m$condition=='1:10')]<-'#ff7d7d'
impact.m$colorpal[which(impact.m$condition=='1:20')]<-'#fcc1c1'

#plot(impact.m$condition,impact.m$ct,type='p')
library(ggplot2)
(ggplot(data=impact.m, aes(x=condition,y=ct))+ #theme(text=element_text(size=10,family='Impact'))+
    geom_rect(aes(ymin=38,ymax=Inf,xmin=-Inf,xmax=Inf),fill='#eec2c2')+
    geom_dotplot(binaxis='y',stackdir='center',aes(fill=condition),alpha=0.75,stackratio = 0.75,dotsize=1,binwidth = 0.5)+
 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",alpha=0.75)+
  scale_y_reverse()+  
  geom_hline(yintercept = 40,linetype='dashed')+ geom_hline(yintercept = 38))+
  #scale_fill_manual(values=c('#ff0000','#ff5448','#ff7d7d','#fcc1c1'))
  scale_fill_manual(values=c('#000000','#27496a','#85c4b9','#afd88d'))+  
  xlab('Pooling Ratio')+
  ylab('Ct value (N1)')+
  labs(fill='Pooling Ratio')+
  theme_minimal(base_size = 10,base_family = 'Arial')+
  annotate(geom='text',x=1,y=50,label='relative sensitivity:')+
  annotate(geom='text',x=2,y=50,label='92.59%',color='#27496a')+
  annotate(geom='text',x=3,y=50,label='88.89%',color='#85c4b9')+
  annotate(geom='text',x=4,y=50,label='85.19%',color='#afd88d')

#pred=predict(mod.d.300,interval = 'conf',new = impact.m)
impact2<-impact.m
impact2$fit<-impact2$ct1+pred[,'fit']
impact2$lwr<-impact2$ct1+pred[,'lwr']
impact2$upr<-impact2$ct1+pred[,'upr']
plot(impact2$ct1,impact2$ct)
lines(x=impact2$ct1,y=impact2$fit,lty=3)
lines(x=impact2$ct1,y=impact2$upr,lty=3,col='red')
lines(x=impact2$ct1,y=impact2$lwr,lty=3,col='blue')


ggplot(data=impact, aes(x=cdc_n1_ct,y=n1_p20))+
  geom_dotplot(binaxis='y',stackdir='center',aes(fill=cdc_n1_ct),alpha=0.5,stackratio = 0.75,dotsize=1,binwidth = 0.5)+
  #stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
  #             geom="pointrange", color="black",alpha=0.75)+
  scale_y_reverse()+ 
  geom_hline(yintercept = 40,linetype='dashed')+ geom_hline(yintercept = 38)+
  geom_hline(yintercept = 38-mod4.3.coef[1],color='blue')+geom_hline(yintercept = 38-mod4.3.coef[1]-mod4.3.coef[2],color='red')+
  geom_hline(yintercept = 38-mod4.3.coef[1]-mod4.3.coef[3],color='green')

#REG COEF
(length(which(impact$cdc_n1_ct+mod4.3.coef[1]<=38)))/135 #7.41
(length(which(impact$cdc_n1_ct+mod4.3.coef[1]+mod4.3.coef[2]>38))-45)/135 #11.11
(length(which(impact$cdc_n1_ct+mod4.3.coef[1]+mod4.3.coef[3]>38))-45)/135 #14.81

#UPPER LIMIT
(length(which(impact$cdc_n1_ct+3.046>38))-45)/135 #11.11
(length(which(impact$cdc_n1_ct+3.952>38))-45)/135 #20.00
(length(which(impact$cdc_n1_ct+4.383>38))-45)/135 #24.44
#LOWER LIMIT
(length(which(impact$cdc_n1_ct+1.398>38))-45)/135 #4.44
(length(which(impact$cdc_n1_ct+2.306>38))-45)/135 #8.15
(length(which(impact$cdc_n1_ct+2.747>38))-45)/135 #8.89

ggplot(data=impact, aes(x=cdc_n1_ct,y=n1_p20))+
  geom_dotplot(binaxis='y',stackdir='center',aes(fill=cdc_n1_ct),alpha=0.5,stackratio = 0.75,dotsize=1,binwidth = 0.5)+
  #stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
  #             geom="pointrange", color="black",alpha=0.75)+
  scale_y_reverse()+ 
  geom_hline(yintercept = 40,linetype='dashed')+ geom_hline(yintercept = 38)+
  geom_hline(yintercept = 38-3.046,color='blue')+geom_hline(yintercept = 38-3.952,color='red')+
  geom_hline(yintercept = 38-4.383,color='green')

plot(impact$cdc_n1_ct)

library(qualityTools)

#dotPlot(impact$cdc_n1_ct,

ggplot(data=impact2, aes(x=ratio,y=ct))+
  geom_dotplot(binaxis='y',stackdir='center',aes(fill=ratio),alpha=0.5,stackratio = 0.75,dotsize=1,binwidth = 0.5)+
  #stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
  #             geom="pointrange", color="black",alpha=0.75)+
  scale_y_reverse()+ 
  geom_hline(yintercept = 40,linetype='dashed')+ geom_hline(yintercept = 38)


#PBS and Water Pooling
pbs1<-read.csv('./Data/pooling_pbs.csv')
names(pbs1)<-c('sample','ct1','saliva','pbs','water')
#pbs1$cts<-pbs1$ct1
#rename vars, add another undiluted??
pbs.m<-melt(pbs1,id.vars = c('sample','ct1'))
names(pbs.m)<-c('sample','ct1','condition','ctp')

pbs.m$ctp[which(pbs.m$ctp>=42)]=42

pbs.m$condition<-as.factor(pbs.m$condition)
pbs.m$ctp[which(pbs.m$ctp>40)] <- 40
pbs.m$dct <- pbs.m$ctp - pbs.m$ct1
#pbs.m$condition<-relevel(pbs.m$condition, 'cts')
modp<-lm(ctp ~ ct1 +condition,data=pbs.m)
summary(modp)

modp.d<-lm(dct~condition,data=pbs.m)
summary(modp.d)


plot(pbs.m$ct1,pbs.m$ctp,col=pbs.m$condition)

#confidence intervals dct model

vcov300= vcov(mod.d.300) 
coef300= coef(mod.d.300)
vcov400= vcov(mod.d.400) 
coef400= coef(mod.d.400)
out.list=list('vcov300'=vcov300, 'vcov400'=vcov400, 'coef300'=coef300,'coef400'=coef400,'mod.d.300'= mod.d.300,'mod.d.400'= mod.d.400)
saveRDS(out.list, 'pooling_mod.rds')

library(MASS)

lm.ci.func <- function(mod.obj){
  samp1 <-
    MASS::mvrnorm(n = 10000,
                  mu = coef(mod.obj),
                  Sigma = vcov(mod.obj))
  beta1.s <- samp1[,1]
  beta1.2.s <- samp1[,1] + samp1[,2]
  beta1.3.s <- samp1[,1] + samp1[,3]
  
  beta1.q <- quantile(beta1.s, probs=c(0.025,0.5, 0.975))
  beta1.2.q <- quantile(beta1.2.s, probs=c(0.025,0.5, 0.975))
  beta1.3.q <- quantile(beta1.3.s, probs=c(0.025,0.5, 0.975))
  
  res1 <- list('beta1'=beta1.q, 'beta1.2'=beta1.2.q, 'beta1.3.q'=beta1.3.q)
  return(res1)
}

lm.ci.func(mod.d.300)
lm.ci.func(mod.d.400)
lm.ci.func(modp.d)

library(lme4)
mod.me<-lmer(dct ~ as.factor(ratio) +(1|sample), data=pooling2.c)
summary(mod.me)
library(nlme)
library(lmerTest)
confint(mod.me)

#pooling rna
rna<-read.csv('./data/pooling_rna.csv')
rna<-rna[,-c(2)]
rna.m<-melt(rna,id.vars = c('ID','Freeze.thaw'))
rna.m$dct<-rna.m$value-rna.m$Freeze.thaw
rna.mod<-lm(dct~variable,data=rna.m)
summary(rna.mod)

lm.ci.func1 <- function(mod.obj){
  samp1 <-
    MASS::mvrnorm(n = 10000,
                  mu = coef(mod.obj),
                  Sigma = vcov(mod.obj))
  beta1.s <- samp1[,1]
  beta1.2.s <- samp1[,1] + samp1[,2]
  #beta1.3.s <- samp1[,1] + samp1[,3]
  
  beta1.q <- quantile(beta1.s, probs=c(0.025,0.5, 0.975))
  beta1.2.q <- quantile(beta1.2.s, probs=c(0.025,0.5, 0.975))
 # beta1.3.q <- quantile(beta1.3.s, probs=c(0.025,0.5, 0.975))
  
  res1 <- list('beta1'=beta1.q, 'beta1.2'=beta1.2.q)#, 'beta1.3.q'=beta1.3.q)
  return(res1)
}
lm.ci.func1(rna.mod)

