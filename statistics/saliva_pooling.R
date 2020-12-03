###Created by: Anne Watkins
###Last Updated: August 16, 2020
if (!require("pacman")) install.packages("pacman")
if(!require("extrafont")) install.packages("extrafont")
p_load(reshape, scales, lmtest, sandwich)


# library(reshape)
# library(scales)
# library(lmtest)
# library(sandwich)
# library(extrafont)
#400ul
pooling <- read.csv('./Data/pooling_n1.csv')
pooling$ct1[which(pooling$Sample=='252 5/12')]=40 #correct issue in data
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


mod4 <- lm(ctp ~  ct1 +as.factor(ratio), data=pooling.c)
summary(mod4)

pooling.c$ctn <- 0
pooling.c$ctn[which(pooling.c$ctp>=38)] <- 1


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

#dct allows us to not use ratio 1, return to "final model"
mod1.1 <- glm(dct ~  ct1 +as.factor(ratio), data=pooling.c2)
summary(mod1.1)

mod2.1 <- glm(dct ~  ctp +as.factor(ratio), data=pooling.c2)
summary(mod2.1)

pooling.c$ratio <- as.factor(pooling.c$ratio)
pcolors <- data.frame(ratio=levels(pooling.c$ratio),color=c('#ad143a','#ea8fa5','#e4c5ea'))
pcolors$color <- as.character(pcolors$color)
plot(pooling.c$ct1,pooling.c$ctp,col=pcolors$color[match(pooling.c$ratio,pcolors$ratio)],pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25)
abline(h=38,lty='dashed')
#abline(h=40,lty='dotted')
legend(x='topleft',legend=pcolors$ratio,col=pcolors$color,pch=16,bty = 'n',pt.cex = 1,cex=0.65)
abline(0,1)


#remove high CT values to explore data - not for use in pooled spits paper
pooling.c.high <- pooling.c2[which(pooling.c2$ct1<=35),]  

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
#lm.ci.func(mod.d.high)

pooling.c.high$ratio <- as.factor(pooling.c.high$ratio)
pcolors <- data.frame(ratio=levels(pooling.c$ratio),color=c('#ad143a','#ea8fa5','#e4c5ea'))
pcolors$color <- as.character(pcolors$color)
plot(pooling.c.high$ct1,pooling.c.high$ctp,col=pcolors$color[match(pooling.c.high$ratio,pcolors$ratio)],pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25)
abline(h=38,lty='dashed')
abline(h=40,lty='dotted')
legend(x='topleft',legend=pcolors$ratio,col=pcolors$color,pch=16,bty = 'n',pt.cex = 1,cex=0.65)
abline(0,1)


#run same basic code for 300ul
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

pcolors$ratio2<-c('1/5','1/10','1/20')
p2colors$ratio2<-c('1/5','1/10','1/20')

#figure 1
#par(mfrow=c(1,2))


plot(pooling2.c$ct1,pooling2.c$ctp,col=alpha(p2colors$color[match(pooling2.c$ratio,p2colors$ratio)],0.9),
     pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25,
     xlim=c(20,41),ylim=c(20,41))#,title('300uL'))
abline(h=38,lty='dashed')
#abline(h=40,lty='dotted')
legend(x='topleft',legend=p2colors$ratio2,col=p2colors$color,pch=16,bty = 'n',
       pt.cex = 1,cex=0.85,y.intersp = 0.5,x.intersp = 0.25)
abline(0,1)
#points(pooling.c$ct1,pooling.c$ctp,col=alpha(pcolors$color[match(pooling.c$ratio,pcolors$ratio)],0.6),pch=16,cex=1.25,)

#UNCOMMENT THIS SECTION AND PAR TO MAKE 300 AND 400 PANEL
# plot(pooling.c$ct1,pooling.c$ctp,col=alpha(pcolors$color[match(pooling.c$ratio,pcolors$ratio)],0.9),
#      pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25,
#      xlim=c(20,41),ylim=c(20,41),title('400uL'))
# abline(h=38,lty='dashed')
# #abline(h=40,lty='dotted')
# legend(x='topleft',legend=pcolors$ratio2,col=pcolors$color,pch=16,bty = 'n',
#        pt.cex = 1,cex=0.85,y.intersp = 0.5,x.intersp = 0.25,)
# abline(0,1)

#pooling2.c$dct<- pooling2.c$ctp -pooling2.c$ct1

cor.test(pooling2.c$dct,pooling2.c$ct1)
mod.d.300<-lm(dct ~ as.factor(ratio), data=pooling2.c)
summary(mod.d.300)
vcov(mod.d.300)


#confidence intervals dct/FINAL MODELS

vcov300= vcov(mod.d.300) 
coef300= coef(mod.d.300)
vcov400= vcov(mod.d.400) 
coef400= coef(mod.d.400)

p_load(MASS)

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


#combine to run together - not for use in pooled spits
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

#too confusing
p3colors <- data.frame(ratio=levels(p3$ratio2),color=c('#27496a','#85c4b9','#afd88d','#ad143a','#ea8fa5','#e4c5ea'))
p3colors$color <- as.character(p3colors$color)
plot(p3$ct1,p3$ctp,col=alpha(p3colors$color[match(p3$ratio2,p3colors$ratio)],0.8),pch=16,xlab = 'Starting CT',ylab='Pooling CT',cex=1.25,ylim=c(20,43),xlim=c(20,43))
abline(h=38,lty='dashed')
abline(h=40,lty='dotted')
legend(x='topleft',legend=p3colors$ratio,col=p3colors$color,pch=16,bty = 'n',pt.cex = 1,cex=0.65,y.intersp = 0.5)
abline(0,1)
#points(pooling.c$ct1,pooling.c$ctp,col=alpha(pcolors$color[match(pooling.c$ratio,pcolors$ratio)],0.6),pch=16,cex=1.25,)
library(car)

plot(mod.d.400)

#testing log % change model - not for use as makes more sense for a different audience
mod4.l.300 <- lm(log(ctp) ~  log(ct1) +as.factor(ratio), data=pooling2.c2)
mod4.l.400 <- lm(log(ctp) ~  log(ct1) +as.factor(ratio), data=pooling.c2)
summary(mod4.l.300)
summary(mod4.l.400)

bptest(mod4.l.300)

coeftest(mod4.1.300, vcov. = vcovHC(mod4.1.300,type='HC1'))
coeftest(mod4.1.400, vcov. = vcovHC(mod4.1.400,type='HC1'))
coeftest(mod4.l.300, vcov. = vcovHC(mod4.l.300,type='HC1'))
coeftest(mod4.l.400, vcov. = vcovHC(mod4.l.400,type='HC1'))


#add coef vals to real data for sensitivity check
impact <- read.csv('./Data/IMPACT_saliva.csv')
mod4.3.coef <- coef(mod.d.300)

impact$n1_p5 <- 0
impact$n1_p5 <- impact$cdc_n1_ct +mod4.3.coef[1]

impact$n1_p10 <- 0
impact$n1_p10 <- impact$cdc_n1_ct +mod4.3.coef[1]+mod4.3.coef[2]

impact$n1_p20 <- 0
impact$n1_p20 <- impact$cdc_n1_ct +mod4.3.coef[1]+mod4.3.coef[3]

impact.c <- impact[,c(1,4,10,11,12)]
names(impact.c) <- c('Sample.ID','Undiluted','1/5','1/10','1/20')
impact.m <- melt(impact.c,id.vars = c('Sample.ID'))
names(impact.m) <- c('Sample.ID','condition','ct')
impact.m$colorpal<-'#ff0000'
impact.m$colorpal[which(impact.m$condition=='1:5')]<-'#ff5448'
impact.m$colorpal[which(impact.m$condition=='1:10')]<-'#ff7d7d'
impact.m$colorpal[which(impact.m$condition=='1:20')]<-'#fcc1c1'

#REG COEF RELATIVE SENSITIVITY
pos=length(which(impact$cdc_n1_ct<=40))
tot=length(impact$cdc_n1_ct)
neg=tot-pos
(length(which(impact$cdc_n1_ct+mod4.3.coef[1]>40))-neg)/pos #14.74
s5<-(length(which(impact$cdc_n1_ct+mod4.3.coef[1]<=40)))/pos #85.26
(length(which(impact$cdc_n1_ct+mod4.3.coef[1]+mod4.3.coef[2]>40))-neg)/pos #17.31
s10<-(length(which(impact$cdc_n1_ct+mod4.3.coef[1]+mod4.3.coef[2]<=40)))/pos #82.69
(length(which(impact$cdc_n1_ct+mod4.3.coef[1]+mod4.3.coef[3]>40))-neg)/pos #17.95
s20<-(length(which(impact$cdc_n1_ct+mod4.3.coef[1]+mod4.3.coef[3]<=40)))/pos #82.05


#sensitivity summary plot
library(ggplot2)
(ggplot(data=impact.m[which(impact.m$ct<=45),], aes(x=condition,y=ct))+ #theme(text=element_text(size=10,family='Impact'))+
    geom_rect(aes(ymin=40,ymax=45,xmin=-Inf,xmax=Inf),fill='#eec2c2')+
    geom_dotplot(binaxis='y',stackdir='center',aes(fill=condition),alpha=0.75,stackratio = 0.75,dotsize=1,binwidth = 0.5)+
  
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",alpha=0.75))+
  scale_y_reverse()+  
  geom_hline(yintercept = 40)+#,linetype='dashed')+ geom_hline(yintercept = 38))+
  #scale_fill_manual(values=c('#ff0000','#ff5448','#ff7d7d','#fcc1c1'))
  scale_fill_manual(values=c('#000000','#27496a','#85c4b9','#afd88d'))+  
  xlab('Pooling Ratio')+
  ylab('Ct value (N1)')+
  labs(fill='Pooling Ratio')+
  theme_minimal(base_size = 10,base_family = 'Arial')+
  annotate(geom='text',x=1,y=12,label='relative sensitivity:')+
  annotate(geom='text',x=2,y=12,label='85.26%')+#,color='#27496a')+
  annotate(geom='text',x=3,y=12,label='82.69%')+#,color='#85c4b9')+
  annotate(geom='text',x=4,y=12,label='82.05%')#,color='#afd88d')



#sensitivity summary plot 2 - not as nice
ggplot(data=impact, aes(x=cdc_n1_ct,y=n1_p20))+
  geom_dotplot(binaxis='y',stackdir='center',aes(fill=cdc_n1_ct),alpha=0.5,stackratio = 0.75,dotsize=1,binwidth = 0.5)+
  #stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
  #             geom="pointrange", color="black",alpha=0.75)+
  scale_y_reverse()+ 
  geom_hline(yintercept = 40,linetype='dashed')+ geom_hline(yintercept = 38)+
  geom_hline(yintercept = 38-mod4.3.coef[1],color='blue')+geom_hline(yintercept = 38-mod4.3.coef[1]-mod4.3.coef[2],color='red')+
  geom_hline(yintercept = 38-mod4.3.coef[1]-mod4.3.coef[3],color='green')



#Lower LIMIT
(length(which(impact$cdc_n1_ct+3.046>40))-neg)/pos #16.67
s5_ll=(length(which(impact$cdc_n1_ct+3.046<=40)))/pos #83.33
(length(which(impact$cdc_n1_ct+3.952>40))-neg)/pos #17.95
s10_ll=(length(which(impact$cdc_n1_ct+3.952<=40)))/pos #82.05
(length(which(impact$cdc_n1_ct+4.383>40))-neg)/pos #20.51
s20_ll=(length(which(impact$cdc_n1_ct+4.383<=40)))/pos #79.49
#Upper LIMIT
(length(which(impact$cdc_n1_ct+1.398>40))-neg)/pos #9.62
s5_ul=(length(which(impact$cdc_n1_ct+1.398<=40)))/pos #90.38
(length(which(impact$cdc_n1_ct+2.306>40))-neg)/pos #14.74
s10_ul=(length(which(impact$cdc_n1_ct+2.306<=40)))/pos #85.26
(length(which(impact$cdc_n1_ct+2.747>40))-neg)/pos #16.03
s20_ul=(length(which(impact$cdc_n1_ct+2.747<=40)))/pos #83.97

#SENSITIVITY SUMMARY PLOT 2 UPPER BOUND
ggplot(data=impact, aes(x=cdc_n1_ct,y=n1_p20))+
  geom_dotplot(binaxis='y',stackdir='center',aes(fill=cdc_n1_ct),alpha=0.5,stackratio = 0.75,dotsize=1,binwidth = 0.5)+
  #stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
  #             geom="pointrange", color="black",alpha=0.75)+
  scale_y_reverse()+ 
  geom_hline(yintercept = 40,linetype='dashed')+ geom_hline(yintercept = 38)+
  geom_hline(yintercept = 38-3.046,color='blue')+geom_hline(yintercept = 38-3.952,color='red')+
  geom_hline(yintercept = 38-4.383,color='green')



p_load(qualityTools)


#PBS and Water Pooling
pbs1<-read.csv('./Data/pooling_pbs.csv')
names(pbs1)<-c('sample','ct1','saliva','pbs','water')
#pbs1$cts<-pbs1$ct1
#rename vars, add another undiluted??
pbs.m<-melt(pbs1,id.vars = c('sample','ct1'))
names(pbs.m)<-c('sample','ct1','condition','ctp')

#pbs.m$ctp[which(pbs.m$ctp>=42)]=42

pbs.m$condition<-as.factor(pbs.m$condition)
pbs.m$ctp[which(pbs.m$ctp>40)] <- 40
pbs.m$dct <- pbs.m$ctp - pbs.m$ct1
#pbs.m$condition<-relevel(pbs.m$condition, 'cts')
modp<-lm(ctp ~ ct1 +condition,data=pbs.m)
summary(modp)

modp.d<-lm(dct~condition,data=pbs.m)
summary(modp.d)

lm.ci.func(modp.d)

#plot(pbs.m$ct1,pbs.m$ctp,col=pbs.m$condition)

#TRY MIXED EFFECTS - very similar result to simpler model
p_load(lme4)
mod.me<-lmer(dct ~ as.factor(ratio) +(1|sample), data=pooling2.c)
summary(mod.me)
p_load(nlme)
p_load(lmerTest)
confint(mod.me)

#pooling rna
rna<-read.csv('./data/pooling_rna.csv')  #if there is an error here check the file, there may be encoding issue with ID
rna<-rna[,-c(2)] #remove fresh to compare freeze thaw as unpooled
rna.m<-melt(rna,id.vars = c('ID','Freeze.thaw'))
rna.m$dct<-rna.m$value-rna.m$Freeze.thaw
names(rna.m)<-c('ID','Freeze.thaw','ratio','pct','dct')
rna.mod<-lm(dct~ratio,data=rna.m)
summary(rna.mod)

lm.ci.func1 <- function(mod.obj){
  samp1 <-
    MASS::mvrnorm(n = 10000,
                  mu = coef(mod.obj),
                  Sigma = vcov(mod.obj))
  beta1.s <- samp1[,1]
  beta1.2.s <- samp1[,1] + samp1[,2]
  
  beta1.q <- quantile(beta1.s, probs=c(0.025,0.5, 0.975))
  beta1.2.q <- quantile(beta1.2.s, probs=c(0.025,0.5, 0.975))
  
  res1 <- list('beta1'=beta1.q, 'beta1.2'=beta1.2.q)
  return(res1)
}
lm.ci.func1(rna.mod)

