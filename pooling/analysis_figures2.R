#Figures for Spit Pools paper. 
#Citation info here. 

if (!require("pacman")) install.packages("pacman")
p_load( ggplot2, dplyr, tidyr, scales, knitr, cowplot)

#Load Functions
source("pooling/functions.R")

#Create a dataframe of pool sizes and sensitivities 
pool.sen <- as.data.frame(rbind(c(1, 1.0),
                                c(5, 1-0.0741),
                                c(10, 1-0.1111),
                                c(20, 1-0.1481)
))
colnames(pool.sen) <- c("pool.size", "sens")


pool.sen.ciu <- as.data.frame(rbind(c(1, 1.0),
                                c(5, 1-0.1111),
                                c(10, 1-0.2),
                                c(20, 1-0.2444)
))
colnames(pool.sen.ciu) <- c("pool.size", "sens")

pool.sen.cil <- as.data.frame(rbind(c(1, 1.0),
                                    c(5, 1-0.0444),
                                    c(10, 1-0.0815),
                                    c(20, 1-0.0889)
))
colnames(pool.sen.cil) <- c("pool.size", "sens")

if(wrapper.run == TRUE){
  pool.sen <- as.data.frame(rbind(c(1, 1.0),
                                  c(5, 1-sens5),
                                  c(10, 1-sens10),
                                  c(20, 1-sens20)
  ))
  colnames(pool.sen) <- c("pool.size", "sens")
  
  
  pool.sen.ciu <- as.data.frame(rbind(c(1, 1.0),
                                      c(5, 1-sens5u),
                                      c(10, 1-sens10u),
                                      c(20, 1-sens20u)
  ))
  colnames(pool.sen.ciu) <- c("pool.size", "sens")
  
  pool.sen.cil <- as.data.frame(rbind(c(1, 1.0),
                                      c(5, 1-sens5l),
                                      c(10, 1-sens10l),
                                      c(20, 1-sens20l)
  ))
  colnames(pool.sen.cil) <- c("pool.size", "sens")
  
}


#=========================================================

#Generate result data frames for prevalence 

p.1.10k <- prev.results(1, 10000)
p.5.10k <- prev.results(5, 10000)
p.10.10k <- prev.results(10, 10000)
p.20.10k <- prev.results(20, 10000)

#p.1.10k.ciu <- prev.results(1, 10000, other.data = pool.sen.ciu)
p.5.10k.ciu <- prev.results(5, 10000, other.data = pool.sen.ciu)
p.10.10k.ciu <- prev.results(10, 10000, other.data = pool.sen.ciu)
p.20.10k.ciu <- prev.results(20, 10000, other.data = pool.sen.ciu)

#p.1.10k.cil <- prev.results(1, 10000, other.data = pool.sen.ciu)
p.5.10k.cil <- prev.results(5, 10000, other.data = pool.sen.cil)
p.10.10k.cil <- prev.results(10, 10000, other.data = pool.sen.cil)
p.20.10k.cil <- prev.results(20, 10000, other.data = pool.sen.cil)




p.10.10.k.cis <-as.data.frame(cbind(p.10.10k.cil$prevalence, p.10.10k.cil$total.tests, p.10.10k.ciu$total.tests))
colnames(p.10.10.k.cis) <- c("prevalence", "cil", "ciu")
p.20.10.k.cis <-as.data.frame(cbind(p.20.10k.cil$prevalence, p.20.10k.cil$total.tests, p.20.10k.ciu$total.tests))
colnames(p.20.10.k.cis) <- c("prevalence", "cil", "ciu")
p.5.10.k.cis <-as.data.frame(cbind(p.5.10k.cil$prevalence, p.5.10k.cil$total.tests, p.5.10k.ciu$total.tests))
colnames(p.5.10.k.cis) <- c("prevalence", "cil", "ciu")

plot10k <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = total.tests ), 
            color = "#85c4b9", size = 1.25) +
  geom_line(data = p.20.10k, aes(x = prevalence, y = total.tests), 
            color = "#afd88d", size = 1.25) +
  geom_line(data = p.5.10k, aes(x = prevalence, y = total.tests ), 
            color = '#27496a', size = 1.25) +
  geom_ribbon(data=p.5.10.k.cis,
            aes(x = prevalence, ymin=cil,ymax=ciu), fill='#27496a', alpha = 0.4) +
  geom_ribbon(data=p.10.10.k.cis,
              aes(x = prevalence, ymin=cil,ymax=ciu), fill="#85c4b9", alpha = 0.4) +
  geom_ribbon(data=p.20.10.k.cis,
              aes(x = prevalence, ymin=cil,ymax=ciu), fill="#afd88d", alpha = 0.4) +
  annotate(geom="text",x=0.1,y= 7520 ,label="pool = 10",color="#85c4b9",  angle = 40) +
  annotate(geom="text",x=0.073,y= 8400 ,label="pool = 20",color="#afd88d", angle = 44) +
  annotate(geom="text",x=0.16,y= 8000 ,label="pool = 5",color="#27496a", angle = 30) +
  annotate(geom="text",x=0.04,y= 10300,label="pool = 1",color='#000000') +
  labs( 
    x= "Prevalence",
    y = "Total Tests")  +
  #ggtitle("population 10,000") +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 10000, linetype = "solid", color = '#000000', size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
plot10k

plot10k.zoom <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = total.tests ), color = "#85c4b9", size = 1.25) +
  geom_line(data = p.20.10k, aes(x = prevalence, y = total.tests), color = "#afd88d", size = 1.25) +
  geom_line(data = p.5.10k, aes(x = prevalence, y = total.tests ), color = '#27496a', size = 1.25) +
  geom_ribbon(data=p.5.10.k.cis,
              aes(x = prevalence, ymin=cil,ymax=ciu), fill='#27496a', alpha = 0.4) +
  geom_ribbon(data=p.10.10.k.cis,
              aes(x = prevalence, ymin=cil,ymax=ciu), fill="#85c4b9", alpha = 0.4) +
  geom_ribbon(data=p.20.10.k.cis,
              aes(x = prevalence, ymin=cil,ymax=ciu), fill="#afd88d", alpha = 0.4) +
  labs(
    x= "",
    y = "")  +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.line = element_line(colour = "black")
  )+
  #geom_hline(yintercept = 10000, linetype = "solid", color = '#000000', size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0,0.05))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0,7000))
plot10k.zoom

plot10k +
  annotation_custom(ggplotGrob(plot10k.zoom), xmin = 0.10, xmax = 0.32, ymin = 0, ymax = 6000)


#======================================================================================================================
#Number people that go undetected
plot10k.undetected <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = numb.undetected.pos ), color = "#85c4b9", size = 1.25) +
  geom_line(data = p.20.10k, aes(x = prevalence, y = numb.undetected.pos), color = "#afd88d", size = 1.25) +
  geom_line(data = p.5.10k, aes(x = prevalence, y = numb.undetected.pos ), color = '#27496a', size = 1.25) +
  geom_line(data = p.10.10k.ciu, aes(x = prevalence, y = numb.undetected.pos ), color = "#85c4b9", linetype = 'dashed', size = 0.75) +
  geom_line(data = p.20.10k.ciu, aes(x = prevalence, y = numb.undetected.pos), color = "#afd88d", linetype = 'dashed', size = 0.75) + 
  geom_line(data = p.5.10k.ciu, aes(x = prevalence, y = numb.undetected.pos ), color = '#27496a', linetype = 'dashed', size = 0.75) + 
  geom_line(data = p.10.10k.cil, aes(x = prevalence, y = numb.undetected.pos ), color = "#85c4b9", linetype = 'dashed', size = 0.75) +
  geom_line(data = p.20.10k.cil, aes(x = prevalence, y = numb.undetected.pos), color = "#afd88d", linetype = 'dashed', size = 0.75) + 
  geom_line(data = p.5.10k.cil, aes(x = prevalence, y = numb.undetected.pos ), color = "#27496a", linetype = 'dashed', size = 0.75) +
  annotate(geom="text",x=0.15,y= 52 ,label="pool = 10",color="#85c4b9",  angle = 0) +
  annotate(geom="text",x=0.2,y= 15 ,label="pool = 20",color="#afd88d", angle = 0) +
  annotate(geom="text",x=0.22,y= 68 ,label="pool = 5",color="#27496a", angle = 0) +
  labs( 
    x= "Prevalence",
    y = "Expected number of people \n who test positive using the undilluted \n test that go undetected")  +
  #ggtitle("population 10,000") +
  theme_classic(base_size = 15) +
  #geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


plot10k.undetected


plot10k.undetected.zoom <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = numb.undetected.pos ), color = "#85c4b9", size = 1.25) +
  geom_line(data = p.20.10k, aes(x = prevalence, y = numb.undetected.pos), color = "#afd88d", size = 1.25) +
  geom_line(data = p.5.10k, aes(x = prevalence, y = numb.undetected.pos ), color = '#27496a', size = 1.25) +
  geom_line(data = p.10.10k.ciu, aes(x = prevalence, y = numb.undetected.pos ), color = "#85c4b9", linetype = 'dashed') +
  geom_line(data = p.20.10k.ciu, aes(x = prevalence, y = numb.undetected.pos), color = "#afd88d", linetype = 'dashed') + 
  geom_line(data = p.5.10k.ciu, aes(x = prevalence, y = numb.undetected.pos ), color = '#27496a', linetype = 'dashed') + 
  geom_line(data = p.10.10k.cil, aes(x = prevalence, y = numb.undetected.pos ), color = "#85c4b9", linetype = 'dashed') +
  geom_line(data = p.20.10k.cil, aes(x = prevalence, y = numb.undetected.pos), color = "#afd88d", linetype = 'dashed') + 
  geom_line(data = p.5.10k.cil, aes(x = prevalence, y = numb.undetected.pos ), color = "#27496a", linetype = 'dashed') +
  annotate(geom="text",x=0.04,y= 34 ,label="pool = 10",color="#85c4b9",  angle = 10) +
  annotate(geom="text",x=0.015,y= 20 ,label="pool = 20",color="#afd88d", angle = 15) +
  annotate(geom="text",x=0.03,y= 22 ,label="pool = 5",color="#27496a", angle = 11) +
  labs( 
    x= "Prevalence",
    y = "Expected number of people \n who test positive using the undilluted \n test that go undetected")  +
  #ggtitle("population 10,000") +
  theme_classic(base_size = 15) +
  #geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0,0.05)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0,70))
plot10k.undetected.zoom


plot_grid(plot10k.undetected, 
          plot10k.undetected.zoom,
          labels = "AUTO", ncol = 2 )


#==============================================================================
#Detection per test

p.10.10.k.cis <-as.data.frame(cbind(p.10.10k.cil$prevalence, p.10.10k.cil$pos.found, p.10.10k.ciu$pos.found, p.10.10k.cil$total.tests, p.10.10k.ciu$total.tests))
colnames(p.10.10.k.cis) <- c("prevalence", "cil.pf", "ciu.pf", "cil.tt", "ciu.tt")
p.20.10.k.cis <-as.data.frame(cbind(p.20.10k.cil$prevalence, p.20.10k.cil$pos.found, p.20.10k.ciu$pos.found, p.20.10k.cil$total.tests, p.20.10k.ciu$total.tests))
colnames(p.20.10.k.cis) <- c("prevalence", "cil.pf", "ciu.pf", "cil.tt", "ciu.tt")
p.5.10.k.cis <-as.data.frame(cbind(p.5.10k.cil$prevalence, p.5.10k.cil$pos.found, p.5.10k.ciu$pos.found, p.5.10k.cil$total.tests, p.5.10k.ciu$total.tests))
colnames(p.5.10.k.cis) <- c("prevalence", "cil.pf", "ciu.pf", "cil.tt", "ciu.tt")

p.10.10k$pos.found/ p.10.10k$total.tests

plot10k.pos.test <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = pos.found/total.tests ), color = "#85c4b9", size = 1.25) +
  geom_line(data = p.20.10k, aes(x = prevalence, y = pos.found/total.tests ), color = "#afd88d", size = 1.25) +
  geom_line(data = p.5.10k, aes(x = prevalence, y = pos.found/total.tests  ), color = '#27496a', size = 1.25) +
  geom_ribbon(data=p.5.10.k.cis,
              aes(x = prevalence, ymin=cil.pf/cil.tt,ymax=ciu.pf/ciu.tt), fill='#27496a', alpha = 0.4) +
  geom_ribbon(data=p.10.10.k.cis,
              aes(x = prevalence, ymin=cil.pf/cil.tt,ymax=ciu.pf/ciu.tt), fill="#85c4b9", alpha = 0.4) +
  geom_ribbon(data=p.20.10.k.cis,
              aes(x = prevalence, ymin=cil.pf/cil.tt,ymax=ciu.pf/ciu.tt), fill="#afd88d", alpha = 0.4) +
  geom_abline(intercept = 0, slope =1, color = "black", size = 1.25) +
  annotate(geom="text",x=0.1,y= 0.15 ,label="pool = 10",color="#85c4b9",  angle = 30) +
  annotate(geom="text",x=0.07,y= 0.105 ,label="pool = 20",color="#afd88d", angle = 30) +
  annotate(geom="text",x=0.15,y= 0.21, label="pool = 5",color="#27496a", angle = 30) +
  annotate(geom="text",x=0.03,y= 0.045, label="pool = 1",color="black", angle = 43) +
  labs( 
    x= "Prevalence",
    y = "Positives detected per test ")  +
  #ggtitle("population 10,000") +
  theme_classic(base_size = 15) +
  #geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


plot10k.pos.test 





plot10k.pos.test.zoom <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = pos.found/total.tests ), color = "#85c4b9", size = 1.25) +
  geom_line(data = p.20.10k, aes(x = prevalence, y = pos.found/total.tests ), color = "#afd88d", size = 1.25) +
  geom_line(data = p.5.10k, aes(x = prevalence, y = pos.found/total.tests  ), color = '#27496a', size = 1.25) +
  geom_ribbon(data=p.5.10.k.cis,
              aes(x = prevalence, ymin=cil.pf/cil.tt,ymax=ciu.pf/ciu.tt), fill='#27496a', alpha = 0.4) +
  geom_ribbon(data=p.10.10.k.cis,
              aes(x = prevalence, ymin=cil.pf/cil.tt,ymax=ciu.pf/ciu.tt), fill="#85c4b9", alpha = 0.4) +
  geom_ribbon(data=p.20.10.k.cis,
              aes(x = prevalence, ymin=cil.pf/cil.tt,ymax=ciu.pf/ciu.tt), fill="#afd88d", alpha = 0.4) +
  geom_abline(intercept = 0, slope =1, color = "black", size = 1.25) +
  labs(
    x= "",
    y = "")  +
  #ggtitle("population 10,000") +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.line = element_line(colour = "black")
  )+
  #geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3), limits = c(0, 0.05)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=3), limits = c(0, 0.12))

plot10k.pos.test.zoom 

plot10k.pos.test +
  annotation_custom(ggplotGrob(plot10k.pos.test.zoom), xmin = 0.12, xmax = 0.32, 
                    ymin = -0.02, ymax = 0.15)




#####
#cost savings


plot10k.pos.test.c <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = 30*(-total.tests / pos.found + 1/prevalence ) ), color = "#85c4b9", size = 1.25) +
  geom_line(data = p.20.10k, aes(x = prevalence, y = 30*(-total.tests /pos.found  + 1/prevalence  )), color = "#afd88d", size = 1.25) +
  geom_line(data = p.5.10k, aes(x = prevalence, y = 30*(-total.tests /pos.found   + 1/prevalence  )), color = '#27496a', size = 1.25) +
  labs( 
    x= "Prevalence",
    y = "The reduction in the average cost \n per positive detected relative to \n individual tests, assuming $30 per test ")  +
  #ggtitle("population 10,000") +
  annotate(geom="text",x=0.125,y= 60 ,label="pool = 10",color="#85c4b9",  angle = -35) +
  annotate(geom="text",x=0.10,y= 52 ,label="pool = 20",color="#afd88d", angle = -45) +
  annotate(geom="text",x=0.25,y= 22 ,label="pool = 5",color="#27496a", angle = -10) +
  theme_classic(base_size = 15) +
  #geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 0.30)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 500), labels = dollar) 

plot10k.pos.test.c 


plot10k.pos.test.c.zoom <- ggplot() +
  geom_line(data = p.5.10k, aes(x = prevalence, y = 30*(-total.tests /pos.found   + 1/prevalence  )), color = '#27496a', size = 1.25) +
  geom_line(data = p.10.10k, aes(x = prevalence, y = 30*(-total.tests / pos.found + 1/prevalence ) ), color = "#85c4b9", size = 1.25) +
  geom_line(data = p.20.10k, aes(x = prevalence, y = 30*(-total.tests /pos.found  + 1/prevalence  )), color = "#afd88d", size = 1.25) +
  labs( 
    x= "",
    y = "")  +
  #ggtitle("population 10,000") +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.line = element_line(colour = "black")
  )+
  #geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0, 0.05)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 20000), labels = dollar) 
plot10k.pos.test.c.zoom



plot10k.pos.test.c +
  annotation_custom(ggplotGrob(
    plot10k.pos.test.c.zoom), xmin = 0.08, xmax = 0.32, ymin = 50, ymax = 520)




