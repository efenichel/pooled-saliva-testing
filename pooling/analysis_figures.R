#Figures for Spit Pools paper. 
#Citation info here. 

if (!require("pacman")) install.packages("pacman")
p_load( ggplot2, dplyr, tidyr, scales, knitr, cowplot)

#Load Functions
source("pooling/functions.R")

#Create a dataframe of pool sizes and sensitivities 
pool.sen <- as.data.frame(rbind(c(1, 1.0),
                                #c(4, 0.98),
                                c(5, 1-0.0741),
                                c(10, 1-0.1111),
                                c(20, 1-0.1481)
))
colnames(pool.sen) <- c("pool.size", "sens")


pool.sen.ci <- as.data.frame(rbind(c(1, 1.0),
                                #c(4, 0.98),
                                c(5, 1-0.1111),
                                c(10, 1-0.2),
                                c(20, 1-0.2444)
))
colnames(pool.sen.ci) <- c("pool.size", "sens")

#=========================================================

#Generate result data frames for prevalence 

p.1.10k <- prev.results(1, 10000)
p.5.10k <- prev.results(5, 10000)
p.10.10k <- prev.results(10, 10000)
p.20.10k <- prev.results(20, 10000)

p.1.10k.ci <- prev.results(1, 10000, other.data = pool.sen.ci)
p.5.10k.ci <- prev.results(5, 10000, other.data = pool.sen.ci)
p.10.10k.ci <- prev.results(10, 10000, other.data = pool.sen.ci)
p.20.10k.ci <- prev.results(20, 10000, other.data = pool.sen.ci)


p.5.10k[1:10,]$total.tests - p.5.10k.ci[1:10,]$total.tests 


#p.2.10k <- prev.results(4, 10000)

p.1.5k <- prev.results(1, 5000)
p.5.5k <- prev.results(5, 5000)
p.10.5k <- prev.results(10, 5000)
p.20.5k <- prev.results(20, 5000)

p.1.100k <- prev.results(1, 100000)
p.5.100k <- prev.results(5, 100000)
p.10.100k <- prev.results(10, 100000)
p.20.100k <- prev.results(20, 100000)

plot10k <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = total.tests ), color = "blue") +
  #geom_line(data = p.2.10k, aes(x = prevalence, y = total.tests), color = "purple") +
  geom_line(data = p.20.10k, aes(x = prevalence, y = total.tests), color = "black") +
  geom_line(data = p.5.10k, aes(x = prevalence, y = total.tests ), color = "dark green") +
  geom_line(data = p.10.10k.ci, aes(x = prevalence, y = total.tests ), color = "blue", linetype = 'dashed') +
  #geom_line(data = p.2.10k, aes(x = prevalence, y = total.tests), color = "purple") + 
  geom_line(data = p.20.10k.ci, aes(x = prevalence, y = total.tests), color = "black", linetype = 'dashed') + 
  geom_line(data = p.5.10k.ci, aes(x = prevalence, y = total.tests ), color = "dark green", linetype = 'dashed') + 
  annotate(geom="text",x=0.1,y= 7500 ,label="pool = 10",color="blue") +
  annotate(geom="text",x=0.06,y= 9000 ,label="pool = 20",color="black") +
  annotate(geom="text",x=0.2,y= 8000 ,label="pool = 5",color="dark green") +
  annotate(geom="text",x=0.04,y= 10300,label="pool = 1",color="red") +
  labs( 
    x= "Prevalence",
    y = "Total Tests")  +
  ggtitle("population 10,000") +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))




plot10k.undetected <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = numb.undetected.pos ), color = "blue") +
  #geom_line(data = p.2.10k, aes(x = prevalence, y = total.tests), color = "purple") +
  geom_line(data = p.20.10k, aes(x = prevalence, y = numb.undetected.pos), color = "black") +
  geom_line(data = p.5.10k, aes(x = prevalence, y = numb.undetected.pos ), color = "dark green") +
  geom_line(data = p.10.10k.ci, aes(x = prevalence, y = numb.undetected.pos ), color = "blue", linetype = 'dashed') +
  #geom_line(data = p.2.10k, aes(x = prevalence, y = total.tests), color = "purple") + 
  geom_line(data = p.20.10k.ci, aes(x = prevalence, y = numb.undetected.pos), color = "black", linetype = 'dashed') + 
  geom_line(data = p.5.10k.ci, aes(x = prevalence, y = numb.undetected.pos ), color = "dark green", linetype = 'dashed') + 
  # annotate(geom="text",x=0.1,y= 7500 ,label="pool = 10",color="blue") +
  # annotate(geom="text",x=0.06,y= 9000 ,label="pool = 20",color="black") +
  # annotate(geom="text",x=0.2,y= 8000 ,label="pool = 5",color="dark green") +
  # annotate(geom="text",x=0.04,y= 10300,label="pool = 1",color="red") +
  labs( 
    x= "Prevalence",
    y = "Expected number of \n positives that go undetected")  +
  ggtitle("population 10,000") +
  theme_classic(base_size = 15) +
  #geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))



ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = numb.undetected.pos ), color = "blue") +
  #geom_line(data = p.2.10k, aes(x = prevalence, y = total.tests), color = "purple") +
  geom_line(data = p.20.10k, aes(x = prevalence, y = numb.undetected.pos), color = "black") +
  geom_line(data = p.5.10k, aes(x = prevalence, y = numb.undetected.pos ), color = "dark green") +
  geom_line(data = p.10.10k.ci, aes(x = prevalence, y = numb.undetected.pos ), color = "blue", linetype = 'dashed') +
  #geom_line(data = p.2.10k, aes(x = prevalence, y = total.tests), color = "purple") + 
  geom_line(data = p.20.10k.ci, aes(x = prevalence, y = numb.undetected.pos), color = "black", linetype = 'dashed') + 
  geom_line(data = p.5.10k.ci, aes(x = prevalence, y = numb.undetected.pos ), color = "dark green", linetype = 'dashed') + 
  # annotate(geom="text",x=0.1,y= 7500 ,label="pool = 10",color="blue") +
  # annotate(geom="text",x=0.06,y= 9000 ,label="pool = 20",color="black") +
  # annotate(geom="text",x=0.2,y= 8000 ,label="pool = 5",color="dark green") +
  # annotate(geom="text",x=0.04,y= 10300,label="pool = 1",color="red") +
  labs( 
    x= "Prevalence",
    y = "Expected number of \n positives that go undetected")  +
  ggtitle("population 10,000") +
  theme_classic(base_size = 15) +
  #geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  xlim(c(0,0.05)) +
  ylim(c(0,70))



plot10kzoom <- ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = total.tests ), color = "blue") +
  #geom_line(data = p.1.10k, aes(y = prevalence, x = prevalence*10000), color = "red") + 
  geom_line(data = p.20.10k, aes(x = prevalence, y = total.tests), color = "black") + 
  geom_line(data = p.5.10k, aes(x = prevalence, y = total.tests ), color = "dark green") + 
  annotate(geom="text",x=0.075,y= 6500 ,label="pool = 10",color="blue") +
  annotate(geom="text",x=0.08,y= 8700 ,label="pool = 20",color="black") +
  annotate(geom="text",x=0.07,y= 4400 ,label="pool = 5",color="dark green") +
  annotate(geom="text",x=0.04,y= 10300,label="pool = 1",color="red") +
  #ylim(c(0,y.pop*prev*1.2)) +
  labs( 
    x= "Prevalence",
    y = "Total Tests")  +
  #ggtitle("population 10,000") +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 10000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 0.1))


##
plot5k <- ggplot() +
  geom_line(data = p.5.5k, aes(x = prevalence, y = total.tests ), color = "blue") +
  #geom_line(data = p.1.10k, aes(y = prevalence, x = prevalence*10000), color = "red") + 
  geom_line(data = p.10.5k, aes(x = prevalence, y = total.tests), color = "black") + 
  geom_line(data = p.20.5k, aes(x = prevalence, y = total.tests ), color = "dark green") + 
  # annotate(geom="text",x=0.1,y= 5000 ,label="pool = 10",color="blue") +
  # annotate(geom="text",x=0.1,y= 8000 ,label="pool = 20",color="black") +
  # annotate(geom="text",x=0.075,y= 9000 ,label="pool = 5",color="dark green") +
  # annotate(geom="text",x=0.02,y= 10200,label="pool = 1",color="red") +
  #ylim(c(0,y.pop*prev*1.2)) +
  labs( 
    x= "Prevalence",
    y = "Total Tests")  +
  ggtitle("population 5,000") +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 5000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

plot100k <- ggplot() +
  geom_line(data = p.5.100k, aes(x = prevalence, y = total.tests ), color = "blue") +
  #geom_line(data = p.1.10k, aes(y = prevalence, x = prevalence*10000), color = "red") + 
  geom_line(data = p.10.100k, aes(x = prevalence, y = total.tests), color = "black") + 
  geom_line(data = p.20.100k, aes(x = prevalence, y = total.tests ), color = "dark green") + 
  # annotate(geom="text",x=0.1,y= 5000 ,label="pool = 10",color="blue") +
  # annotate(geom="text",x=0.1,y= 8000 ,label="pool = 20",color="black") +
  # annotate(geom="text",x=0.075,y= 9000 ,label="pool = 5",color="dark green") +
  # annotate(geom="text",x=0.02,y= 10200,label="pool = 1",color="red") +
  #ylim(c(0,y.pop*prev*1.2)) +
  labs( 
    x= "Prevalence",
    y = "Total Tests")  +
  ggtitle("population 100,000") +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 100000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

plot10k
#======================================================================================

10000 - p.10.10k$total.tests

ggplot() +
  geom_line(data = p.10.10k, aes(x = prevalence, y = 3*(10000 - total.tests)), color = "blue") +
  #geom_line(data = p.1.10k, aes(y = prevalence, x = prevalence*10000), color = "red") + 
  geom_line(data = p.20.10k, aes(x = prevalence, y = 3*(10000 - total.tests)), color = "black") + 
  geom_line(data = p.5.10k, aes(x = prevalence, y = 3*(10000 - total.tests)), color = "dark green") + 
  annotate(geom="text",x=0.25,y= 5000 ,label="pool = 5",color="dark green") +
  annotate(geom="text",x=0.15,y= 2500 ,label="pool = 10",color="blue") +
  annotate(geom="text",x=0.03,y= 9000 ,label="pool = 20",color="black") +
  # annotate(geom="text",x=0.02,y= 10200,label="pool = 1",color="red") +
  #ylim(c(0,y.pop*prev*1.2)) +
  labs( 
    x= "Prevalence",
    y = "Cost Savings")  +
  #ggtitle("population 10,000") +
  theme_classic(base_size = 15) +
  #geom_hline(yintercept = 100000, linetype = "solid", color = "red", size = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = dollar)

