#Reproduction wrapper for Watkins et al. 
# more info to be filled in

#Run statistical analysis
source("statistics/saliva_pooling.R")


#assign key results from statistical analysis
#REG COEF RELATIVE SENSITIVITY
sens5 <- (length(which(impact$cdc_n1_ct+mod4.3.coef[1]>38))-45)/135 #7.41
sens10 <- (length(which(impact$cdc_n1_ct+mod4.3.coef[1]+mod4.3.coef[2]>38))-45)/135 #11.11
sens20 <- (length(which(impact$cdc_n1_ct+mod4.3.coef[1]+mod4.3.coef[3]>38))-45)/135 #14.81

#UPPER LIMIT
sens5u<- (length(which(impact$cdc_n1_ct+3.046>38))-45)/135 #11.11
sens10u <- (length(which(impact$cdc_n1_ct+3.952>38))-45)/135 #20.00
sens20u <-(length(which(impact$cdc_n1_ct+4.383>38))-45)/135 #24.44

#LOWER LIMIT
sens5l <- (length(which(impact$cdc_n1_ct+1.398>38))-45)/135 #4.44
sens10l <- (length(which(impact$cdc_n1_ct+2.306>38))-45)/135 #8.15
sens20l <- (length(which(impact$cdc_n1_ct+2.747>38))-45)/135 #8.89

wrapper.run <- TRUE

#Run pooling analysis
source("pooling/analysis_figures2.R")

#Make pooling plots

plot10k +
  annotation_custom(ggplotGrob(plot10k.zoom), xmin = 0.10, xmax = 0.32, ymin = 0, ymax = 6000)



plot10k.pos.test.c +
  annotation_custom(ggplotGrob(
    plot10k.pos.test.c.zoom), xmin = 0.10, xmax = 0.3, ymin = 50, ymax = 520)


plot10k.pos.test +
  annotation_custom(ggplotGrob(plot10k.pos.test.zoom), xmin = 0.12, xmax = 0.32, 
                    ymin = -0.02, ymax = 0.15)

#crossing point for positives per test
#when 5 is better than 10
pout <- which.min((p.5.10k$pos.found/p.5.10k$total.tests - p.10.10k$pos.found/p.10.10k$total.tests )^2)
p.5.10k$prevalence[pout]

#when 10 is better then 20 
pout <- which.min((p.10.10k[1:300,]$pos.found/p.10.10k[1:300,]$total.tests - p.20.10k[1:300,]$pos.found/p.20.10k[1:300,]$total.tests )^2)
p.10.10k$prevalence[pout]

#prevalence for single testing. 
p.5.10k$prevalence[which.min((p.5.10k$pos.found/p.5.10k$total.tests - p.5.10k$prevalence)^2)]


p_load(cowplot)

plot_grid(plot10k +
            annotation_custom(ggplotGrob(plot10k.zoom), xmin = 0.10, xmax = 0.32, ymin = 0, ymax = 6000),
          plot10k.pos.test.c +
            annotation_custom(ggplotGrob(
              plot10k.pos.test.c.zoom), xmin = 0.10, xmax = 0.3, ymin = 50, ymax = 520),
          ncol = 1,
          #align = c("v"),
          #rel_heights = c(2, 2)
          labels = c('A', 'B'),
          label_x = 0, label_y = 0,
          hjust = -0.5, vjust = -0.5
          )
ggsave('figs/figure2.pdf', scale = 1, units = "in", 
       width = 5,
       height = 1.618 * 5,
       dpi = 300)
