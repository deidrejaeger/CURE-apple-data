---
title: "06_potted tree data"
author: "Deidre Jaeger"
date: "11/1/2019"
output: html_document
---
  
  # load needed libraries
library(plyr)
library(dplyr) 
library(plotrix)
library(ggplot2)

# read in cleaned csv
p.data <- read.csv(file = "data/merged_leaf_summary_pot-level_2019clean.csv")

# look at some histograms of our continuous data
hist(p.data$av_senescence)
hist(p.data$av_fb_percent)
hist(p.data$av_leaflength)
hist(p.data$av_chlorophyll)

# create variables to use in graphs below
treatment <- p.data$treatment_code
snip_density <- p.data$snip_density
inocula <- p.data$inocula_type
fb_strip <- p.data$fire_blight_strip_test
tree_ht <- p.data$X42_Tree_height_neare
fb_percent <- p.data$av_fb_percent
senescence <- p.data$av_senescence
l_length <- p.data$av_leaflength
chloro <- p.data$av_chlorophyll
leaf_num_ob <- p.data$num_leaves_obs
treatment_5 <- p.data$treatment_5 

# load calc st error function
calcSE <- function(x){sd(x)/sqrt(length(x))}

# # combine the yaya and andrus strains into 1 treatment of inoculation
# # create a new column
# p.data$treatment_5 <- p.data$treatment_code
# 
# # create a fused highD inoculum column
# levels(p.data$treatment_5)[levels(p.data$treatment_5)=="highD_ANDR" | levels(p.data$treatment_5)=="highD_YAYA"] <- "highD_inoc"
# 
# # create a fused lowD inoculum column
# levels(p.data$treatment_5)[levels(p.data$treatment_5)=="lowD_ANDR" | levels(p.data$treatment_5)=="lowD_YAYA"] <- "lowD_inoc"

# 4.3 Bar graphs with the full 7 treatments 
# note, im not changing the name of the means and st errors, so you'll overwrite them with each graph

# make a bar plot of tree height
htMean <- aggregate(tree_ht~treatment, data=p.data, mean)
htSE <- aggregate(tree_ht~treatment, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
               ylim=c(0,65), 
               ylab="Tree height (cm)", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
             names.arg=c(levels(p.data$treatment_code))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# 4.3 Bar graph- fireblight % coverage
htMean <- aggregate(fb_percent~treatment, data=p.data, mean)
htSE <- aggregate(fb_percent~treatment, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
               ylim=c(0,6), 
               ylab="Average potential fire blight percent cover per leaf", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_code))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# 4.3 Bar graph- senescence
htMean <- aggregate(senescence~treatment, data=p.data, mean)
htSE <- aggregate(senescence~treatment, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
                ylim=c(0,18), 
               ylab="Average senescence percent cover per leaf", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_code))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# 4.3 Bar graph- leaf length
htMean <- aggregate(l_length~treatment, data=p.data, mean)
htSE <- aggregate(l_length~treatment, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
               ylim=c(0,8), 
               ylab="average leaf length per tree", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_code))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# 4.3 Bar graph- cholorophyll
htMean <- aggregate(chloro~treatment, data=p.data, mean)
htSE <- aggregate(chloro~treatment, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
              ylim=c(0,70), 
               ylab="average chlorophyll per tree", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1")),
               # names.arg=c(levels(p.data$treatment_code))), 
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)


##########################################################################################
# 4.3 Bar graphs using the 5 treatment aggregates
htMean <- aggregate(tree_ht~treatment_5, data=p.data, mean)
htSE <- aggregate(tree_ht~treatment_5, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
              ylim=c(0,55), 
               ylab="Tree height (cm)", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_5))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# 4.3 Bar graph- fireblight % coverage
htMean <- aggregate(fb_percent~treatment_5, data=p.data, mean)
htSE <- aggregate(fb_percent~treatment_5, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
              ylim=c(0,6), 
               ylab="Average potential fire blight percent cover per leaf", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_5))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# 4.3 Bar graph- senescence
htMean <- aggregate(senescence~treatment_5, data=p.data, mean)
htSE <- aggregate(senescence~treatment_5, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
               ylim=c(0,18), 
               ylab="Average senescence percent cover per leaf", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_5))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# 4.3 Bar graph- leaf length
htMean <- aggregate(l_length~treatment_5, data=p.data, mean)
htSE <- aggregate(l_length~treatment_5, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
               ylim=c(0,8), 
               ylab="average leaf length per tree", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_5))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# 4.3 Bar graph- cholorophyll
htMean <- aggregate(chloro~treatment_5, data=p.data, mean)
htSE <- aggregate(chloro~treatment_5, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
               ylim=c(0,70), 
               ylab="average chlorophyll per tree", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
       names.arg=c(levels(p.data$treatment_5))), 
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)


# mosaic plots - all 7 treatments
mosaicplot(~treatment + fb_strip,
           xlab = "Fire blight score",
           ylab = "treatment", 
           cex.axis = 1.4,
           main = "", # get rid of the main title
           color = c("green", "red"))

# mosaic plots - 5 treatments
mosaicplot(~treatment_5 + fb_strip,
           xlab = "Fire blight score",
           ylab = "treatment", 
           cex.axis = 1.4,
           main = "", # get rid of the main title
           color = c("green", "red"))



# # calc means and se's with a function
# data_summary <- function(data, varname, groupnames){
#   summary_func <- function(x, col){
#     c(mean = mean(x[[col]], na.rm=TRUE),
#       sd = sd(x[[col]], na.rm=TRUE))
#   }
#   data_sum<-ddply(data, groupnames, .fun=summary_func,
#                   varname)
#   data_sum <- rename(data_sum, c("mean" = varname))
#   return(data_sum)
# }
# 
# df2 <- data_summary(p.data, "av_fb_percent", levels(snip_density))
# 
# 
# clc means and ses manually for fire blight and treatments
htMean <- aggregate(av_fb_percent~treatment_code, data=p.data, mean)
htSE <- aggregate(av_fb_percent~treatment_code, data=p.data, calcSE)
fb_x_treat7 <- cbind(htMean, htSE)
htMean <- aggregate(av_fb_percent~treatment_5, data=p.data, mean)
htSE <- aggregate(av_fb_percent~treatment_5, data=p.data, calcSE)
fb_x_treat5 <- cbind(htMean, htSE)

# clc means and ses manually for fire blight and senescence
htMean <- aggregate(av_senescence~treatment_code, data=p.data, mean)
htSE <- aggregate(av_senescence~treatment_code, data=p.data, calcSE)
sen_x_treat7 <- cbind(htMean, htSE)
htMean <- aggregate(av_senescence~treatment_5, data=p.data, mean)
htSE <- aggregate(av_senescence~treatment_5, data=p.data, calcSE)
sen_x_treat5 <- cbind(htMean, htSE)

# clc means and ses manually for leaf length and senescence
htMean <- aggregate(av_leaflength~treatment_code, data=p.data, mean)
htSE <- aggregate(av_leaflength~treatment_code, data=p.data, calcSE)
ll_x_treat7 <- cbind(htMean, htSE)
htMean <- aggregate(av_leaflength~treatment_5, data=p.data, mean)
htSE <- aggregate(av_leaflength~treatment_5, data=p.data, calcSE)
ll_x_treat5 <- cbind(htMean, htSE)

# p.test <- p.data %>% 
#   group_by(treatment_code, cultivar_name) %>% 
#   summarize("av_fb_percent_cult" = mean(av_fb_percent, na.rm = TRUE) )
# 
# leaf_sum <- leaf.data %>% 
#   filter(X16_living_tissue != "no") %>% 
#   group_by(cultivar_pot) %>% 
#   summarise("av_fb_percent" = mean(X17_fb_proportionpote, na.rm = TRUE))



# show the amount of observered fire blight % leaf cover by snip density and cultivar
ggplot(data = p.data, aes(x = snip_density, y = av_fb_percent,
                               color = snip_density,
                               fill = snip_density)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar)+
  ylab("average % cover of snip site senescence per leaf") +  # yaxis labels 
  xlab("snip density of leaves") +  # xaxis label
  theme_bw() 


# show the amount of observered senescence % leaf cover by snip density and cultivar
ggplot(data = p.data, aes(x = snip_density, y = av_senescence,
                               color = snip_density,
                               fill = snip_density)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar)+
  ylab("average % cover leaf senescence per leaf") +  # yaxis labels 
  xlab("snip density of leaves") + # xaxis label
  theme_bw()

# show the amount of observered leaf length by snip density and cultivar
ggplot(data = p.data, aes(x = snip_density, y = av_leaflength,
                          color = snip_density,
                          fill = snip_density)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar)+
  ylab("average leaf length (cm) per tree") +  # yaxis labels 
  xlab("snip density of leaves") + # xaxis label
  theme_bw() +
  scale_color_manual(name = "snip_density",
                   labels = c("high" = "18 leaves per pot",
                              "low" = "9 leaves per pot",
                             "no_snip" = "0 leaves per pot"),
                   values = c("high" = "red",
                              "low" = "blue",
                              "no_snip" = "lightgrey")) +
  scale_fill_manual(name = "snip_density",
                    labels = c("high" = "18 leaves per pot",
                               "low" = "9 leaves per pot",
                               "no_snip" = "0 leaves per pot"),
                    values = c("high" = "red",
                               "low" = "blue",
                               "no_snip" = "lightgrey"))

# show the amount of observered senescence % leaf cover by snip density and cultivar
ggplot(data = p.data, aes(x = snip_density, y = av_senescence,
                          color = snip_density,
                          fill = snip_density)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar)+
  ylab("average % cover leaf senescence per leaf") +  # yaxis labels 
  xlab("snip density of leaves") + # xaxis label
  theme_bw() +
  scale_color_manual(name = "snip_density",
                     labels = c("high" = "18 leaves per pot",
                                "low" = "9 leaves per pot",
                                "no_snip" = "0 leaves per pot"),
                     values = c("high" = "red",
                                "low" = "blue",
                                "no_snip" = "lightgrey")) +
  scale_fill_manual(name = "snip_density",
                    labels = c("high" = "18 leaves per pot",
                               "low" = "9 leaves per pot",
                               "no_snip" = "0 leaves per pot"),
                    values = c("high" = "red",
                               "low" = "blue",
                               "no_snip" = "lightgrey"))

# show the amount of snip site senescence % leaf cover by snip density and cultivar
ggplot(data = p.data, aes(x = snip_density, y = av_fb_percent,
                          color = snip_density,
                          fill = snip_density)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar)+
  ylab("average % cover senescence at snip site per leaf") +  # yaxis labels 
  xlab("snip density of leaves") + # xaxis label
  theme_bw() +
  scale_color_manual(name = "snip_density",
                     labels = c("high" = "18 leaves per pot",
                                "low" = "9 leaves per pot",
                                "no_snip" = "0 leaves per pot"),
                     values = c("high" = "red",
                                "low" = "blue",
                                "no_snip" = "lightgrey")) +
  scale_fill_manual(name = "snip_density",
                    labels = c("high" = "18 leaves per pot",
                               "low" = "9 leaves per pot",
                               "no_snip" = "0 leaves per pot"),
                    values = c("high" = "red",
                               "low" = "blue",
                               "no_snip" = "lightgrey"))

# show the amount of snip site senescence % leaf cover by snip density and cultivar
ggplot(data = p.data, aes(x = treatment_code, y = av_fb_percent,
                          color = treatment_code,
                          fill = treatment_code)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar)+
  ylab("average % cover senescence at snip site per leaf") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  scale_color_manual(name = "treatment",
                     labels = c("control",
                                "highD",
                                "highD_ANDR",
                                "highD_YAYA",
                                "lowD",
                                "lowD_ANDR",
                                "lowD_YAYA"),
                     values = c("control" = "grey68",
                                "highD" = "dodgerblue4",
                                "highD_ANDR" = "tomato4",
                                "highD_YAYA" = "sienna4",
                                "lowD" = "dodgerblue1",
                                "lowD_ANDR" = "tomato1", 
                                "lowD_YAYA" = "sienna1")) +
  scale_fill_manual(name = "treatment",
                    labels = c("control",
                               "highD",
                               "highD_ANDR",
                               "highD_YAYA",
                               "lowD",
                               "lowD_ANDR",
                               "lowD_YAYA"),
                    values = c("control" = "grey68",
                               "highD" = "dodgerblue4",
                               "highD_ANDR" = "tomato4",
                               "highD_YAYA" = "sienna4",
                               "lowD" = "dodgerblue1",
                               "lowD_ANDR" = "tomato1", 
                               "lowD_YAYA" = "sienna1"))

# show the amount of total senescence % leaf cover by snip density and cultivar
ggplot(data = p.data, aes(x = treatment, y = av_senescence,
                          color = treatment_code,
                          fill = treatment_code)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar)+
  ylab("average % cover senescence at snip site per leaf") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold", size = 14, angle = 45)) +
  scale_color_manual(name = "treatment",
                     labels = c("control",
                                "highD",
                                "highD_ANDR",
                                "highD_YAYA",
                                "lowD",
                                "lowD_ANDR",
                                "lowD_YAYA"),
                     values = c("control" = "grey68",
                                "highD" = "dodgerblue4",
                                "highD_ANDR" = "tomato4",
                                "highD_YAYA" = "sienna4",
                                "lowD" = "dodgerblue1",
                                "lowD_ANDR" = "tomato1", 
                                "lowD_YAYA" = "sienna1")) +
  scale_fill_manual(name = "treatment",
                    labels = c("control",
                               "highD",
                               "highD_ANDR",
                               "highD_YAYA",
                               "lowD",
                               "lowD_ANDR",
                               "lowD_YAYA"),
                    values = c("control" = "grey68",
                               "highD" = "dodgerblue4",
                               "highD_ANDR" = "tomato4",
                               "highD_YAYA" = "sienna4",
                               "lowD" = "dodgerblue1",
                               "lowD_ANDR" = "tomato1", 
                               "lowD_YAYA" = "sienna1"))

# show the amount of total senescence % leaf cover by snip density and cultivar
ggplot(data = p.data, aes(x = treatment, y = av_senescence,
                          color = treatment_5,
                          fill = treatment_5)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar)+
  ylab("average % cover senescence at snip site per leaf") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  scale_color_manual(name = "treatment",
                     labels = c("control",
                                "highD",
                                "highD_inoc",
                                "lowD",
                                "lowD_inoc"),
                     values = c("control" = "grey68",
                                "highD" = "dodgerblue4",
                                "highD_inoc" = "tomato4",
                                "lowD" = "dodgerblue1", 
                                "lowD_inoc" = "sienna1")) +
  scale_fill_manual(name = "treatment",
                    labels = c("control",
                               "highD",
                               "highD_inoc",
                               "lowD",
                               "lowD_inoc"),
                    values = c("control" = "grey68",
                               "highD" = "dodgerblue4",
                               "highD_inoc" = "tomato4",
                               "lowD" = "dodgerblue1", 
                               "lowD_inoc" = "sienna1")) 

#  add means and se error bars for fire blight with 5 treatment groups
p.data %>% 
group_by(treatment_5, cultivar_name) %>% 
  summarize("av_fb_me" = mean(av_fb_percent, na.rm = TRUE), "av_fb_sd"= sd(av_fb_percent, na.rm = TRUE) ) %>% 
ggplot(aes(x = treatment_5, y = av_fb_me,
                          color = treatment_5,
                          fill = treatment_5)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar_name)+
  ylab("average % cover senescence at snip site per leaf") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  scale_color_manual(name = "treatment",
                     labels = c("control",
                                "highD",
                                "highD_inoc",
                                "lowD",
                                "lowD_inoc"),
                     values = c("control" = "black",
                                "highD" = "black",
                                "highD_inoc" = "black",
                                "lowD" = "black", 
                                "lowD_inoc" = "black")) +
  scale_fill_manual(name = "treatment",
                    labels = c("control",
                               "highD",
                               "highD_inoc",
                               "lowD",
                               "lowD_inoc"),
                    values = c("control" = "grey68",
                               "highD" = "dodgerblue4",
                               "highD_inoc" = "tomato4",
                               "lowD" = "dodgerblue1", 
                               "lowD_inoc" = "sienna1"))  +
  geom_errorbar(aes(ymin = av_fb_me, ymax = av_fb_me + av_fb_sd), width = 0.2, position = position_dodge(0.9))


#  add means and se error bars for fire blight with 7 treatment groups
p.data %>% 
  group_by(treatment_code, cultivar_name) %>% 
  summarize("av_fb_me" = mean(av_fb_percent, na.rm = TRUE), "av_fb_sd"= sd(av_fb_percent, na.rm = TRUE) ) %>% 
  ggplot(aes(x = treatment_code, y = av_fb_me,
             color = treatment_code,
             fill = treatment_code)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar_name)+
  ylab("average % cover senescence at snip site per leaf") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  scale_color_manual(name = "treatment",
                     labels = c("control",
                                "highD",
                                "highD_ANDR",
                                "highD_YAYA",
                                "lowD",
                                "lowD_ANDR",
                                "lowD_YAYA"),
                     values = c("control" = "black",
                                "highD" = "black",
                                "highD_ANDR" = "black",
                                "highD_YAYA" = "black",
                                "lowD" = "black",
                                "lowD_ANDR" = "black", 
                                "lowD_YAYA" = "black")) +
  scale_fill_manual(name = "treatment",
                    labels = c("control",
                               "highD",
                               "highD_ANDR",
                               "highD_YAYA",
                               "lowD",
                               "lowD_ANDR",
                               "lowD_YAYA"),
                    values = c("control" = "grey68",
                               "highD" = "dodgerblue4",
                               "highD_ANDR" = "tomato4",
                               "highD_YAYA" = "sienna4",
                               "lowD" = "dodgerblue1",
                               "lowD_ANDR" = "tomato1", 
                               "lowD_YAYA" = "sienna1")) +
  geom_errorbar(aes(ymin = av_fb_me, ymax = av_fb_me + av_fb_sd), width = 0.2, position = position_dodge(0.9))


#  add means and se error bars for total * leaf senescence* with 7 treatment groups
p.data %>% 
  group_by(treatment_code, cultivar_name) %>% 
  summarize("av_sen_me" = mean(av_senescence, na.rm = TRUE), "av_sen_sd"= sd(av_senescence, na.rm = TRUE) ) %>% 
  ggplot(aes(x = treatment_code, y = av_sen_me,
             color = treatment_code,
             fill = treatment_code)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar_name)+
  ylab("average % cover senescence per leaf") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  scale_color_manual(name = "treatment",
                     labels = c("control",
                                "highD",
                                "highD_ANDR",
                                "highD_YAYA",
                                "lowD",
                                "lowD_ANDR",
                                "lowD_YAYA"),
                     values = c("control" = "black",
                                "highD" = "black",
                                "highD_ANDR" = "black",
                                "highD_YAYA" = "black",
                                "lowD" = "black",
                                "lowD_ANDR" = "black", 
                                "lowD_YAYA" = "black")) +
  scale_fill_manual(name = "treatment",
                    labels = c("control",
                               "highD",
                               "highD_ANDR",
                               "highD_YAYA",
                               "lowD",
                               "lowD_ANDR",
                               "lowD_YAYA"),
                    values = c("control" = "grey68",
                               "highD" = "dodgerblue4",
                               "highD_ANDR" = "tomato4",
                               "highD_YAYA" = "sienna4",
                               "lowD" = "dodgerblue1",
                               "lowD_ANDR" = "tomato1", 
                               "lowD_YAYA" = "sienna1")) +
  geom_errorbar(aes(ymin = av_sen_me, ymax = av_sen_me + av_sen_sd), width = 0.2, position = position_dodge(0.9))


#  add means and se error bars for *senescence* with 5 treatment groups
p.data %>% 
  group_by(treatment_5, cultivar_name) %>% 
  summarize("av_sen_me" = mean(av_senescence, na.rm = TRUE), "av_sen_sd"= sd(av_senescence, na.rm = TRUE) ) %>% 
  ggplot(aes(x = treatment_5, y = av_sen_me,
             color = treatment_5,
             fill = treatment_5)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar_name)+
  ylab("average % cover senescence per leaf") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  scale_color_manual(name = "treatment",
                     labels = c("control",
                                "highD",
                                "highD_inoc",
                                "lowD",
                                "lowD_inoc"),
                     values = c("control" = "black",
                                "highD" = "black",
                                "highD_inoc" = "black",
                                "lowD" = "black", 
                                "lowD_inoc" = "black")) +
  scale_fill_manual(name = "treatment",
                    labels = c("control",
                               "highD",
                               "highD_inoc",
                               "lowD",
                               "lowD_inoc"),
                    values = c("control" = "grey68",
                               "highD" = "dodgerblue4",
                               "highD_inoc" = "tomato4",
                               "lowD" = "dodgerblue1", 
                               "lowD_inoc" = "sienna1"))  +
  geom_errorbar(aes(ymin = av_sen_me, ymax = av_sen_me + av_sen_sd), width = 0.2, position = position_dodge(0.9))


#  count the # of fb strip test positive and negatives
p.data%>% 
  group_by(treatment_5, cultivar_name) %>% 
  filter(!is.na(fire_blight_strip_test)) %>% 
  ggplot(aes(x = treatment_5, fill = factor(fire_blight_strip_test),
             color = treatment_5)) +
  geom_bar(position = 'fill', size = 3) +
  facet_wrap(facets = ~cultivar_name)+
  ylab("proportions of trees") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) +
  scale_color_manual(name = "treatment",
                     labels = c("control",
                                "highD",
                                "highD_inoc",
                                "lowD",
                                "lowD_inoc"),
                     values = c("control" = "grey68",
                                "highD" = "dodgerblue4",
                                "highD_inoc" = "tomato4",
                                "lowD" = "dodgerblue1", 
                                "lowD_inoc" = "sienna1"))   +
  scale_fill_manual(name = "fb strip test result",
                    labels = c("0" = "negative",
                               "1" = "positive"),
                    values = c("0" = "cornsilk",
                               "1" = "black"))
 
# try to add texture to the visualization
# p.data%>% 
#   group_by(treatment_5, cultivar_name) %>% 
#   ggplot(aes(x = treatment_5, fill = factor(fire_blight_strip_test),
#              color = treatment_5)) +
#   geom_bar(position = 'fill') +
#   scale_fill_brewer(palette="OrRd") +
#   facet_wrap(facets = ~cultivar_name)+
#   ylab("proportions of trees") +  # yaxis labels 
#   xlab("experimental treatment") + # xaxis label
#   theme_bw() +
#   theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) +
#   scale_color_manual(name = "treatment",
#                      labels = c("control",
#                                 "highD",
#                                 "highD_inoc",
#                                 "lowD",
#                                 "lowD_inoc"),
#                      values = c("control" = "grey68",
#                                 "highD" = "dodgerblue4",
#                                 "highD_inoc" = "tomato4",
#                                 "lowD" = "dodgerblue1", 
#                                 "lowD_inoc" = "sienna1"))   
#   # scale_fill_manual(name = "test result",
#   #                   labels = c("0" = "negative",
#   #                              "1" = "positive"),
#   #                   values = c("0" = "blue",
#   #                              "1" = "red"))

