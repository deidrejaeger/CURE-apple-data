#' ---
#' title: "07_potted tree data analysis"
#' author: "Deidre Jaeger"
#' date: "11/1/2019"
#' output: pdf_document
#' ---

# rmarkdown::render("07_potted_tree_analysis.R", "pdf_document") Or press Cmd + Shift + K
  
  
# Load the required libraries 
library(dplyr) 
library(plotrix)
library(ggplot2)
library(vcd)
library(gdata)
library(agricolae)
# library(RVAideMemoire) # dont need this, for post hoc chi squared


# if get an error message "Error in library() : there is no package called.." you don't have them you can remove the hashtags from here to install, then replace the hashtag after so not installing over and over
# install.packages("dplyr")
# install.packages("plotrixr")
# install.packages("ggplot2")
# install.packages("vcd")
# install.packages("gdata")
# install.packages("agricolae")
# install.packages("RVAideMemoire")



# read in cleaned csv
p.data <- read.csv(file = "data/merged_leaf_summary_pot-level_2019clean.csv")


# look at some histograms of our continuous data
hist(p.data$av_senescence)
hist(p.data$av_fb_percent)
hist(p.data$av_leaflength)
hist(p.data$av_chlorophyll)

# create variables to use in some graphs below
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
cultivar_name <-p.data$cultivar_name
cultivar_number <- p.data$cultivar

# load calc st error function
calcSE <- function(x){sd(x)/sqrt(length(x))}

# interesting analyses and graphs to answer the question about how cultivar and strain impacted tree response (Vanessa, Maureen, Bekka)
##################################################################################################################################################################################################################################################################################################################################################################################################################


# mosaic plots - cultivar name
mosaicplot(~cultivar_name + fb_strip,
           xlab = "Fire blight score",
           ylab = "Cultivar Name", 
           cex.axis = 1.4,
           main = "", # get rid of the main title
           color = c("blue", "red"))

# chi square test looking at cultivar 
cult_x_fb3 <- data.frame( 
  positive_fb = c(length(which(p.data$cultivar == '105' & p.data$fire_blight_strip_test== 1)),
                  length(which(p.data$cultivar == '1030' & p.data$fire_blight_strip_test== 1)),
                  length(which(p.data$cultivar == '577' & p.data$fire_blight_strip_test== 1))), 
  negative_fb = c(length(which(p.data$cultivar == '105' & p.data$fire_blight_strip_test== 1)),
                  length(which(p.data$cultivar == '1030' & p.data$fire_blight_strip_test== 0)), 
                  length(which(p.data$cultivar == '577' & p.data$fire_blight_strip_test== 0))))

# run the Chi Squared test, no significant differences among cultivars
ch.x <- chisq.test(cult_x_fb3)


# check the Fishers test because of low sample size, this also yields non-significant differences among cultivars
fisher.test(cult_x_fb3, 
            simulate.p.value = TRUE, # for when you have more than 2 x 2  variables
            hybrid = TRUE) # for when you have more than 2 x 2  variables


# incoculated treatment groups for ben davis only
benD <- p.data%>% 
  group_by(treatment_code, cultivar_name) %>% 
  filter(!is.na(fire_blight_strip_test), cultivar == "105", treatment_code == "highD_ANDR" | treatment_code == "highD_YAYA"| treatment_code == "lowD_ANDR" | treatment_code == "lowD_YAYA")

benD %>%  
  ggplot(aes(x = treatment_code, fill = factor(fire_blight_strip_test),
             color = factor(fire_blight_strip_test))) +
  geom_bar(position = 'fill', size = 2) +
  ylab("proportions of trees tested") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold", size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 16)) +
  scale_color_manual(name = "fb strip test result",  
                     labels = c("0" = "negative",
                                "1" = "positive"),
                     values = c("0" = "blue",
                                "1" = "red")) +
  scale_fill_manual(name = "fb strip test result",
                    labels = c("0" = "negative",
                               "1" = "positive"),
                    values = c("0" = "blue",
                               "1" = "red"))

# run a chi square test comparing the strains 

benD_y_x_a <- p.data %>% 
  filter(cultivar == "105")

benD_y_x_a <- data.frame( 
  positive_fb = c(length(which(benD_y_x_a$inocula_type == 'andrus_Ea' & benD_y_x_a$fire_blight_strip_test== 1)),
                  length(which(benD_y_x_a$inocula_type == 'yaya_Ea' & benD_y_x_a$fire_blight_strip_test== 1))), 
  negative_fb = c(length(which(benD_y_x_a$inocula_type == 'andrus_Ea' & benD_y_x_a$fire_blight_strip_test== 1)), 
                  length(which(benD_y_x_a$inocula_type == 'yaya_Ea' & benD_y_x_a$fire_blight_strip_test== 0))))

# chi square test looking at high low
chisq.test(benD_y_x_a)

# check the Fishers test because of low sample size
fisher.test(benD_y_x_a, 
            simulate.p.value = TRUE, # for when you have more than 2 x 2  variables
            hybrid = TRUE) # for when you have more than 2 x 2  variables


# run a chi square test comparing the strains and high low treatments

benD_y_x_a <- p.data %>% 
  filter(cultivar == "105")

benD_y_x_a_x_hl <- data.frame( 
  positive_fb = c(length(which(benD_y_x_a$treatment_code == 'highD_ANDR' & benD_y_x_a$fire_blight_strip_test== 1)),
                  length(which(benD_y_x_a$treatment_code == 'highD_YAYA' & benD_y_x_a$fire_blight_strip_test== 1)),
                  length(which(benD_y_x_a$treatment_code == 'lowD_ANDR' & benD_y_x_a$fire_blight_strip_test== 1)),
                  length(which(benD_y_x_a$treatment_code == 'lowD_YAYA' & benD_y_x_a$fire_blight_strip_test== 1))), 
  negative_fb = c(length(which(benD_y_x_a$treatment_codee == 'highD_ANDR' & benD_y_x_a$fire_blight_strip_test== 0)),
                  length(which(benD_y_x_a$treatment_code == 'highD_YAYA' & benD_y_x_a$fire_blight_strip_test== 0)),
                  length(which(benD_y_x_a$treatment_code == 'lowD_ANDR' & benD_y_x_a$fire_blight_strip_test== 0)),
                  length(which(benD_y_x_a$treatment_code == 'lowD_YAYA' & benD_y_x_a$fire_blight_strip_test== 0))))

# chi square test looking at high low, not significant
chisq.test(benD_y_x_a_x_hl)

# check the Fishers test because of low sample size, also not significant
fisher.test(benD_y_x_a_x_hl, 
            simulate.p.value = TRUE, # for when you have more than 2 x 2  variables
            hybrid = TRUE) # for when you have more than 2 x 2  variables




# check treatment comparisons with ANOVA with cultivar as predictors **this test shows a significant difference in total leaf senescence among cultivars***
anova_model4 <- aov(senescence~cultivar_name, data= p.data)
summary(anova_model4) # view the results
TukeyHSD(anova_model4) # check the group comparisons, we see that spring snow is significantly more senescence than ben davis and the unknown

# chi square test looking snip density and cultivar 

# prepare a dataset with the summarized group variables
results1 <- HSD.test(anova_model4, "cultivar_name", group = TRUE) # save the output of the post hoc test
results1 <- results1$groups[order(rownames(results1$groups)), ] # make sure that the cultivars match the statistics 
sen_means1 <- aggregate(av_senescence ~ cultivar_name, data = p.data, mean) # calc group means
sen_SE1 <- aggregate(av_senescence ~ cultivar_name, data = p.data, calcSE) # calc group st. errors
sen_means1$SE_lower <- sen_means1$av_senescence - sen_SE1$av_senescence # create upper bound of st. error
sen_means1$SE_upper <- sen_means1$av_senescence + sen_SE1$av_senescence # create lower bound of st. error
sen_means1$letters <- results1$groups # save the letter, which represent which groups are statistically significant from each other

# grpah the differences in senescence between cultivars
area.color1 <- c("darkgreen", "red", "darkgreen") # save colors to use, you could change these, but red for senescence is intuitive

ggplot(data = sen_means1, mapping = aes(x = cultivar_name, y = av_senescence)) +
  geom_bar(stat = "identity", color = area.color1, fill = area.color1) + 
  geom_errorbar(aes(ymin = SE_lower, ymax = SE_upper), width = 0.4) + 
  geom_text(aes(x = cultivar_name, y = SE_upper+.7, label = letters), size = 5)+
  ylab("average leaf senescence (% cover)") +
  xlab("cultivar") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), 
        axis.text.y = element_text(size = 16))



############################################################################################################################################################################################################################################################################
#############################################################################################################################################
# interesting analyses and graphs to answer the question about how the high low snip density treatments impacted fire blight response (Dani, Kobain)

# mosaic plots - compare high and low density inoculated treatments
densit.d <- p.data %>% 
  filter(treatment_5 == "highD_inoc" | treatment_5 == "lowD_inoc") %>% 
  filter(snip_density != "no_snip") 
densit.d <- gdata::drop.levels(densit.d) # drop the unused level using gdata package
mosaicplot(~densit.d$snip_density + densit.d$fire_blight_strip_test,
           xlab = "Fire blight score",
           ylab = "treatment", 
           cex.axis = 1.4,
           main = "", # get rid of the main title
           color = c("green", "red"))

# chi square test to compare high and low density inoculated treatments
high_x_low <- data.frame( 
  positive_fb = c(length(which(p.data$treatment_5 == 'highD_inoc' & p.data$fire_blight_strip_test== 1)),
                  length(which(p.data$treatment_5 == 'lowD_inoc' & p.data$fire_blight_strip_test== 1))),
  negative_fb = c(length(which(p.data$treatment_5 == 'highD_inoc' & p.data$fire_blight_strip_test== 0)),
                  length(which(p.data$treatment_5 == 'lowD_inoc' & p.data$fire_blight_strip_test== 0)))) 


# run the Chi Squared test
chisq.test(high_x_low)

# check the Fishers test because of low sample size
fisher.test(high_x_low, 
            simulate.p.value = TRUE, # for when you have more than 2 x 2  variables
            hybrid = TRUE) # for when you have more than 2 x 2  variables


#  count the # of fb strip test positive and negatives for high and low snip inculated treatment groups
p.data%>% 
  group_by(treatment_5, cultivar_name) %>% 
  filter(!is.na(fire_blight_strip_test)) %>% 
  filter(treatment_5 == "highD_inoc" | treatment_5 == "lowD_inoc") %>% 
  ggplot(aes(x = treatment_5, fill = factor(fire_blight_strip_test),
             color = factor(fire_blight_strip_test))) +
  geom_bar(position = 'fill', size = 2) +
  facet_wrap(facets = ~cultivar_name)+
  ylab("proportions of trees tested") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold", size = 16, angle = 45, hjust = 1),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 16)) +
  scale_color_manual(name = "fb strip test result",
                     labels = c("0" = "negative",
                                "1" = "positive"),
                     values = c("0" = "blue",
                                "1" = "red")) +
  scale_fill_manual(name = "fb strip test result",
                    labels = c("0" = "negative",
                               "1" = "positive"),
                    values = c("0" = "blue",
                               "1" = "red"))



# chi square test looking snip density and cultivar 
# filter to look at high and low inoculated data only
high.low.data <- p.data %>% 
  filter(treatment_5 == "highD_inoc" | treatment_5 == "lowD_inoc" ) %>% 
  mutate(treatment_cultivar = as.factor(paste0(cultivar,"-",treatment_5)))

# build the contigency table
cult_x_highlow <- data.frame( 
  positive_fb = c(length(which(high.low.data$treatment_cultivar == '105-highD_inoc' & high.low.data$fire_blight_strip_test== 1)),
                  length(which(high.low.data$treatment_cultivar == '105-lowD_inoc' & high.low.data$fire_blight_strip_test== 1)),
                  length(which(high.low.data$treatment_cultivar == '577-highD_inoc' & high.low.data$fire_blight_strip_test== 1)),
                  length(which(high.low.data$treatment_cultivar == '577-lowD_inoc' & high.low.data$fire_blight_strip_test== 1)),
                  length(which(high.low.data$treatment_cultivar == '1030-highD_inoc' & high.low.data$fire_blight_strip_test== 1)),
                  length(which(high.low.data$treatment_cultivar == '1030-lowD_inoc' & high.low.data$fire_blight_strip_test== 1))),
  negative_fb = c(length(which(high.low.data$treatment_cultivar == '105-highD_inoc' & high.low.data$fire_blight_strip_test== 0)),
                  length(which(high.low.data$treatment_cultivar == '105-lowD_inoc' & high.low.data$fire_blight_strip_test== 0)),
                  length(which(high.low.data$treatment_cultivar == '577-highD_inoc' & high.low.data$fire_blight_strip_test== 0)),
                  length(which(high.low.data$treatment_cultivar == '577-lowD_inoc' & high.low.data$fire_blight_strip_test== 0)),
                  length(which(high.low.data$treatment_cultivar == '1030-highD_inoc' & high.low.data$fire_blight_strip_test== 0)),
                  length(which(high.low.data$treatment_cultivar == '1030-lowD_inoc' & high.low.data$fire_blight_strip_test== 0))))

# view contingency table
cult_x_highlow
# run the Chi Squared test
chisq.test(cult_x_highlow)

# check the Fishers test because of low sample size
fisher.test(cult_x_highlow, 
            simulate.p.value = TRUE, # for when you have more than 2 x 2  variables
            hybrid = TRUE) # for when you have more than 2 x 2  variables
# not significant, even though the graph appears like #577 unknown would be different than ben davis and springsnow


# check treatment comparisons with ANOVA with snip density as predictors **this test shows a significant difference in total senescence at snip site***
anova_model12 <- aov(av_fb_percent~treatment_5, data= p.data)
summary(anova_model12) # view the results
TukeyHSD(anova_model12) # check the group comparisons, we see that spring snow is significantly more senescence than ben davis and the unknown


# prepare a dataset with the summarized group variables
results2 <- HSD.test(anova_model12, "treatment_5", group = TRUE) # save the output of the post hoc test
results2 <- results2$groups[order(rownames(results2$groups)), ] # make sure that the cultivars match the statistics 
sen_means2 <- aggregate(av_fb_percent ~ treatment_5, data = p.data, mean) # calc group means
sen_SE2 <- aggregate(av_fb_percent ~ treatment_5, data = p.data, calcSE) # calc group st. errors
sen_means2$SE_lower <- sen_means2$av_fb_percent - sen_SE2$av_fb_percent # create upper bound of st. error
sen_means2$SE_upper <- sen_means2$av_fb_percent + sen_SE2$av_fb_percent # create lower bound of st. error
sen_means2$letters <- results2$groups # save the letter, which represent which groups are statistically significant from each other

# grpah the differences in senescence between cultivars
area.color2 <- c("darkgreen", "green", "green", "red", "green") # save colors to use, you could change these, but red for senescence is intuitive

ggplot(data = sen_means2, mapping = aes(x = treatment_5, y = av_fb_percent)) +
  geom_bar(stat = "identity", color = area.color2, fill = area.color2) + 
  geom_errorbar(aes(ymin = SE_lower, ymax = SE_upper), width = 0.4) + 
  geom_text(aes(x = treatment_5, y = SE_upper+.7, label = letters), size = 5)+
  ylab("average leaf senescence at snip site (% cover)") +
  xlab("treatment") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), 
        axis.text.y = element_text(size = 16))

# So here we are looking at total senescence as predicted by treatment and cultivar, this test yielded significant results for cultivar
anova_model7 <- aov(fb_percent~treatment_5 + cultivar_name, data= p.data)
summary(anova_model7) # view the results
# post hoc test to see what differed 
TukeyHSD(anova_model7) #its really just the lowD compared to the control, which isn't super interesting since we didn't snip the controls and the LowD has no Fire blight. 

# lets check if this is signficant after taking out the control trees
treatment_4 <- p.data %>% 
  filter(treatment_5 != "control")
anova_model8 <- aov(av_fb_percent~treatment_5 + cultivar_name, data= treatment_4)
summary(anova_model8) # view the results, not significant among high and low density snip trees


# lets check differences in total senescence with treatment and cultivar as predictors
anova_model11 <- aov(av_senescence~treatment_cultivar, data= high.low.data)
summary(anova_model11) # view the results, not significant among high and low density snip tree treatments
TukeyHSD(anova_model11) 
results <- HSD.test(anova_model11, "treatment_cultivar", group = TRUE)
results <- results$groups[order(rownames(results$groups)), ]
sen_means <- aggregate(av_senescence ~ treatment_cultivar, data = high.low.data, mean)
sen_SE <- aggregate(av_senescence ~ treatment_cultivar, data = high.low.data, calcSE)
sen_means$SE_lower <- sen_means$av_senescence - sen_SE$av_senescence
sen_means$SE_upper <- sen_means$av_senescence + sen_SE$av_senescence
sen_means$letters <- results$groups

#
area.color <- c("red", "green", "green", "green", "green", "darkgreen")

ggplot(data = sen_means, mapping = aes(x = treatment_cultivar, y = av_senescence)) +
  geom_bar(stat = "identity", color = area.color, fill = area.color) + 
  geom_errorbar(aes(ymin = SE_lower, ymax = SE_upper), width = 0.4) + 
  geom_text(aes(x = treatment_cultivar, y = SE_upper+.8, label = letters), size = 6)+
  ylab("average leaf senescence (% cover)") +
  xlab("treatment") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16))










# OTHER GRAPHS/TESTS that aren't super interesting, or that are graphed in a better way above. 
######################################################################################################################################
############################################################################################################################################################################################################################################################################
# 4.3 Bar graph- fireblight % coverage with all 7 treatment groups
htMean <- aggregate(fb_percent~treatment, data=p.data, mean)
htSE <- aggregate(fb_percent~treatment, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
               ylim=c(0,6), 
               ylab="average % cover senescence at snip site per leaf", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_code))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# check treatment comparisons with ANOVA

# So here we are looking at fire blight % as predicted by treatment
anova_model2 <- aov(fb_percent~treatment, data= p.data)
summary(anova_model2) # view the results



#  add means and se error bars for snip site with 7 treatment groups
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

# check treatment comparisons with ANOVA with treatment and cultivar as predictors
anova_model1 <- aov(fb_percent~treatment + cultivar, data= p.data)
summary(anova_model1) # view the results


#  add means and se error bars for total * senescence* with 7 treatment groups
p.data %>% 
  group_by(treatment_code, cultivar_name) %>% 
  summarize("av_sen_me" = mean(av_senescence, na.rm = TRUE), "av_sen_sd"= sd(av_senescence, na.rm = TRUE) ) %>% 
  ggplot(aes(x = treatment_code, y = av_sen_me,
             color = treatment_code,
             fill = treatment_code)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar_name)+
  ylab("average senescence per leaf (% cover)") +  # yaxis labels 
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

# check treatment comparisons with ANOVA with treatment as predictor
anova_model3 <- aov(senescence~treatment, data= p.data)
summary(anova_model3) # view the results


# mosaic plots - cultivar name
mosaicplot(~cultivar_name + fb_strip,
           xlab = "Fire blight score",
           ylab = "Cultivar Name", 
           cex.axis = 1.4,
           main = "", # get rid of the main title
           color = c("red", "blue", "green"))

# chi square test looking at cultivar 
cult_x_fb3 <- data.frame( 
  positive_fb = c(length(which(p.data$cultivar == '105' & p.data$fire_blight_strip_test== 1)),
                  length(which(p.data$cultivar == '1030' & p.data$fire_blight_strip_test== 1)),
                  length(which(p.data$cultivar == '577' & p.data$fire_blight_strip_test== 1))), 
  negative_fb = c(length(which(p.data$cultivar == '105' & p.data$fire_blight_strip_test== 1)),
                  length(which(p.data$cultivar == '1030' & p.data$fire_blight_strip_test== 0)), 
                  length(which(p.data$cultivar == '577' & p.data$fire_blight_strip_test== 0))))

# run the Chi Squared test
chisq.test(cult_x_fb3)

# check the Fishers test because of low sample size
fisher.test(cult_x_fb3, 
            simulate.p.value = TRUE, # for when you have more than 2 x 2  variables
            hybrid = TRUE) # for when you have more than 2 x 2  variables


# 7 treatment grouops for ben davis only
p.data%>% 
  group_by(treatment_code, cultivar_name) %>% 
  filter(!is.na(fire_blight_strip_test), cultivar == "105") %>% 
  ggplot(aes(x = treatment_code, fill = factor(fire_blight_strip_test),
             color = treatment_code)) +
  geom_bar(position = 'fill', size = 2) +
  facet_wrap(facets = ~cultivar_name)+
  ylab("proportions of trees tested") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) +
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
                                "lowD_YAYA" = "sienna1"))   +
  scale_fill_manual(name = "fb strip test result",
                    labels = c("0" = "negative",
                               "1" = "positive"),
                    values = c("0" = "cornsilk",
                               "1" = "black"))




# run a chi square test comparing the strains 

benD_y_x_a <- p.data %>% 
  filter(cultivar == "105")

benD_y_x_a <- data.frame( 
  positive_fb = c(length(which(benD_y_x_a$inocula_type == 'andrus_Ea' & p.data$fire_blight_strip_test== 1)),
                  length(which(benD_y_x_a$inocula_type == 'yaya_Ea' & p.data$fire_blight_strip_test== 1))), 
  negative_fb = c(length(which(benD_y_x_a$inocula_type == 'andrus_Ea' & p.data$fire_blight_strip_test== 1)), 
                  length(which(benD_y_x_a$inocula_type == 'yaya_Ea' & p.data$fire_blight_strip_test== 0))))

# chi square test looking at high low
chisq.test(benD_y_x_a)

# check the Fishers test because of low sample size
fisher.test(benD_y_x_a, 
            simulate.p.value = TRUE, # for when you have more than 2 x 2  variables
            hybrid = TRUE) # for when you have more than 2 x 2  variables

######################################################################################################################

# 4.3 Bar graphs of snip site senscense using the 5 treatment aggregates (so lumping YAYA and ANDRUS)
htMean <- aggregate(fb_percent~treatment_5, data=p.data, mean)
htSE <- aggregate(fb_percent~treatment_5, data=p.data, calcSE)
plotCI(barplot(htMean[,2], beside=T, 
               ylim=c(0,6), 
               ylab="average % cover senescence at snip site per leaf", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_5))), # name the levels of categories
       htMean[,2], uiw=htSE[,2], add=T, pch=NA)

# So here we are looking at fire blight % at snip site as predicted by treatment, this test yielded significant results
anova_model6 <- aov(fb_percent~treatment_5, data= p.data)
summary(anova_model6) # view the results
# post hoc test to see what differed 
TukeyHSD(anova_model6) #its really just the lowD compared to the control, which isn't super interesting since we didn't snip the controls and the LowD has no Fire blight. 

# 4.3 Bar graphs of total senscense using the 5 treatment aggregates (so lumping YAYA and ANDRUS)
htMean2 <- aggregate(av_senescence~treatment_5, data=p.data, mean)
htSE2 <- aggregate(av_senescence~treatment_5, data=p.data, calcSE)
plotCI(barplot(htMean2[,2], beside=T, 
               ylim=c(0,15), 
               ylab="average senescence per leaf (% cover)", 
               xlab="Treatment", 
               col=c("aquamarine3","wheat1"),
               names.arg=c(levels(p.data$treatment_5))), # name the levels of categories
       htMean2[,2], uiw=htSE2[,2], add=T, pch=NA)

# So here we are looking at total senescence as predicted by treatment
anova_model10 <- aov(av_senescence~treatment_5, data= p.data)
summary(anova_model10) # view the results

# mosaic plots - 5 treatments
mosaicplot(~treatment_5 + fb_strip,
           xlab = "Fire blight score",
           ylab = "treatment", 
           cex.axis = 1.4,
           main = "", # get rid of the main title
           color = c("green", "red"))


#  add means and se error bars for *snip site senscense* with 5 treatment groups
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


# So here we are looking at total senescence as predicted by treatment and cultivar, this test yielded significant results for cultivar
anova_model7 <- aov(fb_percent~treatment_5 + cultivar_name, data= p.data)
summary(anova_model7) # view the results
# post hoc test to see what differed 
TukeyHSD(anova_model7) #its really just the lowD compared to the control, which isn't super interesting since we didn't snip the controls and the LowD has no Fire blight. 

# lets check if this is signficant after taking out the control trees
treatment_4 <- p.data %>% 
  filter(treatment_5 != "control")
anova_model8 <- aov(av_fb_percent~treatment_5 + cultivar_name, data= treatment_4)
summary(anova_model8) # view the results, not significant among high and low density snip trees


#  look at  * total senescence* between high and low density treatments and cultivars 
p.data %>% 
  group_by(treatment_5, cultivar_name) %>% 
  summarize("av_sen_me" = mean(av_senescence, na.rm = TRUE), "av_sen_sd"= sd(av_senescence, na.rm = TRUE) ) %>% 
  ggplot(aes(x = treatment_5, y = av_sen_me,
             color = treatment_5,
             fill = treatment_5)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar_name)+
  ylab("average senescence per leaf (% cover)") +  # yaxis labels 
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

# lets check differences in total senescence with treatment and cultivar as predictors
anova_model9 <- aov(av_senescence~treatment_5 + cultivar_name, data= p.data)
summary(anova_model9) # view the results, not significant among high and low density snip tree treatments
TukeyHSD(anova_model9) # spring snow is sig dif from ben davis and the unknown tree... but this is comparing all types of snip density

# let's just compare the high and low snip density groups with total senescence
high.low.data <- p.data %>% 
  filter(treatment_5 == "highD_inoc" | treatment_5 == "lowD_inoc" ) %>% 
  mutate(treatment_cultivar = as.factor(paste0(cultivar,"-",treatment_5)))

#  look at  * total senescence* between high and low density treatments and cultivars 
high.low.data %>% 
  group_by(treatment_5, cultivar_name) %>% 
  summarize("av_sen_me" = mean(av_senescence, na.rm = TRUE), "av_sen_sd"= sd(av_senescence, na.rm = TRUE) ) %>% 
  ggplot(aes(x = treatment_5, y = av_sen_me,
             color = treatment_5,
             fill = treatment_5)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = ~cultivar_name)+
  ylab("average senescence per leaf (% cover)") +  # yaxis labels 
  xlab("experimental treatment") + # xaxis label
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  scale_color_manual(name = "treatment",
                     labels = c(
                                "highD_inoc",
                         
                                "lowD_inoc"),
                     values = c(
                                
                                "highD_inoc" = "black",
                              
                                "lowD_inoc" = "black")) +
  scale_fill_manual(name = "treatment",
                    labels = c(
                               "highD_inoc",
                           
                               "lowD_inoc"),
                    values = c(
                               
                               "highD_inoc" = "tomato4",
                             
                               "lowD_inoc" = "sienna1"))  +
  geom_errorbar(aes(ymin = av_sen_me, ymax = av_sen_me + av_sen_sd), width = 0.2, position = position_dodge(0.9))



#  add means and se error bars for *snip site senscense* with 5 treatment groups
p.data %>% 
  group_by(treatment_5) %>% 
  summarize("av_fb_me" = mean(av_fb_percent, na.rm = TRUE), "av_fb_sd"= sd(av_fb_percent, na.rm = TRUE) ) %>% 
  
  
  ggplot(aes(x = treatment_5, y = av_fb_me,
             color = treatment_5,
             fill = treatment_5)) +
  geom_bar(stat = 'identity') +
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






