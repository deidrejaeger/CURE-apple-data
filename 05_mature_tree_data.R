---
title: "05_mature tree data"
author: "Deidre Jaeger"
date: "11/1/2019"
output: html_document
---

# load needed libraries
library(dplyr) 
library(tidyr)
library(plyr)
  
# mature tree data
m.data <- read.csv(file = "data/mature_tree_field_data_2019_clean.csv",
                     header = TRUE,
                     na.strings=c("","NA"))  
# mature tree data, when opened in excel shows dates instead of factored categories
m2.data <- read.csv(file = "/Users/deidrejaeger/Downloads/EBIO1250-urban-ecology/data/mature_tree_field_data_2019_clean.csv")

str(m2.data$X16_fb_per_can)


################## sarrah and gordon's data  
pdata <- read.csv(file= "data/sarrah_gordon_fb_Precipitationclean.csv")

# separate lat long by comma
pdata <- pdata %>% 
  separate(Coordinates, c("latitude", "longitude"), sep = ",")

# write csv
write.csv(pdata, (file= "data/sarrah_gordon_fb_Precipitationclean.csv"))

str(pdata$PercentFireblightCanopy)
pdata$PercentFireblightCanopy <- as.factor(pdata$PercentFireblightCanopy)

boxplot(PrecipitationAvg..in.~ PercentFireblightCanopy, data=pdata,
        # xlab="Property Ownership", 
        # ylab="Tree Drip Line (ft)", 
        col = c("red", "blue"), 
        cex.lab = 1.4,
        cex.axis = 1.4,
horizontal = TRUE) # add this line


##################### alex, molly, nicole data
# read in the data
# edata <- read.csv(file = "data/ELEVATION DATA FINAL CSV.csv")
edata <- read.csv(file = "data/elevation_data_final_clean.csv")
# change the january level
levels(edata$X16_fb_per_can)[levels(edata$X16_fb_per_can)=="5-Jan"] <- "1-5"
# change the june level
levels(edata$X16_fb_per_can)[levels(edata$X16_fb_per_can)=="15-Jun"] <- "6-15"
# change the 0-5 level (this was my mistake in epicollect)
levels(edata$X16_fb_per_can)[levels(edata$X16_fb_per_can)=="0-5"] <- "1-5"
str(edata$X16_fb_per_can)
unique(edata$X16_fb_per_can)
edata$X16_fb_per_can <- factor(edata$X16_fb_per_can, levels = c("0", "1-5", "6-15", "15-100"))
# export the data as csv
#write.csv(edata, file = "data/elevation_data_final_clean.csv")

str(edata$X16_fb_per_can)
unique(edata$X16_fb_per_can)
pdata$PercentFireblightCanopy <- as.factor(pdata$PercentFireblightCanopy)
str(edata$X16_fb_per_can)
unique(edata$X16_fb_per_can)

str(edata$Elevation)
edata$Elevation <- as.integer(edata$Elevation)

boxplot(Elevation ~ X16_fb_per_can, data=edata,
        # xlab="Property Ownership", 
        # ylab="Tree Drip Line (ft)", 
        col = c("red", "blue", "orange", "green"), 
        cex.lab = 1.4,
        cex.axis = 1.4,
        horizontal = TRUE) # add this line

##################### caleb, chance data












### extra- not sure why my if statement wasn't working here


edata$X16_fb_per_can <- as.character(edata$X16_fb_per_can)


# change the fire blight categories to the categories since coded as a date
i = 0
for (i in 1:row_number(edata)) {
  if (edata$X16_fb_per_can[i] = "5-Jan") {
    edata$X16_fb_per_can[i] = "1-5"
  }
}

i = 1
for (i in 1:row_number(edata)) {

}

if (edata$X16_fb_per_can[15] = "5-Jan"){
print("true")
  }
  
  i = 0
for (i in 1:row_number(l.data)) {
  l.data$code = paste(l.data$X1_cultivarnumber, "-", l.data$X2_potnumber, "-", l.data$X14_branch_number, "-", l.data$X15_leaf_number)
}