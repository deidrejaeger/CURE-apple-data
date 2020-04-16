#' ---
#' title: "05_mature tree data"
#' author: "Deidre Jaeger"
#' date: "11/1/2019"
#' output: pdf_document
#' ---


##################### alex, molly, nicole data
# read in the data
edata <- read.csv(file = "data/elevation_data_final_clean.csv")
# change the january level
levels(edata$X16_fb_per_can)[levels(edata$X16_fb_per_can)=="5-Jan"] <- "1-5"
# change the june level
levels(edata$X16_fb_per_can)[levels(edata$X16_fb_per_can)=="15-Jun"] <- "6-15"
# change the 0-5 level (this was my mistake in epicollect)
levels(edata$X16_fb_per_can)[levels(edata$X16_fb_per_can)=="0-5"] <- "1-5"
# check structure
str(edata$X16_fb_per_can)
# check categories
unique(edata$X16_fb_per_can)
# change the order of the categories
edata$X16_fb_per_can <- factor(edata$X16_fb_per_can, levels = c("0", "1-5", "6-15", "15-100")) #reorder the levels in ascending order

# check the elevation structure
str(edata$Elevation)
# Let's make it an integer
edata$Elevation <- as.numeric(as.character(edata$Elevation))

# we can make a box plot based on the different levels of fire blight
boxplot(Elevation ~ X16_fb_per_can, data=edata,
        xlab="Elevation (m)",
        ylab="Fire blight in canopy (% cover)",
        col = c("darkgreen", "green", "orange", "red"), 
        cex.lab = 1.4,
        cex.axis = 1.4,
        horizontal = TRUE) 

# we can also make a box plot with just the presence and absence of elevation

boxplot(Elevation ~ X15_fire_blight, data=edata,
        xlab="Elevation (m)",
        ylab="Observed fire blight in canopy",
        col = c("darkgreen", "orange"), 
        cex.lab = 1.4,
        cex.axis = 1.4,
        horizontal = TRUE) 

# 
# # export the data as csv
# write.csv(edata, file = "data/elevation_data_final_clean.csv")

