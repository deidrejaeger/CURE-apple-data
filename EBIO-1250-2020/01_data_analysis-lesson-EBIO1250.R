# R data analysis tutorial script for - EBIO 2050-012
# Updated by: Deidre Jaeger, 4-Nov-2020

######################################################################################################
# This script is designed to introduce students to using R for statistical analysis
# We will cover
# 1. Setting a working directory
# 2. Opening .csv files
# 3. Exploring dataframes for structure and content
# 4. Fusing two csv files by a column name
# 5. Data visualization: scatter plot, bar graph, mosiac plot
# 6. Exporting figures 
# 7. Calculating summary statistics
# 8. Applying and interpreting a linear regression, t-test, ANOVA + Tukeys post hoc, Chi Square

######################################################################################################

# How to run code in this script: 
# put your cursor anywhere on the line, or highlight the text, hold down CTR + Enter on a PC, or Cmd + Enter on a mac to run the code line or highlighted block

# we also need to make sure all necessary packages are installed, if they aren't installed by running the library() line, then remove the hashtag from "install.packages()" and run that line to install. I recommend replacing a "#" on the install line after you are done so that you aren't installing it again. Once installed, you need to run the "library() " line for the package to become available to R. 
# install.packages("plotrix")
library(plotrix)
#install.packages("foreign")
require(foreign)
#install.packages("ggplot")
require(ggplot2)
#install.packages("MASS")
require(MASS)
# install.packages("Hmisc")
require(Hmisc)
# install.packages("reshape2")
require(reshape2)
# install.packages("dplyr")
library(dplyr) 
# install.packages("agricolae")
library(agricolae)

# load calc st error function
calcSE <- function(x){sd(x)/sqrt(length(x))}


######################################################################################################
################################## Step 1. Setting your working directory

# Working directory: For R the working directory is the location where files are retrieved, stored and saved for a project. If a document is not in the working directory folder, you have to tell R exactly where the file “lives” on your computer with a file path to a subfolder.

# 1.1. Download the folder "EBIO1250-urban-ecology" from canvas and save it on the desktop, or save it where you'd like it to stay on your  computer
# do not put spaces in the title of the folder name. 
# Make sure you know where it is saved
# on a mac where "/Users/" is in the path, or a windows "C:/" is often the start of the path

# 1.2. Set the working  directory to the folder you created in Step 1.1. 
# Typing the setwd() line of code translates to "set working directory", and assigns the specified folder as the wroking directory.           
?setwd() # learn about the setwd() function

# Change the below code to the file path to the folder from your own computer: setwd("File pathname to the wd folder")
# The directory path must be the EXACT path for the folder

## INSERT YOUR OWN FILE PATH HERE ##
setwd("/Users/deidrejaeger/Documents/Career/CU-Boulder/Research/CURE-apple-data/CURE-apple-data/EBIO-1250-2020/") 

# 1.3. Check that your working directory was correctly set to your EBIO1250-urban-ecology folder
?getwd() # learn about the setwd() function
getwd() 
# check your folder path and repeat 1.2 if you get an error message

######################################################################################################
################################## Step 2. Opening .csv files

# 2.1. Download our data folder from canvas, and drag it into your EBIO1250-urban-ecology folder
# other helpful tips for working with your own data
# R cannot open a google sheet (.gs) or excel file extension (xls) but both platforms can export a spreadsheet as a .csv
# make sure your data is a .csv file (R also accepts .txt files)
# spaces in the column names will often cause issues when loading the data into R. Whenever possible, omit spaces.

# 2.2. Open the our data csv files in R
# Sample inventory
sample.inv <- read.csv(file= "data/form-1__sample-inventory.csv",
                     header=T, # there is a header row
                     na.strings=c("","NA")) # have blanks and NAs read as "NA"

# Observations
observations <- read.csv(file= "data/form-2__observations.csv",
                         header=T, # there is a header row
                         na.strings=c("","NA")) # have blanks and NAs read as "NA"


# we have given the data the name "sample.inv", this is user defined but it's nice to pick something shorter so you can remember what it is, rather then super descriptive 
# You should see sample.inv appear in your Global Environment
# another way to choose a file in the line below (you would need to delete the "#" for it to run)
# sample.inv <- read.csv(file.choose()) # (This command works in RStudio, but not R)



# Fix Skylar's nesting of observations under wrong sample ID
str(observations)

# subset 10/16/20
skylar10.16 <- observations %>% 
  filter(created_by == "skda8020@colorado.edu", X14_Observation_date == "10/16/20") 

# pull out the correct parent IDs from 10/16
parent.id <- skylar10.16$ec5_parent_uuid

# subset 10/9/20 and insert the correct parent IDs
skylar10.9 <- observations %>% 
  filter(created_by == "skda8020@colorado.edu", X14_Observation_date == "10/9/20") %>% 
  mutate(ec5_parent_uuid = parent.id)

# subset 10/23/20
skylar10.23 <- observations %>% 
  filter(created_by == "skda8020@colorado.edu", X14_Observation_date == "10/23/20") 

# remove all of skylars observations and replace with the updated observations for 10/9 and add 10/16, 10/23
observations <- observations %>% 
  filter(!created_by %in% ("skda8020@colorado.edu")) %>% 
  rbind(skylar10.9, skylar10.16, skylar10.23)


######################################################################################################
################################## Step 3. Exploring dataframes for structure and content
#  Dataframes are the format of the .csv files when they are uploaded to R, the data is organized into columns with all variables, and rows with observation of each variable (or NA if missing data)

# 3.1 Here are a couple example functions to use to look at various parts of the dataset. These are useful to visualize your data. 

# The function "head()" shows the first several rows of the object.
head(sample.inv) # there are a lot of columns

#The function "str()" shows the structure of the object.
str(sample.inv)

# 3.2 R uses a [row, column] indexing format 
# Let's look at some ways of subsetting of the data
# to get a feel for it, lets look at the 1st row of the 8th column (X3_Treatment_code)
sample.inv[1, 8]

# 3.3 let's try calling the first 8 rows of the 23rd column in three different ways
sample.inv[1:5, 8]

# You can call the same thing using the name of the 23rd column
sample.inv[1:8, "X3_Treatment_code"]

# Or you can specify the column with a $ after the data fame name, then select rows in the brackets
sample.inv$X3_Treatment_code[1:8]

# 3.4. you could try the head and str functions on the first 5 columns of our large dataset
head(sample.inv[, 1:5])
str(sample.inv[, 1:5])

# 3.5. Say you want to save just a portion of the data, like just the columns names for tree tag ID, final cultivar, land owner, and tree height. With "c()" to make a list. You can define a name and then use the "<-" sequence after your name to save the new dataframe

sample.inv_sub <- sample.inv[, c("ec5_uuid", "X2_Cultivar_code", "X7_Inoculation_date", "X12_test_strip_result")]

# 3.6. you can subset your data with the subset() function
subset(sample.inv, X2_Cultivar_code == "GALA")

# Say you want to save this subset, you can define a name and then use the "<-" sequence after your name
gala <- subset(sample.inv, X2_Cultivar_code == "GALA")
# you should see the name "delicious" pop up in your global environment with only 35 instances of the 296 observations  of trees we have

# if you want to get rid of any dataset in your global environment, you can use remove()
remove(gala)

#Lets try creating new variable called "sample_mm3" that will give the volume (length x width x height) in millimeters, you can use math symbols like * / + - to perform basic multiplication, division, addition, and subtraction

sample.inv$sample_mm3 <- (sample.inv$X9_sample_length*sample.inv$X10_sample_width*sample.inv$X11_sample_height)


######################################################################################################
################################## Step 4. Data Cleaning: Fusing two csv files by a column name
# The function "head()" shows the first several rows of the object.
head(observations) # there are a lot of columns

#The function "str()" shows the structure of the object.
str(observations)

#The ec5_parent_uuid  of the observations sheet will link to the sample inventory ec5_uuid

#4.1 Let's link the data by setting the parent id column name of the sample inventory to match the parent id column name of the observations data
colnames(sample.inv)[which(names(sample.inv) == "ec5_uuid")] <- "ec5_parent_uuid"

#4.2. Let's link all of the sample inventory data to the observations data by the "ec5_parent_uuid" column

class.data.full <- observations %>% 
  left_join(sample.inv, by = "ec5_parent_uuid")

# Let's create a column of the average necrosis and ooze columns for the pierce sites in a sample, as well as a new variable with cultivar and treatment code
class.data.full <- class.data.full %>% 
  mutate(necrosis_all = (X17_Necrosis_P1 + X18_Necrosis_P2 + X19_Necrosis_P3)/3) %>% 
  mutate(ooze_all = (X20_Ooze_P1 + X21_Ooze_P2 + X22_Ooze_P3)/3) %>% 
  mutate(cultivar_treatment = paste0(X2_Cultivar_code, "-", X3_Treatment_code))


#4.3 Let's clear out the columns we don't need and reorganize the order
class.data <- class.data.full %>% 
  select(title.y, X6_Observers_initials, cultivar_treatment, X1_Sample_number:X5_Replicate_code, X7_Inoculation_date, sample_mm3, X12_test_strip_result, X14_Observation_date, necrosis_all, ooze_all, X23_Oxidation:X25_Other_observation)


######################################################################################################
################################## Step 5. Data Exploration: comparing treatments and controls

# Let's see how Oooze compared between treatments and controls for each cultivar

# check treatment comparisons with ANOVA with cultivar as predictors **this test shows a significant difference in total leaf senescence among cultivars***
anova_ooze <- aov(ooze_all ~cultivar_treatment, data = class.data)  
summary(anova_ooze) # view the ANOVA results
TukeyHSD(anova_ooze) # check the group comparisons, we see that spring snow is significantly more senescence than ben davis and the unknown

# prepare a dataset with the summarized group variables
results1 <- HSD.test(anova_ooze, "cultivar_treatment", group = TRUE) # save the output of the post hoc test


# Calculate Means and Standard Errors
ooze_means1 <- aggregate(ooze_all ~ cultivar_treatment, data = class.data, mean) # calc group means
ooze_SE1 <- aggregate(ooze_all ~ cultivar_treatment, data = class.data, calcSE) # calc group st. errors
ooze_means1$SE_lower <- ooze_means1$ooze_all - ooze_SE1$ooze_all # create upper bound of st. error
ooze_means1$SE_upper <- ooze_means1$ooze_all + ooze_SE1$ooze_all # create lower bound of st. error
ooze_means1$letters <- results1$groups # save the letter, which represent which groups are statistically significant from each other

# grpah the differences in senescence between cultivars
area.color1 <- c("red", "darkgreen", "red", "darkgreen", "red", "darkgreen", "red", "darkgreen") # save colors to use, you could change these, but red for senescence is intuitive

ooze_means1 %>% 
  ggplot(mapping = aes(x = cultivar_treatment, y = ooze_all)) +
  geom_bar(stat = "identity", color = area.color1, fill = area.color1) +
  geom_errorbar(aes(ymin = SE_lower, ymax = SE_upper), width = 0.4)  +
  ylab("average presence of ooze at 3 pierce sites") +
  xlab("cultivar and treatment") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), 
        axis.text.y = element_text(size = 16))

LEft of here 11/6/2020
  
  
######################################################################################################
################################## Step 5. Data visualization: scatter plot, bar graph, mosiac plot

# 4.1 Scatter Plots: x = continious, y = continuous
# You can observe relationships between two different *continuous* variables in a dataset by using a scatter plot.
# say you are wondering if fungal percent cover is related to size of the fruit sample (volume)
# Plot the fungal percent cover versus the volume of the fruit used
plot(X24_Fungal_per_cov~ # y variable column name
       sample_mm3, # x variable column name
     data=class.data, # the data frame
     ylab="Fungal % cover", # rename the y label
     xlab="Fruit volume used (mm3)", # rename the x label
     cex.lab=1, # change the size of the text of the axis labels
     cex.axis=2, # change the size of the number ticks on the axis
  pch = 19, # changes the points to solid, see more here:  http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
col = "darkgreen") # change the point color to red, see more color names here: https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html


# 4.3 Bar graph
# say we want to know if property owner determines whether we find older, larger trees at a location. So our x variable predictor is categorical, and our y response variable is continuous. 
# Step 1: calculate the mean values for a bargraph and assign the output a meaningful name. 
dbhMean <- aggregate(X12_trunk_diameter_1~, data=a.18data, mean)
# Check out the means for each treatment in the object you created
dbhMean 

# Step 2: calculate the standard error of the means.
# SE = sqrt(variance/n) = standard deviation/sqrt(n), where n is sample size
# Create a function called "calcSE" that will calculate standard errors. 
calcSE <- function(x){sd(x)/sqrt(length(x))}

# Step 3: apply our calcSE function to produce the standard error of the total veg abundance
# You'll use this to create the error bars on the bar graph.
dbhSE <- aggregate(X12_trunk_diameter_1~X6_property_owner, data=a.18data, calcSE)
dbhSE # this prints out the object you just created

# Step 4: Let's first plot the graph without error bars. 
# Highlight all of the following lines and run it.
barplot(dbhMean[,2], beside=T, ylim=c(0,20), 
        ylab="Trunk Diameter (inches)", 
        xlab="Property Ownership", 
        col=c("aquamarine3","wheat1"),
        names.arg=c("private", "public"))


# Step 5: Add the standard error bars to the total veg graph. 
# Note how the barplot command is included inside the plotCI command
plotCI(barplot(dbhMean[,2], beside=T, 
               ylim=c(0,500), 
               ylab="Trunk Diameter (inches)", 
               xlab="Property Ownership", 
               col=c("aquamarine3","wheat1"),
               names.arg=c("Private", "Public")), 
       dbhMean[,2], uiw=dbhSE[,2], add=T, pch=NA)

# Step 6: Scale the y axis to an appropriate size 
plotCI(barplot(dbhMean[,2], beside=T, 
               ylim=c(0,20), # Change "ylim" to a number just above your highest data point
               ylab="Trunk Diameter (inches)", 
               xlab="Property Ownership", 
               col=c("aquamarine3","wheat1"),
               names.arg=c("Private", "Public"), 
               cex.axis=1.4, 
               cex.names=1.4,
               cex.lab = 1.4),
       dbhMean[,2], uiw=dbhSE[,2], add=T, pch=NA)


# 4.4. Mosiac plots: x = categorical, y = categorical
# let's subset to just look at 3 cultivars of apples: delicious, ben davis, and wealthy
# 3.6. you can subset your data with the subset() function
# more on mosaic plots here: https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/mosaicplot.html
cultivar.sub <- subset(a.18data, Final.Cultivar == "Delicious" | Final.Cultivar == "Ben Davis" | Final.Cultivar == "Wealthy",
                       select=c(Final.Cultivar,X15_fire_blight)) # select just the columns of interest
cultivar.sub$Final.Cultivar <- factor(cultivar.sub$Final.Cultivar, levels = c("Ben Davis", "Delicious", "Wealthy"))
mosaicplot(~cultivar.sub$X15_fire_blight + cultivar.sub$Final.Cultivar,
           xlab = "Fire blight score",
           ylab = "Cultivar Name", 
           cex.axis = 1.4,
           main = "", # get rid of the main title
           color = c("red", "blue", "green"))

######################################################################################################
################################## Step 6. Exporting figures and saving outputs

# saving plots in your images folder
# First, create the path where the image will be named and saved
jpeg('images/MosaicPlot-fireblight-cultivar.jpg')
# next, run your full plot
mosaicplot(~cultivar.sub$X15_fire_blight + cultivar.sub$Final.Cultivar,
           xlab = "Fire blight score",
           ylab = "Cultivar Name", 
           cex.axis = 1.4,
           main = "", # get rid of the main title
           color = c("red", "blue", "green"))
# save the plot to your images folder
dev.off()



# alternatively, you can also manually save your images by clicking "Export" in the Plots tab window


######################################################################################################
################################## Step 7. Calculating summary statistics: mean, st deviation, standard error, 
# We can calculate the mean of the full dataset, the average value
dbh_mean <- mean(a.18data$X12_trunk_diameter_1, 
                 na.rm = TRUE) # remove the NA values from the calculation
dbh_mean  # view the mean trunk diameter

# calculating the standard deviation of the trunk diameter
dbh_sd <- sd(a.18data$X12_trunk_diameter_1,
             na.rm = TRUE) # remove the NA values from the calculation)

dbh_sd # view the standard deviation of the trunk diameter

# lets create an object without NAs this time
dbh <-a.18data$X12_trunk_diameter_1[!is.na(a.18data$X12_trunk_diameter_1)]

# Calculate the standard error of the mean, the accuracy of our estimate of the deviation aroudn the mean, given our data sample size
# SE = sqrt(variance/n) = standard deviation/sqrt(n), where n is sample size
# now let's calculate standard error
dbh_se <- sd(dbh)/sqrt(length(dbh))

# the range of values
dbh_ra <- range(dbh)
# or you can use min() and max()

# median, the middle value of a range of values
dbh_med <- median(dbh)


# you can also calculate summary statistics between groups like we did above for a bar graph: 

# Calculate the mean values for a bargraph and assign the output a meaningful name. 
dbhMean <- aggregate(X12_trunk_diameter_1~X6_property_owner, data=a.18data, mean)
# Check out the means for each treatment in the object you created
dbhMean 

# Create a function called "calcSE" that will calculate standard errors. 
# SE = sqrt(variance/n) = standard deviation/sqrt(n), where n is sample size
calcSE <- function(x){sd(x)/sqrt(length(x))}

# Apply our calcSE function to produce the standard error of the total veg abundance
dbhSE <- aggregate(X12_trunk_diameter_1~X6_property_owner, data=a.18data, calcSE)
dbhSE # this prints out the object you just created


######################################################################################################
################################## Step 8. Applying and interpreting linear regression, t-test, ANOVA + Tukey's post goc, chi square

# APA results reporting format: https://depts.washington.edu/psych/files/writing_center/stats.pdf

# 7.1. Linear regression: Independent variable (x) is continuous, Dependent variable (y) is continuous
# Linear regression is used when one response variable is independent variable and the other response variable is dependent.
# how to set it up: lm(RESPONSE or dependent~PREDICTOR or independent, data= NAME_OF_YOUR_DATA_FILE)
# learn more about the linear regression with
?lm()

# let's ask if higher trunk diameter influences a larger drip line
Lm1 <- lm(X11_tree_drip_line_ft ~ X12_trunk_diameter_1, data= a.18data) 
summary(Lm1) # see the results

# Output explained
# The Multiple R-squared value reports how much variation of the dependent variable can be explained by the independent variable.  
# Adjusted R-squared is only used when adding multiple variables at once
# The coefficients are two unknown constants that represent the intercept and slope terms in the linear model 
# The coefficient estimate contains two rows; the first one is the intercept. The second row is the slope
# The F statistic is the division of the model mean square and the residual mean square
# Df is degrees of freedom are the number of values in teh final calculation of a statistic that are free to vary
# p-value is the test significance level, if the value is less than 0.05 then your relationship is considered significant

# Here is our output copy and pasted below, then press Crtl + Shift + C to comment them all out. 

# > summary(Lm1) # see the results
# 
# Call:
#   lm(formula = X11_tree_drip_line_ft ~ X12_trunk_diameter_1, data = a.18data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -19.417  -9.472  -2.629   4.652  97.029 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           21.5822     2.0473  10.542   <2e-16 ***
#   X12_trunk_diameter_1   0.2586     0.1214   2.131   0.0344 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.67 on 196 degrees of freedom
# (97 observations deleted due to missingness)
# Multiple R-squared:  0.02264,	Adjusted R-squared:  0.01765 
# F-statistic:  4.54 on 1 and 196 DF,  p-value: 0.03436

# When writing the results section of a manuscript, you want to clearly tell your reader if the relationship between the variables is significant
# Example language: “There was a [significant/ not a significant (choose one)] [insert direction- e.g. increase/ positive, decrease/ negative] in [dependent variable] with a [insert direction] in [independent variable] (F(degrees of freedom) = ___, p = ___, with an R2 of ___.)
# For the dbh effect on drip line, we would give a biological description: 
# There was a significant increase in dripline with an increase in trunk diameter [F(196)= 4.54, p = 0.034, R^2 = 0.022]. This should go in your figure caption as well as your results summary. 

# if you get a significant result, you can add a line of best fit to your scatter plot
plot(x= a.18data$X12_trunk_diameter_1,
     y =a.18data$X11_tree_drip_line_ft, 
     ylab="Tree Drip Line (ft)", 
     xlab="Trunk diameter (inches)", 
     cex.lab=1.4, 
     cex.axis=1.4, 
     pch = 19, # changes the points to solid, see more here:  http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
     col = "purple") # change the point color to red, see more color names here: https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html

# Add a best fit line to show the positive relationship
abline(Lm1)

# 7.4. T-test: Independent variable (x) is categorical (2 groups), Dependent variable (y) is continuous
# T-tests are used to evaluate whether the means of two experimental groups differ. T-tests are used when you are comparing between ONLY TWO experimental groups.

# Learn about the t.test function in R by running this line of code
?t.test

# Running a t-test in r uses the command t.test, followed by the response (dependent) variable as affected by the predictor (independent) variable
# t.test(RESPONSE(dependent)~PREDICTOR(independent), data= NAME_OF_YOUR_DATA_FILE)
# Run a t-test to determine if the tree trunk diameter varies between private and public ownership. 
t.test(X12_trunk_diameter_1~X6_property_owner, data=a.18data)

# Let's copy the output here

# Welch Two Sample t-test
# 
# data:  X12_trunk_diameter_1 by X6_property_owner
# t = -0.85961, df = 84.893, p-value = 0.3924
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -4.593361  1.820448
# sample estimates:
#   mean in group private  mean in group public 
# 13.73561              15.12207 

# Output explained

# t is the t-value, which measures the size of the difference relative to the variation in your sample data.
# Df is degrees of freedom are the number of values in teh final calculation of a statistic that are free to vary
# p-value is the test significance level, if the value is less than 0.05 then your relationship is considered significant
# 95% the confidence interval defines a range of values that you can be 95% certain contains the population mean. If it overlaps 0, then the mean's are not significantly different
# the means between each groups are provided as well

# When writing the results section of a manuscript, you want to clearly tell your reader whether or not there was a significant difference between group means.
# Here is our example language
# “There was a significant/ not a significant (choose one) difference in the [dependent variable] for [Treatment 1] (M=___, SE=___) and [Treatment 2] (M=___, SE=___) treatments; t(df) = t-value, p = p-value ”

# we have the means, p value, df, and t value in our t-test output, but need to calculate the standard error
# SE = sqrt(variance/n) = standard deviation/sqrt(n), where n is sample size
calcSE <- function(x){sd(x)/sqrt(length(x))}

# Apply our calcSE function to produce the standard error of the total veg abundance
dbhSE <- aggregate(X12_trunk_diameter_1~X6_property_owner, data=a.18data, calcSE)
dbhSE # this prints out the object you just created

# Here is what we would say about these biological results for our figure caption and in our results summary:
# "There was not a significant difference in the trunk diameter between Private land ownership (M=13.73, SE=0.717) and Public Land ownership (M=15.122, SE=1.44) treatments; t(84) = -0.86, p = 0.3924. ”

# 7.5. ANOVA- Analysis of Variance. Independent variable (x) is categorical (more than 2 groups), Dependent variable (y) is continuous
# Anovas are used to analyze the differences among group means in a sample

# Let's subset our data
anova.sub <- subset(a.18data, Final.Cultivar == "Delicious" | Final.Cultivar == "Ben Davis" | Final.Cultivar == "Jonathan")

# So here we are looking at "trunk diameter as predicted by cultivar
anova_model1 <- aov(X12_trunk_diameter_1~Final.Cultivar , data= anova.sub)
summary(anova_model1) # view the results

# Copy of our output
# > summary(anova_model1) # view the results
#                 Df Sum Sq Mean Sq F value Pr(>F)  
# Final.Cultivar  2  300.5  150.24   4.223 0.0223 *
#   Residuals      37 1316.3   35.58                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 7 observations deleted due to missingness

# The output is similar to a t-test, because a t-test is technically a type of ANOVA, just with 2 levels
# Df is degrees of freedom are the number of values in teh final calculation of a statistic that are free to vary
# Sum of Squres is the variability within groups 
# The Mean Squares is dividing the sum-of-squares by the appropriate number of degrees of freedom.
# The F value, is a ratio of two mean square values; a larger F value means that the variation between groups menas is more than you'd expect to see by chance
# Pr(> F) is the p-value is the test significance level, if the value is less than 0.05 then your relationship is considered significant


# Example of results to report whether there was a significant difference among groups: 
# “There was a significant (not a significant) effect of [Independent Variable] on [Dependent variable] for the three groups [F(df between groups, df within groups) = ___, p = ____].

# In our case, the biological results for your figure caption and results summary would be as follow:
# “There was a significant effect of apple cultivar on trunk diameter for the three cultivars [F(2, 37) = 4.22, p = 0.022].

# If the results of the ANOVA are significant, then we perform a follow up test called the Tukey's Post Hoc test to see which of the treatments were different.

# In the DBH/cultivar example, we can conclude that there is a significant difference between cultivar and DBH., however, we do not know if all 3 cultivars have significantly different DBH from one another or just 2 of them. 

#### Do a Tukey's Post Hoc test to help interprete the ANOVA results 
###This test, the Honestly Significant Difference test, compares each pair of
###treatments to each other to isolate the comparisons of greatest interest.  
TukeyHSD(anova_model1) #p adj means p value adjusted 

# > TukeyHSD(anova_model1) #p adj means p value adjusted
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = X12_trunk_diameter_1 ~ Final.Cultivar, data = anova.sub)
# 
# $Final.Cultivar
#                       diff        lwr       upr     p adj
# Delicious-Ben Davis -3.887467 -9.4439567  1.669023 0.2156800
# Jonathan-Ben Davis   7.207792 -4.1760556 18.591639 0.2817426
# Jonathan-Delicious  11.095259  0.4490292 21.741488 0.0395437

# Output explained
# The diff is the difference in mean DBH between 2 cultivars
# The lwr and upp are the confidence intervals that the means are different, if the interval overlaps 0 then they are not significantly diffeernt. 
# p-adj is the p value adjusted for the treatment groups

# Here we see the that only the DBH between Jonathan and Delicious is significantly different
# we need the means and st errors again

# Apply our calcSE function to produce the standard error of the total veg abundance
cult_means <- aggregate(X12_trunk_diameter_1~Final.Cultivar, data=anova.sub, mean)
cult_means # this prints out the object you just created

# Apply our calcSE function to produce the standard error of the total veg abundance
cult_se <- aggregate(X12_trunk_diameter_1~Final.Cultivar, data=anova.sub, calcSE)
cult_se # this prints out the object you just created

# here is what we would also write in our figure caption and results summary in addition to the ANOVA results. 
# “Post hoc comparisons using the Tukey HSD test indicated that the DBH for Delicious (M = 10.31, SE = 0.98) was significantly different than the DBH for Jonathan (M = 21.40, SE = 3.09). However, Ben Davis  (M = 14.19, SE = 2.67) did not significantly differ from the Delicious and Jonathan DBH.”

# 7.6. Chi Squared Test and Fisher's Test: Independent variable (x) is categorical (2 levels), Dependent variable (y) is categorical ( 2 levels)
# more this test here: https://www.datascienceblog.net/post/statistical_test/contingency_table_tests/
# This statistic measures the extent to which the frequencies and distribution of the observed variables differ from that which would be expected based on the null hypothesis.

# To perform this calculation in R, first you need to create a contingency table for the predictor and response variables. We need to create a subset of the groups, then create a summary table
cultivar.sub <- subset(a.18data, Final.Cultivar == "Ben Davis" | Final.Cultivar == "Delicious" | Final.Cultivar == "Wealthy",
                       select=c(Final.Cultivar,X15_fire_blight)) 

# pick 2 to compare at a time
cult_x_fb <- data.frame( 
  positive_fb = c(length(which(cultivar.sub$Final.Cultivar == 'Ben Davis' & cultivar.sub$X15_fire_blight== 1)),
                  length(which(cultivar.sub$Final.Cultivar == 'Delicious' & cultivar.sub$X15_fire_blight== 1))), 
  negative_fb = c(length(which(cultivar.sub$Final.Cultivar == 'Ben Davis' & cultivar.sub$X15_fire_blight== 0)), 
                  length(which(cultivar.sub$Final.Cultivar == 'Delicious' & cultivar.sub$X15_fire_blight== 0))))

str(cult_x_fb)
chisq.test(cult_x_fb)
chisq.post.hoc(cult_x_fb)

# Here is the output:
# > chisq.test(cult_x_fb)
# 
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  cult_x_fb
# X-squared = 0.22122, df = 1, p-value = 0.6381

# Output Explained
# X-squared, also known as the Pearson chi-square statistic. This value is large when the values in your data are far from the values you would expect given a null hypothesis. X-squared and the degrees of freedom are used to calculate the probability of getting this data based on chance alone.
# Degrees of Freedom. This is calculated as (number of rows -1)x(number of columns - 1).
# p-value is the test significance level, if the value is less than 0.05 then your relationship is considered significant. 

# you will have to add up N, the total number of samples

sum(c(length(which(cultivar.sub$Final.Cultivar == 'Ben Davis' & cultivar.sub$X15_fire_blight== 1)),
      length(which(cultivar.sub$Final.Cultivar == 'Delicious' & cultivar.sub$X15_fire_blight== 1))), 
    negative_fb = c(length(which(cultivar.sub$Final.Cultivar == 'Ben Davis' & cultivar.sub$X15_fire_blight== 0)), 
                    length(which(cultivar.sub$Final.Cultivar == 'Delicious' & cultivar.sub$X15_fire_blight== 0))))

# How to report the results
# A chi-square test of independence was performed to examine the relation between apple cultivar and fire blight presence.
# The relation between these variables was not significant, X^2 (2, N = 37) = 0.22, p =0.63. 



# A Fishers exact test is used when 1 or more of our values in the table are less than 5 (small sample sizes of less than 1000), can check if get the same result
fisher.test(cult_x_fb) # the results is similar, though a slightly lower p value

# How to report the results
# A Fisher's Exact test of independence was performed to examine the relation between apple cultivar and fire blight presence.
# The relationship between these variables was not significant, (N = 37, p =0.46). 


# you can also pick 3 to compare at a time
cult_x_fb3 <- data.frame( 
  positive_fb = c(length(which(cultivar.sub$Final.Cultivar == 'Ben Davis' & cultivar.sub$X15_fire_blight== 1)),
                  length(which(cultivar.sub$Final.Cultivar == 'Delicious' & cultivar.sub$X15_fire_blight== 1)),
                  length(which(cultivar.sub$Final.Cultivar == 'Wealthy' & cultivar.sub$X15_fire_blight== 1))), 
  negative_fb = c(length(which(cultivar.sub$Final.Cultivar == 'Ben Davis' & cultivar.sub$X15_fire_blight== 1)),
                  length(which(cultivar.sub$Final.Cultivar == 'Delicious' & cultivar.sub$X15_fire_blight== 0)), 
                  length(which(cultivar.sub$Final.Cultivar == 'Wealthy' & cultivar.sub$X15_fire_blight== 0))))

# run the Chi Squared test
chisq.test(cult_x_fb3)

# > chisq.test(cult_x_fb2)
# 
# Pearson's Chi-squared test
# 
# data:  cult_x_fb2
# X-squared = 0.39459, df = 2, p-value = 0.8209


# and the Fisher test
fisher.test(cult_x_fb3, 
            simulate.p.value = TRUE, # for when you have more than 2 x 2  variables
            hybrid = TRUE) # for when you have more than 2 x 2  variables

# Fisher's Exact Test for Count Data with simulated p-value (based on 2000 replicates)
# 
# data:  cult_x_fb3
# p-value = 0.8846
# alternative hypothesis: two.sided


