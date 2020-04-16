# R data analysis tutorial script for - EBIO 2050-012
# Written by: Deidre Jaeger, 4-Nov-2019

######################################################################################################
# This script is designed to introduce students to using R for statistical analysis
# We will cover
# 1. Setting a working directory
# 2. Opening .csv files
# 3. Exploring dataframes for structure and content
# 4. Data visualization: scatter plot, boxplot, bar graph, mosiac plot
# 5. Exporting figures 
# 6. Calculating summary statistics
# 7. Applying and interpreting a linear regression, logistic regression, t-test, ANOVA + Tukeys post hoc, Chi Square

######################################################################################################

# How to run code in this script: 
# put your cursor anywhere on the line, or highlight the text, hold down CTR + Enter on a PC, or Cmd + Enter on a mac to run the code line or highlighted block

######################################################################################################
################################## Step 1. Setting your working directory

# Working directory: For R the working directory is the location where files are retrieved, stored and saved for a project. If a document is not in the working directory folder, you have to tell R exactly where the file “lives” on your computer with a file path to a subfolder.

# 1.1. Down the folder "EBIO1250-urban-ecology" from canvas and save it on the desktop, or save it where you'd like it to stay on your  computer
      # do not put spaces in the title of the folder name. 
      # Make sure you know where it is saved
      # on a mac where "/Users/" is in the path, or a windows "C:/" is often the start of the path

# 1.2. Set the working  directory to the folder you created in Step 1.1. 
      # Typing the setwd() line of code translates to "set working directory", and assigns the specified folder as the wroking directory.           # Change the below code to the file path to the folder from your own computer: setwd("File pathname to the wd folder")
      # The directory path must be the EXACT path for the folder
?setwd() # learn about the setwd() function
setwd("/Users/deidrejaeger/Documents/Career/CU-Boulder/Research/CURE-apple-data/CURE-apple-data") 
setwd("/Users/deidrejaeger/Documents/Career/") 

# 1.3. Check that your working directory was correctly set to your EBIO1250-urban-ecology folder
?getwd() # learn about the setwd() function
getwd() 
      # check your folder path and resist 1.2 if you get an error message

######################################################################################################
################################## Step 2. Opening .csv files

# 2.1. Download our data folder from canvas, and drag it into your EBIO1250-urban-ecology folder
      # other helpful tips for working with your own data
         # R cannot open a google sheet (.gs) or excel file extension (xls) but both platforms can export a spreadsheet as a .csv
         # make sure your data is a .csv file (R also accepts .txt files)
         # spaces in the column names will often cause issues when loading the data into R. Whenever possible, omit spaces.

# 2.2. Open the csv file in R
a.18data <- read.csv(file= "data/apple-field-data-2018_clean.csv",
                     header=T, # there is a header row
                     na.strings=c("","NA")) # have blanks and NAs read as "NA"
    # we have given the data the name "a.data", this is user defined but it's nice to pick something shorter so you can remember what it is, rather then super descriptive 
    # You should see a18.data appear in your Global Environment
    # another way to choose a file in the line below (you would need to delete the "#" for it to run)
    # AppleData <- read.csv(file.choose()) # (This command works in RStudio, but not R)

######################################################################################################
################################## Step 3. Exploring dataframes for structure and content
#  Dataframes are the format of the .csv files when they are uploaded to R, the data is organized into columns with all variables, and rows with observation of each variable (or NA if missing data)

# 3.1 Here are a couple example functions to use to look at various parts of the dataset. These are useful to visualize your data. 

  # The function "head()" shows the first several rows of the object.
head(a.18data) # there are a lot of columns

  #The function "str()" shows the structure of the object.
str(a.18data)

# 3.2 R uses a [row, column] indexing format 
  # Let's look at some ways of subsetting of the data
  # to get a feel for it, lets look at the 1st row of the 8th column (X10_tree_height_ft)
a.18data[1, 23]

# 3.3 let's try calling the first 8 rows of the 23rd column in three different ways
a.18data[1:8, 23]

  # You can call the same thing using the name of the 23rd column
a.18data[1:8, "X10_tree_height_ft"]

  # Or you can specify the column with a $ after the data fame name, then select rows in the brackets
a.18data$X10_tree_height_ft[1:8]

# 3.4. you could try the head and str functions on the first 5 columns of our large dataset
head(a.18data[, 1:5])
str(a.18data[, 1:5])

# 3.5. Say you want to save just a portion of the data, like just the columns names for tree tag ID, final cultivar, land owner, and tree height. With "c()" to make a list. You can define a name and then use the "<-" sequence after your name to save the new dataframe

a.18subset <- a.18data[, c("X5_tree_tag_id", "Final.Cultivar", "X6_property_owner", "X10_tree_height_ft")]

# 3.6. you can subset your data with the subset() function
subset(a.18data, Final.Cultivar == "Delicious")

  # Say you want to save this subset, you can define a name and then use the "<-" sequence after your name
delicious <- subset(a.18data, Final.Cultivar == "Delicious")
      # you should see the name "delicious" pop up in your global environment with only 35 instances of the 296 observations  of trees we have

# if you want to get rid of any dataset in your global environment, you can use remove()
remove(delicious)

######################################################################################################
################################## 4. Data visualization: scatter plot, bar graph, box plot, mosiac plot

# 4.1 Scatter Plots: x = continious, y = continuous
      # You can observe relationships between two different *continuous* variables in a dataset by using a scatter plot.
# say you are wondering if trunk diameter is related to tree height
# Plot the trunk dripline compared to tree height
plot(X11_tree_drip_line_ft~ # y variable column name
       X10_tree_height_ft, # x variable column name
     data=a.18data, # the data frame
     ylab="Tree Drip Line (ft)", # rename the y label
     xlab="Tree Height (ft)", # rename the x label
     cex.lab=1, # change the size of the text of the axis labels
     cex.axis=0.75) # change the size of the number ticks on the axis

# increase the size of the text on the axis labels
plot(X11_tree_drip_line_ft~
       X10_tree_height_ft, 
     data=a.18data,
     ylab="Tree Drip Line (ft)", 
     xlab="Tree Height (ft)", 
     cex.lab=1.4, # change the size of the text of the axis labels
     cex.axis=1.4) # change the size of the number ticks on the axis

# change the points style with pch and color
plot(x= a.18data$X10_tree_height_ft,
     y =a.18data$X11_tree_drip_line_ft, 
     ylab="Tree Drip Line (ft)", 
     xlab="Tree Height (ft)", 
     cex.lab=1.4, 
     cex.axis=1.4, 
     pch = 19, # changes the points to solid, see more here:  http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
     col = "darkgreen") # change the point color to red, see more color names here: https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html

     
# 4.2. Box plots: x = categorical, y = continuous OR x = continuous, y = categorical
# boxplots display the median, t-tests and ANOVAs that will analyze the group differences are based on the mean. 
# say we wanted to ask do apple trees have larger driplines on private or public land? Public land is a categorical predictor on the X axis, and Drip line is the continuous response variable on the Y axis.
boxplot(X11_tree_drip_line_ft~X6_property_owner, data=a.18data,
        xlab="Property Ownership", 
        ylab="Tree Drip Line (ft)", 
        col = c("red", "blue"), 
        cex.lab = 1.4,
        cex.axis = 1.4)

# what if we have a continuous predictor, and a categorical response variable? 
# flip the boxplot orientation
boxplot(X9_tree_longitude~X16_fb_per_can, data=a.18data,
        xlab="Longitude (Decimal Degrees)", 
        ylab="Fire blight canopy cover (%)", 
        col = c("red", "blue", "green", "orange"), 
        cex.lab = 1.4,
        cex.axis = 1.4,
        horizontal = TRUE) # add this line

# there is a tree in Wyoming that is an outlier to Boulder trees, let's remove this outlier
wyoming <- which.min(a.18data$X9_tree_longitude)
a.18data <- a.18data[-wyoming, ] # notice the number of observations dropped from 296 to 295

# our factors are not in the right order either, let's reset the order manually
a.18data$X16_fb_per_can <- factor(a.18data$X16_fb_per_can, levels = c("0", "0-5", "6-15", "15-100"))

# let's replot
boxplot(X9_tree_longitude~X16_fb_per_can, data=a.18data,
        xlab="Longitude (Decimal Degrees)", 
        ylab="Fire blight canopy cover (%)", 
        col = c("bisque", "coral", "coral3", "coral4"), 
        cex.lab = 1.4,
        cex.axis = 1.4, 
        horizontal = TRUE)


# 4.3 Bar graph
# say we want to know if proprty owner determines whether we find older, larger trees at a location. So our x variable predictor is categorical, and our y response variable is continuous. 
# Step 1: calculate the mean values for a bargraph and assign the output a meaningful name. 
dbhMean <- aggregate(X12_trunk_diameter_1~X6_property_owner, data=a.18data, mean)
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

# Step 5: Install and load the package to add error bars to graphs. 
# Install the package "plotrix".
#install.packages("plotrix")

# Load the package "plotrix".
library(plotrix)

# Step 6: Add the standard error bars to the total veg graph. 
# Note how the barplot command is included inside the plotCI command
plotCI(barplot(dbhMean[,2], beside=T, 
               ylim=c(0,500), 
               ylab="Trunk Diameter (inches)", 
               xlab="Property Ownership", 
               col=c("aquamarine3","wheat1"),
               names.arg=c("Private", "Public")), 
       dbhMean[,2], uiw=dbhSE[,2], add=T, pch=NA)

# Step 7: Scale the y axis to an appropriate size 
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
################################## 5. Exporting figures and saving outputs

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

# Let's save one more: 
jpeg('images/boxplot-fireblight-longitude.jpg')
boxplot(X9_tree_longitude~X16_fb_per_can, data=a.18data,
        xlab="Longitude (Decimal Degrees)", 
        ylab="Fire blight canopy cover (%)", 
        col = c("bisque", "coral", "coral3", "coral4"), 
        cex.lab = 1.4,
        cex.axis = 1.4, 
        horizontal = TRUE)
dev.off()


# alternatively, you can also manually save your images by clicking "Export" in the Plots tab window


######################################################################################################
################################## 6. Calculating summary statistics: mean, st deviation, standard error, 
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
################################## 7. Applying and interpreting linear regression, t-test, ANOVA + Tukey's post goc, chi square

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


#. 7.2. Logisitic Regression: Independent variable (x) is continuous, Dependent variable (y) is categorical (2 groups)
# read more about this test here: https://stats.idre.ucla.edu/r/dae/logit-regression/
# Lets look at how tree height affects fire blight presence or absence
glm1 <- glm(X15_fire_blight ~ X10_tree_height_ft, data = a.18data, family = binomial)
summary(glm1) 	#this will give you the summary stats for the logistic regression

# Call:
#   glm(formula = X15_fire_blight ~ X10_tree_height_ft, family = binomial, 
#       data = a.18data)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.4432  -1.2192   0.9825   1.1191   1.2281  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)        -0.32862    0.33123  -0.992    0.321  
# X10_tree_height_ft  0.02337    0.01352   1.728    0.084 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 269.67  on 195  degrees of freedom
# Residual deviance: 265.39  on 194  degrees of freedom
# (99 observations deleted due to missingness)
# AIC: 269.39
# 
# Number of Fisher Scoring iterations: 4

# In this case, there is not significant difference in fire blight depending on tree height. 

# We want to calculate the mean between groups 
height_means <- aggregate(X10_tree_height_ft~X15_fire_blight, data=a.18data, mean)
height_means
height_se <- aggregate(X10_tree_height_ft~X15_fire_blight, data=a.18data, calcSE)
height_se
# We would report the results as follow: 
# There was not a signficant relationship between tree height and fire blight, (df= 195, n = 196, p = 0.084) for trees with fire blight( (M=25.65ft, SE = 2.00) and without (M= 21.15ft, SE = 0.89).

# I may need to research the reporting of these results more and add more info later.


# 7.3. Ordinal Logistic Regression: Independent variable (x) is continuous, Dependent variable (y) is categorical (more than 2 groups)
# read more about it here: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
# If you have this type of data you will run an ordinal logistic regression. You will need to install the following packages in R: 
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

# run the ordinal logistic regression
OrdLogReg <- polr(X16_fb_per_can ~ X10_tree_height_ft, Hess=TRUE, data = a.18data)
summary(OrdLogReg) 

# Call:
#   polr(formula = X16_fb_per_can ~ X10_tree_height_ft, data = a.18data, 
#        Hess = TRUE)
# 
# Coefficients:
#   Value Std. Error t value
# X10_tree_height_ft 0.01364   0.007629   1.788
# 
# Intercepts:
#   Value  Std. Error t value
# 0|0-5       0.1298 0.2282     0.5688 
# 0-5|6-15    1.4680 0.2556     5.7443 
# 6-15|15-100 2.2941 0.2945     7.7889 
# 
# Residual Deviance: 470.5049 
# AIC: 478.5049 
# (103 observations deleted due to missingness)

#There is no pvalue in this output, but the t-values are useful for understanding some of the variance. 
ctable <- coef(summary(OrdLogReg))

# Let's calculate it
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

# add the p value to the results table
ctable <- cbind(ctable, "p value" = p)

# > ctable
# Value  Std. Error   t value      p value
# X10_tree_height_ft 0.01363744 0.007628557 1.7876825 7.382724e-02
# 0|0-5              0.12978291 0.228165515 0.5688104 5.694848e-01
# 0-5|6-15           1.46804799 0.255566695 5.7442852 9.231008e-09
# 6-15|15-100        2.29410722 0.294537338 7.7888502 6.762169e-15

# Output explained
# We see comparisions between the levels of the fire blight categories. 

# Let's look at our group means and st errors
height_means2 <- aggregate(X10_tree_height_ft~X16_fb_per_can, data=a.18data, mean)
height_means2
height_se2 <- aggregate(X10_tree_height_ft~X16_fb_per_can, data=a.18data, calcSE)
height_se2

# df would be the total observations (295) minus the # of observations with no data that were omitted (103)
# we a significant difference between 0-5 | 6-15  and between 6:15-15-100 because p is less than 0.05. 
# we would write our results for the figure caption and results summary like this:
# There was a significant difference in fire blight intensity depending on tree height (df= 192, p< 0.001). Tree heights were highest in the one of the medium levels of canopy cover of fire blight (6-15%).

# I may need to research the reporting of these results more and add more info later.



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


