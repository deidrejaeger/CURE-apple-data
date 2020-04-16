---
  title: "fusing the leaf and pot data to 1 datasheet"
author: "Deidre Jaeger"
date: "10/21/2019"
output: html_document
---
  
  
# load needed libraries
library(dplyr) 
library(tidyr)

#######  1. Load in csv
pot.data <- read.csv(file = "data/form-1__data-by-pot.csv")
leaf.data <- read.csv(file = "data/form-2__leaf-data.csv")

######## 2. Link the data by having the pot data set have a parent id column name match leaf data parent id column name
colnames(pot.data)[which(names(pot.data) == "ec5_uuid")] <- "ec5_parent_uuid"

######## 3. Add the cultivar, pot, and pallet # to the leaf data

leaf.data2 <- leaf.data %>% 
  left_join(pot.data, by = "ec5_parent_uuid")

########  4. Adjust the columns of the leaf data to match the pot data
# remove the ec5_uuid column from the leaf data
leaf.data2 <- leaf.data2[, -c(1)]

# change the order of the columns
leaf.data2 <- leaf.data2[, c(1, 2, 3, 4, 18, 19, 20, 6:10, 12:14, 11)]

# add a column in the pot data for chlorophyll
pot.data$X19_chlorophyllquanti <- NA

# filter the pot data to just the entries that have leaf data

pot.data2 <- pot.data %>% 
  filter(X4_branch_number_skip > 0)

# create a list with leaf column names
col.names.leaf <- colnames(leaf.data2)

# add those leaf columns names to the pot data
names(pot.data2)[1:16]  = c(col.names.leaf)

# add the filtered pot data to the leaf data

l.data <- rbind(leaf.data2, pot.data2)

# switch the notes to be last
l.data <- l.data[, c(1:14, 16, 15)]

########## 5. create a title for each leaf 
i = 0
for (i in 1:row_number(l.data)) {
  l.data$code = paste(l.data$X1_cultivarnumber, "-", l.data$X2_potnumber, "-", l.data$X14_branch_number, "-", l.data$X15_leaf_number)
}


##########. 6. create a date of observation for each leaf

# need to clip 10 digits of the left of the date column so can factor as a date
# or remove 14 digits to the right

l.data$date <- gsub('.{14}$', '', l.data$created_at.x)

# move the date and title to the front of the csv
l.data <- l.data[, c(17,18, 1:16)]


# sort the data by tree, pot, branch, leaf, date

l.data <- l.data %>% 
  arrange(X1_cultivarnumber, X2_potnumber, X14_branch_number, X15_leaf_number, date)



# remove some questionable entries
# row 7,8 1030-2-1-1-1  and 1030-2-1-1-3, 1020-2-2-3have some interesting entries where the title doesn't match the listed branches.... seems to just be these ones though?

############ 6. export the csv 

write.csv(l.data, file = "data/leaf_fireblight_inoculation_2019_clean.csv")



######################################################## Clean up the pot level data set


####### 1. load data

p.data <- read.csv(file = "data/form-1__day-1-inoculation.csv")

####### 2. see how many entries have the tree height suite of data
p.data2 <- p.data %>% 
  filter( X41__of_branches > 0)


####### 3. remove the columns that are not needed
p.data2 <- p.data2[, -c(9:18, 20:37)]

####### 4. add a date column and a tree-pot code
# remove 14 digits to the right

p.data2$date <- gsub('.{14}$', '', p.data2$created_at)

########## 5. create a title for each leaf 
i = 0
for (i in 1:row_number(p.data2)) {
  p.data2$cultivar_pot = paste0(p.data2$X1_Tree_cultivar_,"-",p.data2$X3_Tree_pot_)
}

####### 6. reorder the columns to have date and code first
# move the date and title to the front of the csv
p.data2 <- p.data2[, c(19,18,1:17)]

# arrange rows by tree and pot
p.data2 <- p.data2 %>% 
  arrange(X1_Tree_cultivar_, X3_Tree_pot_)


############################################################ clean up the fire blight strip test


fb.data <- read.csv(file = "data/fireblight_test_results_by_treatment_2019-updated11.12.19.csv")


# filter to just include the quality results and trees not in the greenhosue
fb.dataq <- fb.data %>% 
  filter(quality_code == 1)


# add the pot level dataset to the fireblight dataset
pfb.data <- fb.data %>% 
  left_join(p.data2, by = "cultivar_pot")

# arrange rows by tree and pot
pfb.data <- pfb.data %>% 
  arrange(cultivar, pot)

# 577-6 was measured twice, let's keep the earliar measurement
# which row is the fire blight data form Oct 15?

omit_r <- which(pfb.data$date == "2019-10-15" & pfb.data$cultivar_pot =="577-6")

# omit that row
pfb.data <-  pfb.data[-omit_r,]


# write a csv file 
write.csv(pfb.data, file = "data/pot_fireblight_inoculation_2019_clean.csv")



# look at the leaf data
str(leaf.data$X2_potnumber)
str(leaf.data$X17_fb_proportionpote)
str(pot.data$cultivar_pot)




# read in the data
# hdata <- read.csv(file = "data/pot_fireblight_inoculation_2019_clean.csv",
#                    header= T,
#                    na.strings=c("","NA"))

pot.data <- read.csv (file = "data/pot_fireblight_inoculation_2019_clean.csv",
                      header = TRUE,
                      na.strings=c("","NA"))  
leaf.data <- read.csv(file = "data/leaf_fireblight_inoculation_2019_clean.csv",
                      header = TRUE,
                      na.strings=c("","NA")) 
# create a cultivar pot column

i = 0
for (i in 1:row_number(leaf.data)) {
  leaf.data$cultivar_pot = paste0(leaf.data$X1_cultivarnumber,"-",leaf.data$X2_potnumber)
}

# summarize *fire blight* by pot
leaf_sum <- leaf.data %>% 
  filter(X16_living_tissue != "no") %>% 
  group_by(cultivar_pot) %>% 
  summarise("av_fb_percent" = mean(X17_fb_proportionpote, na.rm = TRUE))

# remove the control pots
leaf_sum <- leaf_sum[-c(70:74),]

# add the pot level dataset to the leaf summary
pot.data_sum <- pot.data %>% 
  left_join(leaf_sum, by = "cultivar_pot")

# summarize by pot *senescence*
leaf_sum2 <- leaf.data %>% 
  filter(X16_living_tissue != "no") %>% 
  group_by(cultivar_pot) %>% 
  summarise("av_senescence" = mean(X18_senescence_propor, na.rm = TRUE))

# remove the control pots
leaf_sum2 <- leaf_sum2[-c(70:74),]

# add the pot level dataset to the summarized leaf data
pot.data_sum <- pot.data_sum %>% 
  left_join(leaf_sum2, by = "cultivar_pot")

# summarize by leaf length and pot
leaf_sum3 <- leaf.data %>% 
  filter(X16_living_tissue != "no") %>% 
  group_by(cultivar_pot) %>% 
  summarise("av_leaflength" = mean(X20_leaf_length_cm, na.rm = TRUE))

# remove the control pots
leaf_sum3 <- leaf_sum3[-c(70:74),]

# add the pot level dataset to the summarized leaf data
pot.data_sum <- pot.data_sum%>% 
  left_join(leaf_sum3, by = "cultivar_pot")


# summarize by chlorophyll and pot
leaf_sum4 <- leaf.data %>%
  filter(X16_living_tissue != "no") %>% 
  group_by(cultivar_pot) %>% 
  summarise("av_chlorophyll" = mean(X19_chlorophyllquanti, na.rm = TRUE))

# remove the control pots
leaf_sum4 <- leaf_sum4[-c(70:74),]

# add the pot level dataset to the summarized leaf data
pot.data_sum <- pot.data_sum %>% 
  left_join(leaf_sum4, by = "cultivar_pot")


# count the unique number of leaves observed that had partial or fully living tissue
leaf_sum5 <- leaf.data %>% 
  filter(X16_living_tissue != "no") %>% 
  group_by(cultivar_pot) %>% 
  summarise("num_leaves_obs" = n_distinct(code)) # add the number of leaf observations made

# remove the control pots
leaf_sum5 <- leaf_sum5[-c(70:74),]

# add the pot level dataset to the summarized leaf data
pot.data_sum <- pot.data_sum %>% 
  left_join(leaf_sum5, by = "cultivar_pot")

# clean up the extra space after control
levels(pot.data_sum$treatment_code)[levels(pot.data_sum$treatment_code)=="control "] <- "control"
str(pot.data_sum$treatment_code)

# filter NA's  for snip density and add variable name for snip density and inocula type
pot.data_na <- pot.data_sum %>% 
  filter(is.na(snip_density)) %>% 
  mutate(snip_density = "no_snip", inocula_type = "not_inoculated")

# filter the non-NAs
pot.data_snip <- pot.data_sum %>% 
  filter(!is.na(snip_density)) 

# rebind the snipped with the non-snipped 
pot.data_sum <- rbind(pot.data_snip, pot.data_na )

# add the cultivar name column
# create a new column for cultivar name
pot.data_sum <- pot.data_sum %>% 
  mutate(cultivar_name = factor(cultivar))
levels(pot.data_sum$cultivar_name)[levels(pot.data_sum$cultivar_name)=="1030"] <- "spring_snow"
levels(pot.data_sum$cultivar_name)[levels(pot.data_sum$cultivar_name)=="105"] <- "ben_davis"
levels(pot.data_sum$cultivar_name)[levels(pot.data_sum$cultivar_name)=="577"] <- "unknown"
str(pot.data_sum$cultivar_name)


# combine the yaya and andrus strains into 1 treatment of inoculation
# create a new column
pot.data_sum$treatment_5 <- pot.data_sum$treatment_code

# create a fused highD inoculum column
levels(pot.data_sum$treatment_5)[levels(pot.data_sum$treatment_5)=="highD_ANDR" | levels(pot.data_sum$treatment_5)=="highD_YAYA"] <- "highD_inoc"

# create a fused lowD inoculum column
levels(pot.data_sum$treatment_5)[levels(pot.data_sum$treatment_5)=="lowD_ANDR" | levels(pot.data_sum$treatment_5)=="lowD_YAYA"] <- "lowD_inoc"

# write csv with the added summarized data and the updated fire blight results (more conservative)

write.csv(pot.data_sum, file = "data/merged_leaf_summary_pot-level_2019clean.csv")
