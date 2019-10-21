---
title: "fusing-area-geometries-by-tree"
author: "Deidre Jaeger"
date: "10/21/2019"
output: html_document
---

# load needed libraries
library(dplyr) 
library(tidyr)

# if not installed, then install here 
install.packages("dplyr")
install.packages("tidyr")

#######  1. Load in csv
a.data <- read.csv(file = "data/intersect-100m-area-calculated.csv")



#######  2. Group area by tree tag ID and summarize total area

a.data2<- a.data %>% 
  group_by(X5_tree_tag) %>% 
  summarise(
    total_area_100m_r = sum(area)
  )


#######  3. Add area totals to the main spreadsheet  


# load the original spreadsheet
a.data3 <- read.csv( file = "data/form-1__tree-surveys 4 - form-1__tree-surveys 4.csv.csv")

# make column name the same for tree tag in this original spreadsheet (must have gotten truncated in QGIS for our other dataframes)
colnames(a.data3)[which(names(a.data3) == "X5_tree_tag_id")] <- "X5_tree_tag"

# create an outer join for column for area totals, filling NAs when there isn't an area value for the full dataset

a.data4 <- a.data3 %>% 
  full_join(a.data2, by = "X5_tree_tag")

####### 4. Export csv updated with impervious surface areas

write.csv(a.data4, file = "data/2019-apple-data-imp-area100m.csv")



