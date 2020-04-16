---
title: "03_clean_up_mature_apple_data"
author: "Deidre Jaeger"
date: "11/1/2019"
output: html_document
---
  
# load needed libraries
library(dplyr) 
library(tidyr)
  

# load data
raw.data <- read.csv(file = "data/form-1__tree-surveys 8.csv")


# split the data up by survey type
short.d <- raw.data %>% 
  filter(X6_What_kind_of_surve == "Short Ecological Survey")

# find the start and end column names to remove
s.col1 <- which(names(revisit.d)== "X6_property_owner")
e.col1 <-  which(names(revisit.d)== "X35_fresh_eating2")
short.d <- short.d[, -c(s.col:e.col)]


# split out the revisit surveys
revisit.d <- raw.data %>% 
  filter(X6_What_kind_of_surve == "Revisit Tree Survey")

# remove columns not needed
s.col1 <- which(names(revisit.d)== "X6_property_owner")
e.col1 <-  which(names(revisit.d)== "X35_fresh_eating")
s.col2 <- which(names(revisit.d)== "X7_tree_site_locatio2")
e.col2 <- which(names(revisit.d)== "Similar_fruit_to_tag")
revisit.d <- revisit.d[, -c((s.col1:e.col1), (s.col2:e.col2))]

# split out the new data surveys

new.d <- raw.data %>% 
  filter(X6_What_kind_of_surve == "New tree Survey")

# remove columns not needed
new.d <- new.d[, c((1:e.col1), 64)]

# add a similar fruit column to new survey
new.d$Similar_fruit_to_tag <- NA

# move it to come before notes
new.d <- new.d[, c(1:42, 44, 43)]




# add and populate columns for the short survey to match the new survey
# save the new data column names
new.d.colnames <- colnames(new.d)
new.short.df <- data.frame("ec5_uuid" =short.d$ec5_uuid, "created_at" = short.d$created_at,"title" = short.d$title,        "X1_collector_names" = short.d$X1_collector_names,       "X2_email_address" = short.d$X2_email_address,         "X3_phone_number" = short.d$X3_phone_number,         
                    "X4_date" = short.d$X4_date ,                "X5_tree_tag_id" = short.d$X5_tree_tag_id,          "X6_What_kind_of_surve" = short.d$X6_What_kind_of_surve, 
                    
                      "X6_property_owner" = NA,       "X7_tree_site_location" = short.d$X7_tree_site_location,     "lat_8_tree_latitude" = short.d$lat_8_tree_latitude2,      
                   "long_8_tree_latitude" = short.d$long_8_tree_latitude2,     "accuracy_8_tree_latitude" = short.d$accuracy_8_tree_latitude2, "X10_tree_height_ft"   = NA,    
                "X11_tree_drip_line_ft" = NA,     "X12_trunk_diameter_1" = NA,     "X12_trunk_diameter_2" =NA,   
                  "X12_number_trunks" = NA,       "X13_leaves_collected" = short.d$X13_leaves_collected2,     "X14_trunk_rot" = NA,          
                 "X15_fire_blight" = NA,        "X16_fb_per_can" = short.d$X16_fb_per_can3,           "X17_colored_leaves"  = NA,    
                   "X23_18_fallen_leaves_"= NA,    "X19_tree_photo" = short.d$X19_tree_photo3,          "Is_there_any_frui"  = NA,    
                  "X20_fruit_on_tree_pho" = NA,    "X21_fruit_hanging" = short.d$X21_fruit_hanging3,       "X22_fruit_drop"  = short.d$X22_fruit_drop3,        
                  "X24_fruit_dotpaper_ph" = NA,    "X25_fruit_moisture"= NA,      "X26_seed_coat_colo"  = NA,    
                     "X27_mature_seeds_prop" = NA,    "X28_insect_use_fruit" = NA,     "X29_rotten_fruit_out" = NA,    
                     "X30_rotten_fruit_in" = NA,     "X31_fruit_color" = NA,          "X32_fruit_texture" = NA,      
                    "X33_fruit_flavor" = NA,        "X34_variety_analog" = NA,       "X35_fresh_eating" = NA,       
                    "Similar_fruit_to_tag" = short.d$Similar_fruit_to_tag,    "Notes"  = short.d$Notes)

# add and populate columns for the revisit survey to match the new survey
# save the new data column names
new.d.colnames <- colnames(new.d)
new.revisit.df <- data.frame("ec5_uuid" =revisit.d$ec5_uuid, "created_at" = revisit.d$created_at,"title" = revisit.d$title,        "X1_collector_names" = revisit.d$X1_collector_names,       "X2_email_address" = revisit.d$X2_email_address,         "X3_phone_number" = revisit.d$X3_phone_number,         
                           "X4_date" = revisit.d$X4_date ,                "X5_tree_tag_id" = revisit.d$X5_tree_tag_id,          "X6_What_kind_of_surve" = revisit.d$X6_What_kind_of_surve, 
                           
                           "X6_property_owner" = NA,       "X7_tree_site_location" = NA,     "lat_8_tree_latitude" = revisit.d$lat_43_89_tree_latitude_,      
                           "long_8_tree_latitude" = revisit.d$long_43_89_tree_latitude_,     "accuracy_8_tree_latitude" = revisit.d$accuracy_43_89_tree_latitude_, "X10_tree_height_ft"   = NA,    
                           "X11_tree_drip_line_ft" = NA,     "X12_trunk_diameter_1" = NA,     "X12_trunk_diameter_2" =NA,   
                           "X12_number_trunks" = NA,       "X13_leaves_collected" = NA,     "X14_trunk_rot" = NA,          
                             "X15_fire_blight" = revisit.d$X15_fire_blight2,        "X16_fb_per_can" = revisit.d$X16_fb_per_can2,           "X17_colored_leaves"  = NA,    
                           "X23_18_fallen_leaves_"= NA,    "X19_tree_photo" = revisit.d$X19_tree_photo2,          "Is_there_any_frui"  = NA,    
                           "X20_fruit_on_tree_pho" = NA,    "X21_fruit_hanging" = revisit.d$X21_fruit_hanging2,       "X22_fruit_drop"  = revisit.d$X22_fruit_drop2,        
                           "X24_fruit_dotpaper_ph" = NA,    "X25_fruit_moisture"= NA,      "X26_seed_coat_colo"  = NA,    
                           "X27_mature_seeds_prop" = NA,    "X28_insect_use_fruit" = NA,     "X29_rotten_fruit_out" = NA,    
                           "X30_rotten_fruit_in" = NA,     "X31_fruit_color" = NA,          "X32_fruit_texture" = NA,      
                           "X33_fruit_flavor" = revisit.d$X33_fruit_flavor2,        "X34_variety_analog" = NA,       "X35_fresh_eating" = revisit.d$X35_fresh_eating2,       
                           "Similar_fruit_to_tag" = NA,    "Notes"  = revisit.d$Notes)


# combine the short and revisit date with the new data

full.field.data <- rbind(new.d, new.revisit.df, new.short.df)



# write a csv
write.csv(full.field.data, file = "data/mature_tree_field_data_2019_clean.csv")

# add the mature tree environmental variables


m.data <- read.csv(file = "data/mature_tree_field_data_2019_clean.csv")
summary(m.data$X16_fb_per_can)

