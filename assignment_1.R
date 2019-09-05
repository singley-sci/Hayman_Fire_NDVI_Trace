library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)

#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')

ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

# Convert data to wide format, remove NAs, then add month, year, and treatment columns
full_wide <- spread(data=full_long, key='data', value='value') %>%
  filter_if(is.numeric, all_vars(!is.na(.))) %>%
  mutate(month=month(DateTime), 
         year=year(DateTime),
         treatment = cut(year,breaks=c(0,2003,2020),
                         labels=c('pre-burn','post-burn')))
summary(full_wide) #check that all NAs removed
#view(full_wide)

# Filter out only summer months (June-September)
summer_wide <- full_wide %>%
  filter(month %in% c(6:9)) %>%
  mutate(season='summer') #add column 'season' with value 'summer'

# Plot of NDVI vs NDMI colored by site
summer_wide %>%
  ggplot(aes(x=ndmi, y=ndvi, color=site)) +
  geom_point(alpha=0.8) +
  theme_few() +
  scale_color_hc() + #color scale used for sites
  theme(legend.position = c(0.8,0.8)) +
  xlab('NDMI') + ylab('NDVI')

## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 

# Filter out winter only 
winter_wide <- full_wide %>%
  filter(month %in% c(1:4)) %>%
  mutate(season='winter')

seasons_wide <- full_join(x=winter_wide, y=summer_wide) %>%
  group_by(site,season, year) %>%
  summarize(mean_ndvi=mean(ndvi), mean_ndsi=mean(ndsi))
#view(seasons_wide)

# Plot of summer NDVI vs NDSI from prior witner
seasons_wide %>%
  ggplot(aes(x=mean_ndsi, y=mean_ndvi, color=site)) +
  geom_point(alpha=0.8) +
  theme_few() +
  scale_color_hc() + #color scale used for sites
  theme(legend.position = c(0.8,0.8))

## Your code here

## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

seasons_treatment_wide <- full_join(x=winter_wide, y=summer_wide) %>%
  group_by(site,season,treatment, year) %>%
  summarize(mean_ndvi=mean(ndvi), mean_ndsi=mean(ndsi))
#view(seasons_treatment_wide)

seasons_treatment_wide %>%
  ggplot(aes(x=mean_ndsi, y=mean_ndvi, color=treatment)) +
  geom_point(alpha=0.8) +
  facet_wrap(~site) +
  theme_few() +
  scale_color_few() + #color scale used for site
  theme(legend.position = c(0.7,0.2))

## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

monthly_wide <- full_wide %>%
  group_by(site, month, treatment) %>%
  summarize(mean_ndvi=mean(ndvi), mean_ndsi=mean(ndsi), mean_ndmi=mean(ndmi))
#view(monthly_ndvi)

# Line plot of monthly average NDVI by site and treatment
monthly_wide  %>%
  ggplot(aes(x=month, y=mean_ndvi, color=treatment)) +
  geom_line() +
  facet_wrap(~site) +
  theme_few() +
  scale_color_few() + #color scale used for treatments
  theme(legend.position = c(0.75,0.2)) +
  scale_x_continuous(breaks=1:12) + #forces integers on x axis
  xlab('Month') + ylab('NDVI')

# Box plot of NDSI by month and treatment faceted by site
# For this to work instead: comment out summarize line and prior pipe
# when generating monthly_wide

  #monthly_wide  %>%
  #  ggplot(aes(x=factor(month), y=ndvi, color=treatment)) +
  #  geom_boxplot(outlier.size=1, outlier.alpha=0.5) +
  #  facet_wrap(~site) +
  #  theme_few() +
  #  scale_color_few() +
  #  theme(legend.position = c(0.75,0.2)) +
  #  xlab('Month') + ylab('NDVI')


##### Question 5 ####
#What month is the snowiest on average?

monthly_wide  %>%
  ggplot(aes(x=month, y=mean_ndsi, color=treatment)) +
  geom_line() +
  facet_wrap(~site) +
  theme_few() +
  scale_color_few() + #color scale used for treatments
  theme(legend.position = c(0.75,0.8)) +
  scale_x_continuous(breaks=1:12) +
  xlab('Month') + ylab('NDSI')

# Box plot of NDSI by month and treatment faceted by site
# For this to work instead: comment out summarize line and prior pipe
# when generating monthly_wide

#monthly_wide  %>%
#  ggplot(aes(x=factor(month), y=ndsi, color=treatment)) +
#  geom_boxplot(outlier.size=1, outlier.alpha=0.5) +
#  facet_wrap(~site) +
#  theme_few() +
#  scale_color_few() +
#  theme(legend.position = c(0.75,0.8)) +
#  xlab('Month') + ylab('NDSI')