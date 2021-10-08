#Creator: Matthew LH. Cheng
#Date updated: 10/7/2021
#Purpose: To get sumamry statistics for relevant data for Chilko DIDSON study

#make sure to load in R package before starting to auto set up working directory
# load and set up packages, data, and working directories -----------------

#load in packages
library(here)
library(tidyverse)
library(lubridate)

#set up working directory
getwd()

dir.data <- here("Data") #filepath to data

dir.fig<- here("Figures") #filepath to figures

#load in data for copmlete data set
#these data include smolt density data, total reactions, area of didson, bull 
#trout counts, day night cylce
sum_df <- read_csv(file = file.path(dir.data, "DIDSON_CompleteData.csv"))


# cleaning and setting up data --------------------------------------------

#first rename variables so they're easier to reference
sum_df <- sum_df %>% 
  rename(date = fixdate,
         totalrxn = totalrxns,
         upstream = Upstream,
         downstream = Downstream,
         milling = Milling,
         total = Total,
         area = Area) %>% 
  mutate(date=str_sub(date,1,16)) #removing the seconds from this (only has minutes)

#use lubridate to make dates plottable
sum_df$date<-force_tz(ymd_hm(sum_df$date),"America/New_York") #forcing to EDT time zone



# get sumamry statistics --------------------------------------------------

#cumulative mean max and sd reactions
sum_df %>% 
  summarize(mean = mean(standrxn),
            sd = sd(standrxn),
            min = min(standrxn),
            max = max(standrxn))

#mean and sd of reactions across sites standardized by the area
sum_df %>% 
  group_by(site) %>% 
  summarize(mean = mean(standrxn), 
            sd = sd(standrxn),
            count = n(),
            min = min(standrxn),
            max = max(standrxn))

#look for # of videos for each site
(sites_num<-sum_df %>% 
  group_by(site) %>% 
  summarize(count = n()))

#calculate mean and sd for this
mean(sites_num$count)#mean
sd(sites_num$count)#sd

#group by diel cycles - standardized reactions across site by cycle
sum_df %>% 
  group_by(cycle,site) %>% 
  summarize(mean = mean(standrxn),
            sd = sd(standrxn),
            min = min(standrxn),
            max = max(standrxn))

#group only by diel cycle
sum_df %>% 
  group_by(cycle) %>% 
  summarize(mean = mean(standrxn),
            sd = sd(standrxn),
            min (standrxn),
            max(standrxn))


# Summary for smolt density numbers ---------------------------------------
#statistics for smolt density
sum_df %>% 
  group_by(cycle) %>% 
  drop_na() %>% 
  summarize(mean = mean(smoltdens),
            sd = sd(smoltdens),
            min = min(smoltdens),
            max = max(smoltdens))


# Summarizing bull trout length -------------------------------------------
length_df<-read_csv(file.path(dir.data, "Field_DIDSON_comp.csv"))

#summarize length data
length_df %>%  #DIDSON = full dataset, TL = field collected lenghts, 
  #Sub = subsetted DIDSON to minimzie pseudoreplication
  group_by(type) %>% 
  summarize(mean = mean(size),
            sd = sd(size),
            min = min(size),
            max = max(size),
            count = n())
