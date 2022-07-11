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
  mutate(site_agg = ifelse(site %in% c("upstream272829","firstnight2021"),
                           "UF", ifelse(site %in% c("downstreamfence2324",
                                                    "downstreamriver2122"),
                                        "DR", "N"))) %>% 
  group_by(site_agg) %>% 
  summarize(mean = mean(standrxn, na.rm = T),
            sd = sd(standrxn, na.rm = T),
            min = min(standrxn, na.rm = T),
            max = max(standrxn, na.rm = T))

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


# Checking bias between UF2021 and UF272829 ----------------------------------------------------------

UF2021 <- df[df$site == "firstnight2021", ]
UF272829 <- df[df$site == "upstream272829", ]

# Check for mean and check for the proportion of zeros observed
sum(UF2021$Reactionpermsq == 0)/nrow(UF2021) # 10
sum(UF272829$Reactionpermsq == 0)/nrow(UF272829) # 7

mean(UF2021$Reactionpermsq) # 0.44
mean(UF272829$Reactionpermsq) # 1.3



# Reconclide video and didson timestap - uncertainty ----------------------

df <- readxl::read_xlsx(path = here("Data", "Didson_data_complete.xlsx"), sheet = 2) %>% 
  select(Filename, `Length 1`, `Length 2`, `Length 3`, `Length4`, `Length 5`,
         `Length 6`, `Length 7`, `Length 8`) %>% 
  pivot_longer(cols = c(`Length 1`, `Length 2`, `Length 3`, `Length4`, `Length 5`,
                        `Length 6`, `Length 7`, `Length 8`), names_to = "len_num",
               values_to = "length") %>% 
  drop_na()

# Remove ms and coerce to numeric
df$length <- as.numeric(strsplit(df$length, "m", fixed = TRUE))

df <- df[!is.na(df$length),]

# n
df %>% count()
# mean
mean(df$length) * 100
max(df$length) * 100
# sd
sd(df$length) * 100

fun.ecdf <-ecdf(df$length)

# ECDF value 
fun.ecdf(0.415)

pdf(file = file.path(dir.fig, "MS figures", "supp_1.pdf"), width = 7.5, height = 5)
ggplot(df, aes(x = length * 100)) +
  stat_ecdf(geom = "smooth", pad = FALSE) +
  theme_bw() +
  labs(x = "Length (cm)", y = "Cumlative Probability") +
  geom_segment(aes(x = 79.5 , y = 0 ,xend = 79.5, yend = 0.993), lty = 2) +
  geom_segment(aes(x = 41 , y = 0 ,xend = 41, yend = 0.199), lty = 2)


dev.off()
