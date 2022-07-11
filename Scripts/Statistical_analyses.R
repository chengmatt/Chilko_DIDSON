#Creator: Matthew LH. Cheng
#Date updated: 10/7/2021
#Purpose: To get calculate relevant statistical analyses for Chilko DIDSON study

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

# full dataset ------------------------------------------------------------
sum_df <- read_csv(file = file.path(dir.data, "DIDSON_CompleteData.csv"))

# length dataset ---------------------------------------------------------
length_df<-read_csv(file.path(dir.data, "Field_DIDSON_comp.csv"))


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

# Wilcoxon Tests - Diel  --------------------------------------------------

#wilcox rank sum at upstream 2021 and upstream 272829 
#only testing for these two because prelim analyses showed all other sites would
#not require testing (i.e., would be insiginficant


# Wilcox overall
wilcox.test(standrxn ~ cycle, data = sum_df)

#filtering to these datasets just to isolate them
diel_2021<-sum_df %>% 
  filter(site == 'firstnight2021')

diel_272829<-sum_df %>% 
  filter(site == 'upstream272829')

dr2122 <- sum_df %>% 
  filter(site == 'downstreamriver2122')

df2324 <- sum_df %>% 
  filter(site == 'downstreamfence2324')

n2526 <- sum_df %>% 
  filter(site == 'narrows2526')

n29 <- sum_df %>% 
  filter(site == 'narrows29')

#2021 wilcox- testing for differences at site across diel cycles
wilcox.test(standrxn~cycle, data = diel_2021) # sig

#272829 wilcox - testing for differences at site across diel cycles
wilcox.test(standrxn~cycle, data = diel_272829) # sig

# Other sites
wilcox.test(standrxn~cycle, data = df2324) # sig

# Other sites
wilcox.test(standrxn~cycle, data = dr2122) # sig

dr2122 %>% 
  group_by(cycle) %>% 
  summarize(mean = mean(standrxn, na.rm = T),
            sd = sd(standrxn, na.rm = T))

# Kruskal Wallis  ---------------------------------------------------------
kruskal.test(sum_df$standrxn~sum_df$site)#to see if standardized reactions differ across sites

# Pairwise wilcox test
pairwise.wilcox.test(sum_df$standrxn,sum_df$site, p.adjust.method = "holm")

# correlations via pooling the data into hourly observations  --------------------------
all_corr <- sum_df %>% 
  replace_na(list(smoltdens=0))

#sum first two rows
all_corr <-rowsum(all_corr[,4:5], 
                      as.integer(gl(nrow(all_corr), 2, 
                                    nrow(all_corr)))) 

# Filter out zeros
# all_corr <- all_corr %>% 
#   filter(smoltdens != 0 )

cor.test(all_corr$standrxn, all_corr$smoltdens, 
         method = 'spearman', exact = FALSE)


#stratify this by the upstream sites
#upstream 2021
pooled_UF2021<-sum_df %>% 
  replace_na(list(smoltdens=0)) %>% 
  filter(site == 'firstnight2021')

#sum first two rows
pooled_UF2021<-rowsum(pooled_UF2021[,4:5], 
                      as.integer(gl(nrow(pooled_UF2021), 2, 
                                    nrow(pooled_UF2021))))

#get rid of zeros
# pooled_nonzeros_UF2021<-pooled_UF2021 %>% 
#   filter(smoltdens != 0)

#test for perasons UF2021
cor.test(pooled_UF2021$standrxn, pooled_UF2021$smoltdens, 
         method = 'spearman', exact = FALSE)



#upstream 272829
pooled_UF272829<-sum_df %>% 
  replace_na(list(smoltdens=0)) %>% 
  filter(site == 'upstream272829')

#sum first two rows
pooled_UF272829<-rowsum(pooled_UF272829[,4:5], 
                        as.integer(gl(nrow(pooled_UF272829), 2, 
                                      nrow(pooled_UF272829))))

# #get rid of zeros
# pooled_nonzeros_UF272829<-pooled_UF272829 %>% 
#   filter(smoltdens != 0)

#test for perasons UF272829
cor.test(pooled_UF272829$standrxn, pooled_UF272829$smoltdens, 
         method = 'spearman', exact = FALSE)

# dr 2122

dr2122<-sum_df %>% 
  replace_na(list(smoltdens=0)) %>% 
  filter(site == 'downstreamriver2122')

#sum first two rows
dr2122<-rowsum(dr2122[,4:5], 
                        as.integer(gl(nrow(dr2122), 2, 
                                      nrow(dr2122))))

cor.test(dr2122$standrxn, dr2122$smoltdens, 
         method = 'spearman', exact = FALSE)

# df 2324
df2324<-sum_df %>% 
  replace_na(list(smoltdens=0)) %>% 
  filter(site == 'downstreamfence2324')

#sum first two rows
df2324<-rowsum(df2324[,4:5], 
               as.integer(gl(nrow(df2324), 2, 
                             nrow(df2324))))

cor.test(df2324$standrxn, df2324$smoltdens, 
         method = 'spearman', exact = FALSE)


# n25
n2526<-sum_df %>% 
  replace_na(list(smoltdens=0)) %>% 
  filter(site == 'narrows2526')

#sum first two rows
n2526<-rowsum(n2526[,4:5], 
               as.integer(gl(nrow(n2526), 2, 
                             nrow(n2526))))

cor.test(n2526$standrxn, n2526$smoltdens, 
         method = 'spearman', exact = FALSE)


# n29
n29<-sum_df %>% 
  replace_na(list(smoltdens=0)) %>% 
  filter(site == 'narrows29')

#sum first two rows
n29<-rowsum(n29[,4:5], 
              as.integer(gl(nrow(n29), 2, 
                            nrow(n29))))

cor.test(n29$standrxn, n29$smoltdens, 
         method = 'spearman', exact = FALSE)


# Revision 1 - additional stats -------------------------------------------

# Look at whether UF sites had more interactions compared to others
sum_df <- sum_df %>% 
  mutate(uf_sites = ifelse(site %in% c("firstnight2021",
                                       "upstream272829"), "UF Sites", "Other Sites"))

sum_df %>% 
  group_by(uf_sites) %>% 
  summarize(count = n(),
            mean = mean(standrxn, na.rm = T),
            sd = sd(standrxn, na.rm = T))

wilcox.test(standrxn ~ uf_sites, data = sum_df)

ggplot(sum_df, aes(x = uf_sites, y = standrxn)) + 
  geom_point() # Make a figure of this potentially?
