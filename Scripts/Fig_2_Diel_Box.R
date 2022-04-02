# Creator: Matthew LH. Cheng
# Purpose: To create figure 2 for Chilko MS - boxplot of diel relationship


# Set up!
library(tidyverse)
library(cowplot)
library(here)
library(ggdist)
library(ggthemes)

source("theme_matt.R")

#set up directory for data
dir.data<-here("Data")

#set up directory for figures
dir.fig <- here("Figures")

#load in datasets
#data for reaction plots (Figure 2.)
df <- read_csv(file.path(dir.data, "DIDSON_CompleteData.csv")) %>% 
  filter(cycle != "NA")

# Renaming some varaibles
df[df$cycle == "Day", "cycle"] <- "Daytime"
df[df$cycle == "Night", "cycle"] <- "Nighttime"

# Plot! (Figure 2 - Chilko Boxplot diel)
pdf(file = file.path(dir.fig, "MS figures", "fig_2_box.pdf"), width = 5, height = 5)
ggplot(df, aes(x = cycle, y = standrxn))+
  geom_boxplot(width = 0.5, size = 0.8, alpha = 0.6)+
  geom_jitter(alpha = 0.55, width = 0.175)+
  annotate("text", x = 0.6, 
           y = Inf, label  = 'W = 2693.5', vjust = 1.5, size = 5, fontface = 'plain',hjust = 0.3)+
  annotate("text", x = 0.6, 
           y = 5.4, label  = 'P < 0.01', vjust = 1.5, size = 5, fontface = 'italic',hjust = 0.3)+
  labs(x = "Cycle", y = bquote('Total interactions/'~m^2~'(per 30-minutes)'))+
  theme_bw()+
  theme(axis.title = element_text(size = 17), axis.text = element_text(size = 15, color = "black"))
dev.off()


# Renaming some variables here
df[df$site == "downstreamfence2324", "site"] <- "DF2324"
df[df$site == "downstreamriver2122", "site"] <- "DR2122"
df[df$site == "firstnight2021", "site"] <- "UF2021"
df[df$site == "narrows2526", "site"] <- "N2526"
df[df$site == "upstream272829", "site"] <- "UF272829"
df[df$site == "narrows29", "site"] <- "N29"



# Across sites
ggplot(df %>% 
         mutate(site = factor(site,
                              levels = c("UF2021",
                                         "DR2122",
                                         "DF2324",
                                         "N2526",
                                         "UF272829",
                                         "N29"))), aes(x = cycle, y = standrxn, fill = cycle))+
  geom_boxplot(width = 0.5, size = 0.8, alpha = 0.6)+
  geom_jitter(alpha = 0.55, width = 0.175)+
  labs(x = "Cycle", y = bquote('Total interactions/'~m^2~'(per 30-minutes)'))+
  facet_wrap(~site, scales = "free")+
  scale_fill_manual(values = c("orange", "grey"))+
  theme_matt()+
  theme(axis.title = element_text(size = 17), 
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none")
