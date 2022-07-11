# Creator: Matthew LH. Cheng
# Purpose: To create figure 2 for Chilko MS - boxplot of diel relationship


# Set up!
library(tidyverse)
library(cowplot)
library(here)
library(ggdist)
library(ggthemes)
library(ggsci)

source("theme_matt.R")

#set up directory for data
dir.data<-here("Data")

#set up directory for figures
dir.fig <- here("Figures")

#load in datasets
#data for reaction plots (Figure 2.)
df <- read_csv(file.path(dir.data, "DIDSON_CompleteData.csv")) %>% 
  filter(cycle != "NA") %>% 
  
  # Addressing reviewer comments here by color coding data from
  # UF site vs all other sites
  mutate(UF_sites = ifelse(
    site %in% c("upstream272829", "upstream2021"),
    "UF", "Other"
  )) 

# Renaming some varaibles
df[df$cycle == "Day", "cycle"] <- "Daytime"
df[df$cycle == "Night", "cycle"] <- "Nighttime"

# Plot! (Figure 2 - Chilko Boxplot diel)
pdf(file = file.path(dir.fig, "MS figures", "fig2.pdf"), width = 5, height = 5)

ggplot(df %>% 
         mutate(UF_sites = 
                  factor(UF_sites, levels = c("UF", "Other"))),  # Re-leveling factors
       aes(x = UF_sites, y = standrxn))+
  geom_boxplot(width = 0.5, size = 0.8, alpha = 0.6, outlier.shape = NA)+
  geom_jitter(alpha = 0.45, width = 0.17, size = 2)+
  # annotate("text", x = 0.6, 
  #          y = Inf, label  = 'W = 2693.5', vjust = 1.5, size = 5, fontface = 'plain',hjust = 0.3)+
  # annotate("text", x = 0.6, 
  #          y = 5.4, label  = 'P < 0.01', vjust = 1.5, size = 5, fontface = 'italic',hjust = 0.3)+
  labs(x = "Cycle", y = bquote('Total interactions/'~m^2~'(per 30-minutes)'),
       color = "Cycle")+
  theme_bw()+ 
  # scale_fill_manual(values = c("orange", "black"))+
  # scale_color_manual(values = c("orange", "black"))+
  facet_wrap(~cycle)+
  theme(axis.title = element_text(size = 17), 
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "top",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text = element_text(size = 15))

dev.off()


# # Renaming some variables here
df[df$site == "downstreamfence2324", "site"] <- "DF2324"
df[df$site == "downstreamriver2122", "site"] <- "DR2122"
df[df$site == "firstnight2021", "site"] <- "UF2021"
df[df$site == "narrows2526", "site"] <- "N2526"
df[df$site == "upstream272829", "site"] <- "UF272829"
df[df$site == "narrows29", "site"] <- "N29"

# Sig for indv facet
text_df <- data.frame(site = c("UF2021",
                                               "DF2324",
                                               "UF272829",
                                               "DR2122",
                                               "N2526",
                                               "N29"),
                      sig = c("*", "*", "*", "*", "", ""),
                      x = c(1,1,1,1,1),
                      y = )

# pdf(file = file.path(dir.fig, "MS figures", "fig2.pdf"), width = 5, height = 5)
# 
# # Across sites
# ggplot(df %>%
#          mutate(site = factor(site,
#                               levels = c("UF2021",
#                                          "DF2324",
#                                          "UF272829",
#                                          "DR2122",
#                                          "N2526",
#                                          "N29"))), aes(x = cycle, y = standrxn, fill = cycle))+
#   geom_boxplot(width = 0.5, size = 0.8, alpha = 0.6)+
#   geom_jitter(alpha = 0.55, width = 0.175)+
#   # geom_text(data = text_df, mapping = aes(x = 2, y = 3, label = sig)) +
#   labs(x = "Cycle", y = bquote('Total interactions/'~m^2~'(per 30-minutes)'))+
#   facet_wrap(~site, scales = "free")+
#   scale_fill_manual(values = c("orange", "grey"))+
#   theme_bw()+
#   theme(axis.title = element_text(size = 17),
#         axis.text = element_text(size = 15, color = "black"),
#         legend.position = "none",
#         strip.text = element_text(size = 15))
# 
# dev.off()