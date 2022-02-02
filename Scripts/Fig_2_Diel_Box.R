# Creator: Matthew LH. Cheng
# Purpose: To create figure 2 for Chilko MS - boxplot of diel relationship


# Set up!
library(tidyverse)
library(cowplot)
library(here)
library(ggdist)

#set up directory for data
dir.data<-here("Data")

#set up directory for figures
dir.fig <- here("Figures")

#load in datasets
#data for reaction plots (Figure 2.)
df <- read_csv(file.path(dir.data, "DIDSON_CompleteData.csv"))

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
  labs(x = "Cycle", y = bquote('Total reactions/'~m^2~'(per 30-minutes)'))+
  theme_bw()+
  theme(axis.title = element_text(size = 17), axis.text = element_text(size = 15, color = "black"))
dev.off()
