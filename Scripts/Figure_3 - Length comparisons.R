#Creator: Matthew LH. Cheng
#Date updated: 10/8/21
#Purpose: To produce figure 3 (length comparisons) for Cheng et al. Chilko DIDSON studies


# load in data, packages, and set up working directories ------------------
library(tidyverse)
library(cowplot)
library(here)
library(ggpubr)


# set up directory for data
dir.data<-here("Data")

# set up directory for figures
dir.fig <- here("Figures")

# load in datasets

# data for field vs. didson comparisons
fieldod <- read_csv(file.path(dir.data, "Field_DIDSON_comp.csv"))

# load in data for site didson length comparisons (subsetted)
pseudo <- read_csv(file.path(dir.data, "Long_wide_pseudo.csv"))


# Set up data frames -------------------------------------------------------------

# make site names levels and order
levels(pseudo$Sites)<-c("DF2324", 
                        "DR2122", 
                        "N2526", 
                        "N29", 
                        "UF2021", 
                        'UF272829')

# order site names by upstream, downstream, narrows
site1 <- factor(pseudo$Sites,
                levels = c("DF2324", 
                           "DR2122", 
                           "N2526", 
                           "N29", 
                           "UF2021", 
                           'UF272829'),
                ordered = TRUE)

# Rename site names
pseudo$Sites[pseudo$Sites == "UpstreamFence272829"] <- "UF272829"
pseudo$Sites[pseudo$Sites == "UpstreamFence2021"] <- "UF2021"
pseudo$Sites[pseudo$Sites == "NarrowsOppositeCamp29"] <- "N29"
pseudo$Sites[pseudo$Sites == "NarrowsHalfCamp2526"] <- "N2526"
pseudo$Sites[pseudo$Sites == "DownStreamRiver2122"] <- "DR2122"
pseudo$Sites[pseudo$Sites == "DownstreamFence2324"] <- "DF2324"


# Plot figure 3 -----------------------------------------------------------

# first part of the plot (size distributions across sites)
(a<-ggplot(data=pseudo,aes(x = Sites, y= Size*100))+
   # basic plotting functions
   geom_jitter(alpha = 0.2, width = 0.15)+
   geom_boxplot(width = 0.27, position = position_nudge(x = -0.33),
                outlier.alpha = 0, size = 1, alpha = 0.8)+
 
   # adding horizontal lines to denote min and max of size distribution 
  geom_hline(yintercept = 41, color = 'black', 
             lty = 2, size = 1, alpha = 1)+
  geom_hline(yintercept = 79.5, color = 'black', 
             lty = 2, size = 1, alpha = 1)+
   # flip x and y axes and labeling
   coord_flip()+
  labs(x = "", y = "")+
   ylim(20, 90)+
   
   # fine scale tinkering w/ the theme
   theme_classic()+
  theme(panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white",colour = NA),
    plot.background = element_rect(fill = "white",colour = NA),
    axis.text.x = element_blank(), 
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(colour = "black", fill =NA, size = 1),
    axis.ticks.length.y.right  =unit(-0.2, "cm"), 
    axis.ticks.length.x = unit(-0.2,"cm"), 
    axis.text.y.left   = element_text(size = 17, color = 'black'),
    axis.title.x =element_text(size = 13, color = 'black'),
    legend.position = "none", 
    axis.text = element_text(color = 'black', size = 17)))

# Renaming observations
fieldod[fieldod$type == "DIDSON", 1] <- "Unfiltered DIDSON measurements"
fieldod[fieldod$type == "Sub", 1] <- "Filtered DIDSON measurements"
fieldod[fieldod$type == "TL", 1] <- "Field measurements"


# plot for didson measurements (pseudo replciated and subset)
(b<-ggplot(fieldod, aes(type, size))+
  # main plotting functions
  geom_jitter(alpha = 0.2, width = 0.12)+
  geom_boxplot(width = 0.27, position = position_nudge(x = -0.38),
               outlier.alpha = 0, size = 1, alpha = 0.8)+
    
  # adding horizontal lines to denote min and max
  geom_hline(yintercept = 41.1, color = 'black', 
             lty = 2, size = 1, alpha = 1)+
  geom_hline(yintercept = 79.5, color = 'black',
             lty = 2, size = 1, alpha = 1)+
    
    #flipping axes and labeling axes
    coord_flip()+
  labs(y = "", x = "")+
    ylim(20, 90)+
    
    # fine scale tinkering w/ the theme
    theme_minimal()+
  theme(plot.margin=unit(c(-0.71,5,-0.5,1), "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA,colour = NA),
        plot.background = element_rect(fill = "white",colour = NA), 
        axis.text.x = element_text(color = "black"), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        panel.border = element_rect(colour = "black", fill =NA, size = 1),
        axis.ticks.length.y.right  =unit(-0.2, "cm"),
        axis.ticks.length.x = unit(0.2,"cm"), 
        axis.text.y.left   = element_text(size = 17),
        axis.title.x =element_text(size = 17), 
        axis.text.x.bottom = element_text(size = 17), 
        legend.position = 'none', 
        axis.text= element_text(color = 'black', size = 17)))


# Combine these plots -----------------------------------------------------

#combine plots
plot3<-plot_grid(a, b, align = "v", 
                 nrow = 2, rel_heights = c(8, 2.5))

pdf(file = file.path(dir.fig, "MS figures", "Fig3.pdf"), width = 13, height = 8)
annotate_figure(plot3,bottom = text_grob("Total length (cm)", 
                                         x = 0.54, y = 0.8, size = 18, face = 'plain'))
dev.off()

