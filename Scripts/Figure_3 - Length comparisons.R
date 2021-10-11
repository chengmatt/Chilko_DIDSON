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
levels(pseudo$Sites)<-c("Downstream Fence (04-23 - 04-24)", 
                        "Downstream River (04-21 - 04-22)", 
                        "Narrows (04-25 - 04-26)", 
                        "Narrows (04-29)", 
                        "Upstream (04-20 - 04-21)", 
                        'Upstream Fence (04-27 - 04-29)')

# order site names by upstream, downstream, narrows
site1 <- factor(pseudo$Sites,
                levels = c('Upstream Fence (04-27 - 04-29)', 
                           "Upstream (04-20 - 04-21)",
                           "Downstream River (04-21 - 04-22)",
                           "Downstream Fence (04-23 - 04-24)",
                           "Narrows (04-25 - 04-26)",
                           "Narrows (04-29)"),
                ordered = TRUE)


# Plot figure 3 -----------------------------------------------------------

# first part of the plot (size distributions across sites)
(a<-ggplot(data=pseudo,aes(x = Sites, y= Size*100))+
   # basic plotting functions
  geom_jitter(alpha =0.3, width = 0.1)+
  geom_boxplot(width = 0.15, 
               position = position_nudge(x = -0.3), 
               outlier.alpha = 0, size = 1)+
 
   # adding horizontal lines to denote min and max of size distribution 
  geom_hline(yintercept = 41, color = 'black', 
             lty = 2, size = 1, alpha = 1)+
  geom_hline(yintercept = 79.5, color = 'black', 
             lty = 2, size = 1, alpha = 1)+
   # flip x and y axes and labeling
   coord_flip()+
  labs(x = "", y = "")+
   
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

# plot for didson measurements (pseudo replciated and subset)
(b<-ggplot(fieldod, aes(type, size))+
  # main plotting functions
  geom_jitter(alpha =0.3, width = 0.1)+
  geom_boxplot(width = 0.27, position = position_nudge(x = -0.33),
               outlier.alpha = 0, size = 1)+
    
  # adding horizontal lines to denote min and max
  geom_hline(yintercept = 41.1, color = 'black', 
             lty = 2, size = 1, alpha = 1)+
  geom_hline(yintercept = 79.5, color = 'black',
             lty = 2, size = 1, alpha = 1)+
    
    #flipping axes and labeling axes
    coord_flip()+
  labs(y = "", x = "")+
    
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

annotate_figure(plot3,bottom = text_grob("Size(cm)", 
                                         x = 0.63, y = 0.8, size = 15, face = 'plain'))

