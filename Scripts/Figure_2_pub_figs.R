#Creator: Matthew LH. Cheng
#Date updated: 10/8/21
#Purpose: To produce figures for Cheng et al. Chilko DIDSON studies


# load in data, packages, and set up working directories ------------------
library(tidyverse)
library(cowplot)
library(here)
library(scales)
#set up directory for data
dir.data<-here("Data")

#set up directory for figures
dir.fig <- here("Figures")

#load in datasets
#data for reaction plots (Figure 2.)
df <- read_csv(file.path(dir.data, "DIDSON_CompleteData.csv"))

#rename smoltdens to smoltdensity for plotting
df <- df %>% 
  rename(smoltdensity = smoltdens,
         Reactionpermsq = standrxn)


# Data cleaning/setting up ------------------------------------------------
#Set up time to plottable format
Date<-as.POSIXct(df$fixdate, tz = "US/Pacific")

df <-df %>% 
  mutate(Date)   

# filter by site to separate to respective dataframes ---------------------

#first night
first<-df %>% 
  filter(site == 'firstnight2021') 

#second
second<-df %>% 
  filter(site =='downstreamriver2122')

#third
third<-df %>% 
  filter(site == 'downstreamfence2324')

#fourth
fourth<-df %>% 
  filter(site == 'narrows2526') 

#fifth
fifth<-df %>% 
  filter(site == 'upstream272829') 

#sixth
sixth<-df %>% 
  filter(site == 'narrows29') 

# First night fence 2021 -- fig 2 -----------------------------------------

#set up times for shading - for geom rect
down1<-as.POSIXct("2016-04-20 20:30:00 US/Pacific")
up1<-as.POSIXct("2016-04-21 05:56:00 US/Pacific")

#set x axes label to specify location of alphabet labels
lab_x<-as.POSIXct("2016-04-21 12:00:00 US/Pacific")

(first1<-first %>% 
  #main plotting control
    ggplot()+
    geom_line(data=first[!is.na(first$smoltdensity/140000),],
              aes(x=Date, y=smoltdensity/140000, 
                  linetype="Smolt Density", color = 'Smolt Density'),
              size = 1.5, alpha = 0.65)+   #remove MISSING VALUES! for smolt density
  geom_line(aes(x = Date , y = Reactionpermsq, 
                linetype = "Total reactions per meter", color = 'Total reactions per meter'),
            size = 1.5, alpha = 0.65)+
  
    #controlling x and y scale axes so we can have 2 y axes 
    scale_color_manual(name = "",
                     values = c("Smolt Density" = 'orange', 
                                "Total reactions per meter" = 'blue'))+
    scale_y_continuous(name = "", 
                     sec.axis = sec_axis(~.*140000, name = " "))+
  scale_linetype_manual(name = "",
                        values = c("Smolt Density" = 1, 
                                   "Total reactions per meter" = 2))+
   
    #adding in annotations and axes labels and shading
    annotate("text", x = lab_x, 
             y = Inf, label  = 'A', vjust = 1.5, size = 8, fontface = 'bold',hjust = 1)+
  annotate("rect", xmin = down1, xmax = up1, 
           ymin= -Inf,  ymax = Inf, alpha = 0.3)+
  labs(title = 'UF2021', x = '',y="")+
   
     #fine-scale tinkering of theme
    theme_bw()+
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90, vjust = 0.5), 
        axis.text = element_text(size = 16, color = 'black'),
        title = element_text(size = 17, face = 'bold'), 
        axis.title.y = element_text(hjust = -4), 
        plot.margin = unit(c(0.25,0,-0.33,0),'cm'),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)))



# Downstream 2122 -- Figure 2 ---------------------------------------------
#set up times for shading
down2<-as.POSIXct("2016-04-21 20:30:00 US/Pacific")
downc2<-as.POSIXct("2016-04-22 05:56:00 US/Pacific")

#set x axes label to specify location of alphabet labels
lab_x<-as.POSIXct("2016-04-22 12:00:00 US/Pacific")


(second1<-second %>% 
  
  #main plotting function
  ggplot()+
    geom_line(data=second[!is.na(second$smoltdensity/500000),]
              ,aes(x=Date, y=smoltdensity/500000, linetype="Smolt Density", color = 'Smolt Density'),
              size = 1.5, alpha = 0.655)+   #remove MISSING VALUES! for smolt density
  geom_line(aes(x = Date , y = Reactionpermsq, 
                linetype = "Total reactions per meter", color = 'Total reactions per meter'),
            size = 1.5, alpha = 0.65)+
  #setting up scale for x and y axes so we can plot 2 y axes and dates as x axes
    scale_color_manual(name = "",
                     values = c("Smolt Density" = 'orange', 
                                "Total reactions per meter" = 'blue'))+
  scale_y_continuous(name = "", 
                     sec.axis = sec_axis(~.*500000, name = ""))+
  scale_linetype_manual(name = "",
                        values = c("Smolt Density" = 1, 
                                   "Total reactions per meter" = 2))+
  
  #annotations for labelling, axes, shading
  annotate("rect", xmin = down2, 
           xmax = downc2, ymin= -Inf,  ymax = Inf, alpha = 0.3)+
  labs(title = 'DR2122', x = '')+
  annotate("text", x = lab_x, 
           y = Inf, label  = 'B', vjust = 1.5, size = 8, fontface = 'bold',hjust = 0.5)+
  
  #tinkering with the theme
  theme_bw()+
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90, vjust = 0.5),  
        axis.text = element_text(size = 17, color = 'black'),
        title = element_text(size = 18, face = 'bold'), 
        plot.margin = unit(c(0.25,0,-0.33,0),'cm'),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)))


# Downstream 2324 - Fig 2 -------------------------------------------------
downc3<-as.POSIXct("2016-04-23 20:30:00 US/Pacific")
upc3<-as.POSIXct("2016-04-24 05:56:00 US/Pacific")

#set x axes label to specify location of alphabet labels
lab_x<-as.POSIXct("2016-04-24 10:15:00 US/Pacific")


(third1<-third %>% 
  
      #main plotting functions
  ggplot()+
    geom_line(data=third[!is.na(third$smoltdensity/140000),],
              aes(x=Date, y=smoltdensity/140000, linetype="Smolt Density", color = 'Smolt Density'),
              size = 1.5, alpha = 0.65)+   #remove MISSING VALUES! for smolt density
  geom_line(aes(x = Date , y = Reactionpermsq, 
                linetype = "Total reactions per meter", color = 'Total reactions per meter'),
            size = 1.5, alpha = 0.65)+
    #for allowing plots to have appropriate x and y scales w/ 2 y axes left and right
  scale_color_manual(name = "",
                     values = c("Smolt Density" = 'orange', 
                                "Total reactions per meter" = 'blue'))+
  scale_y_continuous(name ="",
                     sec.axis = sec_axis(~.*140000, name = ""))+
  scale_linetype_manual(name = bquote('Total reactions/'~m^2~''),
                        values = c("Smolt Density" = 1, 
                                   "Total reactions per meter" = 2))+
 
    #for annotating, shading, and axes labels
  annotate("rect", xmin = downc3, xmax = upc3,
           ymin= -Inf,  ymax = Inf, alpha = 0.3)+  labs(title = 'DF2324',x = '')+
  annotate("text", x = lab_x, 
           y = Inf, label  = 'C', vjust = 1.5, size = 8, fontface = 'bold',hjust = 1)+
    
    #fine-scale tinkering of the theme
    theme_bw()+
  theme(legend.position = 'none',
        plot.margin = unit(c(0.25,0,-0.33,0),'cm'),
        axis.text = element_text(size = 17, color = 'black'),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.y.right = element_text(size = 18, face = 'plain',hjust = -0.5, vjust = ),
        title = element_text(size = 18, face = 'bold' ),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)))


# Narrows 2526 -- Fig 2 --------------------------------------------------
#setting up time objects for shading
downd4<-as.POSIXct("2016-04-25 20:30:00 US/Pacific")
upd4<-as.POSIXct("2016-04-26 05:56:00 US/Pacific")

#set x axes label to specify location of alphabet labels
lab_x<-as.POSIXct("2016-04-26 15:00:00 US/Pacific")


(fourth1<-fourth %>% 
   
     #main plotting functions
  ggplot()+
    
  geom_line(data=fourth[!is.na(fourth$smoltdensity/1400000),]
            ,aes(x=Date, y=smoltdensity/1400000, linetype="Smolt Density", color = 'Smolt Density'),
            size = 1.5, alpha = 1)+   #remove MISSING VALUES! for smolt density
    #setting up relevant scales for x and y axes - so we can have 2 y axes
    
    geom_line(aes(x = Date , y = Reactionpermsq, 
                  linetype = "Total interactions per meter", color = 'Total interactions per meter'),
              size = 1.5, alpha = 1)+
    
     scale_color_manual(name = "",
                     values = c( "Total interactions per meter" = 'blue',
                       "Smolt Density" = 'orange'))+
    
  scale_y_continuous(name = bquote('Bull trout interactions /'~m^2~'(per 30-minutes)'),
                     sec.axis = sec_axis(~.*1400000, name = " "))+
    
  scale_linetype_manual(name = "",values = c("Smolt Density" = 1,
                                             "Total interactions per meter" = 2))+
    
    #for annotations and labels, and shading
    labs(title = 'N2526', x = '', y = bquote('Total interactions/'~m^2~'(per 30-minutes)'))+
    annotate("rect", xmin = downd4, xmax = upd4,
             ymin= -Inf,  ymax = Inf, alpha = 0.3)+
    annotate("text", x = lab_x, 
             y = Inf, label  = 'D', vjust = 1.5, size = 8, fontface = 'bold',hjust = 0.3)+
    
  #for fine scale tinkering of the themes
  theme_bw()+
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text = element_text(size = 17, color = 'black'), 
        title = element_text(size = 18, face = 'bold'), 
        axis.title.y = element_text(hjust = -0.4 , size = 18, vjust = -2), 
        plot.margin = unit(c(-.51,0,0,0),'cm'),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)))
#we get it approxiamtely in the middle with patchwork


# upstream272829 -- fig2 --------------------------------------------------
#time objects to specify shading areas
downa5<-as.POSIXct("2016-04-27 20:30:00 US/Pacific")
downa5_1<-as.POSIXct("2016-04-28 20:30:00 US/Pacific")
upa5<-as.POSIXct("2016-04-28 05:56:00 US/Pacific")
upa5_1<-as.POSIXct("2016-04-29 05:56:00 US/Pacific")

#set x axes label to specify location of alphabet labels
lab_x<-as.POSIXct("2016-04-29 12:00:00 US/Pacific")

(fifth1<-fifth %>% 
  
  #our main plotting functions

  ggplot()+
    geom_line(data=fifth[!is.na(fifth$smoltdensity/50000),],
              aes(x=Date, y=smoltdensity/50000, linetype="Smolt Density", color = 'Smolt Density'),
              size = 1.5, alpha = 0.65)+ 
  #remove MISSING VALUES! for smolt density
  geom_line(aes(x = Date , y = Reactionpermsq, linetype = "Bull trout interactions", color = 'Bull trout interactions'),
            size = 1.5, alpha = 0.65)+
  # controlling the scale for x and y axes so dates can plot normally and have 2 y axes
  scale_color_manual(name = "",
                     values = c("Smolt Density" = 'orange', 
                                "Bull trout interactions" = 'blue'))+
  scale_y_continuous(name = '', 
                     sec.axis = sec_axis(~.*50000, name = ""))+
  scale_linetype_manual(name = "",values = c("Smolt Density" = "solid", 
                                             "Bull trout interactions" = "twodash"))+
  
  #for specifying annotaitons,labels, and shading
  annotate("rect", xmin = downa5, xmax = upa5, 
           ymin= -Inf,  ymax = Inf, alpha = 0.3)+
  annotate("rect", xmin = downa5_1, xmax = upa5_1,
           ymin= -Inf,  ymax = Inf, alpha = 0.3)+
  annotate("text", x = lab_x, 
           y = Inf, label  = 'E', vjust = 1.5, size = 8, fontface = 'bold',hjust = 0.9)+
  labs(title = 'UF272829', x = 'Date & time')+

  #for fine-scale tinkering of the theme
  theme_bw()+
  theme(legend.position = 'bottom',legend.text = element_text(size = 15.3),
        legend.key.width = unit(0.9, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5),  
        axis.text = element_text(size = 17, color = 'black'),
        title = element_text(size = 18, face = 'bold'),
        axis.title.x = element_text(vjust = -1, size = 18, face = 'plain'), 
        plot.margin = unit(c(-.51,0,0,0),'cm'),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)))


# narrows 29 -- fig 2 ------------------------------------------------------
#no time objects for shading for narrows 29 because we don't have night time observations

#set x axes label to specify location of alphabet labels
lab_x<-as.POSIXct("2016-04-29 19:20:00 US/Pacific")


(sixth1<-sixth %>% 
    
#our main plotting functions
  ggplot()+
    geom_line(data=sixth[!is.na(sixth$smoltdensity/50000),],
              aes(x=Date, y=smoltdensity/50000, linetype="Smolt Density", color = 'Smolt Density'),
              size = 1.5, alpha = 0.65)+ 
  geom_line(aes(x = as.POSIXct(fixdate, tz = "US/Pacific") , y = Reactionpermsq, 
                linetype = "Total reactions per meter", color = 'Total reactions per meter'),
            size = 1.5, alpha = 0.65)+
    #for controlling scale so x y are appropriate scales
  scale_y_continuous(name ="",
                     sec.axis = sec_axis(~., name = "Smolt Density (per hour)"))+
  scale_linetype_manual(name = bquote('Total reactions/'~m^2~''),
                        values = c("Smolt Density" = 1, 
                                   "Total reactions per meter" = 2))+
  scale_color_manual(name = "",
                     values = c("Smolt Density" = 'orange', 
                                "Total reactions per meter" = 'blue'))+
    #for annotations - labels, axes, etc
    labs(title = 'N29', x='')+
    scale_x_datetime(labels = date_format("%b %d %H:%M", tz = "US/Pacific"))+   
    annotate("text", x = as.POSIXct("2016-04-29 18:00"), 
             y = Inf, label  = 'F', vjust = 1.3, size = 8, fontface = 'bold',hjust = 0.7)+
 
       #for fine scale tinkering of theme
  theme_bw()+
  theme(legend.position = 'none', 
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text = element_text(size = 17, angle = 0, color = 'black'),
        title = element_text(size = 17, face = 'bold'),
        axis.title.y.right = element_text(size = 18, hjust = -15, face = 'plain', vjust = -7),
        plot.margin = unit(c(-.51,0,0,0),'cm'),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank()))

# putting this all together (FIGURE 2) ------------------------------------

pdf(file = file.path(dir.fig, "MS figures", "fig3_rxns.pdf"), width = 18.5, height = 10)
plot_grid(first1, second1, third1, fourth1, fifth1, sixth1,  align = "hv", axis = 'b', scale = 1.033)
dev.off()
 



