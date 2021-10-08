#Creator: Matthew LH. Cheng
#Date updated: 10/7/21
#Purpose: To explore count data of bull trout DIDSON MS

# load data and tidyverse -------------------------------------------------
library(here)
library(tidyverse)
library(lubridate)

#set up working directory
getwd()

dir.data <- here("Data") #filepath to data

dir.fig<- here("Figures") #filepath to figures

data <- read_csv(file = file.path(dir.data, "DIDSON_CompleteData.csv"))

# clean/setup data and dates ----------------------------------------------
data$fixdate
#setup dates/times
data<-data %>% 
  rename(upstream = Upstream,
         downstream = Downstream,
         milling = Milling,
         total = Total,
         area = Area) %>% 
    mutate(date=str_sub(fixdate,1,16)) %>% 
  dplyr::select(-fixdate)
  
data$date<-force_tz(ymd_hm(data$date),"America/New_York") #forcing to EDT time zone


# select count data and standardize ---------------------------------------
stand_count<-data %>% 
  mutate(standupstream = upstream/area, 
         standdownstream = downstream/area, 
         standmilling = milling/area,
         standtotalrxns = totalrxns/area) %>% 
  dplyr::select(standupstream, standdownstream, 
                standmilling, site, date)


#pivot standardized data long to wide to plot
stand_count_L<-stand_count %>% 
  pivot_longer(!date & !site , names_to = 'direction', values_to = 'count')


# plotting standardized counts and reactions ------------------------------

#first night 2021
a<-data %>% 
  filter(site == 'firstnight2021') %>% 
  ggplot(.)+
  geom_line(aes(x = date, y = total/area, color = 'standcounts'),size = 1.5, alpha = 0.8)+  
  geom_line(aes(x=date, y=totalrxns/area*30, color = 'standrxn'),size = 1.5, alpha = 0.8)+  
  scale_y_continuous(name = "standcounts", 
                     sec.axis = sec_axis(~./30, name = 'standrxns'))+
    scale_color_manual(name = "",
                       values = c("standcounts" = "dodgerblue", "standrxn" = "orange"))+
        annotate("rect", xmin = as.POSIXct("2016-04-20 20:30:00 EDT"),
                 xmax = as.POSIXct("2016-04-21 05:56:00 EDT"), 
                 ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'firstnight2021')+
  theme_bw()

b<-stand_count_L %>% 
  filter(site == 'firstnight2021') %>% 
  ggplot(.,aes(x = date, y = count, fill = direction))+
geom_bar(stat = 'identity', position = 'stack')+
  annotate("rect", xmin = as.POSIXct("2016-04-20 20:30:00 EDT"), 
           xmax = as.POSIXct("2016-04-21 05:56:00 EDT"), 
           ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'firstnight2021')+
  theme_bw()

firsnight_plots <- a / b

#downstreamriver 2122 plots
c<-data %>% 
  filter(site == 'downstreamriver2122') %>% 
  ggplot(.)+
  geom_line(aes(x = date, y = total/area, color = 'standcounts'),size = 1.5, alpha = 0.8)+  
  geom_line(aes(x=date, y=totalrxns/area*15, color = 'standrxn'),size = 1.5, alpha = 0.8)+  
  scale_y_continuous(name = "standcounts", 
                     sec.axis = sec_axis(~./15, name = 'standrxns'))+
    scale_color_manual(name = "",
                       values = c("standcounts" = "dodgerblue", "standrxn" = "orange"))+
  annotate("rect", xmin = as.POSIXct("2016-04-21 20:30:00 EDT"),
           xmax = as.POSIXct("2016-04-22 05:56:00 EDT"), 
           ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'downstreamriver2122')+
  theme_bw()

d<-stand_count_L %>% 
  filter(site == 'downstreamriver2122') %>% 
  ggplot(.,aes(x = date, y = count, fill = direction))+
geom_bar(stat = 'identity', position = 'stack')+
  annotate("rect", xmin = as.POSIXct("2016-04-21 20:30:00 EDT"), 
           xmax = as.POSIXct("2016-04-22 05:56:00 EDT"), 
           ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'downstreamriver2122')+
  theme_bw()

downstreamriver_plots <- c / d

#downstreamfence2324
e<-data %>% 
  filter(site == 'downstreamfence2324') %>% 
  ggplot(.)+
  geom_line(aes(x = date, y = total/area, color = 'standcounts'),size = 1.5, alpha = 0.8)+  
  geom_line(aes(x=date, y=totalrxns/area*15, color = 'standrxn'),size = 1.5, alpha = 0.8)+  
  scale_y_continuous(name = "standcounts", 
                     sec.axis = sec_axis(~./15, name = 'standrxns'))+
    scale_color_manual(name = "",
                       values = c("standcounts" = "dodgerblue", "standrxn" = "orange"))+
  annotate("rect", xmin =as.POSIXct("2016-04-23 20:30:00 EDT"), 
           xmax = as.POSIXct("2016-04-24 05:56:00 EDT"),
           ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'downstreamfence2324')+
  theme_bw()

f<-stand_count_L %>% 
  filter(site == 'downstreamfence2324') %>% 
  ggplot(.,aes(x = date, y = count, fill = direction))+
geom_bar(stat = 'identity', position = 'stack')+
  annotate("rect", xmin =as.POSIXct("2016-04-23 20:30:00 EDT"), 
           xmax = as.POSIXct("2016-04-24 05:56:00 EDT"), 
           ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'downstreamfence2324')+
  theme_bw()

downstreamfence_plots <- e / f

#narrows 2526
g<-data %>% 
  filter(site == 'narrows2526') %>% 
  ggplot(.)+
  geom_line(aes(x = date, y = total/area, color = 'standcounts'),size = 1.5, alpha = 0.8)+  
  geom_line(aes(x=date, y=totalrxns/area*15, color = 'standrxn'),size = 1.5, alpha = 0.8)+  
  scale_y_continuous(name = "standcounts", 
                     sec.axis = sec_axis(~./15, name = 'standrxns'))+
    scale_color_manual(name = "",
                       values = c("standcounts" = "dodgerblue", "standrxn" = "orange"))+
  annotate("rect", xmin =as.POSIXct("2016-04-25 20:30:00 EDT"), 
           xmax = as.POSIXct("2016-04-26 05:56:00 EDT"), 
           ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'narrows2526')+
  theme_bw()

h<-stand_count_L %>% 
  filter(site == 'narrows2526') %>% 
  ggplot(.,aes(x = date, y = count, fill = direction))+
geom_bar(stat = 'identity', position = 'stack')+
  annotate("rect", xmin =as.POSIXct("2016-04-25 20:30:00 EDT"),
           xmax = as.POSIXct("2016-04-26 05:56:00 EDT"), 
           ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'narrows2526')+
  theme_bw()

narrows2526_plots <- g / h

#upstream272829
i<-data %>% 
  filter(site == 'upstream272829') %>% 
  ggplot(.)+
  geom_line(aes(x = date, y = upstream/area, color = 'standcounts'),size = 1.5, alpha = 0.8)+  
  geom_line(aes(x=date, y=totalrxns/area*5, color = 'standrxn'),size = 1.5, alpha = 0.8)+  
  scale_y_continuous(name = "standcounts", 
                     sec.axis = sec_axis(~./5, name = 'standrxns'))+
    scale_color_manual(name = "",
                       values = c("standcounts" = "dodgerblue", "standrxn" = "orange"))+
  annotate("rect", xmin =as.POSIXct("2016-04-27 20:30:00 EDT"), 
           xmax = as.POSIXct("2016-04-28 05:56:00 EDT"),
           ymin= -Inf,  ymax = Inf, alpha = 0.25)+
    annotate("rect", xmin =as.POSIXct("2016-04-28 20:30:00 EDT"),
             xmax = as.POSIXct("2016-04-29 05:56:00 EDT"), 
             ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'upstream272829')+
  theme_bw()


j<-stand_count_L %>% 
  filter(site == 'upstream272829') %>% 
  ggplot(.,aes(x = date, y = count, fill = direction))+
geom_bar(stat = 'identity', position = 'stack')+
  annotate("rect", xmin =as.POSIXct("2016-04-27 20:30:00 EDT"), 
           xmax = as.POSIXct("2016-04-28 05:56:00 EDT"), 
           ymin= -Inf,  ymax = Inf, alpha = 0.25)+
    annotate("rect", xmin =as.POSIXct("2016-04-28 20:30:00 EDT"), 
             xmax = as.POSIXct("2016-04-29 05:56:00 EDT"), 
             ymin= -Inf,  ymax = Inf, alpha = 0.25)+
  labs(title = 'upstream272829')+
  theme_bw()

upstream272829_plots <- i / j

#narrows 29
k<-data %>% 
  filter(site == 'narrows29') %>% 
  ggplot(.)+
  geom_line(aes(x = date, y = total/area, color = 'standcounts'),size = 1.5, alpha = 0.8)+  
  geom_line(aes(x=date, y=totalrxns/area*3, color = 'standrxn'),size = 1.5, alpha = 0.8)+  
  scale_y_continuous(name = "standcounts", 
                     sec.axis = sec_axis(~./3, name = 'standrxns'))+
    scale_color_manual(name = "",
                       values = c("standcounts" = "dodgerblue", "standrxn" = "orange"))+
  labs(title = 'narrows29')+
  theme_bw()

l<-stand_count_L %>% 
  filter(site == 'narrows29') %>% 
  ggplot(.,aes(x = date, y = count, fill = direction))+
geom_bar(stat = 'identity', position = 'stack')+
  labs(title = 'narrows29')+
  theme_bw()

narrows29_plots <- k / l


#save plots
firsnight_plots <- a / b
downstreamriver_plots <- c / d
downstreamfence_plots <- e / f
narrows2526_plots <- g / h
upstream272829_plots <- i / j
narrows29_plots <- k / l

#saving these figures
ggsave(path = dir.fig, "Exploratory plots", filename = "firstnight_plots.png",firsnight_plots)
ggsave(path = dir.fig, "Exploratory plots", filename = "downstreamriver_plots.png",downstreamriver_plots)
ggsave(path = dir.fig, "Exploratory plots", filename = "downstreamfence_plots.png",downstreamfence_plots)
ggsave(path = dir.fig, "Exploratory plots", filename = "narrows2526_plots.png",narrows2526_plots)
ggsave(path = dir.fig, "Exploratory plots", filename = "upstream272829_plots.png",upstream272829_plots)
ggsave(path = dir.fig, "Exploratory plots", filename = "narrows29_plots.png",narrows29_plots)

# plotting standardized counts and reactions ------------------------------
#summarize results_count
summarized_data<-data %>% 
  group_by(site) %>% 
  summarize(meanUpstream = mean(upstream/area),
            meanDownstream = mean(downstream/area),
            meanTotal = mean(total/area),
            sdUpstream = sd(upstream/area),
            sdDownstream = sd(downstream/area),
            sdTotal = sd(total/area))

upstream<-summarized_data %>% 
  ggplot(.,aes(x = site, y = meanUpstream, fill = site))+
  geom_bar(stat = 'identity')+
      geom_errorbar(aes(ymin = meanUpstream - sdUpstream, ymax = meanUpstream+sdUpstream),size = 0.5)+
  labs(title = 'Upstream', x = '')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 13), legend.position = 'none')

downstream<-summarized_data %>% 
  ggplot(.,aes(x = site, y = meanDownstream, fill = site))+
  geom_bar(stat = 'identity')+
      geom_errorbar(aes(ymin = meanDownstream - sdDownstream, ymax = meanDownstream+sdDownstream),size = 0.5)+
    labs(title = 'Downstream')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 13), legend.position = 'none')

total<-summarized_data %>% 
  ggplot(.,aes(x = site, y = meanTotal, fill = site))+
  geom_bar(stat = 'identity')+
      geom_errorbar(aes(ymin = meanTotal - sdTotal, ymax = meanTotal+sdTotal),size = 0.5)+
    labs(title = 'Total', x = '')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 13), legend.position = 'none')

count_plots<-upstream | downstream | total

count_plots+
  plot_annotation(tag_levels = 'A')&
    theme(plot.tag = element_text(face = 'bold', size = 17))


# Exploratory plots 5/18/21 (minimum fish and correlate with total counts)  -------------- 
# we can also do absolute difference plotted against standardized reactions

#set up data to get aboslute difference
diff<-data %>% 
  mutate(abs_diff = abs(upstream - downstream)/area)

#plot standrxns as a function of absolute difference
diff %>% 
  ggplot(.,aes(x = abs_diff, y = standrxn, color = cycle))+
  geom_point()+
  labs(x = bquote('Absolute difference (Upstream - Downstream)/'~m^2~'') ,
       y = bquote('Reactions/'~m^2~''))+
  theme_bw()

#plot correlations between absolute difference and total 
diff %>% 
  ggplot(.,aes(x = total, y = abs_diff, color = cycle))+
  geom_point()+
  labs(x = 'Total Fish', y = bquote('Absolute difference (Upstream - Downstream)/'~m^2~''))+
  theme_bw()


