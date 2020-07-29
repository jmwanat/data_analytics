# Jennifer Wanat, Assignment 2
# MSDS 455

# install and load needed packages
install.packages("ggplot2")
install.packages('plyr')
install.packages('tidyverse')

library(tidyverse)
library(plyr)
library(ggplot2)
library(RColorBrewer)

# set working directory
setwd("~/Desktop/R/")

##################################
# michelin star restaurants
##################################

# Data source
# https://www.kaggle.com/jackywang529/michelin-restaurants

# Notice:
#Currently, the data only contains restaurant info in the following regions: 
#Austria, California, Chicago, Croatia, Czech Republic, Denmark, Finland, Greece, 
#Hong Kong, Hungary, Iceland, Macau, Norway, New York City, Poland, Ireland, 
#Rio de Janeiro, Sao Paulo, South Korea, Singapore, Sweden, Taipei, Thailand, 
#Washington DC, and United Kingdom
#and explicitly not including these regions: 
#Belgium, France, Germany, Italy, Japan, Luxembourg, Netherlands, Portugal, 
#China, Spain, and Switzerland

# load data set for one star Michelin restaurants
# source data in csv file
one_star <- read.csv("one-star-michelin-restaurants.csv")
# examine the first five rows of the data set
head(one_star)
summary(one_star)

# create new dataframe looking at frequency counts of cuisine
# this data set was not used for the assignment
cuisine <- count(one_star, 'cuisine')
head(cuisine, 10)
# create new dataframe of top ten most frequent cuisines
ten_cuisine <- head(cuisine[order(-cuisine$freq),], n=10)



################### First Chart #####################
#bar chart of top ten regions (cities)

# create new dataframe looking at frequency counts of region (cities)
# this data set was used for the assignment
region <- count(one_star, 'region')
# create new dataframe of top ten most frequent regions (cities)
ten_region <- head(region[order(-region$freq),], n=10)

#preparing dataframe for use
colnames(ten_region)
str(ten_region)
# Changing data type of columns
ten_region$region <- as.character(ten_region$region)
# Checking result
str(ten_region)

#this is the bar color palette color-coding US locations
bar_palette <- c("#b8042f","#999999", "#999999", "#999999", 
                 "#999999", "#999999", "#999999", "#0c63c9", 
                 "#061e67", "#999999")

#this code uses the default font
ggplot(ten_region, aes(x=reorder(region, freq), y=freq)) +
  geom_bar(stat='identity', fill = bar_palette, color = bar_palette, alpha = 0.8) +
  labs(title="Number of 1 Star Michelin Restaurants by Location",
       subtitle="Top ten most frequent locations",
       caption = "Data source: www.kaggle.com
       Dataset does not include: Belgium, France, Germany, Italy, Japan,
       Luxembourg, Netherlands, Portugal, China, Spain, and Switzerland ") +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(face="bold", size=18),
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face="bold", size=16),
    axis.text.y = element_text(face="bold", size=16),
    legend.text = element_text(face="bold", size=12),
    legend.title = element_blank(),
    legend.position="bottom", legend.box = "horizontal")
ggsave("showtext-example-1.png", width = 10, height = 6, dpi=96)




#############Same chart with custom font Montserrat############
#read these links to learn more about showtext
#https://github.com/yixuan/showtext
#https://www.r-bloggers.com/adding-custom-fonts-to-ggplot-in-r/

# install and load needed packages
install.packages("showtext")
library(showtext)

# Find the font you want at: https://fonts.google.com/
font_add_google(name="Montserrat", family="montserrat")

#From the https://github.com/yixuan/showtext site:
#Known Issues
#showtext does not work well with the RStudio graphics device (RStudioGD). 
#Therefore, if you want to display graphs on a window device in RStudio, 
#you need to manually open one, e.g., x11() on Linux, windows() on Windows, and quartz() on Mac OS.

#But if you save to png, then you don't need to run quartz() or windows()


#you can optimize the png height, width, and resolution as you see fit
#the png settings below are for demo purposes
png("show2.png", 960, 960, res=96)
# turn on showtext
showtext_begin()
ggplot(ten_region, aes(x=reorder(region, freq), y=freq)) +
  geom_bar(stat='identity', fill = bar_palette, color = bar_palette, alpha = 0.8) +
  labs(title="Number of 1 Star Michelin Restaurants by Location",
       subtitle="Top ten most frequent locations",
       caption = "Data source: www.kaggle.com
       Dataset does not include: Belgium, France, Germany, Italy, Japan,
       Luxembourg, Netherlands, Portugal, China, Spain, and Switzerland ") +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(family="montserrat", face="bold", size=18),
    plot.subtitle = element_text(family="montserrat"),
    plot.caption = element_text(family="montserrat", hjust = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family="montserrat", face="bold", size=16),
    axis.text.y = element_text(family="montserrat", face="bold", size=16),
    legend.text = element_text(family="montserrat", face="bold", size=12),
    legend.title = element_blank(),
    legend.position="bottom", legend.box = "horizontal")
#turn off showtext
showtext_end()
dev.off()
