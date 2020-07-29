

![first chart](Opioid_related_deaths_0.png)

![second chart](Opioid_related_deaths_1.png)

![third chart](Opioid_related_deaths_2.png)

![fourth chart](Opioid_related_deaths_3.png)






This is the code for the opioid related deaths. This was prepared in the programming language R.

```R
# Jennifer Wanat, Final Project
# Opioid related deaths for Cook County Medical Examiner
# MSDS 455

# install and load needed packages
install.packages('plyr')
install.packages('tidyverse')
install.packages('networkD3')
install.packages('dplyr')
install.packages('caret')
install.packages('ggmap')
install.packages('lubridate')
install.packages('RColorBrewer')
install.packages('gridExtra')


library(tidyverse)
library(plyr)
library(networkD3)
library(dplyr)
library(caret)
library(ggmap)
library(RColorBrewer)
library(gridExtra)
library(lubridate)

# set working directory
setwd("~/Desktop/R/")

##################################
# Medical Examiner Case Archive
##################################

# Data source
# https://datacatalog.cookcountyil.gov/Public-Safety/Medical-Examiner-Case-Archive/cjeq-bs86

# load data set for Cook County Medical Examiner
# source data in csv file
# if there are any blank entries, then replace with NA
me_cases <- read.csv("Medical_Examiner_Case_Archive.csv", header=T, na.strings=c("","NA"))
# examine the first five rows of the data set
head(me_cases)
summary(me_cases)
str(me_cases)


################ Preparing the data ####################

#convert date of death column from string to date
me_cases$FixedDate <- as.Date(me_cases$Date.of.Death,format="%m/%d/%Y")


# check frequency counts of opioid related deaths
opioid_count = count(me_cases, 'Opioid.Related')
opioid_count

#output
#Opioid.Related  freq
#1          false 26791
#2           true  5078


# create new dataframe of me_cases by gender, opioid.related, longitude, latitude
gender_opioid <- ddply(me_cases, .(me_cases$Gender, me_cases$Opioid.Related, me_cases$longitude, me_cases$latitude), nrow)
# check results
summary(gender_opioid)
str(gender_opioid)

#renaming the columns in the dataframe
names(gender_opioid) <- c('Gender', 'Opioid', 'longitude', 'latitude', 'value')


#let's look at only opioid deaths equal to true
#this dataframe will be used to create the map
# create new dataframe 
gender_opioid_only <- subset(gender_opioid, Opioid %in% c('true'))
# removing the NA's from the dataframe
gender_opioid_only <- na.omit(gender_opioid_only)
# checking the results
summary(gender_opioid_only)



# create new dataframe of me_cases by date, gender, opioid.related, longitude, latitude
gender_opioid_date <- ddply(me_cases, .(me_cases$FixedDate, me_cases$Gender, me_cases$Opioid.Related, 
                                        me_cases$longitude, me_cases$latitude), nrow)
# check results
summary(gender_opioid_date)
str(gender_opioid_date)

#renaming the columns in the dataframe
names(gender_opioid_date) <- c('Date', 'Gender', 'Opioid', 'longitude', 'latitude', 'value')
str(gender_opioid_date)


#let's look at only opioid deaths equal to true
# create new dataframe 
gender_opioid_date <- subset(gender_opioid_date, Opioid %in% c('true'))
# removing the NA's from the dataframe
gender_opioid_date <- na.omit(gender_opioid_date)


################## Preparing the map #####################


#https://statisticaloddsandends.wordpress.com/2018/10/25/getting-started-stamen-maps-with-ggmap/
#set map boundaries based on data
height <- max(gender_opioid_only$latitude) - min(gender_opioid_only$latitude)
width <- max(gender_opioid_only$longitude) - min(gender_opioid_only$longitude)
chi_borders <- c(bottom  = min(gender_opioid_only$latitude)  - 0.1 * height,
                 top     = max(gender_opioid_only$latitude)  + 0.1 * height,
                 left    = min(gender_opioid_only$longitude) - 0.1 * width,
                 right   = max(gender_opioid_only$longitude) + 0.1 * width)


#create basic map with specified type
map <- get_stamenmap(chi_borders, zoom = 10, maptype = "toner-lines") 

#this shows the map
ggmap(map)


################# 2018 #######################

#https://stackoverflow.com/questions/28335715/r-how-to-filter-subset-a-sequence-of-dates
#filtering opioid related deaths by date
#looking for 2018
gender_opioid_date2018 <- filter(gender_opioid_date, between(Date, as.Date("2018-01-01"), as.Date("2018-12-31")))


#create map with 2018 data points
map2018 <- ggmap(map) + 
  geom_point(data = gender_opioid_date2018, mapping = aes(x = longitude, y = latitude,
                                                          col = Gender), alpha=0.5) +
  labs(title="2018") +
#       subtitle="2018",
#       caption = "Data source: Cook County Medical Examiner") +
  scale_color_manual(values=c("#f95d6a", "#2f4b7c")) +
  theme(
    plot.title = element_text(face="bold", size=20),
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(face="bold", size=12),
    legend.title = element_blank(),
    legend.position= "none")
#    legend.position="bottom", legend.box = "horizontal")

#show map with 2018 data points
map2018


#########################################
#stacked bar chart of male v female by year

#https://community.rstudio.com/t/converting-dates-year-month-day-to-3-separate-columns-year-month-day/8585
gender_opioid_date <- gender_opioid_date %>%
  dplyr::mutate(Year = lubridate::year(Date), 
                Month = lubridate::month(Date), 
                Day = lubridate::day(Date))

# create new dataframe of frequency count of gender by year for opioid related deaths
gender_counts <- ddply(gender_opioid_date, .(gender_opioid_date$Year, gender_opioid_date$Gender), nrow)

# rename columns in dataframe
names(gender_counts) <- c('Year', 'Gender', 'freq')

#remove 2019 data since this is incomplete
gender_counts <- gender_counts[!(gender_counts$Year=="2019"),]


#this is to assist in labeling every year in the subsequent stacked bar chart
ii <- 2014:2018

#stacked bar chart
ggplot(gender_counts, aes(fill=Gender, y=freq, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#f95d6a", "#2f4b7c"), name="region") +
  scale_color_manual(values=c("#f95d6a", "#2f4b7c"), name="region") +
  scale_x_continuous(breaks=ii) +
  labs(title="Number of Opioid Related Deaths by Gender",
       subtitle = "Cook County Medical Examiner",
       caption = "Data source: Cook County Open Data") +
  theme_classic() +
  theme(
    plot.title = element_text(face="bold", size=18),
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face="bold", size=16),
    axis.text.y = element_text(face="bold", size=16),
    legend.text = element_text(face="bold", size=12),
    legend.title = element_blank())



