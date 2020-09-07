# Jennifer Wanat, Capstone
# MSDS 498
# Medicare claims data set

# install and load needed packages
install.packages('ggplot2')
install.packages('plyr')
install.packages('tidyverse')
install.packages('gridExtra')
install.packages('psych')
install.packages('ggmap')
install.packages('showtext')
install.packages('ggpubr')


library(tidyverse)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(psych)
library(ggmap)
library(dplyr)
library(showtext)
library(ggpubr)

# set working directory
setwd("~/Desktop/R/")


####################################################################

# load data set 
# source data in csv file
# if there are any blank entries, then replace with NA
claims <- read.csv("Medicare_Claims_data.csv", header=T, na.strings=c("","NA"))

# examine the first six rows of the data set
head(claims)

# provide summary descriptive statistics of num data by column
# mean,median,25th and 75th quartiles,min,max
# frequency counts for other columns, up to six rows
# this will also tell you the number of NA's in each column
summary(claims)


# examine the datatype of the columns in the dataframe
str(claims)

####################################################################

# Preparing the data set for use
# Renaming the data columns
#NOT NEEDED, DID NOT CUSTOMIZE FOR THIS DATASET
#names(claims) <- c('CMS_Number', 'Facility_Name', 'Address_1', 'Address_2', 'City',
#                           'State', 'Zip_Code', 'County', 'Phone_Number', 'CMS_Region', 'Condition',
#                           'Count', 'Footnote', 'Location')

#checking result
#str(claims)

# Changing data type of columns
# DIDN"T THINK THIS WAS NECESSARY
# THE DATA IS IN GOOD SHAPE SINCE IT IS ALREADY AGGREGATED
# DID NOT CUSTOMIZE FOR THIS DATASET
#claims$Year <- as.numeric(claims$Year)
#claims$LocationAbbr <- as.character(claims$LocationAbbr)
#claims$LocationDesc <- as.character(claims$LocationDesc)
#claims$DataSource <- as.character(claims$DataSource)
#claims$PriorityArea1 <- as.character(claims$PriorityArea1)
#claims$PriorityArea2 <- as.character(claims$PriorityArea2)
#claims$PriorityArea3 <- as.character(claims$PriorityArea3)
#claims$PriorityArea4 <- as.character(claims$PriorityArea4)
#claims$City <- as.character(claims$City)
#claims$State <- as.character(claims$State)
#claims$Zip_Code <- as.numeric(claims$Zip_Code)
#claims$County <- as.character(claims$County)
#claims$Phone_Number <- as.charcter(claims$Phone_Number)
#claims$CMS_Region <- as.factor(claims$CMS_Region)
#claims$Condition <-  as.character(claims$Condition)
#claims$Count <- as.numeric(claims$Count)
#claims$Footnote <- as.numeric(claims$Footnote)
#claims$Location <- as.character(claims$Location)
# Checking result
#str(claims)


#This is useful for variables that are numerical and continuous
#table of n, mean, sd, median, trimmed, mad, min, max, range, skew, kurtosis, se
#from psych package
#not so useful here, but the code is here if you need it
claims_table <- (round(describe(claims), 1))


#create a png of table
#saved to same location where the cvs file is located
#https://stackoverflow.com/questions/23365096/r-save-table-as-image
#edit the height and width as necessary
png('t1.png', height = 25*nrow(claims_table), width = 60*ncol(claims_table))
t1 <- tableGrob(claims_table)
grid.arrange(t1)
dev.off()

#DID NOT USE THIS
#function to describe variables with missing data 
#myNA.variable.summary <- function(variable){
#  NAsum.summary <- sum(is.na(variable))
#  NAmean.summary <- (round(mean(is.na(variable)), 2)*100)
#  return(c(NAsum.summary, NAmean.summary))
#}

#table of variables with missing data
#NA.CMS <- myNA.variable.summary(rehab_facility$CMS_Number)
#NA.Facility <- myNA.variable.summary(rehab_facility$Facility_Name)
#NA.Add1 <- myNA.variable.summary(rehab_facility$Address_1)
#NA.Add2 <- myNA.variable.summary(rehab_facility$Address_2)
#NA.City <- myNA.variable.summary(rehab_facility$City)
#NA.State <- myNA.variable.summary(rehab_facility$State)
#NA.Zip <- myNA.variable.summary(rehab_facility$Zip_Code)
#NA.County <- myNA.variable.summary(rehab_facility$County)
#NA.Phone <- myNA.variable.summary(rehab_facility$Phone_Number)
#NA.Region <- myNA.variable.summary(rehab_facility$CMS_Region)
#NA.Condition <- myNA.variable.summary(rehab_facility$Condition)
#NA.Count <- myNA.variable.summary(rehab_facility$Count)
#NA.Footnote <- myNA.variable.summary(rehab_facility$Footnote)
#NA.Location <- myNA.variable.summary(rehab_facility$Location)

# table of NA counts for variables
#overview.NA <- rbind(NA.CMS, NA.Facility, NA.Add1, NA.Add2, NA.City, NA.State,
#                     NA.Zip, NA.County, NA.Phone, NA.Region, NA.Condition,
#                     NA.Count, NA.Footnote, NA.Location)
#colnames(overview.NA) <- c('Number of missing values', 'Percent of missing values')
#rownames(overview.NA) <- c('CMS_Number', 'Facility_Name', 'Address_1', 'Address_2', 'City',
#                           'State', 'Zip_Code', 'County', 'Phone_Number', 'CMS_Region', 'Condition',
#                           'Count', 'Footnote', 'Location')

#create a png of table
#saved to same location where the cvs file is located
#https://stackoverflow.com/questions/23365096/r-save-table-as-image
#png('t2.png')
#t2 <- tableGrob(overview.NA)
#grid.arrange(t2)
#dev.off()


#when you are finished with your work you can 
#write dataframe to new csv file
#write.csv(rehab_facility,'rehab_facility.csv')


###################################
#working on creating lat and long for data

#https://stackoverflow.com/questions/41551719/spliting-the-location-column-into-zipcode-latitude-and-longitude
claims$latitude <- as.numeric(gsub('.*\\((.*),.*', '\\1', claims$GeoLocation))
claims$longitude <- as.numeric(gsub('.*, (.*)\\).*', '\\1', claims$GeoLocation))

#check result
str(claims)


###################################
#Creating new dataframes for possible use based on various specifications

# create new dataframe of Topic = Stroke
topic_stroke <- subset(claims, subset = claims$Topic == 'Stroke')

# create new dataframe of Topic = Major Cardiovascular Disease
topic_mcd <- subset(claims, subset = claims$Topic == 'Major Cardiovascular Disease')

# create new dataframe of Topic = Diseases of the Heart (Heart Disease)
topic_hd <- subset(claims, subset = claims$Topic == 'Diseases of the Heart (Heart Disease)')

# create new dataframe of Topic = Heart Failure
topic_hf <- subset(claims, subset = claims$Topic == 'Heart Failure')

# create new dataframe of Topic = Acute Myocardial Infarction (Heart Attack)
topic_ami <- subset(claims, subset = claims$Topic == 'Acute Myocardial Infarction (Heart Attack)')

# create new dataframe of Topic = Coronary Heart Disease
topic_chd <- subset(claims, subset = claims$Topic == 'Coronary Heart Disease')

# create new dataframe of Break_Out_Category = Age
breakout_age <- subset(claims, subset = claims$Break_Out_Category == 'Age')

# create new dataframe of Break_Out_Category = Gender & Break_Out = Female
breakout_gender_f <- subset(claims, subset = claims$Break_Out_Category == 'Gender' & claims$Break_Out == 'Female')

# create new dataframe of Break_Out_Category = Gender & Break_Out = Male
breakout_gender_m <- subset(claims, subset = claims$Break_Out_Category == 'Gender' & claims$Break_Out == 'Male')

###################################
################## Heart Disease by Gender Line Charts #################### 


####### FEMALE #######
female1 <- aggregate(Data_Value ~ Break_Out+Topic+Year, breakout_gender_f, mean)

# Find the font you want at: https://fonts.google.com/
font_add_google(name="Montserrat", family="montserrat")

#you can optimize the png height, width, and resolution as you see fit
#the png settings below are for demo purposes
png("female1.png", 960, 480, res=96)
# turn on showtext
showtext_begin()
#plot
f1 <- ggplot(female1, aes(x=Year, y=Data_Value, group=Topic, color=Topic)) + 
  geom_line(size=1.5) +
  ylim(0,32) +
#  scale_colour_manual(values=c('#003f5c', '#444e86','#955196','#dd5182','#ff6e54','#ffa600')) +
#  scale_colour_manual(values=c("#72ab59","#9c51a2","#bc7738","#6569c5","#ba4767","#3ba7e5")) +
  scale_colour_manual(values=c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15')) +
#  scale_colour_manual(values=c('#d40000', '#df3b00', '#e95a00', '#f17500', '#f98e00', '#ffa600')) +
  theme_classic() + 
  labs(subtitle="Females", y="Average Percentage", x="Year") +
  theme(
    plot.title = element_text(family="montserrat", face="bold", size=18),
    plot.subtitle = element_text(family="montserrat", face="bold", size=16),
#    plot.caption = element_text(family="montserrat", hjust = 0),
    axis.title.x = element_text(family="montserrat", size=16),
    axis.title.y = element_text(family="montserrat", size=16),
    axis.text.x = element_text(family="montserrat", face="bold", size=16),
    axis.text.y = element_text(family="montserrat", face="bold", size=16),
    legend.text = element_text(family="montserrat", face="bold", size=12),
    legend.title = element_blank())
plot(f1)
#turn off showtext
showtext_end()
dev.off()

####### MALE #######

male1 <- aggregate(Data_Value ~ Break_Out+Topic+Year, breakout_gender_m, mean)

#you can optimize the png height, width, and resolution as you see fit
#the png settings below are for demo purposes
png("male1.png", 960, 480, res=96)
# turn on showtext
showtext_begin()
#plot
m1 <- ggplot(male1, aes(x=Year, y=Data_Value, group=Topic, color=Topic)) + 
  geom_line(size=1.5) +
  ylim(0,32) +
#  scale_colour_manual(values=c('#003f5c', '#444e86','#955196','#dd5182','#ff6e54','#ffa600')) +
#  scale_colour_manual(values=c("#72ab59","#9c51a2","#bc7738","#6569c5","#ba4767","#3ba7e5")) +
  scale_colour_manual(values=c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15')) +
  theme_classic() + 
  labs(subtitle="Males", y="Average Percentage", x="Year") +
  theme(
    plot.title = element_text(family="montserrat", face="bold", size=18),
    plot.subtitle = element_text(family="montserrat", face="bold", size=16),
#    plot.caption = element_text(family="montserrat", hjust = 0),
    axis.title.x = element_text(family="montserrat", size=16),
    axis.title.y = element_text(family="montserrat", size=16),
    axis.text.x = element_text(family="montserrat", face="bold", size=16),
    axis.text.y = element_text(family="montserrat", face="bold", size=16),
    legend.text = element_text(family="montserrat", face="bold", size=12),
    legend.title = element_blank())
plot(m1)
#turn off showtext
showtext_end()
dev.off()

######## side-by-side Female and Male charts ##########

png("female&male1.png", 1200, 480, res=96)
showtext_begin()
ggarrange(f1, m1, ncol=2, nrow=1,
          common.legend = TRUE, legend = "bottom")
showtext_end()
dev.off()

