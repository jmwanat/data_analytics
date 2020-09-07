#Most of this R code was created from https://cran.r-project.org/web/packages/dataPreparation/vignettes/train_test_prep.html

#install dataPreparation
install.packages("dataPreparation")

#pull important libraries for use
library(tidyverse)
library(dataPreparation)
library(cdata)

#set working dictionary
setwd("C:/users/emers/OneDrive/Erin_Schoolwork/MSPA/MSDS498/capstone_healthcare-master/capstone_healthcare-master")

#load the dataset
cardio_split <- read.csv("cardio_facility_combine.csv", header=T)

data("cardio_split")
print(head(cardio_split,n=4))

#random sample, using 80% train and 20% test
cardio_train_index <- sample(1:nrow(cardio_split), 0.8 *nrow(cardio_split))
cardio_test_index <- setdiff(1:nrow(cardio_split), cardio_train_index)

#creating the x & y train and test splits
x_train <- cardio_split[cardio_train_index, -15]
y_train <- cardio_split[cardio_train_index, "cardio"]

x_test <- cardio_split[cardio_test_index, -15]
y_test <- cardio_split[cardio_test_index, "cardio"]

#Find and filter out useless variables
constant_cols <- whichAreConstant(cardio_split)
double_cols <- whichAreInDouble(cardio_split)
bijection_cols <- whichAreBijection(cardio_split)

#it's likely that section will come up empty when we run it, as we've already done a lot of data cleaning.

#make sure the general shapes of the test and train files match up

print(dim(x_test))
print(dim(x_train))

#write the files to CSV for future models
write.csv(x_test, file = "cardio_x_test.csv")
write.csv(x_train, file = "cardio_x_train.csv")
write.csv(y_test, file = "cardio_y_test.csv")
write.csv(y_train, file = "cardio_y_train.csv")