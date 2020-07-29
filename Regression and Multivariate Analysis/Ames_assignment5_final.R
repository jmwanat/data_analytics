# Jennifer Wanat
# Fall 2017
# Ames_assignment5.R


install.packages("maptools")
install.packages("stargazer")

require(maptools)
require(gridExtra)
require(gtable)
require(sjPlot)
require(broom)

path.name <- "~/Desktop/R/"
file.name <- paste(path.name, "ames_housing_data.csv", sep = "")

# Read in the csv file into an R data frame;
ames.df <- read.csv(file.name, header = TRUE, stringsAsFactors = FALSE)

# Show the header of the data frame;
head(ames.df)

# Show the structure of the data frame;
str(ames.df)

#####################################Waterfall###########################
#Creating a waterfall of drop conditions
ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                                ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                                       ifelse(ames.df$Street!='Pave','03: Street Not Paved',
                                              ifelse(ames.df$YearBuilt <1950,'04: Built Pre-1950',
                                                     ifelse(ames.df$GrLivArea <800,'05: Less than 800 SqFt',
                                                            ifelse(ames.df$GrLivArea >4000,'06: Greater than 4000 SqFt',
                                                                   ifelse(ames.df$LotArea >100000,'07: Lot 100000 SqFt',
                                                                          ifelse(ames.df$BedroomAbvGr <1, '08: No Bedrooms',
                                                                                 '99: Eligible Sample')
                                              )))))))

table(ames.df$dropCondition)

# Save the table
waterfall <- table(ames.df$dropCondition);

# Format the table as a column matrix for presentation;
as.matrix(waterfall,8,1)

# Eliminate all observations that are not part of the eligible sample population;
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample');

# Check that all remaining observations are eligible;
table(eligible.population$dropCondition)

#check the structure of the data frame
str(eligible.population)

#Table of waterfall variables and counts, but not used as it did not reflect the adjusted population count
test <- as.data.frame(t(waterfall))
str(test)
grid.table(test[c(-1)], cols = c("Variable", "Number"), rows = NULL)
#this is an alternate table of above
#same table output but left alignment
tt3 <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.05)),
                      rowhead=list(fg_params=list(hjust=0, x=0)))
g <- grid.table(test[c(-1)], cols = c("Variable", "Number"), rows = NULL,
           theme = tt3)



##################################Create additional predictor variables#####

# Define total square footage
eligible.population$TotalSqftCalc <- eligible.population$BsmtFinSF1+eligible.population$BsmtFinSF2+eligible.population$GrLivArea;

# Define Quality Index
eligible.population$QualityIndex <- eligible.population$OverallQual*eligible.population$OverallCond;
str(eligible.population$QualityIndex)

# Define total bathrooms
eligible.population$TotalBathCalc <- eligible.population$BsmtFullBath + 0.5*eligible.population$BsmtHalfBath + 
  eligible.population$FullBath + 0.5*eligible.population$HalfBath;

# Define Pool Indicator
eligible.population$PoolInd <- ifelse(eligible.population$PoolArea>0,1,0);

# Define Exterior Siding Type
eligible.population$BrickInd <- ifelse(eligible.population$Exterior1=='BrkFace',1,0);
eligible.population$VinylSidingInd <- ifelse(eligible.population$Exterior1=='VinylSd',1,0);

# Define Wood Deck Indicator
eligible.population$WoodDeckInd <- ifelse(eligible.population$WoodDeckSF>0,1,0);

# Define Porch Indicator - Open Porch OR Screen Porch
eligible.population$PorchInd <- ifelse((eligible.population$OpenPorchSF>0)|(eligible.population$ScreenPorch>0),1,0);

# Define Neighborhoods by price per square foot
eligible.population$Neighborhood1 <- ifelse((eligible.population$Neighborhood == c("IDOTRR")|
                                               (eligible.population$Neighborhood == c("OldTown")|
                                                  (eligible.population$Neighborhood == c("SWISU")))), 1,0);

eligible.population$Neighborhood2 <- ifelse((eligible.population$Neighborhood == c("BrkSide")|
                                               (eligible.population$Neighborhood == c("Edwards"))), 1,0);

eligible.population$Neighborhood3 <- ifelse((eligible.population$Neighborhood == c("NWAmes")|
                                               (eligible.population$Neighborhood == c("Crawfor")|
                                                  (eligible.population$Neighborhood == c("Gilbert")|
                                                     (eligible.population$Neighborhood == c("NAmes")|
                                                        (eligible.population$Neighborhood == c("Sawyer")|
                                                           (eligible.population$Neighborhood == c("SawyerW")|
                                                              (eligible.population$Neighborhood == c("Blmngtn")|
                                                                 (eligible.population$Neighborhood == c("ClearCr")|
                                                                    (eligible.population$Neighborhood == c("NoRidge")|
                                                                       (eligible.population$Neighborhood == c("Veenker")|
                                                                          (eligible.population$Neighborhood == c("CollgCr")|
                                                                             (eligible.population$Neighborhood == c("Mitchel")
                                                                             )))))))))))), 1,0);


eligible.population$Neighborhood4 <- ifelse((eligible.population$Neighborhood == c("Timber")|
                                               (eligible.population$Neighborhood == c("Somerst"))), 1,0);

eligible.population$Neighborhood5 <- ifelse((eligible.population$Neighborhood == c("StoneBr")|
                                               (eligible.population$Neighborhood == c("NridgHt"))), 1,0);


####################################################
#Table of variable parameters added to eligible population data frame
#colnames(eligible.population)[84:97]
indicator1 <- t(colnames(eligible.population)[84:88])
indicator2 <- t(colnames(eligible.population)[89:93])
indicator3 <- t(colnames(eligible.population)[94:98])
indicator.table <- rbind(indicator1, indicator2, indicator3)
grid.table(indicator.table)


######################Save as .RData object##########
# Save the R data frame as an .RData object
saveRDS(eligible.population,file='/Users/jmwanat/Documents/Northwestern classes/MSPA 410/410 R/ames_assignment3.RData')


####################Random Number seed generator and Train / Test split########
# Set the seed on the random number generator so you get the same split every time that
# you run the code.
set.seed(123)
eligible.population$u <- runif(n=dim(eligible.population)[1],min=0,max=1);

# Create train/test split;
train.df <- subset(eligible.population, u<0.70);
test.df <- subset(eligible.population, u>=0.70);

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(eligible.population)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

#Create table of observations in each data frame and calculate percentage split
total <- dim(eligible.population)[1]
total.train <- dim(train.df)[1]
total.test <- dim(test.df)[1]

percent.total <- round((total/total)*100, 1)
percent.train <- round((total.train/total)*100, 1)
percent.test <- signif((total.test/total)*100, 4)

total.all <- c(total.train, total.test, total)
percent.all <- c(percent.train, percent.test, percent.total)

overview.split <- cbind(total.all, percent.all)
colnames(overview.split) <- c("Number\nof Observations", "Percentage")
rownames(overview.split) <- c("Train Set", "Test Set", "Total")
grid.table(overview.split)


######################Model Identification by Automated Variable Selection##############
#Initial drop listed generated but not used after evaluation of model output
#kept for documentation purposes

drop.list1 <- c('SID', 'PID', 'SubClass', 'Zoning','LotFrontage', 'Street', 'Alley', 'LotShape', 'Utilities', 'LotConfig',
              'LandSlope', 'Neighborhood', 'Condition1', 'Condition2', 'BldgType', 'HouseStyle', 'OverallQual', 'OverallCond',
              'YearRemodel', 'RoofStyle', 'RoofMat', 'Exterior1', 'Exterior2', 'MasVnrType', 'MasVnrArea', 'ExterQual', 
              'ExterCond', 'Foundation', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinSF1', 'BsmtFinType2',
              'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 'HeatingQC', 'CentralAir', 'FirstFlrSF', 'SecondFlrSF', 'LowQualFinSF',
              'BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath', 'KitchenAbvGr', 'KitchenQual', 'Functional', 'Fireplaces',
              'FireplaceQu', 'GarageTyBlt', 'GarageFinish', 'GarageCars', 'GarageQual', 'GarageCond', 'PavedDrive', 'WoodDeckSF',
              'OpenPorchSF', 'EnclosedPorch', 'ThreeSsnPorch', 'ScreenPorch', 'PoolArea', 'PoolQC', 'Fence', 'MiscFeature',
              'MiscVal', 'MoSold', 'YrSold', 'SaleType', 'SaleCondition', 'dropCondition', 'WoodDeckInd', 'Neighborhood1',
              'Neighborhood2', 'Neighborhood3', 'Heating', 'u');

train.clean1 <-train.df[,!(names(eligible.population) %in% drop.list1)];

#Table of variables used in train.clean1 data frame
r1 <- t(colnames(train.clean1)[1:4])
r2 <- t(colnames(train.clean1)[5:8])
r3 <- t(colnames(train.clean1)[9:12])
r4 <- t(colnames(train.clean1)[13:16])
r5 <- t(colnames(train.clean1)[17:20])
train.clean1.table <- rbind(r1,r2,r3,r4,r5)
rownames(train.clean1.table) <- c('train.clean1 data frame', ' ', ' ', ' ', ' ')
grid.table((train.clean1.table))


###############################Alternate drop list for Automated Variable Selection##########
#This was the drop list used for all analysis in the assignment

drop.list <- c('SID', 'PID', 'SubClass', 'Zoning','LotFrontage', 'Street', 'Alley', 'LotShape', 'Utilities', 'LotConfig',
               'LandSlope', 'Neighborhood', 'Condition1', 'Condition2', 'BldgType', 'HouseStyle', 'OverallQual', 'OverallCond',
               'YearRemodel', 'RoofStyle', 'RoofMat', 'Exterior1', 'Exterior2', 'MasVnrType', 'MasVnrArea', 'ExterQual', 
               'ExterCond', 'Foundation', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinSF1', 'BsmtFinType2',
               'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 'HeatingQC', 'CentralAir', 'FirstFlrSF', 'SecondFlrSF', 'LowQualFinSF',
               'BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath', 'KitchenAbvGr', 'KitchenQual', 'Functional', 'Fireplaces',
               'FireplaceQu', 'GarageTyBlt', 'GarageFinish', 'GarageCars', 'GarageQual', 'GarageCond', 'PavedDrive', 'WoodDeckSF',
               'OpenPorchSF', 'EnclosedPorch', 'ThreeSsnPorch', 'ScreenPorch', 'PoolArea', 'PoolQC', 'Fence', 'MiscFeature',
               'MiscVal', 'MoSold', 'YrSold', 'SaleType', 'SaleCondition', 'dropCondition', 'WoodDeckInd', 'Neighborhood1',
               'Neighborhood2', 'Heating', 'u', 'GarageType', 'Electrical', 'PorchInd', 'TotRmsAbvGrd', 'Neighborhood3');

train.clean <-train.df[,!(names(eligible.population) %in% drop.list)];

#This list is the same as drop.list1 except:
#'GarageType', 'Electrical', 'PorchInd', 'TotRmsAbvGrd', 
#were also dropped from the data set

#Table of variables used in train.clean data frame
r1tc <- t(colnames(train.clean)[1:4])
r2tc <- t(colnames(train.clean)[5:8])
r3tc <- t(colnames(train.clean)[9:12])
r4tc <- t(colnames(train.clean)[13:16])
train.clean.table <- rbind(r1tc,r2tc,r3tc,r4tc)
rownames(train.clean.table) <- c('train.clean data frame', ' ', ' ', ' ')
grid.table((train.clean.table))



##########################################################################################
# Delete observations with missing values 
##########################################################################################
#########This is for the first attempt at a train list

train.clean1.df2 <- na.omit(train.clean1);

dim(train.clean1)
dim(train.clean1.df2)

dim(train.clean1)-dim(train.clean1.df2)

##########This is for alternate train list - the list used for the assignment
train.clean.df2 <- na.omit(train.clean);

#this worked with the stepAIC() below
# Check the change in dimension;
dim(train.clean)
dim(train.clean.df2)

dim(train.clean)-dim(train.clean.df2)


##########################Model Identification#########################################

#################Using train.clean1.df2################################################
#####This was the initial analysis, but I decided to go back and remove some predictor variables###
#####and re-run the stepAIC. The code is left for documentation purposes.#######################
#####################################################
# Define the upper model as the FULL model
#upper1.lm <- lm(SalePrice ~ .,data=train.clean1.df2);
#summary(upper1.lm)

# Define the lower model as the Intercept model
#lower1.lm <- lm(SalePrice ~ 1,data=train.clean1.df2);
#summary(lower1.lm)

# Need a SLR to initialize stepwise selection
#sqft1.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean1.df2);
#summary(sqft1.lm)

##################StepAIC - FS, BE, and SM methods##########
# Call stepAIC() for variable selection
#forward1.lm <- stepAIC(object=lower1.lm, scope=list(upper=formula(upper1.lm), lower=~1), direction=c('forward'));
#summary(forward1.lm)

#Model residual summary table
#grid.table(t(round(summary(forward1.lm$residuals), 0)))
#Model summary table of coefficient
#f1.coefficients <-  tidy(forward1.lm)
#grid.table(f1.coefficients, row = NULL)

#backward1.lm <- stepAIC(object=upper1.lm,direction=c('backward'));
#summary(backward1.lm)

#stepwise1.lm <- stepAIC(object=sqft1.lm,scope=list(upper=formula(upper1.lm),lower=~1),
#                       direction=c('both'));
#summary(stepwise1.lm)



###########################Alternate drop list modeling###############################
# This is the analysis used for the assignment
#################Using train.clean.df2##############
# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean.df2);
summary(upper.lm)

anova(upper.lm)
# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean.df2);
summary(lower.lm)

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean.df2);
summary(sqft.lm)


################Junk Model###################################
#Fourth "junk" model for model comparison purposes
junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea +
                TotalSqftCalc, data=train.df)
summary(junk.lm)
#Model residual summary table
grid.table(t(round(summary(junk.lm$residuals), 0)), rows = c('Residuals'))
#Model summary table of coefficient
j.coefficients <-  tidy(junk.lm)
grid.table(j.coefficients, row = NULL)
#Table of model ANOVA
Junk.summary <- t(model.summary(junk.lm))
colnames(Junk.summary) <- c('Residual Standard Error', 'Adjusted R-squared','Multiple R-squared','F-Statistic')
grid.table(Junk.summary)

##############################################################
# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection
#########################Forward Selection#####################
forward.lm <- stepAIC(object=lower.lm, scope=list(upper=formula(upper.lm), lower=~1), direction=c('forward'));
summary(forward.lm)

#Model residual summary table
grid.table(t(round(summary(forward.lm$residuals), 0)), rows = c('Residuals'))
#Model summary table of coefficient
f.coefficients <-  tidy(forward.lm)
grid.table(f.coefficients, row = NULL)

options(scipen = 2)
#Function for model summary statistics
model.summary <- function(model){
  residualse.summary <- round(summary(lm(model))$sigma,1)
  adjrs.summary <- round(summary(lm(model))$adj.r.squared,4)
  multrs.summary <- round(summary(lm(model))$r.squared,4)
  fstat.summary <- round(unname(summary(lm(model))$fstatistic)[1], 1)
  return(c(residualse.summary, adjrs.summary, multrs.summary, fstat.summary))
}

#Table of model ANOVA
Forward.summary <- t(model.summary(forward.lm))
colnames(Forward.summary) <- c('Residual Standard Error', 'Adjusted R-squared','Multiple R-squared','F-Statistic')
grid.table(Forward.summary)

########################Backward Elimination####################
backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

#Model residual summary table
grid.table(t(round(summary(backward.lm$residuals), 0)), rows = c('Residuals'))
#Model summary table of coefficient
b.coefficients <-  tidy(backward.lm)
grid.table(b.coefficients, row = NULL)
#Table of model ANOVA
Backward.summary <- t(model.summary(backward.lm))
colnames(Backward.summary) <- c('Residual Standard Error', 'Adjusted R-squared','Multiple R-squared','F-Statistic')
grid.table(Backward.summary)

########################Stepwise Method########################
stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                        direction=c('both'));
summary(stepwise.lm)

#Model residual summary table
grid.table(t(round(summary(stepwise.lm$residuals), 0)), rows = c('Residuals'))
#Model summary table of coefficient
s.coefficients <-  tidy(stepwise.lm)
grid.table(s.coefficients, row = NULL)
#Table of model ANOVA
Stepwise.summary <- t(model.summary(stepwise.lm))
colnames(Stepwise.summary) <- c('Residual Standard Error', 'Adjusted R-squared','Multiple R-squared','F-Statistic')
grid.table(Stepwise.summary)

##########The models from train.clean.df2 will be used for the rest of the assignment
#These were selected because it was a simplier model (fewer variables) with more significant t values for the 
#coefficients, and the adjustment had little effect on the r-squared.




###############Compute VIF values for the variable selection models#################

library(car)
forward.vif <- sort(vif(forward.lm),decreasing=TRUE)
backward.vif <- sort(vif(backward.lm),decreasing=TRUE)
stepwise.vif <- sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm), decreasing = TRUE)
#These values are sorted from all the output columns, but allows for easy visualization of the highest VIF values

#Table of VIF function output for each model
grid.table(round(vif(forward.lm), 4))
grid.table(round(vif(backward.lm), 4))
grid.table(round(vif(stepwise.lm), 4))

#Table of VIF for junk model
vif.junk <- round(vif(junk.lm),5)
rownames(vif.junk) <- c('OverallQual', 'OverallCond', 'QualityIndex', 'GrLivArea', 'TotalSqftCalc')
grid.table(vif.junk, rows = c('OverallQual', 'OverallCond', 'QualityIndex', 'GrLivArea', 'TotalSqftCalc'),
           cols = c('VIF'))


###################Compute MSE and MAE for models

#Function to calculate Adjusted R-squared, AIC, BIC, MSE, and MAE 
#model = model.lm
Model.fit <- function(model){
  adj.rsquare <- round(summary(lm(model))$adj.r.squared, 4)
  AIC.fit <- round(AIC(model), 0)
  BIC.fit <- round(BIC(model), 0)
  MSE.fit <- round(mean(model$residuals^2), 0)
  MAE.fit <- round(mean(abs(model$residuals)), 0)
  return(c(adj.rsquare, AIC.fit, BIC.fit, MSE.fit, MAE.fit))
}

#Model Comparison
#Table of R-squared, AIC, BIC, MSE, and MAE for each Model
Forward <- Model.fit(forward.lm)
Backward <- Model.fit(backward.lm)
Stepwise <- Model.fit(stepwise.lm)
Junk <- Model.fit(junk.lm)
fit.compare <- rbind(Forward, Backward, Stepwise, Junk)
colnames(fit.compare) <- c('Adjusted\nR-squared', 'AIC', 'BIC', 'MSE', 'MAE')
grid.table(fit.compare)

#Model Rank
#Table of Model Rank for each criterion above (R-squared, AIC, BIC, MSE, MAE)
First <- c('Model 1, 2, and 3', 'Model 1, 2, and 3', 'Model 1, 2, and 3', 'Model 1, 2, and 3', 'Model 1, 2, and 3')
Second <- c('Junk', 'Junk', 'Junk', 'Junk', 'Junk')
model.rank <- rbind(First, Second)
colnames(model.rank) <- c('Adjusted\nR-squared', 'AIC', 'BIC', 'MSE', 'MAE')
grid.table(model.rank)

##############################Predictive Accuracy#####################
forward.test <- predict(forward.lm,newdata=test.df);
backward.test <- predict(backward.lm, newdata=test.df)
stepwise.test <- predict(stepwise.lm, newdata=test.df)
junk.test <- predict(junk.lm, newdata=test.df)

ft.residuals <- (test.df$SalePrice - forward.test)
bt.residuals <- (test.df$SalePrice - backward.test)
st.residuals <- (test.df$SalePrice - stepwise.test)
jt.residuals <- (test.df$SalePrice - junk.test)


#Calculate the MSE and MAE of each model
ft.mse <- round(mean(ft.residuals^2), 0)
ft.mae <- round(mean(abs(ft.residuals)), 0)

bt.mse <- round(mean(bt.residuals^2), 0)
bt.mae <- round(mean(abs(bt.residuals)), 0)

st.mse <- round(mean(st.residuals^2), 0)
st.mae <- round(mean(abs(st.residuals)), 0)

jt.mse <- round(mean(jt.residuals^2), 0)
jt.mae <- round(mean(abs(jt.residuals)), 0)


#Create table of predictive accuracy with model MSE and MAE
forward.predict.fit <- cbind(ft.mse, ft.mae)
backward.predict.fit <- cbind(bt.mse, bt.mae)
stepwise.predict.fit <- cbind(st.mse, st.mae)
junk.predict.fit <- cbind(jt.mse, jt.mae)
Predict.fit.table <- rbind(forward.predict.fit, backward.predict.fit, stepwise.predict.fit, junk.predict.fit)
colnames(Predict.fit.table) <- c('MSE', 'MAE')
rownames(Predict.fit.table) <- c('Forward', 'Backward', 'Stepwise', 'Junk')
grid.table(Predict.fit.table)


#Plots of sale price vs test data predicted sale price
par(mfrow=c(2,2))
plot(forward.test, test.df$SalePrice,
     col = "darkblue",
     cex.lab = 1.5,
     xlab = "Forward Test Prediction", ylab = "Test Data Sale Price")

plot(backward.test, test.df$SalePrice,
     col = 'darkred',
     cex.lab = 1.5,
     xlab = "Backward Test Prediction", ylab = "Test Data Sale Price")

plot(stepwise.test, test.df$SalePrice,
     col = 'darkgreen',
     cex.lab = 1.5,
     xlab = "Stepwise Test Prediction", ylab = "Test Data Sale Price")

plot(junk.test, test.df$SalePrice,
     cex.lab = 1.5,
     xlab = "Junk Test Prediction", ylab = "Test Data Sale Price")
par(mfrow=c(1,1))


#################################Operational Validation############################

# Training Data
# Abs Pct Error
########Forward Train Test###################################
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
forward.trainPredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )
)
forward.trainTable <- table(forward.trainPredictionGrade)
forward.trainTable/sum(forward.trainTable)
#Create table for paper
forward.traingrade <- cbind(signif((forward.trainTable[1]/sum(forward.trainTable)),4), 
                       signif((forward.trainTable[2]/sum(forward.trainTable)),4),
                       signif((forward.trainTable[3]/sum(forward.trainTable)),4), 
                       signif((forward.trainTable[4]/sum(forward.trainTable)),4))
colnames(forward.traingrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(forward.traingrade) <- c('Forward Train')
grid.table(forward.traingrade)

########Backward Train Test####################################
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
backward.trainPredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )
)
backward.trainTable <- table(backward.trainPredictionGrade)
backward.trainTable/sum(backward.trainTable)
#Create table for paper
backward.traingrade <- cbind(signif((backward.trainTable[1]/sum(backward.trainTable)),4), 
                        signif((backward.trainTable[2]/sum(backward.trainTable)),4),
                        signif((backward.trainTable[3]/sum(backward.trainTable)),4), 
                        signif((backward.trainTable[4]/sum(backward.trainTable)),4))
colnames(backward.traingrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(backward.traingrade) <- c('Backward Train')
grid.table(backward.traingrade)

########Stepwise Train Test###################################
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
stepwise.trainPredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0.0.10]',
                                        ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]',
                                               ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                      'Grade 4: (0.25+]')
                                        )
)
stepwise.trainTable <- table(stepwise.trainPredictionGrade)
stepwise.trainTable/sum(stepwise.trainTable)
#Create table for paper
stepwise.traingrade <- cbind(signif((stepwise.trainTable[1]/sum(stepwise.trainTable)),4), 
                        signif((stepwise.trainTable[2]/sum(stepwise.trainTable)),4),
                        signif((stepwise.trainTable[3]/sum(stepwise.trainTable)),4), 
                        signif((stepwise.trainTable[4]/sum(stepwise.trainTable)),4))
colnames(stepwise.traingrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(stepwise.traingrade) <- c('Stepwise Train')
grid.table(stepwise.traingrade)


#########Junk Train Test#######################################
junk.pct <- abs(junk.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
junk.trainPredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0.0.10]',
                                        ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                               ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                      'Grade 4: (0.25+]')
                                        )
)
junk.trainTable <- table(junk.trainPredictionGrade)
junk.trainTable/sum(junk.trainTable)
#Create table for paper
junk.traingrade <- cbind(signif((junk.trainTable[1]/sum(junk.trainTable)),4), 
                    signif((junk.trainTable[2]/sum(junk.trainTable)),4),
                    signif((junk.trainTable[3]/sum(junk.trainTable)),4), 
                    signif((junk.trainTable[4]/sum(junk.trainTable)),4))
colnames(junk.traingrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(junk.traingrade) <- c('Junk Train')
grid.table(junk.traingrade)

#####################Summary of all Train data grades###########
#Create table for all Train data grades
all.traingrade <- rbind(forward.traingrade, backward.traingrade, stepwise.traingrade, junk.traingrade)
colnames(all.traingrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(all.traingrade) <- c('Forward Train','Backward Train','Stepwise Train','Junk Train')
grid.table(all.traingrade)




##############################################################
# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice;
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice;
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice;
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice;

# Assign Prediction Grades;
###############Forward.test##################################
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )
)
forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)
#Create table for paper
forward.testgrade <- cbind(signif((forward.testTable[1]/sum(forward.testTable)),4), 
                       signif((forward.testTable[2]/sum(forward.testTable)),4),
                       signif((forward.testTable[3]/sum(forward.testTable)),4), 
                       signif((forward.testTable[4]/sum(forward.testTable)),4))
colnames(forward.testgrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(forward.testgrade) <- c('Forward Test')
grid.table(forward.testgrade)


#############Backward.test####################################
backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )
)
backward.testTable <-table(backward.testPredictionGrade)
backward.testTable/sum(backward.testTable)
#Create table for paper
backward.testgrade <- cbind(signif((backward.testTable[1]/sum(backward.testTable)),4), 
                        signif((backward.testTable[2]/sum(backward.testTable)),4),
                        signif((backward.testTable[3]/sum(backward.testTable)),4), 
                        signif((backward.testTable[4]/sum(backward.testTable)),4))
colnames(backward.testgrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(backward.testgrade) <- c('Backward Test')
grid.table(backward.testgrade)


#############Stepwise.test################################
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )
)
stepwise.testTable <-table(stepwise.testPredictionGrade)
stepwise.testTable/sum(stepwise.testTable)
#Create table for paper
stepwise.testgrade <- cbind(signif((stepwise.testTable[1]/sum(stepwise.testTable)),4), 
                        signif((stepwise.testTable[2]/sum(stepwise.testTable)),4),
                        signif((stepwise.testTable[3]/sum(stepwise.testTable)),4), 
                        signif((stepwise.testTable[4]/sum(stepwise.testTable)),4))
colnames(stepwise.testgrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(stepwise.testgrade) <- c('Stepwise Test')
grid.table(stepwise.testgrade)




###########Junk.test#################################
junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )
)
junk.testTable <-table(junk.testPredictionGrade)
junk.testTable/sum(junk.testTable)
#Create table for paper
junk.testgrade <- cbind(signif((junk.testTable[1]/sum(junk.testTable)),4), 
                    signif((junk.testTable[2]/sum(junk.testTable)),4),
                    signif((junk.testTable[3]/sum(junk.testTable)),4), 
                    signif((junk.testTable[4]/sum(junk.testTable)),4))
colnames(junk.testgrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(junk.testgrade) <- c('Junk Test')
grid.table(junk.testgrade)

#####################Summary of all Test data grades###########
#Create table for all Test data grades
#all.testgrade <- rbind(forward.grade, backward.grade, stepwise.grade, junk.grade)
#colnames(all.testgrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
#rownames(all.testgrade) <- c('Forward Test','Backward Test','Stepwise Test','Junk Test')
#grid.table(all.testgrade)
#str(forward.grade)

all.testgrade <- rbind(forward.testgrade, backward.testgrade, stepwise.testgrade, junk.testgrade)
colnames(all.testgrade) <- c('Grade 1: [0.0.10]', 'Grade 2: (0.10,0.15]', 'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
rownames(all.testgrade) <- c('Forward Test','Backward Test','Stepwise Test','Junk Test')
grid.table(all.testgrade)

