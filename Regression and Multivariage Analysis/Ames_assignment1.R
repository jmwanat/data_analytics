# Jennifer Wanat
# Fall 2017
# read_ames.R

install.packages("Hmisc")
require(Hmisc)
require(gridExtra)
require(moments)

path.name <- "~/Desktop/R/"
file.name <- paste(path.name, "ames_housing_data.csv", sep = "")

# Read in the csv file into an R data frame;
ames.df <- read.csv(file.name, header = TRUE, stringsAsFactors = FALSE)

# Show the header of the data frame;
head(ames.df)

# Show the structure of the data frame;
str(ames.df)


# This plot was created to view the 5 outliers and unusual values mentioned 
# in the data documentation special notes section
plot(ames.df$GrLivArea, ames.df$SalePrice,
     xlab = "Above Grade Living Area (square feet)", ylab = "Sale Price")


#Creating a waterfall of drop conditions
# Single ifelse() statement
# ifelse(condition, value if condition is TRUE, value if the condition is FALSE)

# Nested ifelse() statement
# ifelse(condition1, value if condition1 is TRUE,
#	ifelse(condition2, value if condition2 is TRUE,
#	value if neither condition1 nor condition2 is TRUE
#	)
# )


# Create a waterfall of drop conditions;
# Work the data frame as a 'table' like you would in SAS or SQL;
ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                                ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                                       ifelse(ames.df$Street!='Pave','03: Street Not Paved',
                                              ifelse(ames.df$GrLivArea >4000,'04: LT 4000 SqFt',
                                                     ifelse(ames.df$LotArea >100000,'05: Lot 100000 SqFt',
                                                            '99: Eligible Sample')
                                ))))

table(ames.df$dropCondition)

# Save the table
waterfall <- table(ames.df$dropCondition);

# Format the table as a column matrix for presentation;
as.matrix(waterfall,6,1)

data[,"waterfall", drop=FALSE]

# Eliminate all observations that are not part of the eligible sample population;
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample');

# Check that all remaining observations are eligible;
table(eligible.population$dropCondition)


#Pick twenty variables and run a data quality check on them;

table(eligible.population$LotArea!="NA")
table(eligible.population$LotArea!=0)
summary(eligible.population$LotArea)
quantile(eligible.population$LotArea)

table(eligible.population$Street,useNA = c("always"))

table(eligible.population$Utilities, useNA = c("always"))

table(eligible.population$HouseStyle, useNA = c("always"))

table(eligible.population$YearBuilt!="NA")
table(eligible.population$YearBuilt, useNA = c("always"))
summary(eligible.population$YearBuilt)
quantile(eligible.population$YearBuilt)

table(eligible.population$RoofMat, useNA = c("always"))

table(eligible.population$Exterior1, useNA = c("always"))

table(eligible.population$BsmtFinType1, useNA = c("always"))

table(eligible.population$Heating, useNA = c("always"))

table(eligible.population$CentralAir, useNA = c("always"))

table(eligible.population$Electrical, useNA = c("always"))

table(eligible.population$GrLivArea!="NA")
table(eligible.population$GrLivArea!=0)
summary(eligible.population$GrLivArea)
quantile(eligible.population$GrLivArea)
describe(eligible.population$GrLivArea)

table(eligible.population$FullBath, useNA = c("always"))

table(eligible.population$HalfBath, useNA = c("always"))

table(eligible.population$BedroomAbvGr, useNA = c("always"))

table(eligible.population$Fireplaces, useNA = c("always"))

table(eligible.population$GarageType, useNA = c("always"))

table(eligible.population$PavedDrive, useNA = c("always"))

table(eligible.population$SaleCondition, useNA = c("always"))

table(eligible.population$SalePrice!="NA")
summary(eligible.population$SalePrice)
quantile(eligible.population$SalePrice)
describe(eligible.population$SalePrice)


#Pick ten variables from the twenty variables from the data 
#quality check to explore in initial exploratory data analysis;

#The ten variables are:
#lot area, utilities, year built, GrLivArea, Full bath
#half bath, bedroom, garage type, heating, sale price

#Continuous: lot area, year built, GrLivArea, sale price
#Discrete: utilities, full bath, half bath, bedroom, garage type, heating

par(mfrow = c(1,2))
boxplot(eligible.population$SalePrice, ylab = "Sale Price ($$)", col = c("lightgrey"),
        coef = 3.0, do.conf = TRUE, do.out = TRUE)
hist(eligible.population$SalePrice, xlab = "Sale Price ($$)", col = "lightgrey",
     main = "")
par(mfrow = c(1,1))

par(mfrow = c(1,2))
plot(eligible.population$LotArea, eligible.population$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Lot Size (square feet)")
abline(lm(eligible.population$SalePrice ~ eligible.population$LotArea), col = "red", lwd = 2, lty = 2)
lines(lowess(eligible.population$LotArea, eligible.population$SalePrice),
      col = "blue", lwd = 2, lty = 1)
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010", outer = TRUE)
hist(eligible.population$LotArea, xlab = "Lot Size (square feet)", col = "lightgrey",
     main = "")
par(mfrow = c(1,1))

par(mfrow = c(1,2))
plot(eligible.population$YearBuilt, eligible.population$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Year Built")
abline(lm(eligible.population$SalePrice ~ eligible.population$YearBuilt), col = "red", lwd = 2, lty = 2)
lines(lowess(eligible.population$YearBuilt, eligible.population$SalePrice),
      col = "blue", lwd = 2, lty = 1)
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010", outer = TRUE)
hist(eligible.population$YearBuilt, xlab = "Year Built", col = "lightgrey",
     ylim = c(0,400),
     main = "")
par(mfrow = c(1,1))

par(mfrow = c(1,2))
plot(eligible.population$GrLivArea, eligible.population$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Above Grade Living Area (square feet)")
abline(lm(eligible.population$SalePrice ~ eligible.population$GrLivArea), col = "red", lwd = 2, lty = 2)
lines(lowess(eligible.population$GrLivArea, eligible.population$SalePrice),
      col = "blue", lwd = 2, lty = 1)
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010")
hist(eligible.population$GrLivArea, xlab = "Above Grade Living Area (square feet)", col = "lightgrey",
     ylim = c(0,800),
     main = "")
par(mfrow = c(1,1))

par(mfrow = c(1,2))
table(eligible.population$BedroomAbvGr, eligible.population$FullBath)
plot(jitter(eligible.population$BedroomAbvGr), jitter(eligible.population$FullBath),
     ylab = "Number of Full Baths",
     xlab = "Number of Bedrooms Above Grade")
lines(lowess(eligible.population$BedroomAbvGr, eligible.population$FullBath),
      col = "blue", lwd = 2, lty = 1)
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010")
table(eligible.population$BedroomAbvGr, eligible.population$HalfBath)
plot(jitter(eligible.population$BedroomAbvGr), jitter(eligible.population$HalfBath),
     ylab = "Number of Half Baths",
     xlab = "Number of Bedrooms Above Grade")
lines(lowess(eligible.population$BedroomAbvGr, eligible.population$HalfBath),
      col = "blue", lwd = 2, lty = 1)
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010")
par(mfrow = c(1,1))

par(mfrow = c(1,3))
boxplot(eligible.population$SalePrice ~ eligible.population$BedroomAbvGr, col = "lightgrey",
        ylab = "Sale Price ($$)",
        xlab = "Number of Bedrooms Above Grade")
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010")
boxplot(eligible.population$SalePrice ~ eligible.population$FullBath, col = "lightgrey",
        ylab = "Sale Price ($$)",
        xlab = "Number of Full Baths")
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010")
boxplot(eligible.population$SalePrice ~ eligible.population$HalfBath, col = "lightgrey",
        ylab = "Sale Price ($$)",
        xlab = "Number of Half Baths")
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010")
par(mfrow = c(1,1))



par(mfrow = c(1,3))
boxplot(eligible.population$SalePrice ~ eligible.population$Heating, col = "lightgrey",
        ylab = "Sale Price ($$)",
        xlab = "Type of Heating")
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010")
boxplot(eligible.population$SalePrice ~ eligible.population$Utilities, col = "lightgrey",
        ylab = "Sale Price ($$)",
        xlab = "Utilities")
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010")
boxplot(eligible.population$SalePrice ~ eligible.population$GarageType, col = "lightgrey",
        ylab = "Sale Price ($$)",
        xlab = "Garage Type")
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010")
par(mfrow = c(1,1))

grid.table(addmargins(table(eligible.population$FullBath, eligible.population$HalfBath, dnn = c("FullBath", "HalfBath"))))

#Pick three variables from the ten variables from the initial exploratory data analysis
#Explore their relationship with SalePrice and Log(SalePrice)

par(mfrow = c(1,2))
plot(eligible.population$YearBuilt, eligible.population$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Year Built")
abline(lm(eligible.population$SalePrice ~ eligible.population$YearBuilt), col = "red", lwd = 2, lty = 2)
lines(lowess(eligible.population$YearBuilt, eligible.population$SalePrice),
      col = "blue", lwd = 2, lty = 1)
#log of sale price
plot(eligible.population$YearBuilt, log(eligible.population$SalePrice),
     ylab = "log(Sale Price)",
     xlab = "Year Built")
abline(lm(log(eligible.population$SalePrice) ~ eligible.population$YearBuilt), col = "red", lwd = 2, lty = 2)
lines(lowess(eligible.population$YearBuilt, log(eligible.population$SalePrice)),
      col = "blue", lwd = 2, lty = 1)
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010", outer=TRUE)
par(mfrow = c(1,1))

par(mfrow = c(1,2))
plot(eligible.population$GrLivArea, eligible.population$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Above Grade Living Area (square feet)")
abline(lm(eligible.population$SalePrice ~ eligible.population$GrLivArea), col = "red", lwd = 2, lty = 2)
lines(lowess(eligible.population$GrLivArea, eligible.population$SalePrice),
      col = "blue", lwd = 2, lty = 1)
#log of GrLivArea
plot(eligible.population$GrLivArea, log(eligible.population$SalePrice),
     ylab = "log(Sale Price)",
     xlab = "Above Grade Living Area (square feet)")
abline(lm(log(eligible.population$SalePrice) ~ eligible.population$GrLivArea), col = "red", lwd = 2, lty = 2)
lines(lowess(eligible.population$GrLivArea, log(eligible.population$SalePrice)),
      col = "blue", lwd = 2, lty = 1)
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010", outer=TRUE)
par(mfrow = c(1,1))

par(mfrow = c(1,2))
plot(jitter(eligible.population$BedroomAbvGr), eligible.population$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Number of Bedrooms")
abline(lm(eligible.population$SalePrice ~ eligible.population$BedroomAbvGr), col = "red", lwd = 2, lty = 2)
lines(lowess(eligible.population$BedroomAbvGr, eligible.population$SalePrice),
      col = "blue", lwd = 2, lty = 1)
#log with bedroom
plot(jitter(eligible.population$BedroomAbvGr), log(eligible.population$SalePrice),
     ylab = "log(Sale Price)",
     xlab = "Number of Bedrooms")
abline(lm(log(eligible.population$SalePrice) ~ eligible.population$BedroomAbvGr), col = "red", lwd = 2, lty = 2)
lines(lowess(eligible.population$BedroomAbvGr, log(eligible.population$SalePrice)),
      col = "blue", lwd = 2, lty = 1)
#title(main = "Residential Properties sold in Ames, IA from 2006 - 2010", outer = TRUE)
par(mfrow = c(1,1))