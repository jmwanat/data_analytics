
# Generalized Linear Models

This course utilized linear regression, logistic regression and poisson regression.

## Linear Regression
The goal of this project was to predict the number of wins for a professional baseball team. Exploratory Data Analysis (EDA) was conducted. EDA is used to understand the types of variables in the data set. EDA uses descriptive statistics, when possible, to summarize the variables. The data is also examined for errors, outliers, and missing values. Visualization techniques such as boxplots, histograms, and scatterplots are used to visualize the data relationships and distribution.

The data was prepared for analysis in which missing values were imputed, variables with outliers were transformed in order to reduce the effect of outliers on model accuracy, and the data set was split into a 70/30 train/test split. Splitting the data allows for cross-validation after a predictive model has been developed. The train data set was used for in-sample model development, and the test data set was used for out-of-sample model assessment of predictive accuracy.

This report utilized R Markdown, in which the document text and code are prepared in the same notebook interface. 

![first assignment](Unit1_Moneyball_Wanat_Final.pdf)

## Logistic Regression
The goal of this project was to predict a car crash for a customer at an auto insurance company. EDA was conducted to understand the types of variables in the data set. The data was prepared for anaysis by imputing missing values, and performing transformation of variables with outliers in order to reduce the effet of outliers on model accuracy. The data set was not split into train/test sets, as this was already provided. 

This report utilized R Markdown, in which the document teext and code are prepared in the same notebook interface.

![second assignment](Unit2_Insurance_Wanat_Final.pdf)

## Poisson Regression
The goal of this project was to predict the number of wine cases ordered based on wine characteristics. The data set contains records of commercially available wines and
their physicochemical properties such as acidity, sulfur dioxide, sulphates and residual sugar. In order to build a model to provide a prediction of the number of wine cases ordered, an EDA of the data set was completed prior to this analysis, which allowed for an evaluation and selection of the most promising predictor variables.

Various models were prepared and evluated: Linear regression, linear regression using stepwise variable selection, poisson, negative binomial, zero inflated poisson, and zero inflated negative binomial. Various metrics were used to assess model fit: deviance, log likelihood, Akaike’s Information Criterion (AIC), Bayesian Information Criterion (BIC), mean squared error (MSE) and mean absolute error (MAE).

This report utilized R Markdown, in which the document text and code are prepared in the same notebook interface. 

![third assignment](Unit3_Wine_Sales_Wanat_Final.pdf)
