
# Regression and Multivariable Analysis

## Introduction

This course focused on statistical modeling as a three step process: 1) exploratory data analysis (EDA), 2) model identification, and 3) model validation. Weekly assignments for the EDA and regression models utilized a housing data set obtained from the Ames, IA Assessor's Office and assembled by Dr. Dean De Cock at Truman State University. These processes were used to build models to estimate home values.

In addition to linear regression, the concepts of principal component analysis (PCA), factor analysis, and cluster analysis were taught.

The programming language R was used for all work.

## Exploratory Data Analysis
EDA is used to understand the types of variables in the data set. EDA uses descriptive statistics, when possible, to summarize the variables. The data is also examined for errors, outliers, and missing values. Visualization techniques such as boxplots, histograms, and scatterplots are used to visualize the data relationships and distribution. 
![first chart](Assignment1_Wanat.pdf)

## Linear Regression Model
From the EDA analyis, two of the best predictor variables were each fitted in a simple linear regression model, and then combined into a multiple linear regression model.
![second chart](Assignment2_Wanat.pdf)

## Analysis of Model by Neighborhood
The models were assessed by neighborhood for accuracy. Two models were fit using the same set of predictor variables, using the response varibles of sale price and the log transformation of sale price.
![third chart](Assignment3_Wanat.pdf)

## Model Identification Utilizing Automated Variable Selection Techniques
Forward, backward and stepwise method selection techniques were used to build models. Automated variable selection techniques are useful when there is a high number of variables in a data set. Model selection was evaluated with the metic Akaike Information Criterion (AIC). All models were assessed with various metrics and compared.
![fourth chart](Assignment5_Wanat.pdf)

## Principal Component Analysis
Linear regression and PCA were used to create models using the log-returns of individual stocks to explain the variation in the log-returns of the market index.
![fifth chart](Assignment6_Wanat.pdf)

## Factor Analysis
Factor analysis was used to explain consumer preferences for nine liquors. This report utilized R Markdown, in which the document text and code are prepared in the same notebook interface. There were no points for this assignment, and the document reflects a simplified approach. 
![sixth chart](Assignment7_Wanat.pdf)

## Cluster Analysis
Cluster analysis was used on an European employment data set. There were no points for this assignment, and the document reflects instructions provided by the instructor, brief notes by me, and code output. This report utilized R Markdown, in which the document text and code are prepared in the same notebook interface.
![seventh chart](Assignment8_Wanat.pdf)
