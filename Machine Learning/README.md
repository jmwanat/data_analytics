## Machine Learning

This survey course covered a lot of information:

1. Exploring and visualizing data
2. Evaluating Classification Models
3. Evaluating Regression Models
4. Random Forests
5. Principal Component Analysis
6. Neural Networks
7. Deep Learning: Image processing with a CNN
8. Deep Learning: Language modeling with an RNN

Python was used throughout the course. 

### Assignments

1. Exploring and Visualizing Data: This was a data exploration of survey data from the Northwestern University Master’s in Predictive Analytics program to determine programming 
language and software interest.

2. Evaluating Classification Models: Two classification methods (logistic regression and naïve Bayes) were used to create models from a telephone marketing campaign by a Portuguese 
bank. The client characteristics were used to identify clients that are more likely to invest in term deposits offered by the bank. Models were evaluated for accuracy, sensitivity,
specificity, precision, F1 score, class error, and false positive rate. Cross-validation of the training data was used and scored by ROC-AUC (receiver operating characteristic 
curve – area under the curve).

3. Evaluating Regression Models: A Boston housing study collected various variables to assess the market value of residential real estate and predict the median value of homes. Data 
was split into a 75:25 train:test sets. Four different algorithms were used to create models for comparison: (1) linear regression; (2) ridge regression; (3) Lasso; and 
(4) Elastic Net. 

4. Random Forests: This utilized the same data set (Boston housing) used in evaluating regression models. Two new ensemble methods were created and compared against the regression 
models: (1) Random Forests; (2) Gradient Boosting.

5. Principal Component Analysis: The MNIST (Modified National Institute of Standards and Technology) dataset contains 70,000 small images of handwritten digits. Each digit image is 
labeled with the digit it represents. The dataset is used for machine learning classification algorithms. Principal Component Analysis (PCA) was used to reduce the dimensionality 
and determine the proportion of the variance explained by each feature.

6. Neural Networks: The MNIST dataset was used again, but this time neural nets in TensorFlow were used to classify the data in a 2x2 completely crossed design benchmark experiment. 
The accuracy between two and five hidden layers were evaluated for accuracy and processing time. Then a TensorFlow premade estimator called a DNN Classifier with two hidden layers 
was evaluated with different numbers of nodes on each layer, and Keras was used to create two models.

7. Deep Learning - Image processing with a CNN: A dogs and cats dataset by Microsoft Research was used to evaluate the classification of images with the use of a convolutional 
neural network (CNN) within Python TensorFlow. The images were split into training and test sets, and a 2x2 completely crossed design benchmark experiment was conducted to 
evaluate two and five convolutional layers for accuracy and processing time. This project emphasized that models should be tested multiple times to account and assess for 
different local optima.

8. Deep Learning - Language modeling with an RNN: Movie reviews, 500 positive and negative each, were used to train language models for predicting sentiment (positive versus 
negative). Recurrent neural networks (RNNs) were utilized in Python TensorFlow to evaluate language models developed with pre-trained word vectors. A 2x2 completely crossed design 
benchmark experiment was conducted to evaluate the number of dimensions and vocabulary size of global vectors (GloVe) developed by individuals at Stanford University. Model 
performance improved with a vocabulary size of 100,000 words using a Gated Recurrent Unit (GRU) cell. Models should be tested multiple times to account and assess for different 
local optima.

