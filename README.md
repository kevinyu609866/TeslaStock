# TeslaStock

The dataset used in this project can be found at https://finance.yahoo.com/quote/TSLA/history?period1=1413158400&period2=1602547200&int
erval=1d&filter=history&frequency=1d&includeAdjustedClose=true.

In part 1,we explored using KNN to predict the class(a day's stock price movement, denoted as "Up", "Down", or "Stable (within -0.6% to + .6%))
We further analyzed which predictors were most prominent in predicting the days movement. 
Topics covered in this section: KNN, visualization with histograms and line graphs, Welch two sample t-tests, and predictions.

High prediction accuracies were founded because of the formula used in the model. That is, because some of the features included in the data used for training, the model was 
able to correctly predict the class of each days movement.

In Part 2, we explore using principal component analysis, Kmeans, random forest, and SVM to predict a day's stock price movement based on the PREVIOUS days predictors.
That is, we are essentially testing how helpful having a previous days data would be in predicting a days stock price movement. 
Topics explored in this section: PCA, 3d plotting, K-means clustering, gini impurity, random sampling and SMOTE, Kolomogorov-Smirnov tests, random forests, and SVM.
