### Practical Data Analysis
#### Assignment
by Jeff Leek, PhD, Roger D. Peng, PhD, Brian Caffo, PhD
Coursera June 2014 session 
  



```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
## Loading required package: lattice
## Loading required package: ggplot2
```

<style>
body {
    font-size: 1.0em;
    font-family: "Arial, Helvetica, sans-serif"";
}
</style>

#### Introduction

Portable devices can collect large amounts of data about personal activities. 

The goal of this assignment is to build a model predicting the way exercise will be performed by using data from this devices.

This analysis has been performed using R software package for statistical analysis and the following packages.
The version of R used was R version 3.1.0 (2014-04-10).

Document generated on 22-Jun-2014 at 17:06:10.

#### Loading Data

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har (section on the Weight Lifting Exercise Dataset).

Datasets:
  - https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
  - https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv



```
## The training dataset is located at ../data/train.csv and was downloaded on downloaded on 2014-Jun-05 13:11:40
```

```
## The test dataset is located at ../data/test.csv and was downloaded on downloaded on 2014-Jun-05 12:37:23
```

```
## [1] 19622   160
```

#### Pre-processing

The training dataset provided (19622 rows) wil be used to train and test our model, the test dataset provided (20 rows) being used to validate our model.

Let's remove variables not significant and transform to numeric the rest.


```r
trainSet <- subset(trainSet, select=-c(user_name,X,new_window,num_window, raw_timestamp_part_1, raw_timestamp_part_2,cvtd_timestamp))
testSet <- subset(testSet, select=-c(user_name,X,new_window,num_window, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))
#trainSet <- trainSet[complete.cases(trainSet),]

nzv <- nearZeroVar(trainSet)
trainSet <- trainSet[-nzv]
testSet <- testSet[-nzv]
ctrain <- nrow(trainSet)
for (i in names(trainSet)) {
  if (i != "classe")  {
    #cat (i, sum(is.na(trainSet[,i]))/ctrain, "\n")
    if (sum(is.na(trainSet[,i]))/ctrain > .9) { # remove col where na > 95%
      trainSet[,i] <- NULL
      testSet[,i] <- NULL
    } else {
      trainSet[,i]  <- as.numeric(trainSet[,i])
      testSet[,i]  <- as.numeric(testSet[,i])
    }
  }
}
#str(trainSet)
```


#### Partitioning

Let's first partition our sample in training and test data:

```r
inTrain <- createDataPartition(y = trainSet$classe, p = 0.7, list = FALSE)
training <- trainSet[inTrain, ]
test <- trainSet[-inTrain, ]
```

#### Training a model

Then, train our model.

```r
model <- randomForest(classe ~ ., training, ntree=200)
model
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training, ntree = 200) 
##                Type of random forest: classification
##                      Number of trees: 200
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.54%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3902    2    1    0    1    0.001024
## B   12 2643    3    0    0    0.005643
## C    0   16 2377    3    0    0.007930
## D    0    0   29 2220    3    0.014210
## E    0    0    1    3 2521    0.001584
```


#### In-sample error

Calculating the error through a confusion matrix:

```r
pred <- predict(model, training)
confusionMatrix(pred, training$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 3906    0    0    0    0
##          B    0 2658    0    0    0
##          C    0    0 2396    0    0
##          D    0    0    0 2252    0
##          E    0    0    0    0 2525
## 
## Overall Statistics
##                                 
##                Accuracy : 1     
##                  95% CI : (1, 1)
##     No Information Rate : 0.284 
##     P-Value [Acc > NIR] : <2e-16
##                                 
##                   Kappa : 1     
##  Mcnemar's Test P-Value : NA    
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    1.000    1.000    1.000    1.000
## Specificity             1.000    1.000    1.000    1.000    1.000
## Pos Pred Value          1.000    1.000    1.000    1.000    1.000
## Neg Pred Value          1.000    1.000    1.000    1.000    1.000
## Prevalence              0.284    0.193    0.174    0.164    0.184
## Detection Rate          0.284    0.193    0.174    0.164    0.184
## Detection Prevalence    0.284    0.193    0.174    0.164    0.184
## Balanced Accuracy       1.000    1.000    1.000    1.000    1.000
```

#### Out of sample error

Let's apply the model to our testing sample and calculate a confusion matrix:

```r
testing_pred <- predict(model, test)
confusionMatrix(testing_pred, test$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1671    7    0    0    0
##          B    3 1129   12    0    0
##          C    0    3 1013    8    0
##          D    0    0    1  954    3
##          E    0    0    0    2 1079
## 
## Overall Statistics
##                                         
##                Accuracy : 0.993         
##                  95% CI : (0.991, 0.995)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.992         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.998    0.991    0.987    0.990    0.997
## Specificity             0.998    0.997    0.998    0.999    1.000
## Pos Pred Value          0.996    0.987    0.989    0.996    0.998
## Neg Pred Value          0.999    0.998    0.997    0.998    0.999
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.284    0.192    0.172    0.162    0.183
## Detection Prevalence    0.285    0.194    0.174    0.163    0.184
## Balanced Accuracy       0.998    0.994    0.993    0.994    0.998
```

#### Validation

We apply the model to predict the 20 test  sample provided and verify the accuracy of the predications:

```r
testSet$pred <- predict(model, testSet)
testSet[,c("problem_id","pred")]
```

```
##    problem_id pred
## 1           1    B
## 2           2    A
## 3           3    B
## 4           4    A
## 5           5    A
## 6           6    E
## 7           7    D
## 8           8    B
## 9           9    A
## 10         10    A
## 11         11    B
## 12         12    C
## 13         13    B
## 14         14    A
## 15         15    E
## 16         16    E
## 17         17    A
## 18         18    B
## 19         19    B
## 20         20    B
```

#### Conclusion

All tests were predicted correctly on the validation sample (provided).

#### References

1. R Core Team. R: A language and environment for statistical computing. URL: http://www.R-project.org. R Foundation for Statistical Computing, 2013.

2. Seber, George AF, and Alan J. Lee. Linear regression analysis. Vol. 936. Wiley, 2012.

3. R Markdown Page. URL: http://www.rstudio.com/ide/docs/authoring/using_markdown. 

4. Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. Wearable Computing: Accelerometers' Data Classification of Body Postures and Movements. Proceedings of 21st Brazilian Symposium on Artificial Intelligence. Advances in Artificial Intelligence - SBIA 2012. In: Lecture Notes in Computer Science. , pp. 52-61. Curitiba, PR: Springer Berlin / Heidelberg, 2012. ISBN 978-3-642-34458-9. DOI: 10.1007/978-3-642-34459-6_6.

5. http://groupware.les.inf.puc-rio.br/har#sbia_paper_section#ixzz33n5Wpc66
