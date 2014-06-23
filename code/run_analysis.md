### Practical Data Analysis
#### Assignment
by Jeff Leek, PhD, Roger D. Peng, PhD, Brian Caffo, PhD
Coursera June 2014 session 
  



```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
## Loading required package: lattice
## Loading required package: ggplot2
## 
## Attaching package: 'pander'
## 
## The following object is masked from 'package:knitr':
## 
##     pandoc
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

Document generated on 22-Jun-2014 at 17:04:48.

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
str(trainSet)
```

```
## 'data.frame':	19622 obs. of  53 variables:
##  $ roll_belt           : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt          : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt            : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt    : num  3 3 3 3 3 3 3 3 3 3 ...
##  $ gyros_belt_x        : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y        : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z        : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x        : num  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y        : num  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z        : num  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x       : num  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y       : num  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z       : num  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm            : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm           : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm             : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm     : num  34 34 34 34 34 34 34 34 34 34 ...
##  $ gyros_arm_x         : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y         : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z         : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x         : num  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y         : num  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z         : num  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x        : num  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y        : num  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z        : num  516 513 513 512 506 513 509 510 518 516 ...
##  $ roll_dumbbell       : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell      : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell        : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ total_accel_dumbbell: num  37 37 37 37 37 37 37 37 37 37 ...
##  $ gyros_dumbbell_x    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ gyros_dumbbell_y    : num  -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 ...
##  $ gyros_dumbbell_z    : num  0 0 0 -0.02 0 0 0 0 0 0 ...
##  $ accel_dumbbell_x    : num  -234 -233 -232 -232 -233 -234 -232 -234 -232 -235 ...
##  $ accel_dumbbell_y    : num  47 47 46 48 48 48 47 46 47 48 ...
##  $ accel_dumbbell_z    : num  -271 -269 -270 -269 -270 -269 -270 -272 -269 -270 ...
##  $ magnet_dumbbell_x   : num  -559 -555 -561 -552 -554 -558 -551 -555 -549 -558 ...
##  $ magnet_dumbbell_y   : num  293 296 298 303 292 294 295 300 292 291 ...
##  $ magnet_dumbbell_z   : num  -65 -64 -63 -60 -68 -66 -70 -74 -65 -69 ...
##  $ roll_forearm        : num  28.4 28.3 28.3 28.1 28 27.9 27.9 27.8 27.7 27.7 ...
##  $ pitch_forearm       : num  -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.8 -63.8 -63.8 ...
##  $ yaw_forearm         : num  -153 -153 -152 -152 -152 -152 -152 -152 -152 -152 ...
##  $ total_accel_forearm : num  36 36 36 36 36 36 36 36 36 36 ...
##  $ gyros_forearm_x     : num  0.03 0.02 0.03 0.02 0.02 0.02 0.02 0.02 0.03 0.02 ...
##  $ gyros_forearm_y     : num  0 0 -0.02 -0.02 0 -0.02 0 -0.02 0 0 ...
##  $ gyros_forearm_z     : num  -0.02 -0.02 0 0 -0.02 -0.03 -0.02 0 -0.02 -0.02 ...
##  $ accel_forearm_x     : num  192 192 196 189 189 193 195 193 193 190 ...
##  $ accel_forearm_y     : num  203 203 204 206 206 203 205 205 204 205 ...
##  $ accel_forearm_z     : num  -215 -216 -213 -214 -214 -215 -215 -213 -214 -215 ...
##  $ magnet_forearm_x    : num  -17 -18 -18 -16 -17 -9 -18 -9 -16 -22 ...
##  $ magnet_forearm_y    : num  654 661 658 658 655 660 659 660 653 656 ...
##  $ magnet_forearm_z    : num  476 473 469 469 473 478 470 474 476 473 ...
##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
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
model <- randomForest(classe ~ ., training, ntree=50)
model
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training, ntree = 50) 
##                Type of random forest: classification
##                      Number of trees: 50
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.83%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3903    2    0    0    1    0.000768
## B   18 2623   13    3    1    0.013168
## C    0   22 2368    5    1    0.011686
## D    3    1   33 2213    2    0.017318
## E    1    0    1    7 2516    0.003564
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
##          A 1670    8    0    0    0
##          B    4 1127   11    0    0
##          C    0    4 1014    7    0
##          D    0    0    1  955    3
##          E    0    0    0    2 1079
## 
## Overall Statistics
##                                         
##                Accuracy : 0.993         
##                  95% CI : (0.991, 0.995)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.991         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.998    0.989    0.988    0.991    0.997
## Specificity             0.998    0.997    0.998    0.999    1.000
## Pos Pred Value          0.995    0.987    0.989    0.996    0.998
## Neg Pred Value          0.999    0.997    0.998    0.998    0.999
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.284    0.192    0.172    0.162    0.183
## Detection Prevalence    0.285    0.194    0.174    0.163    0.184
## Balanced Accuracy       0.998    0.993    0.993    0.995    0.998
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
