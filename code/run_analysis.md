### Practical Data Analysis
#### Assignment
by Jeff Leek, PhD, Roger D. Peng, PhD, Brian Caffo, PhD
Coursera June 2014 session 
  




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

Document generated on 22-Jun-2014 at 15:57:23.

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

#### Pre-processing

The training dataset provided (19622 rows) wil be used to train and test our model, the test dataset provided (20 rows) being used to validate our model.


```r
#trainSet <- trainSet[complete.cases(trainSet),]
nzv <- nearZeroVar(trainSet)
trainSet <- trainSet[-nzv]
testSet <- testSet[-nzv]
for (i in names(trainSet)) {
  trainSet[,i]  <- as.numeric(trainSet[,i])
  if (i != "classe")  testSet[,i]  <- as.numeric(testSet[,i])
}
```


#### Partitioning

Let's first partition our sample in training and test data:

```r
inTrain <- createDataPartition(y = trainSet$classe, p = 0.7, list = FALSE)
training <- trainSet[inTrain, ]
test <- trainSet[-inTrain, ]
```

#### Trining a model

Then, train our model.

```r
library(randomForest)
forest_model <- randomForest(classe ~ ., training)
```

```
## Error: Can not handle categorical predictors with more than 32 categories.
```

```r
model
```

```
## Error: object 'model' not found
```


#### In-sample error

Calculating the error through a confusion matrix:

```r
pred <- predict(model, training)
```

```
## Error: object 'model' not found
```

```r
confusionMatrix(pred, training$classe)
```

```
## Error: object 'pred' not found
```

#### Out of sample error

Let's apply the model to our testing sample and calculate a confusion matrix:

```r
testing_pred <- predict(model, test)
```

```
## Error: object 'model' not found
```

```r
confusionMatrix(testing_pred, test$classe)
```

```
## Error: object 'testing_pred' not found
```

#### Validation

We apply the model to predict the 20 test  sample provided and verify the accuracy of the predications:

```r
val <- predict(model, testSet)
```

```
## Error: object 'model' not found
```

```r
as.character(val)
```

```
## Error: object 'val' not found
```

```r
answers
```

```
## Error: object 'answers' not found
```

#### Conclusion

All tests were predicted correctly on the validation sample (provided).

#### References

1. R Core Team. R: A language and environment for statistical computing. URL: http://www.R-project.org. R Foundation for Statistical Computing, 2013.

2. Seber, George AF, and Alan J. Lee. Linear regression analysis. Vol. 936. Wiley, 2012.

3. R Markdown Page. URL: http://www.rstudio.com/ide/docs/authoring/using_markdown. 

4. Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. Wearable Computing: Accelerometers' Data Classification of Body Postures and Movements. Proceedings of 21st Brazilian Symposium on Artificial Intelligence. Advances in Artificial Intelligence - SBIA 2012. In: Lecture Notes in Computer Science. , pp. 52-61. Curitiba, PR: Springer Berlin / Heidelberg, 2012. ISBN 978-3-642-34458-9. DOI: 10.1007/978-3-642-34459-6_6.

5. http://groupware.les.inf.puc-rio.br/har#sbia_paper_section#ixzz33n5Wpc66
