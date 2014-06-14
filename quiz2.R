library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
str(training)
str(testing)
library(Hmisc)
str(mixtures$CompressiveStrength[inTrain])

index = createDataPartition(mixtures$CompressiveStrength, p = 3/4)
str(mixtures)
index <- as.factor(inTrain)
str(index)
str(inTrain)
sum(is.na(mixtures$CompressiveStrength))

featurePlot(x=training, y=training$CompressiveStrength)
q=qplot (inTrain, training$CompressiveStrength)
q=qplot (FlyAsh, CompressiveStrength , data=training)
qplot (inTrain, training$CompressiveStrength, color=training$Cement)
qplot (inTrain, training$CompressiveStrength, color=training$BlastFurnaceSlag)
qplot (inTrain, training$CompressiveStrength, color=training$Water)
qplot (inTrain, training$CompressiveStrength, color=training$Superplasticizer)
qplot (inTrain, training$CompressiveStrength, color=training$FineAggregate)
qplot (inTrain, training$CompressiveStrength, color=training$CoarseAggregate)
qplot (inTrain, training$CompressiveStrength, color=training$Age)
q=qplot (inTrain, training$CompressiveStrength, color=training$FlyAsh)
q + geom_smooth(method=lm,formula=y~x)


library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

str(training)
histogram(training$Superplasticizer)
histogram(log(training$Superplasticizer+1))

summary(training$Superplasticizer)
summary(log(training$Superplasticizer))
summary(log(training$Superplasticizer+1))


set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

n <- names(training)[substr(names(training),1,2) == "IL"]
prcomp(training[,n])
preProcess(training[,n],method="pca",thresh=.9)

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

n <- names(training)[substr(names(training),1,2) == "IL"]
t <- training[,c("diagnosis",n)]
str(t)
pp <- preProcess(training[,n],method="pca",thresh=.9)
tt <- predict(pp, training[,n])
ff <- train(training$diagnosis ~ ., method="glm",data=tt)
testPC <- predict(pp, testing[,n])
confusionMatrix(testing$diagnosis, predict(ff,testPC))

m1 <- train(training$diagnosis ~ ., method="glm", data=t)
confusionMatrix(testing$diagnosis, predict(m1,testing[,n]))

warnings()