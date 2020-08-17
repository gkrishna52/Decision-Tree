## Decision Tree for Iris dataset

library(rpart)
library(rpart.plot)

v <- iris$Species

table(v)

set.seed(522)

# runif function returns a uniform distribution which can be further conditionally split into 75-25 ratio
iris[, 'train'] <- ifelse(runif(nrow(iris)) < 0.75, 1, 0)

trainSet <- iris[iris$train == 1,]
testSet <- iris[iris$train == 0, ]

trainColNum <- grep('train', names(trainSet))

trainSet <- trainSet[, -trainColNum]
testSet <- testSet[, -trainColNum]

treeFit <- rpart(Species~.,data=trainSet,method = 'class')
print(treeFit)


rpart.plot(treeFit, box.col=c("red", "green"))

Prediction1 <- predict(treeFit,newdata=testSet[-5],type = 'class')


## Print the confusion matrix to check the accuracy and other statistics
library(caret)

confusionMatrix(Prediction1,testSet$Species)

printcp(treeFit)

opt  <-  which.min(treeFit$cptable[,'xerror'])

cp <-  treeFit$cptable[opt, 'CP']
pruned_model <-  prune(treeFit,cp)
rpart.plot(pruned_model, box.col=c("red", "green"))

rpart_pruned_predict <- predict(pruned_model, newdata=testSet[-5],type = 'class')
mn2 <- mean(rpart_pruned_predict==testSet$Species)
mn2


confusionMatrix(rpart_pruned_predict,testSet$Species)
