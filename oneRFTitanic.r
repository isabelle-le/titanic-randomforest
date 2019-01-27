source("prepDataTitanic.r")

#Building the Rformula

ind <- sample(2, nrow(train), replace=T, prob=c(0.7, 0.3)) 
dataset.train <- train[ind==1, ] 
dataset.test <- train[ind==2, ]

formula <- Survived~Pclass+Sex+Title+Family+FareGroup+Cabin2+AgeGroup

library(randomForest)

# the default value is not the best in all the problem, you ve to find the best value
rf <- randomForest(formula, data = dataset.train, mtry = 2, ntree = 1000, importance= TRUE)
print(rf)
rf$importance[,2] # tell the importance feature to measure the classY (survived and not survived)

pred <- predict(rf,newdata=dataset.test)
confTest <- table(pred,dataset.test$Survived)
confTest

confOOB <- rf$confusion
confOOB
accTest <- (confTest[1,1]+confTest[2,2])/(confTest[1,1]+confTest[1,2]+confTest[2,1]+confTest[2,2])
accOOB <- (confOOB[1,1]+confOOB[2,2])/(confOOB[1,1]+confOOB[1,2]+confOOB[2,1]+confOOB[2,2])
cat("ntree = ",rf$ntree," mtry = ",rf$mtry,", AccuracyOOB = ",accOOB,", AccTest = ",accTest,"\n")

#function calls RF nbiter times for the same values of ntrees and mtry,
#return the mean/std OOB and test

library(randomForest)
formula <- Survived~Pclass+Sex+Title+Family+FareGroup+Cabin2+AgeGroup
myRandomForest = function(ntr, nbv, nbiter) {
  accOOB = c()
  accTest = c()
  for (num in 1 : nbiter){
    set.sed(num)
ind <- sample(2, nrow(train), replace=T, prob=c(0.7, 0.3)) 
dataset.train <- train[ind==1, ] 
dataset.test <- train[ind==2, ]

rf <- randomForest(formula, data = dataset.train, mtry = nbv, ntree= ntr) # importance = TRUE
}
confOOB <- rf$confusion
acc <- (confOOB[1,1]+confOOB[2,2])/(confOOB[1,1]+confOOB[1,2]+confOOB[2,1]+confOOB[2,2])
accOOB <- c(accOOB, acc)

pred <- predict(rf,newdata=dataset.test)
confTest <- table(pred,dataset.test$Survived)
accTest <- (confTest[1,1]+confTest[2,2])/(confTest[1,1]+confTest[1,2]+confTest[2,1]+confTest[2,2])
cat("ntree = ",rf$ntree," mtry = ",rf$mtry,", AccuracyOOB = ",accOOB,", AccTest = ",accTest,"\n")
