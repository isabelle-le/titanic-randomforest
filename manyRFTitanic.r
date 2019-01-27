# This function calls RF nbiter times for the same values of ntree and mtry
# and returns mean/std for OOB and Test accuracies
myRandomForest <- function(ntr, nbv, nbiter)
{
  accOOB <- c()
  accTest <- c()
  
  for(num in 1:nbiter)
  {
    set.seed(num)
    ind <- sample(2, nrow(train), replace=T, prob=c(0.7, 0.3)) 
    dataset.train <- train[ind==1, ] 
    dataset.test <- train[ind==2, ]

    rf <- randomForest(formula, data = dataset.train, mtry=nbv, ntree=ntr)
	conf <- rf$confusion
	acc <- (conf[1,1]+conf[2,2])/(conf[1,1]+conf[1,2]+conf[2,1]+conf[2,2])
	accOOB <- c(accOOB, acc)

	pred <- predict(rf,newdata=dataset.test)
	conf <- table(pred,dataset.test$Survived)
 	acc <- (conf[1,1]+conf[2,2])/(conf[1,1]+conf[1,2]+conf[2,1]+conf[2,2])
	accTest <- c(accTest, acc)
 }
 
 ret <- c(mean(accOOB), mean(accTest), sd(accOOB), sd(accTest))
 
 return(ret)
}


source("prepDataTitanic.r")

#Building the Rformula

library(randomForest)

ntreeTest <- c(500,1000,1500)
#ntreeTest <- c(10,20,50,100,200,300,400,500,1000,2000,3000)
#mtryTest <- seq(1,7)
mtryTest <- seq(3,5)

formula <- Survived~Pclass+Sex+Title+Family+FareGroup+Cabin2+AgeGroup

accOOB  <- c()
accTest <- c()
for(i in 1:length(ntreeTest))
{
  
  ntr <- ntreeTest[i]
  acc1 <- 0.0
  acc2 <- 0.0
  for(j in 1:length(mtryTest))
  {
    nbv <- mtryTest[j]
    res <- myRandomForest(ntr, nbv, 50)
	acc1 <- max(res[1], acc1)
	acc2 <- max(res[2], acc2)
    cat("ntree = ",ntr," mtry = ",nbv,", errors = ",acc1,",",acc2,"\n")
	
  }
  accOOB <- c(accOOB, acc1)
  accTest <- c(accTest, acc1)
}

plot(ntreeTest, accOOB, type = "n", xlim=c(0,3000),ylim=c(0.8,0.9), xlab = "ntree", ylab = "Error")
lines(ntreeTest, accOOB, col = "red")
lines(ntreeTest, accTest, col = "green")








rf <- randomForest(formula, data = dataset.train)


rf$confusion

