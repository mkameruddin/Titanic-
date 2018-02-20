
training_data <- read.csv("G:/data/titanic/training_data.csv")
testing_data <- read.csv("G:/data/titanic/testing_data.csv")

training_data <- training_data[,-5]
testing_data <- testing_data[,-4]


w <- which(training_data$Embarked=="Unknown")
training_data <- training_data[-w,]
training_data$Embarked <- factor(training_data$Embarked)


trainx <- training_data[,-2]
trainy <- training_data[,2]
testx <- testing_data
testx[is.na(testx)] <- 0

#age and pclass
library(e1071)
model1 <- svm(factor(trainy)~., trainx[,-1])
pre1 <- predict(model1, testx[,-1])
table(pre1)
d1 <- data.frame(testx[,1],pre1)
names(d1) <- c("PassengerID","Survived")
write.csv(d1,"G:/data/titanic/result1.csv",row.names=F)




model2 <- naiveBayes(factor(trainy)~., trainx[,-1])
pre2 <- predict(model2, testx[,-1])
table(pre2)
d2 <- data.frame(testx[,1],pre2)
names(d2) <- c("PassengerID","Survived")
write.csv(d2,"G:/data/titanic/result2.csv",row.names=F)


library(randomForest)
model3 <- randomForest(factor(trainy)~., trainx[,-1])
pre3 <- predict(model3, testx[,-1])
table(pre3)
d3 <- data.frame(testx[,1],pre3)
names(d3) <- c("PassengerID","Survived")
write.csv(d3,"G:/data/titanic/result3.csv",row.names=F)
