data <- read.csv("G:/data/titanic/train.csv")
str(data)
#table(is.na(data))
#table(is.na(data$Age))
#data$Age[which(is.na(data$Age))]  <- "Unknown"


table(data=="")

table(data$Cabin=="")
data$Cabin <- as.character(data$Cabin)
data$Cabin[which(data$Cabin=="")]  <- "Unknown"
data$Cabin <- as.factor(data$Cabin)

table(data$Embarked=="")
data$Embarked <- as.character(data$Embarked)
data$Embarked[which(data$Embarked=="")]  <- "Unknown"
data$Embarked <- as.factor(data$Embarked)

data$Pclass <- factor (data$Pclass, levels = 1:3, labels = c("First", "Second", "Third"))
str(data)
_______________________
library(leaps)
r <- regsubsets(data[,2]~ data[,3]+data[,5]+data[,11]+data[,12],data,method = "backward")
summary(r)

r1 <- regsubsets(data[,2]~ data[,3]+data[,5]+data[,11]+data[,12],data,method = "forward")
summary(r1)

r2 <- regsubsets(data[,2]~ data[,6]+data[,7]+data[,8]+data[,10],data,method = "forward")
summary(r2)

r3 <- regsubsets(data[,2]~ data[,6]+data[,7]+data[,8]+data[,10],data,method = "backward")
summary(r3)

r4 <- regsubsets(data[,2]~ data[,6]+data[,7]+data[,8]+data[,10],data,method = "exhaustive")
summary(r4)
____________________



names(data)

#data <- na.omit(data)


write.csv(data,"G:/data/titanic/training_data.csv",row.names=F)


data <- read.csv("G:/data/titanic/training_data.csv")
names(data)
data <- data[,-5]
#data$Age[is.na(data$Age)] <- 0

#sampling
n <- sample(nrow(data),0.7*nrow(data))
trainx <- data[n,-2]
trainy <- data[n,2]
testx <- data[-n,-2]
testy <- data[-n,2]


#age and pclass
library(e1071)
model1 <- svm(factor(trainy)~., trainx[,-1])
pre1 <- predict(model1, testx[,-1])
t1 <- table(pre1,testy)
prop.table(t1,2)




model2 <- naiveBayes(factor(trainy)~., trainx[,-1])
pre2 <- predict(model2, testx[,-1])
t2 <- table(pre2,testy)
prop.table(t2,2)


library(randomForest)
model3 <- randomForest(factor(trainy)~., trainx[,-1])
pre3 <- predict(model3, testx[,-1])
t3 <- table(pre3,testy)
prop.table(t3,2)
