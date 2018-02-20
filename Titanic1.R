data <- read.csv("G:/data/titanic/train.csv")
str(data)
#table(is.na(data))
#table(is.na(data$Age))
#data$Age[which(is.na(data$Age))]  <- "Unknown"


table(data=="")

table(data$Cabin=="")
summary(data)
data$Cabin <- as.character(data$Cabin)
data$Cabin[which(data$Cabin=="")]  <- "Unknown"
data$Cabin <- as.factor(data$Cabin)

table(data$Embarked=="")
summary(data)
data$Embarked <- as.character(data$Embarked)
data$Embarked[which(data$Embarked=="")]  <- "S" # replacing with highest freq
data$Embarked <- as.factor(data$Embarked)

summary(data$Age)
data$Age[is.na(data$Age)] <- 29.7 #average value

data$Pclass <- factor (data$Pclass, levels = 1:3, labels = c("First", "Second", "Third"))
str(data)
names(data)
write.csv(data,"G:/data/titanic/train_data_cleaned.csv",row.names=F)
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
#All variables seems imp;
_______________




________________________________

train_data <- read.csv("G:/data/titanic/train_data_cleaned.csv")
names(train_data)
train_data <- train_data[,-c(1,4)]

#No sampling
#n <- sample(nrow(data),0.7*nrow(data))
trainx <- train_data[,-1]
trainy <- train_data[,1]
testx <- train_data[,-1]
testy <- train_data[,1]
names(trainx)
names(testx)
str(trainx)
#age and pclass
library(e1071)
model1 <- svm(factor(trainy)~., trainx[,-c(6,8)])
summary(model1)

pre1 <- predict(model1, testx[,-c(6,8)])
t1 <- table(pre1,testy)
prop.table(t1,2) 



model2 <- naiveBayes(factor(trainy)~., trainx[,-c(6,8)])
pre2 <- predict(model2, testx[,-c(6,8)])
t2 <- table(pre2,testy)
prop.table(t2,2)



str(trainx)
library(randomForest)
model3 <- randomForest(factor(trainy)~., trainx[,-c(6,8)],importance=T)
varImpPlot(model3)
varUsed(model3)
plot(model3)
model3$confusion
pre3 <- predict(model3, testx[,-c(6,8)])
t3 <- table(pre3,testy)
prop.table(t3,2)


___________________________________
test_data <- read.csv("G:/data/titanic/test.csv")
str(test_data)
table(is.na(test_data$Age))
summary(test_data)

test_data$Age[is.na(test_data$Age)] <- 30.27 #average value
table(is.na(test_data$Age))
summary(test_data)
w1<-which(is.na(test_data$Fare))
test_data$Fare[w1] <- 35.627 #replace with mean; 1 value
table(is.na(test_data))

table(test_data$Cabin=="")
summary(test_data)
test_data$Cabin <- as.character(test_data$Cabin)
test_data$Cabin[which(test_data$Cabin=="")]  <- "Unknown"
test_data$Cabin <- as.factor(test_data$Cabin)

test_data$Pclass <- factor (test_data$Pclass, levels = 1:3, labels = c("First", "Second", "Third"))
summary(test_data)

write.csv(test_data,"G:/data/titanic/test_data_cleaned.csv",row.names=F)

str(trainx[,-c(6,8)])
str(test_data[,-c(1,3,8,10)])
________________________



pre11 <- predict(model1, test_data[,-c(1,3,8,10)])

table(pre11)
d11 <- data.frame(test_data[,1],pre11)
names(d11) <- c("PassengerID","Survived")
write.csv(d11,"G:/data/titanic/result12.csv",row.names=F)
___

pre21 <- predict(model2, test_data[,-c(1,3,8,10)])

table(pre21)
d21 <- data.frame(test_data[,1],pre21)
names(d21) <- c("PassengerID","Survived")
write.csv(d21,"G:/data/titanic/result13.csv",row.names=F)
___________

pre31 <- predict(model3, test_data[,-c(1,3,8,10)])

table(pre31)
d31 <- data.frame(test_data[,1],pre31)
names(d31) <- c("PassengerID","Survived")
write.csv(d31,"G:/data/titanic/result22.csv",row.names=F)

