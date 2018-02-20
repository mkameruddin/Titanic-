data <- read.csv("G:/data/titanic/train.csv")
str(data)
#table(is.na(data))
#table(is.na(data$Age))
#data$Age[which(is.na(data$Age))]  <- "Unknown"

w <- which(is.na(data$Age))
w

df<- data
str(df)
  df$Pclass <- as.factor(df$Pclass)
  df$Survived <- as.factor(df$Survived)
  df$Sex <- as.factor(df$Sex)
  
  df <- df[,c("Survived","Pclass","Sex","Age","SibSp","Parch")]
  df <- df[complete.cases(df),]
dim(df)
table(df[,1])

train_data <- df

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
model1 <- svm(factor(trainy)~., trainx)
summary(model1)

pre1 <- predict(model1, testx)
t1 <- table(pre1,testy)
prop.table(t1,2) 



model2 <- naiveBayes(factor(trainy)~., trainx)
pre2 <- predict(model2, testx)
t2 <- table(pre2,testy)
prop.table(t2,2)



str(trainx)
library(randomForest)
model3 <- randomForest(factor(trainy)~.,trainx,importance=T)
varImpPlot(model3)
varUsed(model3)
plot(model3)
model3$confusion
pre3 <- predict(model3, testx)
t3 <- table(pre3,testy)
prop.table(t3,2)


___________________________________
test_data <- read.csv("G:/data/titanic/test.csv")
str(test_data)

summary(test_data)

test_data$Age[is.na(test_data$Age)] <- 27 #average value
table(is.na(test_data$Age))
summary(test_data)
w1<-which(is.na(test_data$Fare))
test_data$Fare[w1] <- 35.627 #replace with mean; 1 value
table(is.na(test_data))

  test_data1 <- test_data[,c("Pclass","Sex","Age","SibSp","Parch")]
   test_data1$Pclass <- as.factor(test_data1$Pclass)
_____________________



pre11 <- predict(model1, test_data1)

table(pre11)
d11 <- data.frame(test_data[,1],pre11)
names(d11) <- c("PassengerID","Survived")
write.csv(d11,"G:/data/titanic/result27.csv",row.names=F)
___

pre21 <- predict(model2, test_data1)

table(pre21)
d21 <- data.frame(test_data[,1],pre21)
names(d21) <- c("PassengerID","Survived")
write.csv(d21,"G:/data/titanic/result28.csv",row.names=F)
___________

pre31 <- predict(model3, test_data1)

table(pre31)
d31 <- data.frame(test_data[,1],pre31)
names(d31) <- c("PassengerID","Survived")
write.csv(d31,"G:/data/titanic/result29.csv",row.names=F)

