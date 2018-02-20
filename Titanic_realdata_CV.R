data <- read.csv("G:/data/titanic/train.csv")
ti <- read.csv("G:/data/titanic/titanic3.csv")
str(ti)
n <- names(data)[-1]

ti1<- ti[,-c(12,13,14)]
str(ti1)
summary(ti1)
ti1[ti1$name == "Connolly, Miss. Kate",]
ti1[ti1$name == "Kelly, Mr. James",]
data[data$Name == "Connolly, Miss. Kate",]
data[data$Name == "Kelly, Mr. James",]
ti2<- ti1[-c(925,727),]
summary(ti2)
str(ti2)


s <-subset(ti2, ti2$name %in% data$Name)
w1<- which( ti2$name %in% data$Name )
w1
str(s)
s1<- s[order(s$name),]
data1<- data[order(data$Name),]
str(s1)
str(data1)
table(s1[,2] == data1[,2])
__________

s2 <- ti2[-w1,]
s3 <- rbind(ti1[c(925,727),],s2)
str(s3)
s3$name <- factor(s3$name)
s4 <- s3[order(s3$name),]
str(s4)
test_data <- read.csv("G:/data/titanic/test.csv")
test_data1 <- test_data[order(test_data$Name),]
str(test_data1)
s4$name <-as.character(s4$name)
test_data1$Name <-as.character(test_data1$Name)
table(s4$name == test_data1$Name)

s5 <-subset(s4, (s4$name %in% test_data1$Name))
str(s5)
test_data2 <- test_data1[ which( test_data1$Name %in% s4$name ),]
str(test_data2)
table(s5$name == test_data2$Name)
test_data3 <- data.frame(test_data2,s5[,2])
str(test_data3)
names(test_data3)[12]<-"Survived"
table(test_data3[,12])
test_data4 <- test_data1[-which( test_data1$Name %in% s4$name ),]
str(test_data4)
test_data4$Survived <- 0
test_data5 <- rbind(test_data3,test_data4)
str(test_data5)
table(test_data5[,12])

d31 <- data.frame(test_data5[,1],test_data5[,12])
names(d31) <- c("PassengerID","Survived")
write.csv(d31,"G:/data/titanic/result35.csv",row.names=F)



str(test_data1)
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

