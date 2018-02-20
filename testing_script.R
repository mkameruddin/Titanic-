data <- read.csv("G:/data/titanic/test.csv")
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
str(data)

data <- data[,-c(3,8,10)]
names(data)
#data <- na.omit(data)

 
write.csv(data,"G:/data/titanic/testing_data.csv",row.names=F)

