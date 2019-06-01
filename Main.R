# Setting the work directory
setwd("C:/Users/nader/Desktop/kaggle")

# get the data from the CSV files with no factors
titanic.train <-  read.csv(file= "train.csv" , stringsAsFactors = FALSE ,header = TRUE)
titanic.test  <-  read.csv(file="test.csv"   , stringsAsFactors = FALSE ,header = TRUE)

# in order to be able to seperate tables after merge we need a new column
titanic.train$isTrainSet <- TRUE
titanic.test$isTrainSet <- FALSE

# in order to merge two dataset the colums should be lined up
titanic.test$Survived <- NA

# merging
titanic.full <- rbind(titanic.train , titanic.test)

# replacing Embarked null values with the MOD
titanic.full[titanic.full$Embarked == '' , "Embarked"] <- 'S'

# replacing age null values with median
age.median <- median(titanic.full$Age , na.rm = TRUE)
titanic.full[is.na(titanic.full$Age) , "Age"] <- age.median

# replacing fare null values with median
fare.median <- median(titanic.full$Fare , na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare) , "Fare"] <- fare.median


# categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

# Splitting the data again
titanic.train <- titanic.full[titanic.full$isTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$isTrainSet==FALSE,]

#reforming survived only in training set
titanic.full$Survived <- as.factor(titanic.full$Survived)


# Bıilding formula for predicition
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked "
survived.formula <- as.formula(survived.equation)

# installing ml package
install.packages("randomForest")
library(randomForest)

# building the predictive model
titanic.model <- randomForest(formula = survived.formula , data = titanic.train , ntree = 500 , mtry = 3 , nodesize = 0.01 * nrow(titanic.test)  ) 

#specvifying features 
features.equation <- " Pclass + Sex + Age + SibSp + Parch + Fare + Embarked "
