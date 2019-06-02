# installing ml package
install.packages("randomForest")
library(randomForest)

# Setting the work directory
setwd("C:/Users/nader/Desktop/titanic")

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

# categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

# Cleaning missing values of Fare
Fare.q3 <- boxplot.stats(titanic.full$Fare)$stats[5]
Fare.filter <- titanic.full$Fare < Fare.q3 
Fare.equation <- "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked "
#Fare.formula <- as.formula(Fare.equation)
Fare.model <- lm(
  formula = Fare.equation ,
  data = titanic.full[Fare.filter , ]
)

Fare.model.missing <- titanic.full[
  is.na(titanic.full$Fare) , 
  c("Pclass","Sex","Age","SibSp","Parch","Embarked")
  ]

Fare.prediction <- predict(Fare.model , newdata = Fare.model.missing)
titanic.full[is.na(titanic.full$Fare) , "Fare" ] <- Fare.prediction


# Cleaning missing values of Age
Age.q3 <- boxplot.stats(titanic.full$Age)$stats[5]
Age.filter <- titanic.full$Age < Age.q3
Age.equation <- "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked "
Age.model.missing <- titanic.full [
  is.na(titanic.full$Age) , 
  c("Pclass" , "Sex" , "SibSp" , "Parch" , "Fare" , "Embarked")
  ]

Age.model <- lm (
  formula = Age.equation , 
  data = titanic.full[ Age.filter & !is.na(titanic.full$Age) , ]
  )

Age.prediction <- predict(Age.model , newdata = titanic.full[ is.na(titanic.full$Age) , ])
Age.prediction <- abs(as.integer(Age.prediction))
titanic.full[is.na(titanic.full$Age) , "Age"] <- Age.prediction


# replacing age null values with median
#age.median <- median(titanic.full$Age , na.rm = TRUE)
#titanic.full[is.na(titanic.full$Age) , "Age"] <- age.median

# Splitting the data again
titanic.train <- titanic.full[titanic.full$isTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$isTrainSet==FALSE,]

#reforming survived only in training set
titanic.train$Survived <- as.factor(titanic.train$Survived)


# Bıilding formula for predicition
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked "
survived.formula <- as.formula(survived.equation)


# building the predictive model
titanic.model <- randomForest(formula = survived.formula , data = titanic.train , ntree = 500 , mtry = 3 , nodesize = 0.01 * nrow(titanic.test)  ) 

#specvifying features 
features.equation <- " Pclass + Sex + Age + SibSp + Parch + Fare + Embarked "

#prediction 
survived <- predict(titanic.model , newdata = titanic.test)
PassengerId <- titanic.test$PassengerId

output.df <- as.data.frame(PassengerId) 
output.df$Survived <- survived

write.csv(output.df , "kaggle_submission.csv" , row.names = FALSE )
