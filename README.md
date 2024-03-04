# Machine-Learning-Project
```
library(caret); library(randomForest); library(rpart); library(RColorBrewer); library(rattle); library(ggplot2);library(lattice)

## reading data, adding NA for missing info
training <- read.csv("C:/Users/admin/Downloads/pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing <- read.csv("C:/Users/admin/Downloads/pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

## removing the columns with NAs 
training <- training[,colSums(is.na(training))==0]
testing <- testing[,colSums(is.na(testing))==0]

## removing the columns 1-7 with data not used
training <- training[,-c(1:7)]
testing <- testing[,-c(1:7)]

## making "classe" the factor
training$classe <- as.factor(training$classe)

#################################################################################

set.seed(12345)
## split training data into training and validation parts
trainingset <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
trainingNew <- training[trainingset, ]
validationNew <- training[-trainingset, ]
####################################################################################

plot(as.factor(trainingNew$classe), col="lightblue", main = "Classe distribution in training data set", xlab="classe", ylab="Frequency")

##################################################################################
## Desision Tree Model
model1 <- rpart(classe ~ ., data=trainingNew, method="class")
prediction1 <- predict(model1, validationNew, type = "class")

cm1 <- confusionMatrix(prediction1, validationNew$classe)
cm1

##################################################################################
# Random Forest Model
model2 <- randomForest(classe ~. , data=trainingNew, method="class")
prediction2 <- predict(model2, validationNew, type = "class")
cm2 <- confusionMatrix(prediction2, validationNew$classe)
cm2

##################################################################################
finalpredict <- predict(model2, testing, type="class")
finalpredict
