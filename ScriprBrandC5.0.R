###IMPORT DATASET
CompleteResponses <- read.csv("../Desktop/UBIQUM/CompleteResponses.csv")

###EDUCATION LEVEL AS INTEGER

CompleteResponses$elevel <- as.integer(CompleteResponses$elevel)
CompleteResponses$car <- as.factor(CompleteResponses$car)
CompleteResponses$brand <- as.factor(CompleteResponses$brand)
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)
CompleteResponses$age <- as.numeric(CompleteResponses$age)


#load library and set seed

library(caret)
library(ggplot2)

set.seed(998)

# define an 75%/25% train/test split of the dataset

inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

#10 fold cross validation

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

###TRAIN C5.0###
MODC5 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 10, list = FALSE)
print(MODC5)
str(MODC5)
################
#model  winnow  Accuracy   Kappa    
#rules  FALSE   0.8286667  0.6570877
#rules   TRUE   0.8282622  0.6521621
#tree   FALSE   0.8227445  0.6370083
#tree    TRUE   0.8224748  0.6360823
################

postResample <- postResample(PRED2, testing$brand) 
##this is to compare the testing brand preference prediction and the actual value. 

#postResample
#Accuracy     Kappa 
#0.9187551 0.8284672 

###APPLY MODEL TO TESTSET 
PRED2 <- predict(MODC5, SurveyIncomplete)
PRED2
summary(PRED2)
SurveyIncomplete$brand <- PRED2

#to ascertain how the model prioritized each feature in the training (more on this in the resour
varImp(MODC5)
head(testing)


ConfusionM <- confusionMatrix(PRED2, testing$brand)
ConfusionM
      

