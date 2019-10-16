#### IMPORT DATASET #### 
CompleteResponses <- read.csv("../Desktop/UBIQUM/CompleteResponses.csv")

# DataCleaning

CompleteResponses$elevel <- as.integer(CompleteResponses$elevel)
CompleteResponses$car <- as.factor(CompleteResponses$car)
CompleteResponses$brand <- as.factor(CompleteResponses$brand)
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)
CompleteResponses$age <- as.numeric(CompleteResponses$age)

#load library and set seed

library(caret)
set.seed(998)

# define an 75%/25% train/test split of the dataset

inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

#10 fold cross validation

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest Regression model with a tuneLenght = 2
#(trains with 1 mtry value for RandomForest)#

M0D <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 2, list = FALSE )
###M0D <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 2, list = FALSE )
### The Accuracy and the Kappa returned by using tunelenght = 10 does not provide significantly better outputs. 

save(M0D, file = "dataMOD.RData")

print(M0D)
#  7424 samples
#  6 predictor
#  2 classes: '0', '1' 
  
#  Accuracy   Kappa    
#  0.9220084  0.8349224
  
varImp(M0D)


###APPLY MODEL TO TESTSET 

PRED1 <- predict(M0D, testing)
PRED1
summary(PRED1)

postResample <- postResample(PRED1, testing$brand) 
postResample

#to ascertain how the model prioritized each feature in the training 
varImp(M0D)

head(testing)

confusionMatrix(PRED1, testing$brand)

TotData <-rbind(SurveyIncomplete, CompleteResponses)
TotData


