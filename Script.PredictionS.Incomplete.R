#Import Dataset
SurveyIncomplete <- read.csv("~/Desktop/UBIQUM/SurveyIncomplete.csv")

#DataCleaning
SurveyIncomplete$elevel
str(SurveyIncomplete$elevel)
SurveyIncomplete$elevel <- as.integer(SurveyIncomplete$elevel)
str(SurveyIncomplete$elevel)

SurveyIncomplete$car
str(SurveyIncomplete$car)
SurveyIncomplete$car <- as.factor(SurveyIncomplete$car)
str(SurveyIncomplete$car)

SurveyIncomplete$brand
str(SurveyIncomplete$brand)
SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)
str(SurveyIncomplete$brand)

#Prediction
BRAND <- predict(MODC5, SurveyIncomplete)
BRAND

postResample(BRAND)
str(BRAND)

#Insert the Prediction column in the SurveyIncomplete
SurveyIncomplete$brand <- BRAND
