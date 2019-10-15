Brand Preference
================
Gherardo Lattanzi

``` r
CompleteResponses <- read.csv("../Desktop/UBIQUM/CompleteResponses.csv")
SurveyIncomplete <- read.csv("~/Desktop/UBIQUM/SurveyIncomplete.csv")
```

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(ggplot2)
```

# Data Cleaning

``` r
CompleteResponses$elevel <- as.integer(CompleteResponses$elevel)
CompleteResponses$car <- as.factor(CompleteResponses$car)
CompleteResponses$brand <- as.factor(CompleteResponses$brand)
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)
CompleteResponses$age <- as.numeric(CompleteResponses$age)


SurveyIncomplete$elevel <- as.integer(SurveyIncomplete$elevel)
SurveyIncomplete$car <- as.factor(SurveyIncomplete$car)
SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)
SurveyIncomplete$zipcode <- as.factor(SurveyIncomplete$zipcode)
SurveyIncomplete$age <- as.numeric(SurveyIncomplete$age)
```

``` r
library(ggplot2)
ggplot(CompleteResponses, aes(x=age, y=salary, col=brand)) +
  geom_point() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](MD_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
library(ggplot2)
ggplot(CompleteResponses, aes(x=age, fill=brand)) + geom_histogram(color="black", bins=10)
```

![](MD_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#With this graph we can easily see how th
```

# define an 75%/25% train/test split

``` r
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
```

# 10 fold cross validation

``` r
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
```

\#train Random Forest Regression model with a tuneLenght = 2

    ## Random Forest 
    ## 
    ## 7424 samples
    ##    6 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 1 times) 
    ## Summary of sample sizes: 6682, 6682, 6681, 6682, 6681, 6682, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.6217672  0.0000000
    ##   31    0.9144627  0.8185704
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 31.

# Apply Model to TestSet

``` r
PRED1 <- predict(M0D, testing)
summary(PRED1)
```

    ##    0    1 
    ##  932 1542

``` r
#SurveyIncomplete$brand <- PRED1
```

``` r
confusionMatrix(PRED1, testing$brand)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0  837   95
    ##          1   99 1443
    ##                                           
    ##                Accuracy : 0.9216          
    ##                  95% CI : (0.9103, 0.9319)
    ##     No Information Rate : 0.6217          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.8332          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.8295          
    ##                                           
    ##             Sensitivity : 0.8942          
    ##             Specificity : 0.9382          
    ##          Pos Pred Value : 0.8981          
    ##          Neg Pred Value : 0.9358          
    ##              Prevalence : 0.3783          
    ##          Detection Rate : 0.3383          
    ##    Detection Prevalence : 0.3767          
    ##       Balanced Accuracy : 0.9162          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

# 

# TRAIN C5.0

    ## C5.0 
    ## 
    ## 7424 samples
    ##    6 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 1 times) 
    ## Summary of sample sizes: 6681, 6683, 6681, 6681, 6681, 6682, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   model  winnow  trials  Accuracy   Kappa    
    ##   rules  FALSE    1      0.8119658  0.6291224
    ##   rules  FALSE   10      0.9207922  0.8306925
    ##   rules   TRUE    1      0.8118298  0.6283039
    ##   rules   TRUE   10      0.9201189  0.8294300
    ##   tree   FALSE    1      0.8103507  0.6215665
    ##   tree   FALSE   10      0.9197138  0.8295100
    ##   tree    TRUE    1      0.8068402  0.6086897
    ##   tree    TRUE   10      0.9206594  0.8315935
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were trials = 10, model = rules
    ##  and winnow = FALSE.

    ## List of 24
    ##  $ method      : chr "C5.0"
    ##  $ modelInfo   :List of 15
    ##   ..$ label     : chr "C5.0"
    ##   ..$ library   : chr [1:2] "C50" "plyr"
    ##   ..$ loop      :function (grid)  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 3 26 16 19 26 19 3 16
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##   ..$ type      : chr "Classification"
    ##   ..$ parameters:'data.frame':   3 obs. of  3 variables:
    ##   .. ..$ parameter: Factor w/ 3 levels "model","trials",..: 2 1 3
    ##   .. ..$ class    : Factor w/ 3 levels "character","logical",..: 3 1 2
    ##   .. ..$ label    : Factor w/ 3 levels "# Boosting Iterations",..: 1 2 3
    ##   ..$ grid      :function (x, y, len = NULL, search = "grid")  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 21 26 31 19 26 19 21 31
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##   ..$ fit       :function (x, y, wts, param, lev, last, classProbs, ...)  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 32 25 43 19 25 19 32 43
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##   ..$ predict   :function (modelFit, newdata, submodels = NULL)  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 44 29 57 19 29 19 44 57
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##   ..$ prob      :function (modelFit, newdata, submodels = NULL)  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 58 26 73 19 26 19 58 73
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##   ..$ levels    :function (x)  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 74 28 74 50 28 50 74 74
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##   ..$ predictors:function (x, ...)  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 75 32 78 19 32 19 75 78
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##   ..$ varImp    :function (object, ...)  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 79 28 79 72 28 72 79 79
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##   ..$ tags      : chr [1:7] "Tree-Based Model" "Rule-Based Model" "Implicit Feature Selection" "Boosting" ...
    ##   ..$ sort      :function (x)  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 82 26 85 19 26 19 82 85
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##   ..$ trim      :function (x)  
    ##   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 86 26 92 19 26 19 86 92
    ##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ffcb43451b8> 
    ##  $ modelType   : chr "Classification"
    ##  $ results     :'data.frame':    8 obs. of  7 variables:
    ##   ..$ model     : Factor w/ 2 levels "rules","tree": 1 1 2 2 1 1 2 2
    ##   ..$ winnow    : logi [1:8] FALSE TRUE FALSE TRUE FALSE TRUE ...
    ##   ..$ trials    : num [1:8] 1 1 1 1 10 10 10 10
    ##   ..$ Accuracy  : num [1:8] 0.812 0.812 0.81 0.807 0.921 ...
    ##   ..$ Kappa     : num [1:8] 0.629 0.628 0.622 0.609 0.831 ...
    ##   ..$ AccuracySD: num [1:8] 0.0144 0.0155 0.0129 0.0115 0.0137 ...
    ##   ..$ KappaSD   : num [1:8] 0.026 0.0285 0.0237 0.0293 0.0303 ...
    ##  $ pred        : NULL
    ##  $ bestTune    :'data.frame':    1 obs. of  3 variables:
    ##   ..$ trials: num 10
    ##   ..$ model : Factor w/ 2 levels "rules","tree": 1
    ##   ..$ winnow: logi FALSE
    ##  $ call        : language train.formula(form = brand ~ ., data = training, method = "C5.0",      trControl = fitControl, tuneLength = 2, list = FALSE)
    ##  $ dots        :List of 1
    ##   ..$ list: logi FALSE
    ##  $ metric      : chr "Accuracy"
    ##  $ control     :List of 27
    ##   ..$ method           : chr "repeatedcv"
    ##   ..$ number           : num 10
    ##   ..$ repeats          : num 1
    ##   ..$ search           : chr "grid"
    ##   ..$ p                : num 0.75
    ##   ..$ initialWindow    : NULL
    ##   ..$ horizon          : num 1
    ##   ..$ fixedWindow      : logi TRUE
    ##   ..$ skip             : num 0
    ##   ..$ verboseIter      : logi FALSE
    ##   ..$ returnData       : logi TRUE
    ##   ..$ returnResamp     : chr "final"
    ##   ..$ savePredictions  : chr "none"
    ##   ..$ classProbs       : logi FALSE
    ##   ..$ summaryFunction  :function (data, lev = NULL, model = NULL)  
    ##   ..$ selectionFunction: chr "best"
    ##   ..$ preProcOptions   :List of 6
    ##   .. ..$ thresh   : num 0.95
    ##   .. ..$ ICAcomp  : num 3
    ##   .. ..$ k        : num 5
    ##   .. ..$ freqCut  : num 19
    ##   .. ..$ uniqueCut: num 10
    ##   .. ..$ cutoff   : num 0.9
    ##   ..$ sampling         : NULL
    ##   ..$ index            :List of 10
    ##   .. ..$ Fold01.Rep1: int [1:6681] 1 2 3 4 5 6 7 8 9 10 ...
    ##   .. ..$ Fold02.Rep1: int [1:6683] 1 2 4 5 6 7 8 10 12 13 ...
    ##   .. ..$ Fold03.Rep1: int [1:6681] 1 3 4 5 7 8 9 11 12 13 ...
    ##   .. ..$ Fold04.Rep1: int [1:6681] 2 3 4 5 6 7 8 9 10 11 ...
    ##   .. ..$ Fold05.Rep1: int [1:6681] 1 2 3 4 5 6 7 9 10 11 ...
    ##   .. ..$ Fold06.Rep1: int [1:6682] 1 2 3 4 5 6 7 8 9 10 ...
    ##   .. ..$ Fold07.Rep1: int [1:6682] 1 2 3 4 5 6 8 9 10 11 ...
    ##   .. ..$ Fold08.Rep1: int [1:6681] 1 2 3 5 6 7 8 9 10 11 ...
    ##   .. ..$ Fold09.Rep1: int [1:6681] 1 2 3 4 5 6 7 8 9 10 ...
    ##   .. ..$ Fold10.Rep1: int [1:6683] 1 2 3 4 6 7 8 9 10 11 ...
    ##   ..$ indexOut         :List of 10
    ##   .. ..$ Resample01: int [1:743] 74 78 101 104 106 111 131 156 161 173 ...
    ##   .. ..$ Resample02: int [1:741] 3 9 11 17 18 26 37 46 51 79 ...
    ##   .. ..$ Resample03: int [1:743] 2 6 10 19 25 39 45 48 49 57 ...
    ##   .. ..$ Resample04: int [1:743] 1 23 24 44 47 56 94 98 102 118 ...
    ##   .. ..$ Resample05: int [1:743] 8 12 21 34 36 43 55 62 65 82 ...
    ##   .. ..$ Resample06: int [1:742] 13 29 30 31 41 50 58 72 75 76 ...
    ##   .. ..$ Resample07: int [1:742] 7 14 20 32 60 70 88 89 125 128 ...
    ##   .. ..$ Resample08: int [1:743] 4 16 22 38 67 80 81 122 130 146 ...
    ##   .. ..$ Resample09: int [1:743] 15 27 35 40 42 52 53 54 59 63 ...
    ##   .. ..$ Resample10: int [1:741] 5 28 33 64 83 87 90 95 113 114 ...
    ##   ..$ indexFinal       : NULL
    ##   ..$ timingSamps      : num 0
    ##   ..$ predictionBounds : logi [1:2] FALSE FALSE
    ##   ..$ seeds            :List of 11
    ##   .. ..$ : int [1:4] 707172 121021 869926 637489
    ##   .. ..$ : int [1:4] 282009 645742 785678 367856
    ##   .. ..$ : int [1:4] 885441 576005 631774 773497
    ##   .. ..$ : int [1:4] 921615 350596 783188 827
    ##   .. ..$ : int [1:4] 886260 573789 719767 486474
    ##   .. ..$ : int [1:4] 367001 214922 71613 212690
    ##   .. ..$ : int [1:4] 567455 261528 305215 947735
    ##   .. ..$ : int [1:4] 237305 655846 595950 849274
    ##   .. ..$ : int [1:4] 107872 347931 812584 272339
    ##   .. ..$ : int [1:4] 20125 93865 127944 295477
    ##   .. ..$ : int 623919
    ##   ..$ adaptive         :List of 4
    ##   .. ..$ min     : num 5
    ##   .. ..$ alpha   : num 0.05
    ##   .. ..$ method  : chr "gls"
    ##   .. ..$ complete: logi TRUE
    ##   ..$ trim             : logi FALSE
    ##   ..$ allowParallel    : logi TRUE
    ##  $ finalModel  :List of 21
    ##   ..$ names       : chr "| Generated using R version 3.6.1 (2019-07-05)\n| on Tue Oct 15 17:47:00 2019\noutcome.\n\noutcome: 0,1.\nsalar"| __truncated__
    ##   ..$ cost        : chr ""
    ##   ..$ costMatrix  : NULL
    ##   ..$ caseWeights : logi FALSE
    ##   ..$ control     :List of 11
    ##   .. ..$ subset         : logi TRUE
    ##   .. ..$ bands          : num 0
    ##   .. ..$ winnow         : logi FALSE
    ##   .. ..$ noGlobalPruning: logi FALSE
    ##   .. ..$ CF             : num 0.25
    ##   .. ..$ minCases       : num 2
    ##   .. ..$ fuzzyThreshold : logi FALSE
    ##   .. ..$ sample         : num 0
    ##   .. ..$ earlyStopping  : logi TRUE
    ##   .. ..$ label          : chr "outcome"
    ##   .. ..$ seed           : int 142
    ##   ..$ trials      : Named num [1:2] 10 10
    ##   .. ..- attr(*, "names")= chr [1:2] "Requested" "Actual"
    ##   ..$ rbm         : logi TRUE
    ##   ..$ boostResults:'data.frame': 10 obs. of  5 variables:
    ##   .. ..$ Trial  : num [1:10] 1 2 3 4 5 6 7 8 9 10
    ##   .. ..$ Size   : num [1:10] 9 10 7 6 10 11 12 12 12 6
    ##   .. ..$ Errors : num [1:10] 1368 680 1380 1332 751 ...
    ##   .. ..$ Percent: num [1:10] 18.4 9.2 18.6 17.9 10.1 9.4 19.8 13.1 11.8 8.1
    ##   .. ..$ Data   : chr [1:10] "Training Set" "Training Set" "Training Set" "Training Set" ...
    ##   ..$ size        : num [1:10] 9 10 7 6 10 11 12 12 12 6
    ##   ..$ dims        : int [1:2] 7424 31
    ##   ..$ call        : language (function (x, y, trials = 1, rules = FALSE, weights = NULL, control = C5.0Control(),      costs = NULL, ...)  ...
    ##   ..$ levels      : chr [1:2] "0" "1"
    ##   ..$ output      : chr "\nC5.0 [Release 2.07 GPL Edition]  \tTue Oct 15 17:47:00 2019\n-------------------------------\n\nClass specifi"| __truncated__
    ##   ..$ tree        : chr ""
    ##   ..$ predictors  : chr [1:31] "salary" "age" "elevel" "car2" ...
    ##   ..$ rules       : chr "id=\"See5/C5.0 2.07 GPL Edition 2019-10-15\"\nentries=\"10\"\nrules=\"9\" default=\"1\"\nconds=\"4\" cover=\"47"| __truncated__
    ##   ..$ xNames      : chr [1:31] "salary" "age" "elevel" "car2" ...
    ##   ..$ problemType : chr "Classification"
    ##   ..$ tuneValue   :'data.frame': 1 obs. of  3 variables:
    ##   .. ..$ trials: num 10
    ##   .. ..$ model : Factor w/ 2 levels "rules","tree": 1
    ##   .. ..$ winnow: logi FALSE
    ##   ..$ obsLevels   : chr [1:2] "0" "1"
    ##   .. ..- attr(*, "ordered")= logi FALSE
    ##   ..$ param       :List of 1
    ##   .. ..$ list: logi FALSE
    ##   ..- attr(*, "class")= chr "C5.0"
    ##  $ preProcess  : NULL
    ##  $ trainingData:'data.frame':    7424 obs. of  7 variables:
    ##   ..$ .outcome: Factor w/ 2 levels "0","1": 1 2 1 2 2 1 2 2 1 2 ...
    ##   ..$ salary  : num [1:7424] 119807 106880 78021 136459 103867 ...
    ##   ..$ age     : num [1:7424] 45 63 23 24 62 29 41 52 62 23 ...
    ##   ..$ elevel  : int [1:7424] 0 1 0 4 3 4 1 3 2 1 ...
    ##   ..$ car     : Factor w/ 20 levels "1","2","3","4",..: 14 11 15 8 3 17 5 20 6 11 ...
    ##   ..$ zipcode : Factor w/ 9 levels "0","1","2","3",..: 5 7 3 6 1 1 5 5 4 5 ...
    ##   ..$ credit  : num [1:7424] 442038 45007 48795 80501 359804 ...
    ##  $ resample    :'data.frame':    10 obs. of  3 variables:
    ##   ..$ Accuracy: num [1:10] 0.933 0.923 0.919 0.921 0.922 ...
    ##   ..$ Kappa   : num [1:10] 0.857 0.836 0.827 0.833 0.834 ...
    ##   ..$ Resample: chr [1:10] "Fold09.Rep1" "Fold01.Rep1" "Fold03.Rep1" "Fold08.Rep1" ...
    ##  $ resampledCM :'data.frame':    80 obs. of  8 variables:
    ##   ..$ model   : Factor w/ 2 levels "tree","rules": 1 1 1 1 2 2 2 2 1 1 ...
    ##   ..$ winnow  : logi [1:80] FALSE FALSE TRUE TRUE FALSE FALSE ...
    ##   ..$ trials  : num [1:80] 10 1 10 1 10 1 10 1 10 1 ...
    ##   ..$ cell1   : num [1:80] 248 263 249 266 250 263 251 266 244 266 ...
    ##   ..$ cell2   : num [1:80] 33 18 32 15 31 18 30 15 36 14 ...
    ##   ..$ cell3   : num [1:80] 25 125 27 128 26 123 29 128 33 124 ...
    ##   ..$ cell4   : num [1:80] 437 337 435 334 436 339 433 334 428 337 ...
    ##   ..$ Resample: chr [1:80] "Fold01.Rep1" "Fold01.Rep1" "Fold01.Rep1" "Fold01.Rep1" ...
    ##  $ perfNames   : chr [1:2] "Accuracy" "Kappa"
    ##  $ maximize    : logi TRUE
    ##  $ yLimits     : NULL
    ##  $ times       :List of 3
    ##   ..$ everything: 'proc_time' Named num [1:5] 35.82 0.909 37.211 0 0
    ##   .. ..- attr(*, "names")= chr [1:5] "user.self" "sys.self" "elapsed" "user.child" ...
    ##   ..$ final     : 'proc_time' Named num [1:5] 0.806 0.011 0.825 0 0
    ##   .. ..- attr(*, "names")= chr [1:5] "user.self" "sys.self" "elapsed" "user.child" ...
    ##   ..$ prediction: logi [1:3] NA NA NA
    ##  $ levels      : chr [1:2] "0" "1"
    ##   ..- attr(*, "ordered")= logi FALSE
    ##  $ terms       :Classes 'terms', 'formula'  language brand ~ salary + age + elevel + car + zipcode + credit
    ##   .. ..- attr(*, "variables")= language list(brand, salary, age, elevel, car, zipcode, credit)
    ##   .. ..- attr(*, "factors")= int [1:7, 1:6] 0 1 0 0 0 0 0 0 0 1 ...
    ##   .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. ..$ : chr [1:7] "brand" "salary" "age" "elevel" ...
    ##   .. .. .. ..$ : chr [1:6] "salary" "age" "elevel" "car" ...
    ##   .. ..- attr(*, "term.labels")= chr [1:6] "salary" "age" "elevel" "car" ...
    ##   .. ..- attr(*, "order")= int [1:6] 1 1 1 1 1 1
    ##   .. ..- attr(*, "intercept")= int 1
    ##   .. ..- attr(*, "response")= int 1
    ##   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. ..- attr(*, "predvars")= language list(brand, salary, age, elevel, car, zipcode, credit)
    ##   .. ..- attr(*, "dataClasses")= Named chr [1:7] "factor" "numeric" "numeric" "numeric" ...
    ##   .. .. ..- attr(*, "names")= chr [1:7] "brand" "salary" "age" "elevel" ...
    ##  $ coefnames   : chr [1:31] "salary" "age" "elevel" "car2" ...
    ##  $ contrasts   :List of 2
    ##   ..$ car    : chr "contr.treatment"
    ##   ..$ zipcode: chr "contr.treatment"
    ##  $ xlevels     :List of 2
    ##   ..$ car    : chr [1:20] "1" "2" "3" "4" ...
    ##   ..$ zipcode: chr [1:9] "0" "1" "2" "3" ...
    ##  - attr(*, "class")= chr [1:2] "train" "train.formula"

# Apply Model to SurveyIncomplete

``` r
PRED2 <- predict(MODC5, SurveyIncomplete)
PRED2
```

    ##    [1] 0 1 0 1 0 0 0 1 1 0 1 0 0 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 0 0 1 0 0
    ##   [35] 1 1 0 0 1 0 1 1 1 0 1 1 1 1 0 1 0 0 1 0 1 0 1 1 1 0 0 0 0 0 0 1 1 0
    ##   [69] 1 1 1 0 1 1 1 1 1 1 0 0 0 0 1 1 1 1 0 1 1 1 0 1 0 1 1 0 0 1 1 1 0 1
    ##  [103] 0 1 1 0 1 1 1 1 1 1 0 0 0 1 0 0 1 0 1 1 1 0 0 1 1 0 1 0 1 1 1 1 0 1
    ##  [137] 0 0 0 0 1 0 0 0 1 0 0 0 1 1 0 1 1 1 0 0 0 0 1 1 0 1 1 0 1 1 0 1 1 1
    ##  [171] 0 0 0 1 1 1 1 1 1 1 1 1 0 1 0 0 1 1 1 0 0 1 1 1 0 1 1 1 1 1 1 1 1 1
    ##  [205] 1 1 1 1 1 1 1 1 0 1 1 0 0 0 1 0 1 1 1 0 0 0 1 1 0 0 1 0 1 0 0 1 0 1
    ##  [239] 0 1 0 0 1 1 0 1 1 1 1 0 0 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1
    ##  [273] 1 1 0 0 0 0 0 1 0 1 0 1 0 1 1 0 1 1 1 1 0 1 0 1 0 0 0 1 0 0 1 0 1 0
    ##  [307] 1 1 0 1 0 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 1 1 1 0
    ##  [341] 0 0 1 1 1 1 0 0 1 1 1 0 1 0 0 1 0 1 1 1 1 0 1 1 1 1 0 1 0 0 1 0 1 1
    ##  [375] 1 0 1 0 1 1 0 1 1 1 0 1 0 1 0 1 1 1 0 1 1 1 0 1 0 0 1 1 1 0 0 1 1 1
    ##  [409] 0 0 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1 0 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0
    ##  [443] 0 0 0 0 1 1 1 1 0 0 1 0 1 1 1 0 0 0 1 0 0 0 1 1 0 0 1 1 1 0 1 0 1 1
    ##  [477] 0 0 1 1 0 1 1 0 1 0 0 1 0 0 1 0 0 1 1 1 1 0 0 0 0 0 1 1 1 1 1 0 0 1
    ##  [511] 1 1 1 0 1 1 1 0 1 1 1 0 1 1 0 1 1 0 1 0 1 1 1 1 1 1 0 1 0 0 1 1 0 0
    ##  [545] 0 1 0 1 1 1 1 1 0 0 1 0 1 1 1 1 1 0 0 0 1 1 1 0 0 1 1 1 1 1 1 1 0 1
    ##  [579] 0 1 1 1 0 1 1 1 1 0 1 1 1 0 1 1 1 0 0 1 0 1 1 0 1 0 0 1 0 1 1 1 0 1
    ##  [613] 1 0 1 0 1 1 0 1 1 1 1 0 1 0 0 0 1 1 1 1 1 0 1 0 0 1 1 1 0 1 0 1 1 0
    ##  [647] 0 1 0 0 0 0 0 1 0 1 1 1 1 0 1 0 1 1 0 1 1 0 1 1 1 0 1 1 0 0 1 1 0 1
    ##  [681] 1 1 0 0 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 0 1 1 1 1 1
    ##  [715] 0 1 1 0 1 1 0 1 1 0 0 1 1 1 1 1 1 0 1 0 0 0 0 0 1 1 0 1 0 1 1 1 0 0
    ##  [749] 1 0 1 0 0 1 0 0 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 0 0 0 1 1 1 1 1 0 1 1
    ##  [783] 1 1 0 1 0 0 1 0 1 0 1 1 1 0 0 1 0 1 0 1 1 1 1 1 0 0 1 1 1 1 0 0 1 0
    ##  [817] 1 1 1 1 1 0 1 1 0 1 0 0 1 0 0 1 1 1 1 1 1 1 0 1 1 1 0 1 0 1 1 1 0 0
    ##  [851] 0 0 1 0 1 1 1 1 0 1 1 0 1 1 0 1 1 1 1 0 0 1 0 1 1 1 1 0 0 0 1 0 1 1
    ##  [885] 0 1 0 1 0 0 0 1 1 0 0 0 1 1 1 0 1 0 0 1 1 1 1 1 1 0 0 1 0 1 1 1 1 0
    ##  [919] 0 1 1 1 1 1 0 0 0 1 0 1 1 1 0 1 0 0 1 1 1 0 1 1 1 1 1 0 0 0 1 0 0 0
    ##  [953] 0 1 1 0 0 1 1 0 1 1 1 0 1 1 0 0 1 1 0 1 0 0 0 1 1 0 0 1 1 0 1 1 0 0
    ##  [987] 1 0 1 0 0 1 1 1 1 0 1 1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 1 1 1 1 0 1 1
    ## [1021] 1 0 0 1 1 1 1 1 1 0 0 1 1 0 0 0 1 1 0 1 0 1 0 1 1 1 0 1 1 1 1 1 1 1
    ## [1055] 1 1 0 1 1 1 0 0 1 1 1 1 0 1 0 1 1 1 1 0 0 0 1 1 1 1 1 0 1 0 0 1 1 0
    ## [1089] 1 0 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 1 1 0 1 1 0 0 0 0 1 1 0 1 0
    ## [1123] 1 0 1 1 1 0 1 1 0 1 1 1 0 1 1 1 0 0 1 1 1 1 0 1 1 0 1 0 0 0 0 1 0 1
    ## [1157] 1 0 1 1 1 1 1 1 0 1 1 0 0 1 1 1 0 1 0 1 1 1 0 1 0 0 1 0 1 1 1 0 1 1
    ## [1191] 0 1 1 1 1 0 0 1 1 1 0 1 1 0 1 1 1 0 1 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1
    ## [1225] 0 1 1 1 0 0 1 0 0 1 1 1 1 0 0 1 0 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 0 1
    ## [1259] 1 0 0 1 1 1 1 0 1 1 1 1 1 0 0 1 0 1 1 1 0 0 1 0 1 0 1 0 1 0 1 1 1 0
    ## [1293] 1 1 1 0 1 0 1 0 1 0 1 0 0 1 1 1 0 1 1 0 1 1 0 1 1 1 1 0 0 1 1 0 1 0
    ## [1327] 0 1 1 1 0 1 0 1 0 1 0 1 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 1 1 0 0 1 1 0
    ## [1361] 1 1 1 0 0 1 0 0 0 1 0 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1
    ## [1395] 1 1 1 1 1 0 0 1 1 0 0 1 1 1 1 1 1 1 1 0 0 1 0 1 1 1 0 1 1 0 1 0 1 0
    ## [1429] 1 1 0 1 0 0 1 0 1 1 1 0 0 1 0 0 1 0 0 1 0 1 0 1 0 1 1 1 1 1 0 1 1 1
    ## [1463] 1 0 1 1 1 1 1 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 0 0 1 0 1 0 1 1 0 1
    ## [1497] 0 0 1 0 1 0 0 0 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1 1 1 1 0 0 1 0 1 1 1 1
    ## [1531] 1 0 0 1 0 1 0 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0
    ## [1565] 1 0 1 1 0 1 0 1 0 1 0 0 1 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 1 1 0 1 0 1
    ## [1599] 1 0 0 1 1 1 1 1 0 1 1 1 0 1 1 0 1 1 0 0 1 1 0 0 1 0 0 0 0 0 0 0 1 1
    ## [1633] 0 1 1 1 1 1 0 1 0 1 1 1 1 0 0 0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 0 0 1 0
    ## [1667] 1 0 1 0 1 0 1 1 1 1 1 0 1 1 0 1 1 1 0 1 1 0 0 0 1 0 0 0 1 0 0 1 0 1
    ## [1701] 1 1 1 0 0 0 1 0 1 1 1 0 1 1 1 1 0 1 0 0 0 1 1 1 1 1 1 1 0 1 0 0 1 1
    ## [1735] 1 1 0 0 0 0 0 1 1 1 0 0 1 0 0 1 0 1 0 1 1 1 1 1 0 0 0 1 1 0 1 1 1 1
    ## [1769] 1 0 0 0 0 1 1 1 1 1 0 1 1 1 0 1 0 1 0 0 0 1 1 0 1 1 0 1 1 0 1 0 1 1
    ## [1803] 1 1 1 0 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 0 1 0
    ## [1837] 1 1 1 1 0 0 1 0 1 1 0 1 1 0 0 1 1 1 0 0 1 1 0 0 1 1 0 0 1 1 1 1 1 0
    ## [1871] 0 1 0 0 0 1 0 1 1 0 1 1 1 1 0 1 1 1 0 1 0 1 0 0 1 0 1 0 1 1 0 1 1 1
    ## [1905] 1 1 0 1 1 0 1 1 0 0 0 1 0 1 0 1 0 1 1 1 1 1 0 1 1 1 1 0 0 1 1 0 1 1
    ## [1939] 0 1 0 0 1 0 1 1 0 1 0 0 1 0 1 1 1 1 1 0 1 0 0 1 1 1 0 1 1 1 1 1 1 1
    ## [1973] 1 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 1 0 0 1 0 0 1 1 1 1 1 1 1
    ## [2007] 0 0 1 1 1 0 1 1 1 1 0 0 1 1 1 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 1 1 1
    ## [2041] 1 0 1 0 0 1 1 1 1 1 0 1 0 1 0 1 1 1 0 0 1 0 0 1 0 1 0 0 1 0 0 1 1 1
    ## [2075] 0 1 0 1 0 0 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 0 0
    ## [2109] 0 1 1 1 0 0 1 0 1 0 1 1 1 1 0 0 1 0 1 0 1 1 1 1 0 1 1 1 0 0 0 1 0 1
    ## [2143] 0 0 1 1 0 1 0 1 1 1 1 1 1 0 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 1 0 1 1
    ## [2177] 0 0 1 1 1 1 1 1 1 1 0 0 0 1 0 0 0 0 0 1 1 0 1 1 1 1 1 1 1 0 1 1 1 0
    ## [2211] 0 1 1 0 1 1 0 1 1 1 0 1 1 1 1 0 1 0 1 1 0 0 1 1 1 1 0 1 1 1 0 1 1 0
    ## [2245] 1 1 0 0 1 1 1 1 0 0 1 1 0 1 1 0 1 1 1 1 1 0 0 1 0 0 1 1 1 1 1 1 1 0
    ## [2279] 0 1 0 0 1 0 1 1 1 1 0 0 1 0 1 1 0 0 1 0 1 1 1 1 0 1 0 1 0 0 1 1 0 1
    ## [2313] 1 1 1 0 1 1 1 1 1 0 0 0 0 0 0 1 1 0 1 0 0 1 0 1 0 0 0 1 0 1 1 1 1 1
    ## [2347] 0 1 0 1 1 0 1 1 0 0 1 1 1 1 1 0 0 0 0 1 0 0 1 1 1 1 1 0 1 1 1 0 0 0
    ## [2381] 0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 1 0 1 1 1 1 1 0 0 1 1 1 1 1 1 0 0 1 0
    ## [2415] 1 0 1 1 1 0 1 0 0 1 1 0 1 1 1 1 1 0 1 1 1 0 0 0 0 0 1 1 0 0 0 1 1 1
    ## [2449] 1 1 0 0 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 0 1 1 1 0 1 0
    ## [2483] 1 1 1 1 1 1 0 1 1 1 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 1 0 0 0 1 1 1
    ## [2517] 0 0 0 1 1 1 0 0 0 0 1 0 1 0 0 0 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 0 0 0
    ## [2551] 1 1 1 1 1 1 1 1 0 0 1 1 0 0 1 0 1 0 1 1 1 1 1 1 1 1 0 0 1 1 1 0 0 1
    ## [2585] 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 0 1
    ## [2619] 1 0 1 0 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 1 0 1 1 0 1 0 0 0 1 1 0 0 1 0
    ## [2653] 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 1 1 0 0 0
    ## [2687] 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 0 1 0 1 0 0 1 1 1 1 0 0 1 0 1 1 0 1 1
    ## [2721] 1 1 0 0 1 0 0 0 0 1 1 1 0 1 1 1 1 0 1 0 1 1 1 0 1 1 1 1 0 0 1 1 1 0
    ## [2755] 1 0 0 1 0 1 0 1 0 0 1 1 1 1 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 0 0 0 1 0
    ## [2789] 0 0 1 0 0 0 0 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 0 1 0 1 0 1 1
    ## [2823] 1 1 1 0 1 0 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 0 1 1 0 0 1 1 1 1 0 0 0 0
    ## [2857] 0 0 0 0 1 1 0 1 1 0 0 0 1 0 1 1 0 0 1 0 1 1 1 1 1 0 0 1 0 1 1 1 1 1
    ## [2891] 1 1 1 1 1 0 1 1 0 1 0 1 1 1 0 1 0 1 1 0 0 1 1 0 0 1 0 0 0 1 0 0 1 1
    ## [2925] 1 1 1 0 0 1 1 0 1 1 1 1 0 0 0 0 1 1 0 1 0 1 1 1 1 0 1 0 0 1 1 1 1 0
    ## [2959] 0 1 0 1 0 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 0
    ## [2993] 0 0 1 1 1 1 0 1 0 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 0 1 0 1 1 0 1 1 1 1
    ## [3027] 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 0 1 1 0 1 1 0 1 1 1 0 1 0 1 1
    ## [3061] 0 0 1 1 1 0 0 0 0 1 1 1 1 0 1 1 0 0 1 1 1 0 1 1 0 1 1 1 0 1 1 0 0 1
    ## [3095] 1 0 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 0 1 1 0 0 0 0 0 1 0
    ## [3129] 1 0 0 0 1 0 1 1 1 1 0 0 1 1 0 1 0 0 1 0 1 1 0 1 1 0 0 1 1 0 1 1 0 0
    ## [3163] 0 0 1 0 1 0 1 1 1 0 0 1 0 0 1 1 1 0 1 0 0 1 1 0 1 0 1 1 0 0 1 1 1 0
    ## [3197] 1 1 1 1 1 0 1 0 0 1 1 1 0 1 0 1 0 1 1 1 0 1 0 1 0 0 0 1 1 0 1 0 1 0
    ## [3231] 1 0 0 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 0 0 1 1 0 0 0 1 1 1 1 0 1
    ## [3265] 0 1 1 0 1 1 1 1 0 0 0 1 0 1 0 1 0 1 0 1 1 0 1 0 1 1 1 1 1 1 1 1 1 0
    ## [3299] 0 0 1 0 1 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 1 0 0 0 1 0 0 0 1 1 1 1 0 1
    ## [3333] 1 0 1 1 1 0 1 0 1 0 0 1 1 1 0 1 0 1 0 1 0 1 0 0 0 1 1 1 1 1 1 1 1 1
    ## [3367] 1 0 1 1 1 1 0 1 1 0 1 0 1 1 0 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 1 1 1 0
    ## [3401] 1 0 1 0 0 1 0 1 1 1 0 0 0 1 0 1 1 0 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1
    ## [3435] 0 0 1 1 1 1 1 0 0 1 0 1 1 0 0 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1
    ## [3469] 1 0 0 1 1 0 0 0 1 0 0 1 1 1 1 0 1 1 0 0 0 0 0 1 0 0 1 0 1 1 0 1 1 1
    ## [3503] 1 0 1 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1
    ## [3537] 1 0 0 1 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 0 1 1
    ## [3571] 0 1 0 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 0 0 1 1 1 1 1 0 0 1 0 1 1 1 1 0
    ## [3605] 0 1 1 0 1 0 1 1 1 0 0 1 1 0 0 1 1 1 0 1 0 1 1 1 1 0 1 1 1 1 1 0 1 1
    ## [3639] 1 1 1 0 1 1 1 0 1 1 0 1 0 1 1 0 0 1 1 1 1 1 0 1 0 1 1 0 0 1 1 0 1 0
    ## [3673] 1 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 0 1 1 0 1 0 1 0 0 0 1 0 0
    ## [3707] 1 1 0 1 0 1 0 1 0 1 1 1 0 0 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 0
    ## [3741] 1 1 1 1 1 0 1 1 1 0 1 1 1 0 1 0 0 1 1 1 1 1 0 1 0 0 0 0 1 0 0 1 0 1
    ## [3775] 0 0 0 0 1 1 1 1 0 1 0 1 0 0 0 1 0 1 1 1 1 0 0 1 1 0 0 1 1 0 0 1 1 1
    ## [3809] 0 1 1 0 1 1 1 0 1 1 1 0 0 0 1 1 0 1 1 1 1 1 0 1 1 1 1 1 0 0 1 0 1 0
    ## [3843] 1 0 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 0 1 1 1 0 1 1 1 0 1 0 0 0 1 0 0 1
    ## [3877] 0 0 1 1 1 0 1 0 0 0 0 0 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1
    ## [3911] 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 1 1 0 1 1 1 1 1 0 0 0 0 0 1 1 0 0 1 1
    ## [3945] 0 0 0 1 1 1 0 0 0 1 1 0 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 0 0 1 1 0 0 0
    ## [3979] 0 0 1 0 1 1 0 1 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 0 1
    ## [4013] 0 0 1 1 1 0 0 0 1 1 1 0 1 0 1 1 1 0 0 0 1 1 1 0 1 1 1 0 1 0 1 0 0 1
    ## [4047] 1 1 1 0 0 1 1 0 0 1 1 1 0 0 1 0 0 0 1 1 1 1 0 1 0 1 1 1 0 0 0 1 1 1
    ## [4081] 1 1 1 1 0 0 0 0 1 1 0 1 0 1 1 0 0 1 0 1 1 0 0 1 1 0 1 1 0 0 0 0 1 1
    ## [4115] 0 1 1 0 1 1 0 0 1 0 0 1 1 1 0 1 0 1 1 0 1 1 1 1 0 0 0 0 0 0 1 0 1 1
    ## [4149] 1 1 1 1 1 1 1 1 0 0 1 1 1 0 1 0 1 1 0 0 1 0 1 1 0 1 1 0 1 0 1 0 1 1
    ## [4183] 1 0 0 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1 0 1 1 1 0 1 0 1 1 0 0 1 1 1
    ## [4217] 1 0 1 1 1 0 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 0 0 1 1 1 1 1
    ## [4251] 0 0 0 1 0 1 1 0 0 0 1 1 1 0 1 1 1 1 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1
    ## [4285] 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 1 1 1 0 1 0 1 0 0 0 1 1
    ## [4319] 1 1 0 0 1 0 1 1 0 0 1 0 0 1 1 1 0 1 1 0 1 0 0 1 1 0 1 1 1 1 1 1 0 0
    ## [4353] 0 1 1 0 1 1 1 0 1 1 0 1 1 1 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 1 0 1 1 1
    ## [4387] 1 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 0 0 1 1 1 0 1 1 1 0 1 1 0 1 1 0 1 1
    ## [4421] 1 0 1 1 0 1 0 0 1 1 0 1 0 1 0 0 1 0 1 0 1 1 1 0 0 0 1 1 1 1 0 0 1 1
    ## [4455] 0 1 1 0 1 1 0 0 1 0 0 1 1 1 1 1 0 1 1 1 1 1 0 0 0 1 0 1 1 0 0 0 0 1
    ## [4489] 0 0 1 0 0 1 0 1 1 0 1 1 1 0 1 1 1 1 0 0 0 1 1 1 0 0 1 1 1 1 1 1 0 1
    ## [4523] 0 1 0 0 1 0 1 0 1 1 0 0 1 1 0 1 0 1 1 0 0 0 1 1 1 1 1 1 0 1 0 0 1 1
    ## [4557] 1 0 0 1 1 0 1 1 0 1 0 0 0 0 1 0 1 1 1 0 1 1 0 0 0 1 1 1 0 0 0 0 0 0
    ## [4591] 1 1 1 1 0 0 1 0 1 0 1 1 1 1 0 1 1 1 1 1 0 0 0 1 1 1 1 1 1 0 1 0 0 1
    ## [4625] 1 0 0 1 1 1 0 0 1 1 1 1 0 1 1 1 1 1 0 1 0 1 1 0 0 1 1 1 0 0 1 0 1 0
    ## [4659] 1 0 0 1 1 1 1 1 0 0 0 0 1 0 0 1 0 1 0 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1
    ## [4693] 1 0 1 1 1 1 1 0 0 1 1 1 1 0 1 0 1 0 1 1 1 1 0 1 0 1 1 1 0 1 1 1 1 1
    ## [4727] 1 0 1 0 1 1 1 1 1 0 1 0 0 0 1 1 0 0 1 1 1 0 0 1 1 1 1 0 1 1 1 1 1 0
    ## [4761] 1 1 0 0 1 1 0 0 0 1 1 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 0 1 1 1 0 1 0
    ## [4795] 1 0 0 0 0 0 1 1 0 1 0 1 1 0 1 1 0 1 1 0 1 1 1 0 0 0 0 0 1 1 1 1 0 0
    ## [4829] 1 1 1 0 1 0 0 0 1 1 0 0 1 0 1 1 0 0 0 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1
    ## [4863] 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 0 1
    ## [4897] 1 0 1 1 0 1 1 0 0 1 1 0 1 1 1 0 1 1 0 1 1 0 1 1 1 1 1 0 0 1 0 1 1 1
    ## [4931] 0 1 0 0 1 1 1 1 1 0 1 0 1 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 0 0 0 0 0 1
    ## [4965] 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0
    ## [4999] 0 0
    ## Levels: 0 1

``` r
summary(PRED2)
```

    ##    0    1 
    ## 1876 3124

# this is to compare the testing brand preference prediction and the actual value.

``` r
postResample <- postResample(PRED2, testing$brand) 
```

``` r
varImp(MODC5)
```

    ## C5.0 variable importance
    ## 
    ##   only 20 most important variables shown (out of 31)
    ## 
    ##          Overall
    ## salary    100.00
    ## age        96.23
    ## zipcode1   28.37
    ## car11      17.73
    ## car13       1.20
    ## car6        0.00
    ## car17       0.00
    ## car14       0.00
    ## zipcode5    0.00
    ## credit      0.00
    ## car10       0.00
    ## car19       0.00
    ## zipcode3    0.00
    ## zipcode8    0.00
    ## elevel      0.00
    ## car5        0.00
    ## car16       0.00
    ## car9        0.00
    ## car18       0.00
    ## car3        0.00

``` r
head(testing)
```

    ##       salary age elevel car zipcode    credit brand
    ## 4   63689.94  51      3   6       5  40888.88     1
    ## 5   50873.62  20      3  14       4 352951.50     0
    ## 6  130812.74  56      3  14       3 135943.02     1
    ## 11  63704.26  48      4  16       5 299460.23     1
    ## 12 128999.94  52      1   6       0 152232.51     0
    ## 14  82474.58  33      4  13       3 424657.50     0

# Confusion Matrix

\#Final Table of the Brand Preferences

``` r
TotData <-rbind(SurveyIncomplete, CompleteResponses)
```
