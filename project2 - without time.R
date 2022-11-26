library(tidyverse)
library(skimr)   # skimming data frames
library(ggthemes)
library(patchwork)  # combine separate ggplots into the same graphic
library(corrplot)
library(randomForest)
library(rsample)    # initial_split()
library(DescTools)  # PseudoR2()
library(sjPlot)     # tab_model(), print regression models as HTML table
library(caret)      # confusionMatrix()
library(mlr)        # Machine Learning in R (for SVM)
library(rpart)      # Recursive Partitioning and Regression Trees
library(rpart.plot)
library(ranger)     # Random forest
library(lightgbm)   # LightGBM (GBDT: gradient boosting decision tree)

data <- read_csv(file.choose(), col_types="nfnfnfnnnffnf")


set.seed(235)
#split test data
data_split <- initial_split(data)
data_test <- testing(data_split)

data_train <- training(data_split)


str(data_train)


numericVars <- select_if(data_train, is.numeric)
factorVars <- select_if(data_train, is.factor)
cat("There are ", length(numericVars),"numerical Variables and ", length(factorVars),"Categorical Variables in this dataset" )

corrplot(cor(numericVars))



Log1 = glm(DEATH_EVENT ~ age+log(ejection_fraction+1)+log(serum_creatinine+1)+sex,data=data_train, family=binomial)


summary(Log1)


testPredict = predict(Log1, newdata=data_test, type="response")

hist(sqrt(data_train$platelets))



confusion.matrix<-table(data_test$DEATH_EVENT, testPredict >= 0.388)




confusion.matrix

Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]



Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]




Accuracy.rate<-Count.correct/(Count.correct+Count.wrong)
Accuracy.rate



library(pROC)

test_prob = predict(Log1, newdata = data_test, type = "response")


test_prob

test_roc = roc(data_test$DEATH_EVENT ~ testPredict, plot = TRUE, print.auc = TRUE)

view(data_test)

rf <- randomForest(DEATH_EVENT ~ ., data=data_train, proximity=TRUE) 
print(rf)
 Call:
  randomForest(DEATH_EVENT ~age +time +ejection_fraction+serum_creatinine, data=data_train)
 library(randomForest)
 library(datasets)
 library(caret)

 rf <- ranger(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+sex+platelets,
              data = data_train,
              mtry = 2, num.trees = 500, write.forest=TRUE, importance = "permutation")
summary(rf)
rf$confusion.matrix



pred <- predict(rf, data=data_test)$predictions

confusionMatrix(pred, data_test$DEATH_EVENT, positive = "1")
