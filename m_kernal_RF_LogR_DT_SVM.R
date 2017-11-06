.VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post: Logistic Regressions and Subset Selection for the Titanic Kaggle Competition
# Source: https://www.kaggle.com/swamysm/beginners-titanic
# Modeling part
#  ------------------------------------------------------------------------

# !diagnostics off
library('caret') 
library('randomForest') 
library('rpart')
library('rpart.plot')
library('car')
library('e1071')


# Localize working data frame ---------------------------------------------
source('d_dataexploration2.R', local= TRUE)
full_titanic <- readRDS(FILE_RAW_DATA_BASE_DR2_RDS)


# Divide data into train and set for internal validation ------------------
##From the Explortory anlysis part we have decided to use below variables for our model building 
##"Pclass", "title","Sex","Embarked","FamilySized","ticket.size"
##Any redaundant varible among above will drop in the course of analysis


###lets prepare and keep data in the proper format
colToUse <- c("DataTag",
              "Survived", 
              "Pclass", "title","Sex","Embarked","FamilySized","ticket.size")
full_titanic %>% 
    filter(DataTag==DATA_TAG_TRAIN) %>%     
    select(colToUse) ->
    feauter1

response <- feauter1$Survived


###For Cross validation purpose will keep 20% of data aside from my orginal train set
##This is just to check how well my data works for unseen data
## 714:177
set.seed(500)
ind=createDataPartition(feauter1$Survived,times=1,p=0.8,list=FALSE)
train_val=feauter1[ind,]
test_val=feauter1[-ind,]


####check the proprtion of Survival rate in orginal training data, current traing and testing data
round(prop.table(table(feauter1$Survived)*100), digits = 1)
round(prop.table(table(train_val$Survived)*100),digits = 1)
round(prop.table(table(test_val$Survived)*100),digits = 1)


# Predictive Analysis and Cross Validation 
# Decison tree ------------------------------------------------------------
set.seed(1234)
Model_DT=rpart(Survived~.,
               data=train_val, 
               method="class")

rpart.plot(Model_DT, extra = 3, fallen.leaves = T)
###Surprise, Check out the plot,  our Single tree model is using only Title, Pclass and Ticket.size and vomited rest
###Lets Predict train data and check the accuracy of single t
PRE_TDT=predict(Model_DT, data=train_val, type="class")
confusionMatrix(PRE_TDT,train_val$Survived, positive = "1")
#####Accuracy is 0.8375
####Not at all bad using Single tree and just 3 feauters


##There is chance of overfitting in Single tree, So I will go for cross validation using '10 fold techinque'
set.seed(1234)
cv.10 <- createMultiFolds(train_val$Survived, k = 10, times = 10)

# Control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)
##Train the data
colForModel <- c("Pclass", "title", "Sex", "Embarked", "FamilySized", "ticket.size")
Model_CDT <- train(x = as.data.frame(train_val[,colForModel]), 
                   y = train_val$Survived,
                   method = "rpart", tuneLength = 30,
                   trControl = ctrl)

##Check the accurcay
##Accurcay using 10 fold cross validation of Single tree is 0.8139 
##Seems Overfitted earlier using Single tree, there our accurcay rate is 0.8319

# check the variable imporatnce, is it the same as in Single tree?
rpart.plot(Model_CDT$finalModel,extra = 3,fallen.leaves = T)
##Yes, there is no change in the imporatnce of variable


###Lets cross validate the accurcay using data that kept aside for testing purpose
PRE_VDTS=predict(Model_CDT$finalModel,
                 newdata=test_val, type="class")
confusionMatrix(PRE_VDTS,test_val$Survived)
###There it is, How exactly our train data and test data matches in accuracy (0.8192)


# my Evaluation
# full_titanic %>% 
#     filter(DataTag==DATA_TAG_TEST) ->
#     realTest
# SeanTest <- predict(Model_CDT$finalModel,
#                     newdata=realTest, type="class")
# evaluateAccuracy(realTest$PassengerId, SeanTest)
# 325/418 0.777



# Random Forest -----------------------------------------------------------
##Random forest is for more better than Single tree however single tree is very easy to use and illustrate
set.seed(1234)
colForModel <- c("Pclass", "title", "Sex", "Embarked", "FamilySized", "ticket.size")
rf.1 <- randomForest(x = as.data.frame(train_val[,colForModel]),
                     y=train_val$Survived, 
                     importance = TRUE, 
                     ntree = 1000)
rf.1
varImpPlot(rf.1)
####Random Forest accurcay rate is 82.91 which is 1% better than the decison tree

####Lets remove 2 redaundant varibles and do the modeling again
train_val1=subset(train_val,,-c(Embarked, FamilySized, DataTag))
test_val1=subset(test_val,,-c(Embarked, FamilySized, DataTag))

#colForModel <- c("Pclass", "title", "Sex", "ticket.size")
#colForModel <- c("Pclass", "title", "Sex", "FamilySized") # get same result

set.seed(1234)
rf.2 <- randomForest(x = as.data.frame(train_val1[,-Survived]),
                     y=train_val1$Survived, 
                     importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
###Can see the Magic now, increase in accuracy by just removing 2 varibles, accuracy now is 84.03 


##Even though random forest is so power full we accept the model only after cross validation
set.seed(2348)
cv10_1 <- createMultiFolds(train_val$Survived, k = 10, times = 10)

# Set up caret's trainControl object per above.
ctrl_1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv10_1)

set.seed(1234)
rf.5<- train(x = as.data.frame(train_val[,colForModel]),
             y=train_val$Survived, 
             method = "rf", tuneLength = 3,
             ntree = 1000, trControl =ctrl_1)

rf.5
pr.rf=predict(rf.5, newdata = test_val[,colForModel])
confusionMatrix(pr.rf,test_val$Survived)
####accuracy rate is 0.8192, lower than what we have expected  


# my Evaluation
# full_titanic %>%
#     filter(DataTag==DATA_TAG_TEST) ->
#     realTest
# SeanTest <- predict(rf.5, newdata = realTest)
# evaluateAccuracy(realTest$PassengerId, SeanTest)
# 325/418 0.777512



## Support Vector Machine
# Linear Support vector Machine -------------------------------------------
###Before going to model lets tune the cost Parameter
set.seed(1274)
liner.tune=tune.svm(Survived~.,
                    data=as.data.frame(train_val1), 
                    kernel="linear",
                    cost=c(0.01,0.1,0.2,0.5,0.7,1,2,3,5,10,15,20,50,100))

liner.tune

## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost
##     3
## 
## - best performance: 0.1736502
###best perforamnce when cost=3 and accuracy rate is 82.7


###Lets get a best.liner model  
best.linear=liner.tune$best.model

##Predict Survival rate using test data
best.test=predict(best.linear, 
                  newdata=as.data.frame(test_val1), 
                  type="class")
confusionMatrix(best.test,test_val1$Survived)


# my Evaluation
# full_titanic %>%
#     filter(DataTag==DATA_TAG_TEST) %>% 
#     select(PassengerId, Pclass, title, Sex, ticket.size) %>% 
#     as.data.frame() ->
#     realTest
# SeanTest <- predict(best.linear, 
#                     newdata = realTest, 
#                     type="class")
# evaluateAccuracy(realTest$PassengerId, SeanTest)
# 313/418 0.7488038


# Radial Support vector Machine -------------------------------------------
######Lets go to non liner SVM, Radial Kerenl
set.seed(1274)
rd.poly=tune.svm(Survived~.,data=train_val1,kernel="radial",gamma=seq(0.1,5))

summary(rd.poly)

best.rd=rd.poly$best.model

###Non Linear Kerenel giving us a better accuray 

##Lets Predict test data
pre.rd=predict(best.rd,newdata = test_val1)
confusionMatrix(pre.rd,test_val1$Survived)
####Accurcay of test data using Non Liner model is 0.81
####it could be due to we are using smaller set of sample for testing data


## my Evaluation
# full_titanic %>%
#     filter(DataTag==DATA_TAG_TEST) %>%
#     select(PassengerId, Pclass, title, Sex, ticket.size) %>%
#     as.data.frame() ->
#     realTest
# SeanTest <- predict(best.rd,
#                     newdata = realTest,  type="class")
# evaluateAccuracy(realTest$PassengerId, SeanTest)
# 325/418 0.777512


# Logistic Regression -----------------------------------------------------
contrasts(train_val1$Sex)
contrasts(train_val1$Pclass)
##The above shows how the varible coded among themself

##Lets run Logistic regression model
log.mod <- glm(Survived ~ ., family = binomial(link=logit), 
               data = train_val1)
###Check the summary
summary(log.mod)
confint(log.mod)


###Predict train data
train.probs <- predict(log.mod, data=train_val1,type =  "response")
table(train_val1$Survived,train.probs>0.5)


###Logistic regression predicted train data with accuracy rate of 0.83 
test.probs <- predict(log.mod, newdata=test_val1, type =  "response")
table(test_val1$Survived,test.probs>0.5)

confusionMatrix(as.integer(test.probs>0.5), 
                test_val1$Survived, 
                positive = "1")

## my Evaluation
# full_titanic %>%
#     filter(DataTag==DATA_TAG_TEST) %>%
#     select(PassengerId, Pclass, title, Sex, ticket.size) %>%
#     as.data.frame() ->
#     realTest
# SeanTest <- predict(log.mod,
#                     newdata = realTest,  type="response")
# evaluateAccuracy(realTest$PassengerId, SeanTest)
# 260/418 0.622


# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), .VAR_BEFORE))
rm(.VAR_BEFORE)