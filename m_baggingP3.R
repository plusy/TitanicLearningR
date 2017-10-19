VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post 3 -------------------------------------------------------------
# Predicting Titanic deaths on Kaggle III: Bagging  
# Source: http://wiekvoet.blogspot.com/2015/08/predicting-titanic-deaths-on-kaggle-iii.html
#  ------------------------------------------------------------------------

# !diagnostics off
library(ipred)
library(rpart)
library(lattice)


# Localize working data frame ---------------------------------------------
df_working <- DF_Data_0_ALL


# Age ---------------------------------------------------------------------
colForAgeImputation <- c('Age','Sex',
                         'Pclass','SibSp','Parch','Fare_Adj_MedianOfAll',
                         'Name_TitleSimple','Embarked',
                         'Cabin_A', 'Cabin_B','Cabin_C','Cabin_D','Cabin_E','Cabin_F',
                         'Cabin_Length', 'Ticket_PC','Ticket_STON', 'Cabin_NumberOE')
df_working %>% 
    filter(DataTag==DATA_TAG_TRAIN & !is.na(Age)) %>%     
    select(colForAgeImputation) %>% 
    as_data_frame() ->
    forage


ipbag1 <- bagging(Age ~., data=forage)
ipbag1

plot(df_working$Age~predict(ipbag1,df_working))


# Impute and Split data into train/test ----------------------------------------------
df_working %>%  
    mutate(Age_ImputeByBagging = if_else(is.na(Age),
                                         predict(ipbag1, .),
                                         Age)) %>% 
    mutate(Age_ImputeByBagging_Bin=cut(Age_ImputeByBagging, AGE_BIN_CUTOFF)) -> 
    df_working

train <- df_working %>% filter(DataTag==DATA_TAG_TRAIN)
test <- df_working %>% filter(DataTag==DATA_TAG_TEST)


# Selecting the survival model  ------------------------------------------------------------------------
colForModeling <- c('Age_ImputeBy35_Bin','SibSp', 'Parch', 'Fare_Adj_MedianOfAll','Sex','Pclass',
                    'Name_TitleSimple','Embarked',
                    'Cabin_A', 'Cabin_B','Cabin_C','Cabin_D','Cabin_E','Cabin_F',
                    'Cabin_Length', 'Ticket_PC','Ticket_STON', 'Cabin_NumberOE',
                    'Age_ImputeByBagging',
                    'Survived')
train %>%
    filter(DataTag==DATA_TAG_TRAIN) %>% 
    select(colForModeling) %>% 
    as_data_frame() ->
    di1

# every iteration 3 minutes: 9*1*6=54*3=150/60=2.5 hours
crossing(ns=seq(100,300,25), nbagg=c(500), minsplit=1:6) %>%
    rowwise() %>%
    mutate(error = errorest(Survived ~ .,
                            ns=ns,
                            control=rpart.control(minsplit=minsplit, cp=0, 
                                                  xval=0,maxsurrogate=0),
                            nbagg=nbagg,
                            model=bagging,
                            data=di1,
                            est.para=control.errorest(k=20))$error) ->
    las

xyplot(error ~ ns, groups= minsplit,
       data=las, 
       auto.key=TRUE, type='l')


# Predictions -------------------------------------------------------------
# nbag size=250, minsplit = 5, the same result of 275/5
bagmod <- bagging(Survived ~.,
                  ns=250,nbagg=500,
                  control=rpart.control(minsplit=5, cp=0, xval=0,maxsurrogate=0),
                  data=di1)

pp <- predict(bagmod,test)
out <- data.frame(PassengerId=test$PassengerId,
                  Survived=pp,
                  row.names=NULL)
evaluateAccuracy(out$PassengerId ,
                 out$Survived)

FILE_OUT_BAGGING_P3_FINAL_CSV <- file.path(DIR_MIDPUT, 'RFP3_final_result.csv')
write.csv(x=out,
          file=FILE_OUT_BAGGING_P3_FINAL_CSV,
          row.names=FALSE,
          quote=FALSE)


# play around
# pp <- predict(bagmod, test, type='prob')          
# evaluateROC(test$PassengerId,
#             pp[,2])
# # threshold  accuracy AUC
# # 0.359000 0.7822967 0.8278
# 
# out <- data.frame(PassengerId=test$PassengerId,
#                   Survived = as.integer(pp[,1] <= 0.5),
#                   row.names=NULL)
# evaluateAccuracy(out$PassengerId ,
#                  out$Survived)

                    


# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), VAR_BEFORE))