VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post 1 -------------------------------------------------------------
# Predicting Titanic deaths on Kaggle  ------------------------------------
# http://wiekvoet.blogspot.com/2015/07/predicting-titanic-deaths-on-kaggle.html
#  ------------------------------------------------------------------------
# !diagnostics off
library(randomForest)


# Localize working data frame ---------------------------------------------
df_working <- DF_Data_0_ALL


# A simple prediction using randomForest ----------------------------------
colForSimpleModel <- c('DataTag',
                       'Survived',
                       'Sex', 'Pclass', 'SibSp',
                       'Parch','Fare',
                       'Embarked','Age_ImputeBy35_Bin',
                       'Cabin_A', 'Cabin_B','Cabin_C','Cabin_D', 'Cabin_NumberOE')

df_working %>% 
    filter(DataTag==DATA_TAG_TRAIN) %>%     
    select(colForSimpleModel) %>% 
    select(-DataTag) %>% 
    mutate(train = sample(c(TRUE,FALSE),
                          size=n(),
                          prob = c(0.9, 0.1),
                          replace=TRUE)) ->
    titanic


rfFor <- as.formula('Survived ~ Sex + Pclass + SibSp +
                    Parch + Fare +
                    Embarked + Age_ImputeBy35_Bin +
                    Cabin_A + Cabin_B + Cabin_C + Cabin_D + Cabin_NumberOE')
rf1 <- randomForest(rfFor,
                    data = titanic,
                    subset = train,
                    replace = FALSE,
                    ntree = 1000)
rf1

# Acurracy on validation set: (1-0.15)
titanic$pred <- predict(rf1,titanic)
with(titanic[!titanic$train,],
     sum(pred!=Survived)/length(pred))


# Tune hyperparameter -----------------------------------------------------------
# crossing(mtry=2:5, nodesize=c(2,4,6), wt=seq(.5,.7,.05)) %>%
#     by_row(.to = 'pw',
#            function(aRow){
#                rfx <- randomForest(rfFor,
#                                    data=titanic,
#                                    subset=train,
#                                    replace=TRUE,
#                                    ntree=4000,
#                                    nodesize=aRow$nodesize,
#                                    mtry=aRow$mtry,
#                                    classwt=c(1-aRow$wt,
#                                              aRow$wt))  
#            
#                 preds <- predict(rfx,titanic[!titanic$train,])
#                 return(sum(preds!=titanic$Survived[!titanic$train])/
#                        length(preds))
#             })->
#     unnest() -> 
#     tsa
# 
# xyplot(pw ~ wt | mtry, group=factor(nodesize), 
#        data=tsa,
#        auto.key=TRUE, type='l')
#select mtry=5, node = 4, wt=0.5


# Final predictions -------------------------------------------------------
rf2 <- randomForest(rfFor,
                    data=titanic,
                    replace=TRUE,
                    ntree=5000,
                    nodesize=4,
                    mtry=5,
                    classwt=c(.4,.6))  

df_working %>% 
    filter(DataTag==DATA_TAG_TEST) %>%     
    select(c('PassengerId', colForSimpleModel)) %>% 
    select(-DataTag) ->
    test

pp <- predict(rf2, test, type='prob')
evaluateROC(test$PassengerId,
            pp[,2])

out <- data.frame(PassengerId=test$PassengerId,
                  Survived=as.integer(pp[,2]>=0.455), 
                  row.names=NULL)
evaluateAccuracy(out$PassengerId ,
                 out$Survived)
# Your submission scored 0.7751196  324/418
# hypers in OP can also get 0.77

FILE_OUT_RF_P1_FINAL_CSV <- file.path(DIR_MIDPUT, 'RFP1_final_result.csv')
write.csv(x=out,
          file=FILE_OUT_RF_P1_FINAL_CSV,
          row.names=FALSE,
          quote=FALSE)


# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), VAR_BEFORE))
