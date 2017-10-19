VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post 2 -------------------------------------------------------------
# Predicting Titanic deaths on Kaggle III: gbm  
# Source: http://wiekvoet.blogspot.com/2015/07/predicting-titanic-deaths-on-kaggle-ii.html
#  ------------------------------------------------------------------------

# !diagnostics off
library(gbm)
set.seed(4321)


# Localize working data frame ---------------------------------------------
df_working <- DF_Data_0_ALL


# Age ---------------------------------------------------------------------
colForAgeImputation <- c('Age','SibSp','Parch','Fare_Adj_MedianOfAll','Sex','Pclass',
                         'Name_TitleSimple','Embarked',
                         'Cabin_A', 'Cabin_B','Cabin_C','Cabin_D','Cabin_E','Cabin_F',
                         'Cabin_Length', 'Ticket_PC','Ticket_STON', 'Cabin_NumberOE')

df_working %>% 
    filter(DataTag==DATA_TAG_TRAIN & !is.na(Age)) %>%     
    select(colForAgeImputation) %>% 
    as_data_frame() ->
    forage

rfa1 <- gbm(Age ~ ., 
            data=forage,
            interaction.depth=4,
            cv.folds=10,
            n.trees=8000,
            shrinkage=0.0005,
            n.cores=2)

gbm.perf(rfa1, plot.it = TRUE, oobag.curve = TRUE)

# Impute and Split data into train/test ----------------------------------------------
df_working %>%  
    mutate(Age_ImputeByGBM = if_else(is.na(Age),
                                     predict(rfa1, ., n.trees=7612),
                                     Age)) %>% 
    mutate(Age_ImputeByGBM_Bin=cut(Age_ImputeByGBM, AGE_BIN_CUTOFF)) -> 
    df_working

test <- df_working %>% filter(DataTag==DATA_TAG_TEST)

df_working %>% 
    filter(DataTag==DATA_TAG_TRAIN) %>% 
    mutate(train = sample(c(TRUE,FALSE),
                          size=891,
                          replace=TRUE, 
                          prob=c(.9,.1)) ) ->
    titanic


# Survival ------------------------------------------------------------------------
colForModeling <- c('Age_ImputeBy35_Bin', 'SibSp', 'Parch', 
                    'Fare_Adj_MedianOfAll', 'Sex', 'Pclass',
                    'Name_TitleSimple', 'Embarked',
                    'Cabin_A', 'Cabin_B','Cabin_C','Cabin_D','Cabin_E','Cabin_F',
                    'Cabin_Length', 'Ticket_PC', 'Ticket_STON', 'Cabin_NumberOE',
                    'Age_ImputeByGBM',
                    'Survived')

titanic %>% 
    filter(train) %>% 
    select(colForModeling) %>% 
    mutate(Survived=c(0,1)[Survived]) %>% # not integer or factor but float
    as_data_frame() ->
    gb1

#table(gb1$Survived)
gb1m <- gbm(Survived ~ .,
             cv.folds=11,
             n.cores=2,
             interaction.depth=5,
             shrinkage = 0.0005,
             distribution='adaboost',
             data=gb1,
             n.trees=10000)
gbm.perf(gb1m)


preds <- predict(gb1m, titanic,
                 n.trees=7000, type='response')
density(preds) %>% plot

# find best cutoff
preds2<- preds[!titanic$train]
target <- c(0,1)[titanic$Survived[!titanic$train]]
sapply(seq(.3,.7,.01), 
       function(step) c(step,sum(ifelse(preds2<step,0,1)!=target)))

# extra code for cutoff
cuts <- seq(.3, .7, .01)
mislabelled <- map_int(cuts,
                       ~ sum(as.numeric(preds2>.x)!=target))
plot(mislabelled, x=cuts)

evaluateROC(titanic$PassengerId[!titanic$train],
            preds2)


# Predictions -------------------------------------------------------------
pp <- predict(gb1m, test, n.trees=7000, type='response')

evaluateROC(test$PassengerId, pp)

out <- data.frame(PassengerId=test$PassengerId,
                  Survived=ifelse(pp<0.6, 0, 1), 
                  row.names=NULL)
evaluateAccuracy(out$PassengerId ,
                 out$Survived)

FILE_OUT_GBM_P2_FINAL_CSV <- file.path(DIR_MIDPUT, 'GBMP2_final_result.csv')
write.csv(x=out,
          file=FILE_OUT_GBM_P2_FINAL_CSV,
          row.names=FALSE,
          quote=FALSE)


# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), VAR_BEFORE))