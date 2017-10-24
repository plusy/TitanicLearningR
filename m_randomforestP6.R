VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post 6 -------------------------------------------------------------
# Predicting Titanic deaths on Kaggle IV: random forest revisited 
# Source: http://wiekvoet.blogspot.com/2015/08/predicting-titanic-deaths-on-kaggle-iv.html
# Impute missing Age by Random forest -----------------------------------------------------
#  ------------------------------------------------------------------------

# !diagnostics off
library(randomForest)
library(ipred)
library(lattice)


# Localize working data frame ---------------------------------------------
df_working <- getCleanBaseDf()


colForAgeImputation <- c('Age','Sex',
                         'Pclass','Fare_Adj_MedianOfTrain','Name_TitleSimple',
                         'SibSp','Parch',
                         'Cabin_NumberOE', 'Cabin_A', 'Cabin_B','Cabin_C','Cabin_D','Cabin_E','Cabin_F',
                         'Embarked','Ticket_PC','Ticket_STON')

df_working %>% 
    filter(DataTag==DATA_TAG_TRAIN & !is.na(Age)) %>%     
    select(colForAgeImputation) %>% 
    as_data_frame() ->
    forage


# select hyperparamter
# crossing(mtry=4:7, nodesize=3:6) %>%
#     rowwise() %>% 
#     mutate(errro = errorest(Age ~.,
#                             mtry=mtry,
#                             nodesize=nodesize,
#                             model=randomForest,
#                             data=forage)$error) ->
#     sla
# 
# xyplot(errro ~ mtry, 
#        data=sla, groups= nodesize, 
#        auto.key=TRUE, type='l')

# Yang: The result is not statble but usrally mtry=4 give better result
# chosen m=5,n=4 in OP
rfa1 <- randomForest(Age ~ .,
                     data=forage,
                     mtry=4,
                     nodesize=6, 
                     ntree=1000,
                     importance = TRUE)

plot(df_working$Age,predict(rfa1,df_working))
abline(a=0,b=1,col='red')

# compare to age imputed by 35
# age1 <- df_working$Age
# age1[is.na(age1)] <- 0
# age2 <- df_working$Age_ImputeBy35
# plot(age1, age2)
# abline(a=0,b=1,col='red')
# cor.test(age1, age2)   #0.62
# cor.test(age1, predict(rfa1,df_working)) #0.52
# plot(age1, predict(rfa1,df_working))
# abline(a=0,b=1,col='red')


df_working %>%  
    mutate(Age_ImputeByRF = if_else(is.na(Age),
                                    predict(rfa1, .),
                                    Age)) %>% 
    mutate(Age_ImputeByRF_Bin=cut(Age_ImputeByRF, AGE_BIN_CUTOFF)) -> 
    df_working


# Split Train and Test ---------------------------------------------------
# table(df_working$DataTag)
train <- df_working %>% filter(DataTag==DATA_TAG_TRAIN)
test <- df_working %>% filter(DataTag==DATA_TAG_TEST)


colModeling <- c('Survived',
                 'Age_ImputeByRF_Bin', 'Age_ImputeByRF', 
                 'Sex',
                 'Pclass', 'SibSp', 
                 'Parch', 'Fare_Adj_MedianOfTrain',
                 'Name_TitleSimple','Embarked', 
                 'Cabin_A', 'Cabin_B','Cabin_C','Cabin_D','Cabin_E','Cabin_F', 
                 'Cabin_Length', 'Cabin_NumberOE',
                 'Ticket_PC','Ticket_STON')

train %>% 
    select(colModeling) -> 
    forSurf


# Model selection: mtry and nodesize -------------------------------------------------------
# crossing(m=6:9, n=3:7) %>%
#     rowwise() %>% 
#     mutate(ee = errorest(Survived ~.,
#                          mtry=m,
#                          nodesize=n,
#                          model=randomForest,
#                          data=forSurf,
#                          ntree=1000,
#                          est.para=control.errorest(k=20))$error) ->
#     sla
# 
# xyplot(data=sla,
#        ee ~ m, groups= n, 
#        auto.key=TRUE, type='l')
# choose mtry = 8, node = 7

# model evaluation: cutoff interval ---------------------------------------------
rfx <- randomForest(Survived ~.,
                    data=forSurf,
                    nodesize=7,
                    mtry=8,
                    ntree=1000)

pp <- predict(rfx, type='prob')
#evaluateROC(forSurf$Survived, pp[,2])

densityplot(~pp[,1] | forSurf$Survived, adj=.3)

cuts <- seq(.2, .7, .001)
y <- map_int(cuts,
             ~ sum(as.numeric(pp[,1]<.x)==forSurf$Survived))
plot(y, x=cuts)
# choose 0.25 ~ 0.65 as candidates
# pp <- predict(rfx, test, type='prob')
# evaluateAccuracy(test$PassengerId, 
#                  pp[,2]>0.55)


# Model evaluation: cutoff -----------------------------------------------------
# cuts <- seq(.25,.65,.001) 
# cutsLength <- length(cuts)
# 
# crossing(nodesize=seq(4,100,8), mtry=seq(2,8,2), count=1:10) %>% 
#     rowid_to_column(var = 'i') %>% 
#     by_row(.to = 'nestedCol',
#            function(aRow){
#                rfx <- randomForest(Survived ~.,
#                                    data=forSurf,
#                                    nodesize=aRow$nodesize,
#                                    mtry=aRow$mtry,
#                                    ntree=1000)
#                
#                pp <- predict(rfx,type='prob')
#                
#                return(data.frame(nerr = map_int(cuts, 
#                                                 ~ sum(as.numeric(pp[,1]<.x)==forSurf$Survived)),
#                                  cuts = cuts))
#            }) %>%  
#     unnest() -> 
#     sach 

# FILE_CP_SACH_RDATA <- file.path(DIR_MIDPUT, 'cp_sach.RDS')
# FILE_CP_SACH_CSV   <- file.path(DIR_MIDPUT, 'cp_sach.csv')
# 
# saveRDS(sach, file = FILE_CP_SACH_RDATA)
# sach <- readRDS(FILE_CP_SACH_RDATA)
# write_csv(sach, FILE_CP_SACH_CSV)

# # nerr should be correct predictions which is a indicator of accuracy (oppssit to error)
# xyplot(nerr ~ cuts | nodesize + mtry ,
#        group=i, 
#        data= sach,
#        auto.key =FALSE, type='l')



# biased prediction -------------------------------------------------------
# chose cuts at .55
# twpred <- function(object, newdata=NULL) {
#     pp <- predict(object, newdata, type='prob')
#     return(as.numeric(pp[,1]<0.55), levels=c('0','1'))
# }
# 
# crossing(nodesize=seq(2,30,4), mtry=seq(2,8,2), count=1:10) %>% 
#     rowwise() %>% 
#     mutate(error = errorest(Survived ~.,
#                             mtry=mtry,
#                             nodesize=nodesize,
#                             model=randomForest,
#                             data=forSurf,
#                             predict=twpred,
#                             ntree=500,
#                             est.para=control.errorest(k=10))$error) ->
#     sla2


# FILE_CP_SLA2_RDATA <- file.path(DIR_MIDPUT, 'cp_sla2.RDS')
# FILE_CP_SLA2_CSV   <- file.path(DIR_MIDPUT, 'cp_sla2.csv')
# 
# write_csv(sla2, FILE_CP_SLA2_CSV)
# 
# saveRDS(sla2, file = FILE_CP_SLA2_RDATA)
# sla2 <- readRDS(FILE_CP_SLA2_RDATA)

# xyplot(ee ~ factor(mtry) | factor(nodesize),
#        groups = count,
#        data = sla2,
#        auto.key=FALSE, type='l')
# select mtry=8, nodesize=6


# Final preidction --------------------------------------------------------
rf2 <-randomForest(Survived ~ .,
                   data=forSurf,
                   replace=TRUE,
                   ntree=2000,
                   nodesize=6,
                   mtry=8)  
<<<<<<< HEAD:m_randomforestP6.R

pp <- predict(rf2, test, type='prob')
# best I can get mtry =8, node = 6, cutoff 0.75, 78.6%=327/418
evaluateROC(test$PassengerId,
            pp[,2])

out <- data.frame(PassengerId=test$PassengerId,
                  Survived=as.integer(pp[,2]>=0.75), 
=======
pp <- predict(rf2, test)
out <- data.frame(PassengerId=test$PassengerId,
                  Survived=(pp==1), 
>>>>>>> c4978355d8d88075fe2623e572fe1fad46af5884:m_randomforest.R
                  row.names=NULL)
evaluateAccuracy(out$PassengerId ,
                 out$Survived)
# get a result
# Your submission scored 0.75598

<<<<<<< HEAD:m_randomforestP6.R
# FILE_OUT_RF_P6_FINAL_CSV <- file.path(DIR_MIDPUT, 'RFP6_final_result.csv')
=======
# FILE_OUT_RF_FINAL_CSV <- file.path(DIR_MIDPUT, 'RF_final_result.csv')
>>>>>>> c4978355d8d88075fe2623e572fe1fad46af5884:m_randomforest.R
# write.csv(x=out,
#           file=FILE_OUT_RF_P6_FINAL_CSV,
#           row.names=FALSE,
#           quote=FALSE)

<<<<<<< HEAD:m_randomforestP6.R

# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), VAR_BEFORE))
=======
# get a result
# Your submission scored 0.75598
evaluateAccuracy(out$PassengerId ,
                 out$Survived)


# best I can get mtry =8, node = 6, cutoff 0.75, 327/78.6%
# pp <- predict(rf2, test, type='prob')
# evaluateROC(test$PassengerId, 
#             pp[,2])
# evaluateAccuracy(test$PassengerId, 
#                  pp[,2]>0.75)
>>>>>>> c4978355d8d88075fe2623e572fe1fad46af5884:m_randomforest.R
