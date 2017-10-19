VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post 5 -------------------------------------------------------------
# Predicting Titanic deaths on Kaggle V: Ranger
# Source: http://wiekvoet.blogspot.com/2015/09/predicting-titanic-deaths-on-kaggle-v.html
#  ------------------------------------------------------------------------

# !diagnostics off
library(ranger)
library(lattice)
library(latticeExtra)
library(ipred) # has cross validation


# Localize working data frame ---------------------------------------------
df_working <- DF_Data_0_ALL

df_working%>% 
    mutate(Embarked_Ranger = ifelse(Embarked =='N/A',
                                    'S',
                                    Embarked)) %>%     
    mutate(Name_TitleSimple_Ranger = ifelse(Name_TitleSimple =='Dr' & Sex == 'female',
                                            'Miss',
                                            Name_TitleSimple)) %>% 
    mutate(Cabin_Ranger = str_sub(Cabin,1,1),
           Cabin_Ranger = if_else(Cabin_Ranger %in% c('F','G','T'),  'X' , Cabin_Ranger),
           Cabin_Ranger = factor(Cabin_Ranger)) %>% 
    mutate(Ticket_Ranger = str_replace_all(Ticket, '[[:digit:]]+$',''),
           Ticket_Ranger = str_replace_all(Ticket_Ranger, '(\\.)|( )|(/)',''),
           Ticket_Ranger = str_to_upper(Ticket_Ranger),
           Ticket_Ranger = if_else(Ticket_Ranger %in% c('A2','A4','AQ3','AQ4','AS'),'An', Ticket_Ranger),
           Ticket_Ranger = if_else(Ticket_Ranger %in% c('SCA3','SCA4','SCAH','SC','SCAHBASLE','SCOW'), 'SC', Ticket_Ranger),
           Ticket_Ranger = if_else(Ticket_Ranger %in% c('CASOTON','SOTONO2','SOTONOQ'), 'SOTON', Ticket_Ranger),
           Ticket_Ranger = if_else(Ticket_Ranger %in% c('STONO2','STONOQ'),'STON', Ticket_Ranger),
           Ticket_Ranger = if_else(Ticket_Ranger %in% c('C'), 'CA', Ticket_Ranger),
           Ticket_Ranger = if_else(Ticket_Ranger %in% c('SOC','SOP','SOPP'), 'SOP', Ticket_Ranger),           
           Ticket_Ranger = if_else(Ticket_Ranger %in% c('SWPP','WC','WEP'), 'W', Ticket_Ranger),           
           Ticket_Ranger = if_else(Ticket_Ranger %in% c('FA','FC','FCC'), 'F', Ticket_Ranger),                      
           Ticket_Ranger = if_else(Ticket_Ranger %in% c('PP','PPP','LINE','LP','SP'), 'PPPP', Ticket_Ranger),
           Ticket_Ranger = factor(Ticket_Ranger)) ->
    df_working


# Age Section -------------------------------------------------------------
colForAgeImputation <- c('Age','Sex', 'Pclass', 'SibSp', 'Parch',
                         'Fare_Adj_MedianOfTrain', 
                         'Name_TitleSimple_Ranger',
                         'Embarked_Ranger', 
                         'Cabin_Ranger', 'Cabin_Length', 
                         'Ticket_Ranger')
                         
# get an age without missings
df_working %>% 
    filter(DataTag==DATA_TAG_TRAIN & !is.na(Age)) %>%     
    select(colForAgeImputation) %>% 
    as_data_frame() ->
    forage
# oe is side of vessel, not relevant for age?
 
# crossing(mtry=1:4, min.node.size=1:11,rep=1:10) %>%
#     rowwise() %>%
#     mutate(error = errorest(Age ~.,
#                             mtry=mtry,
#                             min.node.size= min.node.size,
#                             model=ranger,
#                             predict=function(object,newdata) { predict(object, data=newdata)$predictions},
#                             write.forest=TRUE,
#                             data=forage)$error) ->
#     sla
# 
#  
# useOuterStrips(densityplot(~ error | factor(mtry)+factor(min.node.size), 
#                data=sla))
 
 
 # 2,7?
 rfa1 <- ranger(Age ~ .,
                data=forage,
                mtry=2,
                write.forest=TRUE,
                min.node.size=7)


# Impute and Split data into train/test ----------------------------------------------
df_working %>%  
    mutate(Age_ImputeByRanger = if_else(is.na(Age),
                                         predict(rfa1,.)$predictions,
                                         Age))  -> 
    df_working
 
#table(df_working$Age_ImputeByRanger)
 
train <- df_working %>% filter(DataTag==DATA_TAG_TRAIN)
test <- df_working %>% filter(DataTag==DATA_TAG_TEST)
 

# model selection 1 -----------------------------------------------------------------------
colModeling <- c('Survived',
                 'Age_ImputeByRanger', 'Sex',
                 'Pclass', 'SibSp', 'Parch', 'Fare_Adj_MedianOfTrain',
                 'Name_TitleSimple_Ranger', 'Embarked_Ranger', 
                 'Cabin_Length', 
                 'Cabin_Ranger', 
                 'Ticket_Ranger',
                 'Cabin_NumberOE')

train %>% 
    select(colModeling) -> 
    forSurf

# crossing(mtry=2:5,rep=1:10,
#          min.node.size=c(1:4,seq(6,12,2),15,20,25)) %>%
#     rowwise() %>%
#     mutate(error = errorest(Survived ~.,
#                              mtry=mtry,
#                              min.node.size=min.node.size,
#                              model=ranger,
#                              predict=function(object,newdata) {predict(object,data=newdata)$predictions},
#                              write.forest=TRUE,
#                              data=forSurf)$error) ->
#     sla2
# 
# useOuterStrips(densityplot(~ error | factor(mtry)+factor(min.node.size), 
#                data=sla2))
# 
# 
# ############
# crossing(num.trees=c(50,500,5000),rep=1:10) %>%
#     rowwise() %>%
#     mutate(error = errorest(Survived ~.,
#                             mtry=4,
#                             min.node.size=10,
#                             num.trees=num.trees,
#                             model=ranger,
#                             predict=function(object,newdata) {predict(object,data=newdata)$predictions},
#                             write.forest=TRUE,
#                             data=forSurf)$error) ->
#     sla3
# 
# densityplot(~ error | factor(num.trees), data=sla3)


# Predictions -------------------------------------------------------------
rang3 <- ranger(Survived ~ .,
                mtry=4,
                min.node.size=12,
                write.forest=TRUE,
                data=forSurf,
                num.trees=5000) 

pp <- predict(rang3,test)

out <- data.frame(PassengerId=test$PassengerId,
                  Survived=pp$predictions,
                  row.names=NULL)
evaluateAccuracy(out$PassengerId ,
                 out$Survived)

FILE_OUT_RANGER_P5_FINAL_CSV <- file.path(DIR_MIDPUT, 'RFP5_final_result.csv')
write.csv(x=out,
          file=FILE_OUT_RANGER_P5_FINAL_CSV,
          row.names=FALSE,
          quote=FALSE)


# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), VAR_BEFORE))
#########