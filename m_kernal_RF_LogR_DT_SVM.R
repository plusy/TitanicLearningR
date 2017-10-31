.VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post: Logistic Regressions and Subset Selection for the Titanic Kaggle Competition
# Source: https://www.kaggle.com/swamysm/random-forest-logistic-r-decison-t-svm-for-titanic
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
set.seed(500)
ind=createDataPartition(feauter1$Survived,times=1,p=0.8,list=FALSE)
train_val=feauter1[ind,]
test_val=feauter1[-ind,]

####check the proprtion of Survival rate in orginal training data, current traing and testing data
round(prop.table(table(feauter1$Survived)*100), digits = 1)
round(prop.table(table(train_val$Survived)*100),digits = 1)
round(prop.table(table(test_val$Survived)*100),digits = 1)


# Predictive Analysis and Cross Validation --------------------------------


# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), .VAR_BEFORE))
rm(.VAR_BEFORE)