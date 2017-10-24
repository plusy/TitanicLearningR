VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post: Logistic Regressions and Subset Selection for the Titanic Kaggle Competition
# Source: http://rstudio-pubs-static.s3.amazonaws.com/16118_cb674d1430404916891d2bbb0cd40882.html
# Data Exploration part
#  ------------------------------------------------------------------------

# !diagnostics off
#library(randomForest)

# Localize working data frame ---------------------------------------------
df_working <- getCleanBaseDf()


# Cleaning the Training Data ----------------------------------------------
colMaskBasic <- c('DataTag', 
                  'PassengerId', 'Survived', 
                  'Pclass', 'Name', 'Sex', 'Age', 
                  'SibSp', 'Parch', 
                  'Ticket', 'Fare', 'Cabin', 'Embarked')

df_working %>% 
    select(colMaskBasic) %>% 
    mutate(Pclass = c(1,2,3)[Pclass]) -> #use factor is even better
    df_working 


# Emabarked 
# Remove 'Embarked' variable
# table(df_working$Embarked)
# unique(df_working$Embarked)

df_working %>% 
    mutate(Embarked_C = if_else(Embarked == "C", 1, 0), 
           Embarked_Q = if_else(Embarked == "Q", 1, 0)) %>% 
    select(-Embarked) ->
    df_working 

# sum(df_working$Embarked_C)
# sum(df_working$Embarked_Q)


# Dummy variable for Gender:
df_working %>% 
    mutate(Sex = if_else(Sex == "female", 1, 0)) ->
    df_working 


# Making inference on missing Age value by titles (Mr,Mrs,Ms,etc) ----------------------------------------------------------------------
# S.Y Note: here we use global mean to impute whereas in orignal post local (train or test) mean was used
# Divide data by titles:
df_working %>% 
    mutate(Name = ifelse(str_detect(Name, fixed("Master.")), 
                         "Master", 
                         ifelse(str_detect(Name, fixed("Miss.")) | str_detect(Name, fixed("Ms.")),
                                "Miss",
                                ifelse(str_detect(Name, fixed("Mrs.")),
                                       "Mrs",
                                       ifelse(str_detect(Name, fixed("Mr.")),
                                              "Mr",
                                              ifelse(str_detect(Name, fixed("Dr.")),
                                                     "Dr",
                                                     Name))))))->
   df_working                                       



SpecialTitles <- c('Miss', 'Dr', 'Mr', 'Mrs', 'Master')
df_working %>% 
    filter(Name %in% SpecialTitles) %>% 
    group_by(Name) %>% 
    summarise(AvgAgeForSpecialTitle = mean(Age, na.rm = TRUE)) ->
    avgAge_SpecialTitles

df_working %>% 
    left_join(avgAge_SpecialTitles, by = 'Name') %>% 
    mutate(AgeImputated = coalesce(Age, AvgAgeForSpecialTitle)) ->
    df_working

# print("Uncaught Title")
# one special title "O'Donoghue, Ms. Bridget", has been fixed by Sean to Miss
df_working %>% 
    filter(is.na(AgeImputated)) %>% 
    select(Name, Age, AvgAgeForSpecialTitle, AgeImputated)


# Creating Additional Predictors ------------------------------------------
df_working %>% 
    mutate(Child = if_else(AgeImputated <= 12, 1, 2)) %>% 
    mutate(Family = SibSp+ Parch + 1,
           FamilyLog = log(Family),
           FamilyScaled = scale(Family)) %>% 
    mutate(Mother = if_else(Name == "Mrs" & Parch > 0, 1, 2)) ->
    df_working 


# Split data --------------------------------------------------------------
trainData <- df_working %>% filter(DataTag==DATA_TAG_TRAIN)
testData <- df_working %>% filter(DataTag==DATA_TAG_TEST)        
        
        
# Fitting Logistic Regression Model as First Attempt for Submission ---------------------------------------------------------
train.glm <- glm(Survived ~ Pclass + Sex + AgeImputated + SibSp + Parch + Fare + 
                            Embarked_C + Embarked_Q + 
                            Child + Mother + FamilyLog +
                            Sex * Pclass, 
                 family = binomial, 
                 data = trainData)

# USE SPLINES FOR FARES!!! IT'S A PIECEWISE STEP FUNCTION!! BREAK IT DOWN
# INTO BASIS FUNCTIONS. FARE DEFINITELY CONTAINS INFO - DIFFERENT FROM
# CLASS...
summary(train.glm)

# Make Predictions using the Test Set
p.hats <- predict.glm(train.glm, newdata = testData, type = "response")
survival = ifelse(p.hats > 0.5, 1, 0)


out <- data.frame(PassengerId=testData$PassengerId,
                  Survived=survival,
                  row.names=NULL)
evaluateAccuracy(out$PassengerId ,
                 out$Survived)
# 326 0.7799

FILE_OUT_LREG_P1_FINAL_CSV <- file.path(DIR_MIDPUT, 'LRegP1_final_result.csv')
write.csv(x=out,
          file=FILE_OUT_LREG_P1_FINAL_CSV,
          row.names=FALSE,
          quote=FALSE)


# Subset Selection  -------------------------------------------------------
library(leaps)
regfit.full <- regsubsets(Survived ~ Pclass + Sex + AgeImputated + 
                                     SibSp + Parch + 
                                     Fare + 
                                     Embarked_C + Embarked_Q + 
                                     Child + Mother + 
                                     Sex * Pclass, 
                          data = trainData, 
                          nvmax = 11)  #Best Subset Selection for ALL variables
reg.summary <- summary(regfit.full)


names(reg.summary)
reg.summary$rsq  #summary of the R-squares for all 11 variables


# Plot RSS, adjusted r-square, Cp, BIC for all the models at once
par(mfrow = c(2, 2))
# RSS Plot
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
# Adjusted RSq plot
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", 
     type = "l")

which.max(reg.summary$adjr2)
points(10, reg.summary$adjr2[10], col = "red", cex = 2, pch = 20)


# Cp plot
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")

which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)


# BIC plot
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)


# Plotting built into regsubsets()
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# Coefficients for these best subsets:
coef(regfit.full, 10)
coef(regfit.full, 6)


# So now do 2 fittings: One with 10 predictor and one with 6 predictors to
# see which gives better prediction accuracy.

# Model 10, only Fare is removed. Fare is noise
train.glm10 <- glm(Survived ~ Pclass + Sex + AgeImputated + SibSp + Parch + 
                              Embarked_C + Embarked_Q + 
                              Child + Mother + 
                              Sex * Pclass,
                   family = binomial, 
                   data = trainData)
p.hats10 <- predict.glm(train.glm10, newdata = testData, type = "response")
survival10 = ifelse(p.hats10 > 0.5, 1, 0)
out <- data.frame(PassengerId=testData$PassengerId,
                  Survived=survival10,
                  row.names=NULL)
evaluateAccuracy(out$PassengerId ,
                 out$Survived)

# 332 correct, 79.4%

# Model 6
train.glm6 <- glm(Survived ~ Pclass + Sex + AgeImputated + SibSp + 
                             Child + Sex * Pclass, 
                  family = binomial, 
                  data = trainData)


p.hats6 <- predict.glm(train.glm6, newdata = testData, type = "response")
survival6 = ifelse(p.hats6 > 0.5, 1, 0)
out <- data.frame(PassengerId=testData$PassengerId,
                  Survived=survival6,
                  row.names=NULL)
evaluateAccuracy(out$PassengerId ,
                 out$Survived)
# 326 correct, 0.7799