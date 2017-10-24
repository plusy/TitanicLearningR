VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post: Logistic Regressions and Subset Selection for the Titanic Kaggle Competition
# Source: http://rstudio-pubs-static.s3.amazonaws.com/16118_cb674d1430404916891d2bbb0cd40882.html
# Data Exploration part
#  ------------------------------------------------------------------------

# !diagnostics off


# Localize working data frame ---------------------------------------------
df_working <- getCleanBaseDf()

trainData <- df_working %>% filter(DataTag==DATA_TAG_TRAIN)
testData <- df_working %>% filter(DataTag==DATA_TAG_TEST)


# par(mfrow = c(2, 3))
par(mar = rep(2, 4))
plot(density(trainData$Age, na.rm = TRUE))


plot(density(trainData$Fare, na.rm = TRUE))

#trainData.new <- trainData[, -c(4, 9, 11, 12)]
#trainData.new$Sex = ifelse(trainData.new$Sex == "male", 1, 0)
trainData.new <- trainData %>% 
    select(PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare)

plot(trainData.new)


# Survival rate by Gender -------------------------------------------------
counts <- table(trainData$Survived, trainData$Sex)
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")

counts[2]/(counts[1] + counts[2])  #Female survival rate
counts[4]/(counts[3] + counts[4])  #Male survival rate

prop.table(table(trainData$Survived, trainData$Sex),2)


# Survival rate by Class --------------------------------------------------
Pclass_survival <- table(trainData$Survived, trainData$Pclass)
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People", main = "survived and deceased between male and female")

Pclass_survival[2]/(Pclass_survival[1] + Pclass_survival[2])  #1st Class Survival Rate
table(trainData$Survived, trainData$Pclass)
prop.table(table(trainData$Survived, trainData$Pclass),2)



# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), VAR_BEFORE))

