VAR_BEFORE <- ls()

#  ------------------------------------------------------------------------
# Blog post: Logistic Regressions and Subset Selection for the Titanic Kaggle Competition
# Source: https://www.kaggle.com/swamysm/random-forest-logistic-r-decison-t-svm-for-titanic
# Data Exploration part
#  ------------------------------------------------------------------------

# !diagnostics off


# Localize working data frame ---------------------------------------------
full_titanic <- getRawBaseDf()

train.tit <- full_titanic %>% filter(DataTag==DATA_TAG_TRAIN)
test.tit <- full_titanic %>% filter(DataTag==DATA_TAG_TEST)


# Check the structure -----------------------------------------------------
str(full_titanic)
full_titanic
summary(full_titanic)


###is there any Missing obesrvation
colSums(is.na(full_titanic))
####Empty data
colSums(full_titanic=='')
##Summary shows, Age missing 263 value, Cabin too having lot of missing value and embarked just 2


# Missing value imputation ------------------------------------------------
###Lets replace Embarked by most frequest observation 
table(full_titanic$Embarked)