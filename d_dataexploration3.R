VAR_BEFORE <- ls()

FILE_RAW_DATA_BASE <- file.path(DIR_MIDPUT, 'datareview_ktsp.csv')


#  ------------------------------------------------------------------------
# Blog post: Kaggle Titanic Survivor Prediction: Comparison of Machine Learning Methods
# Source: https://rpubs.com/JamesENelson/153826
# Data Exploration part
#  ------------------------------------------------------------------------
# Note: not finished, just study the data part, not the model part

# !diagnostics off

# Localize working data frame ---------------------------------------------
df_working <- getRawBaseDf()

train <- df_working %>% filter(DataTag==DATA_TAG_TRAIN)
test <- df_working %>% filter(DataTag==DATA_TAG_TEST)


# Data Transformation and Verification ------------------------------------
train$Survived<- factor(train$Survived)

test<- mutate(test, Survived = "none")
test$Survived<- factor(test$Survived)

data <- 
    df_working %>% mutate(Survived = factor(Survived))

data$Pclass <- factor(data$Pclass)
data$dataset <- factor(data$dataset)
data$Survived<- factor(data$Survived)


# dupcheck: PassengerId
IDdups <- distinct(data, PassengerId)
dim(IDdups)


# dupcheck: Name
Namedups <- distinct(data, Name)
dim(Namedups)


#filter(data, duplicated(Name)) 
#filter(data, grepl('Kelly|Connolly', Name, Age ))
data %>% 
    filter(duplicated(Name)) %>% 
    select(Name) %>% 
    inner_join(data) %>% 
    arrange(Name) 


# Data Exploration --------------------------------------------------------
summary(tbl_df(data))
Hmisc::describe(data)

head(data)


# Visualize some potentially important features as a function of s --------
# Age
trainset <- data%>% 
    filter(DataTag=="Train")
    
head (trainset)
glimpse(trainset)

hist_Age <- ggplot(trainset, aes(x=Age, fill=Survived))
hist_Age + geom_bar() # defaults to stacking
hist_Age + geom_bar(position= "fill") #proportions


# Sex
hist_Sex <- ggplot(trainset, aes(x=Sex, fill=Survived))
hist_Sex + geom_bar(position= "fill") # defaults to stacking


#Pclass (cabin type)
hist_Pclass <- ggplot(trainset, aes(x=Pclass, fill=Survived))
hist_Pclass + geom_bar() # defaults to stacking 
hist_Pclass + geom_bar(position= "fill") #proportions


#SibSp (no. siblings/spouse)
hist_SibSp <- ggplot(trainset, aes(x=SibSp, fill=Survived, binwidth = .0005))
hist_SibSp + geom_bar() # defaults to stacking
hist_SibSp + geom_bar(position= "fill") #proportions  


#Parch (no. parents/children)
hist_Parch <- ggplot(trainset, aes(x=Parch, fill=Survived))
hist_Parch + geom_bar() # defaults to stacking

hist_Parch + geom_bar(position= "fill") #proportions  


# Feature Engineering -----------------------------------------------------


##  Hypothesis 1: data visualization suggests being a child and/or a female increased your odds of survival ====
data <- data %>%
    mutate(Child = Age <=16) 
data$Child <- factor(data$Child)
glimpse (data)

trainset<-data%>% filter(DataTag == 'Train')

hist_Child <- ggplot(trainset, aes(x=Child, fill=Survived))
hist_Child + geom_bar() # defaults to stacking
hist_Child + geom_bar(position= "fill") #proportions  


## Hypothesis 2: Did a persons Title effect survivability? ====
Mr<-filter(data, grepl('Mr.' ,Name, fixed=TRUE ))
Mr<-mutate(Mr, title = 'Mr')

Mrs<-filter(data, grepl('Mrs.', Name, fixed=TRUE ))
Mrs<-mutate(Mrs, title = 'Mrs')

Miss<-filter(data, grepl('Miss.', Name, fixed=TRUE ))
Miss<-mutate(Miss, title = 'Miss')

Master<-filter(data, grepl('Master.', Name, fixed=TRUE  ))
Master<-mutate(Master, title = 'Master')

Dr <-filter(data, grepl('Dr.', Name, fixed=TRUE  ))
Dr<-mutate(Dr, title = 'UCMale')

Rev<-filter(data, grepl('Rev.', Name, fixed=TRUE  ))
Rev<-mutate(Rev, title = 'UCMale')

Ms<-filter(data, grepl('Ms.', Name, fixed=TRUE  ))
Ms<-mutate(Ms, title = 'Mrs')

Major<-filter(data, grepl('Major.', Name, fixed=TRUE  ))
Major<-mutate(Major, title = 'UCMale')

Col<-filter(data, grepl('Col.', Name, fixed=TRUE  ))
Col<-mutate(Col, title = 'UCMale')

Dona<-filter(data, grepl('Dona.', Name, fixed=TRUE  ))
Dona<-mutate(Dona, title = 'UCFemale')

Don<-filter(data, grepl('Don.', Name, fixed=TRUE  ))
Don<-mutate(Don, title = 'UCMale')

Capt<-filter(data, grepl('Capt.', Name, fixed=TRUE  ))
Capt<-mutate(Capt, title = 'UCMale')

Sir<-filter(data, grepl('Sir.', Name, fixed=TRUE  ))
Sir<-mutate(Sir, title = 'UCMale')

Lady<-filter(data, grepl('Lady.', Name, fixed=TRUE  ))
Lady<-mutate(Lady, title = 'UCFemale')

Mlle<-filter(data, grepl('Mlle.', Name, fixed=TRUE  ))
Mlle<-mutate(Mlle, title = 'Miss')

Mme<-filter(data, grepl('Mme.', Name, fixed=TRUE  ))
Mme<-mutate(Mme, title = 'Miss')

Ctss<-filter(data, grepl('Countess.', Name, fixed=TRUE  ))
Ctss<-mutate(Ctss, title = 'UCFemale')

Jonk<-filter(data, grepl('Jonkheer.', Name, fixed=TRUE  ))
Jonk<-mutate(Jonk, title = 'UCMale')

Dr<-Dr[-8, ] # remove the female Dr from 'Dr' df

FDr<-filter(data, grepl('Leader', Name, fixed=TRUE  ))
FDr<-mutate(FDr, title = 'UCFemale')

# Create seperate title class, by sex, for people with titles indicative of the upper class
UCMale<- rbind(Dr, Rev, Sir, Major, Col, Capt, Don, Jonk)
UCFemale<- rbind(Lady, Dona, Ctss, FDr)

# combine "Ms" with "Mrs" and "Mme"/"Mlle" with Miss
Mrs<- rbind(Mrs, Ms)
Miss<- rbind(Miss, Mme, Mlle)  

# combine all title into one variable "title"
tbl_df(alltitles<-rbind(Mr, Mrs, Miss, Master, UCMale, UCFemale))
glimpse (alltitles) 
tail(alltitles)

# create dummy variable for data df
data<-mutate(data, title = "none")
glimpse(data)

data<-arrange(data, PassengerId)
head(data)

alltitles<- arrange(alltitles, PassengerId)
head(alltitles)

# add new feature "title" to data df
data$title<-alltitles$title
summary(data)

data$title <- factor(data$title)#factorize 'title'


trainset<-data%>% filter(DataTag == "Train")
head (trainset)
glimpse(trainset)

hist_title <- ggplot(trainset, aes(x=title, fill=Survived))
hist_title + geom_bar() # defaults to stacking
hist_title + geom_bar(position= "fill") #proportions  

## Verify Age range for each title group
data%>%
    group_by(title)%>%
    filter(!is.na(Age))%>%
    summarise(min(Age))

data%>%
    group_by(title)%>%
    filter(!is.na(Age))%>%
    summarise(max(Age))


# How many people with titles of “Mr” and “Mrs” are <=16
under16<-filter(data, Age<=16)
under16%>%group_by(title)%>% summarise(n())


is.na(data$Child[data$title=="Master"]<-TRUE)
is.na(data$Child[data$title=="Mr" ]<-FALSE)
is.na(data$Child[data$title=="Mrs" ]<-FALSE)
is.na(data$Child[data$title=="UCMale" ]<-FALSE)
is.na(data$Child[data$title=="UCFemale" ]<-FALSE)
is.na(data$Child[data$title=="Miss" ]<-FALSE)


# Hypothesis 3: Data visualization suggests traveling alone decrea --------
data<- data %>% 
    mutate (familysize = SibSp  + Parch +1 ) %>%
    mutate(notalone = familysize >1) 

data$notalone<- factor(data$notalone)
glimpse (data)

trainset<-data%>% filter(DataTag == 'Train')
head (trainset)
glimpse(trainset)

hist_notalone <- ggplot(trainset, aes(x=notalone, fill=Survived))
hist_notalone + geom_bar() # defaults to stacking
hist_notalone + geom_bar(position= "fill") #proportions

hist_familysize <- ggplot(trainset, aes(x=familysize, fill=Survived))
hist_familysize + geom_bar(position= "fill") #proportions  
hist_familysize + geom_bar() # defaults to stacking
hist_familysize + geom_bar(position= "fill") #proportions  





# Hypothesis 4: data visualization suggests that small families ha --------
#Create new categorical feature smallfamily from familysize >1 but <4 (ie between 2-4 people total)
data$smallfamily[data$familysize >1 & data$familysize<=4] <-1
data$smallfamily[data$familysize == 1 | data$familysize>4 ] <-0
data$smallfamily <- factor(data$smallfamily)


#Create feature for just 3rd Class to test as a surrogate for Pclass
data$thirdClass[data$Pclass ==3 ] <-1
data$thirdClass[data$Pclass ==1 | data$Pclass==2 ] <-0
data$thirdClass <- factor(data$thirdClass)  


#Visualize survival as a function of having a smallfamily or 3rd class cabin
trainset<-data%>% filter(DataTag = "Train")
head (trainset)
glimpse(trainset)

hist_smallfamily <- ggplot(trainset, aes(x=smallfamily, fill=Survived))
hist_smallfamily + geom_bar() # defaults to stacking
hist_smallfamily + geom_bar(position= "fill") #proportions

hist_thirdclass <- ggplot(trainset, aes(x=thirdClass, fill=Survived))
hist_thirdclass + geom_bar() # defaults to stacking
hist_thirdclass+ geom_bar(position= "fill") #proportions


# Impute value for Age based on logit model
ageimp <- lm(Age~ Pclass+smallfamily+SibSp+title,  data= data)
summary(ageimp)
# assign imputed Age values for NAs in combined.df
for(i in 1:nrow(data)) {
    if(is.na(data[i, "Age"])) {
        data[i, "Age"] <- predict(ageimp, newdata = data[i, ])  
    }
}


## Impute missing fare value for passenger 1044 based on median cos --------
data<-arrange(data, desc(thirdClass))
data<-arrange(data, SibSp)
data<-arrange(data, Parch)

threemeanfare<-data[1:472, "Fare"]
summary(threemeanfare)

arrange(data, PassengerId)
data[59,"Fare"]<-7.854
summary(data$Fare)


# Split data df into train and test datasets ------------------------------
data<-arrange(data, dataset)
test<- data[1:418, ]
class(test)

train<-data[419:1309, ]
train$Survived <- droplevels(train$Survived) 
test$Survived <- droplevels(test$Survived) 
str(test)


str(train)

# write data for Tableau and Modeling --------------------------------------------------------------
write_csv(full_titanic, FILE_RAW_DATA_BASE)


# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), VAR_BEFORE))


