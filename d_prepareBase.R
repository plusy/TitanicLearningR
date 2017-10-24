VAR_BEFORE <- ls()

# Descritpion from https://github.com/IamGianluca/titanic
# This is an analysis based on the data made available by Kaggle
# (www.kaggle.com) on the Titanic distaster competition. This work aims to
# predict what sorts of people were more likely to survive.
# 
# On the folder text you can find the R Markdown file to generate a pdf 
# version of the final report, which summarises the key findings of our
# reseach. 
# 
# On the folder R code you can find two sub-folders. Raw scripts 
# contains all tests we conducted and it is not intended to be a place
# where find perfectly formatted and optimised R code. Whereas final
# scripts include the final version of R code we wrote to conduct
# our analysis. We advice you to give it a read to have a better 
# overview of how the analysis has been conducted.
# 
# Data contains both the raw dataset we downloaded from Kaggle.com and 
# a tiny dataset with variables transformed in the format we used when
# conducting our analysis.
# 
# 
# VARIABLES DESCRIPTION:
#     
#     survival        Survival
# (0 = No; 1 = Yes)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)
# 
# 
# SPECIAL NOTES:
#     
# Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
# 
# Age is in Years; Fractional if Age less than One (1)
# If the Age is Estimated, it is in the form xx.5
# 
# With respect to the family relation variables (i.e. sibsp and parch)
# some relations were ignored.  The following are the definitions used
# for sibsp and parch.
# 
# Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
# Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
# Parent:   Mother or Father of Passenger Aboard Titanic
# Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic
# 
# Other family relatives excluded from this study include cousins,
# nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
# only with a nanny, therefore parch=0 for them.  As well, some
# travelled with very close friends or neighbors in a village, however,
# the definitions do not support such relations.



# Based on this post: http://wiekvoet.blogspot.com/2015/08/predicting-titanic-deaths-on-kaggle-iv.html
# Load raw data -----------------------------------------------------------
df_RawInput_Train <- read_csv(FILE_SRC_DATA_RAW_TRAIN)
df_RawInput_Train$DataTag <- DATA_TAG_TRAIN

df_RawInput_Test <- read_csv(FILE_SRC_DATA_RAW_TEST)
df_RawInput_Test$DataTag <- DATA_TAG_TEST
df_RawInput_Test$Survived <- NA


df_RawInput <- rbind(df_RawInput_Train,df_RawInput_Test)

df_working <- df_RawInput


# Factorize -----------------------------------------------------------------
colList <- c('PassengerId', 'Pclass', 'Survived', 'Sex')
df_working[colList] <- lapply(df_working[colList], factor) 


#Embarked ----------------------------------------------------------
df_working%>% 
    mutate(Embarked=factor(coalesce(Embarked,"N/A"))) -> 
    df_working


#Fare ----------------------------------------------------------
MadiaOfAll <- median(df_working$Fare,na.rm=TRUE)
MadiaOfTrain <-  median(df_working$Fare[df_working$DataTag==DATA_TAG_TRAIN],na.rm=TRUE) 
df_working%>% 
    mutate(Fare_Adj_MedianOfAll=coalesce(Fare, MadiaOfAll),
           Fare_Adj_MedianOfTrain=coalesce(Fare, MadiaOfTrain)) -> 
    df_working


#Cabin ----------------------------------------------------------
df_working%>%
    mutate(Cabin = coalesce(Cabin, "")) ->
    df_working

for(cab in LETTERS[1:7]){ # 1:7 -> A:G
    df_working[paste0("Cabin_",cab)] <- factor(grepl(cab,df_working$Cabin))
}

df_working%>%
    mutate(Cabin_Length = nchar(Cabin),
           Cabin_Section = factor(if_else(Cabin_Length==0,
                                          0,
                                          str_count(df_working$Cabin, ' ') + 1))) ->
    df_working

df_working%>%
    separate(Cabin, sep = " ",
             into = c("Cabin_Number"), extra = "drop", remove = FALSE) %>%
    mutate(Cabin_Number = coalesce(as.integer(str_replace(Cabin_Number, "[[:alpha:]]", "")), as.integer(0))) %>%
    mutate(Cabin_NumberOE = factor(if_else(is.na(Cabin_Number), -1, Cabin_Number%%2))) ->
    df_working


#Name ----------------------------------------------------------
df_working%>% 
    #Name_Title = sapply(Name,function(x) strsplit(as.character(x),'[.,]')[[1]][2]),
    separate(Name, sep = '[.,]',
             into = c("Name_Last", "Name_Title", "Name_First"), extra = "merge", remove = FALSE) %>% 
    mutate(Name_TitleSimple = str_trim(Name_Title),
           Name_TitleSimple = ifelse(Name_TitleSimple %in% c('Capt','Col','Don','Sir','Jonkheer','Major'), 
                                     'Mr',
                                     ifelse(Name_TitleSimple %in% c('Lady','Ms','the Countess','Mlle','Mme','Ms','Dona'),
                                            'Miss',
                                             Name_TitleSimple)),
           Name_TitleSimple = factor(Name_TitleSimple),
           Name_Title = factor(Name_Title)) -> 
    df_working


#Ticket ----------------------------------------------------------
df_working%>% 
    mutate(Ticket_PC = factor(grepl('PC',Ticket)), 
           Ticket_STON = factor(grepl('STON',Ticket))) -> 
    df_working                                         


#Age ----------------------------------------------------------
df_working%>%  
    mutate(Age_ImputeBy35 = coalesce(Age, 35)) %>% 
    mutate(Age_ImputeBy35_Bin=cut(Age_ImputeBy35, AGE_BIN_CUTOFF))-> 
    df_working



# Save clean data ---------------------------------------------------------
saveRDS(df_working, file = FILE_CLEAN_DATA_BASE_RDS)
write_csv(df_working, FILE_CLEAN_DATA_BASE_CSV)


# Prepare ground Truth ----------------------------------------------------
# Groud truth data from http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.csv
# Two dup name: Kelly, Mr. James and Connolly, Miss. Kate
read_csv(FILE_SRC_DATA_RAW_TRUTH) %>% 
    select(name, survived, ticket) %>% 
    mutate(name = str_replace_all(name, '\"', '')) ->
    df_GroundTruth

df_working %>% 
    mutate(Name = str_replace_all(Name, '\"', '')) %>% 
    select(Name, Ticket, PassengerId, DataTag) %>% 
    inner_join(df_GroundTruth, by = c("Name"="name", "Ticket"="ticket" )) %>%    
    select(PassengerId, GTSurvived = survived, Name, Ticket, DataTag) ->
    df_GroundTruth

# if inner join found mismatch    
stopifnot(nrow(df_GroundTruth)==nrow(df_working))    
write_csv(df_GroundTruth, FILE_CLEAN_DATA_GROUND_TRUTH)


# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), VAR_BEFORE))