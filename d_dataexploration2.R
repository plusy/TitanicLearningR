# dependency for Model
FILE_RAW_DATA_BASE_DR2_TABLEAU <- file.path(DIR_MIDPUT, 'datareview2_tableua.csv')
FILE_RAW_DATA_BASE_DR2_RDS     <- file.path(DIR_MIDPUT, 'datareview2.RDS')

.VAR_BEFORE <- ls()
#  ------------------------------------------------------------------------
# Blog post: Logistic Regressions and Subset Selection for the Titanic Kaggle Competition
# Source: https://www.kaggle.com/swamysm/random-forest-logistic-r-decison-t-svm-for-titanic
# Data Exploration part
#  ------------------------------------------------------------------------

# !diagnostics off


# Localize working data frame ---------------------------------------------
full_titanic <- getRawBaseDf()


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
table(full_titanic$Embarked, full_titanic$DataTag) #two NAs are all in Ttrianing data
full_titanic$Embarked[full_titanic$Embarked=="N/A"]="S"  #S.Y.: replaced '' with 'N/A' in data prepare
table(full_titanic$Embarked)


###Check the length and see how many varibles of them we can move to factor for our analysis
apply(full_titanic,2, function(x) length(unique(x)))
###will convert the below varible into factor for ananlysis 
cols=c("Survived","Pclass","Sex","Embarked") 
#for (i in cols){ 
#    full_titanic[,i]=as.factor(full_titanic[,i]) 
#}
full_titanic[cols] <- lapply(full_titanic[cols], factor) 

# Exploratory Analysis on Pclass ------------------------------------------
## Hypothesis is that,  Rich folks survival rate is much better than poor folks, Does any diffrence in the Titanic?  
###Visualize P class which is the best proxy for Rich and Poor  
ggplot(full_titanic[1:891,],
       aes(x = Pclass,fill=factor(Survived))) +
    geom_bar() +
    ggtitle("Pclass v/s Survival Rate")+
    xlab("Pclass") +
    ylab("Total Count") +
    labs(fill = "Survived")  
##No diffrences in the Titanic too, First class Survival rate is far more better than the 3rd class  
##No doubt Rich peope having better Survival rate than the poor


# Visualize the 3-way relationship of sex, pclass, and survival
ggplot(full_titanic[1:891,], 
       aes(x = Sex, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass) + 
    ggtitle("3D view of sex, pclass, and survival") +
    xlab("Sex") +
    ylab("Total Count") +
    labs(fill = "Survived")
##In the all the class female Survival rate is better than Men


# Exploratory Analysis on Title -------------------------------------------
head(full_titanic$Name)

##Lets extract the title and check if we have predictive power in that
names <- full_titanic$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)
full_titanic$title <- title

table(title)


###MISS, Mrs, Master and Mr are taking more numbers
###Better to group Other titles into bigger basket by checking gender and survival rate to aviod any overfitting
full_titanic$title[full_titanic$title == 'Mme']   <- 'Mrs' 
full_titanic$title[full_titanic$title == 'Mlle']  <- 'Miss' 
full_titanic$title[full_titanic$title == 'Ms']    <- 'Miss'
full_titanic$title[full_titanic$title == 'Lady']  <- 'Miss'
full_titanic$title[full_titanic$title == 'Dona']  <- 'Miss'


## I am afraid creating a new varible with small data can causes a overfit
## However, My thinking is that combining below feauter into original 
## variable may loss some predictive power as they are all army folks, 
## doctor and nobel peoples 
# full_titanic$title[full_titanic$title == 'Capt'] <- 'Officer' 
# full_titanic$title[full_titanic$title == 'Col']  <- 'Officer' 
# full_titanic$title[full_titanic$title == 'Major']<- 'Officer'
# full_titanic$title[full_titanic$title == 'Dr']   <- 'Officer'
# full_titanic$title[full_titanic$title == 'Rev']  <- 'Officer'
# full_titanic$title[full_titanic$title == 'Don']  <- 'Officer'
# full_titanic$title[full_titanic$title == 'Sir']  <- 'Officer'
# full_titanic$title[full_titanic$title == 'the Countess'] <- 'Officer'
# full_titanic$title[full_titanic$title == 'Jonkheer']     <- 'Officer'
titleOfficer <- c('Capt','Col','Major','Dr','Rev', 
                  'Don','Sir','the Countess', 'Jonkheer')
full_titanic %>% 
    mutate(title = if_else(title %in% titleOfficer, 
                           'Officer', 
                           title)) ->
    full_titanic

table(full_titanic$title)


# Lets check who among Mr, Master, Miss having a better survival rate
ggplot(full_titanic[1:891,],
       aes(x = title,fill=factor(Survived))) +
    geom_bar() +
    ggtitle("Title V/S Survival rate")+
    xlab("Title") +
    ylab("Total Count") +
    labs(fill = "Survived") 


##In the titanic you are Mr then there is less chance of survival, Miss and Mrs having better survival rate then Master and Officer 
### Visualize the 3-way of relationship of Title, Pclass, and Survival
ggplot(full_titanic[1:891,], aes(x = title, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass) + 
    ggtitle("3-way relationship of Title, Pclass, and Survival") +
    xlab("Title") +
    ylab("Total Count") +
    labs(fill = "Survived")


# Exploratory Analysis on Family ------------------------------------------
# Lets create a Family size using Sibsp and Parch
full_titanic$FamilySize <-full_titanic$SibSp + full_titanic$Parch + 1

full_titanic$FamilySized <- NA
full_titanic$FamilySized[full_titanic$FamilySize == 1]   <- 'Single'
full_titanic$FamilySized[full_titanic$FamilySize < 5 & full_titanic$FamilySize >= 2]   <- 'Small'
full_titanic$FamilySized[full_titanic$FamilySize >= 5]   <- 'Big'

full_titanic$FamilySized=as.factor(full_titanic$FamilySized)


###Lets Visualize the Survival rate by Family size 
ggplot(full_titanic[1:891,],aes(x = FamilySized,fill=factor(Survived))) +
    geom_bar() +
    ggtitle("Family Size V/S Survival Rate") +
    xlab("FamilySize") +
    ylab("Total Count") +
    labs(fill = "Survived")
###Big Family in Titanic having worst survival rate then Smaller and Alone

####Why Big Family has a probelm?, Check in the below visualization
ggplot(full_titanic[1:891,], aes(x = FamilySized, fill = Survived)) +
    geom_bar() +
    facet_wrap(~title) + 
    ggtitle("3D View of Fmily Size, Title and Survival rate") +
    xlab("family.size") +
    ylab("Total Count") +
    ylim(0,300) +
    labs(fill = "Survived")
##You are a Master in the Big Family your Survival rate is absolutely nill even though overall survival rate of master is very good

###I am very surprised to see Single coming out to be bulk, 
### however there is chance that, they could come with friends or servants
##I though to extract those unique number using same ticket number distributed.


##Engineer features based on all the passengers with the same ticket
# ticket.unique <- rep(0, nrow(full_titanic))
# tickets <- unique(full_titanic$Ticket)
# 
# for (i in 1:length(tickets)) {
#     current.ticket <- tickets[i]
#     party.indexes <- which(full_titanic$Ticket == current.ticket)
# 
#     for (k in 1:length(party.indexes)) {
#         ticket.unique[party.indexes[k]] <- length(party.indexes)
#     }
# }
# 
# full_titanic$ticket.unique <- ticket.unique
# full_titanic$ticket.size <- NA
# full_titanic$ticket.size[full_titanic$ticket.unique == 1]   <- 'Single'
# full_titanic$ticket.size[full_titanic$ticket.unique < 5 & full_titanic$ticket.unique>= 2]   <- 'Small'
# full_titanic$ticket.size[full_titanic$ticket.unique >= 5]   <- 'Big'
ticketShared <- as.data.frame(table(full_titanic$Ticket),
                              stringsAsFactors = FALSE)
names(ticketShared) <- c('Ticket','ticket.unique')
full_titanic %>% 
    left_join(ticketShared, by = 'Ticket') %>% 
    mutate(ticket.size = cut(ticket.unique,
                             breaks = c(-Inf, 1, 4.99, Inf),
                             labels = c('Single', 'Small', 'Big')))->
    full_titanic


##Lets check the Ticket size through grpah
ggplot(full_titanic[1:891,],aes(x = ticket.size,fill=factor(Survived))) +
    geom_bar() +
    ggtitle("ticket.Size VS Survival")+
    xlab("ticket.size") +
    ylab("Total Count") +
    labs(fill = "Survived")

##Lets check the Ticket and title size through grpah
ggplot(full_titanic[1:891,], aes(x = ticket.size, fill = Survived)) +
    geom_bar() +
    facet_wrap(~title) + 
    ggtitle("3D View of Ticket, Title and Survival rate") +
    xlab("ticket.size") +
    ylab("Total Count") +
    ylim(0,300) +
    labs(fill = "Survived")
##We can't see huge diffrence b/w ticket size and Family Size, 
##May be we will use any one of them which is contributing more
cor(full_titanic$FamilySize, full_titanic$ticket.unique)


# Exploratory Analysis on Embarked ----------------------------------------
###is there any association between Survial rate and where he get into the Ship.   
ggplot(full_titanic[1:891,],aes(x = Embarked,fill=factor(Survived))) +
    geom_bar() +
    ggtitle("Embarked vs Survival") +
    xlab("Embarked") +
    ylab("Total Count") +
    labs(fill = "Survived") 

##Lets further divide the grpah by Pclass
ggplot(full_titanic[1:891,], aes(x = Embarked, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass) + 
    ggtitle("Pclass vs Embarked vs survival") +
    xlab("Embarked") +
    ylab("Total Count") +
    labs(fill = "Survived")

##Haha..I don't think there is a correlation between Survival rate and Embarked 

##There is a lot of Missing value in Cabin, i dont think its good idea to use that
##As mentioned earlier will use Title inplace of Age 
##Fare is definitelly correlate with Pclass..so i am not going to use that too

full_titanic$ticket.size <- as.factor(full_titanic$ticket.size)
full_titanic$title <- as.factor(full_titanic$title)


# Conclusion --------------------------------------------------------------
##From the Explortory anlysis part we have decided to use below variables for our model building 
##"Pclass", "title","Sex","Embarked","FamilySized","ticket.size"
##Any redaundant varible among above will drop in the course of analysis


# write data for Tableau and Modeling --------------------------------------------------------------
write_csv(full_titanic, FILE_RAW_DATA_BASE_DR2_TABLEAU)
saveRDS(full_titanic, file = FILE_RAW_DATA_BASE_DR2_RDS)

# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), .VAR_BEFORE))
rm(.VAR_BEFORE)