VAR_BEFORE <- ls()

FILE_RAW_DATA_BASE <- file.path(DIR_MIDPUT, 'datareview_tidytitarnic.csv')


#  ------------------------------------------------------------------------
# Blog post: Tidy TitaRnic
# Source: https://www.kaggle.com/headsortails/tidy-titarnic
# Data Exploration part
#  ------------------------------------------------------------------------



#  library ------------------------------------------------------------------------
# Note: not finished, just study the data part, not the model part
# !diagnostics off
library(ggthemes) # visualization
library(ggridges) # visualization
library(ggforce) # visualization
library(ggExtra) # visualization
library(GGally) # visualisation
library(scales) # visualization
library(grid) # visualisation
library(gridExtra) # visualisation
library(corrplot) # visualisation
library(VIM) # missing values
#library(Rmisc) # for multiplot function, but load plyr so it is conflict with dplyr::summarise()

# library('forcats') # factor manipulation
# library('modelr') # factor manipulation
# library('randomForest') # classification
# library('xgboost') # classification
# library('ROCR') # model validation

# Localize working data frame ---------------------------------------------
df_working <- getRawBaseDf()


# 1.1 Load libraries, functions, and data files ---------------------------
df_working %>% 
    mutate(Survived = factor(Survived),
           Pclass = factor(Pclass),
           Embarked = factor(Embarked),
           Sex = factor(Sex)) ->
    combine
# Two Embarked has been imputed

train <- combine %>% filter(DataTag==DATA_TAG_TRAIN)
test <- combine %>% filter(DataTag==DATA_TAG_TEST)


# 1.2 Data overview -------------------------------------------------------
summary(combine)
glimpse(combine)

train %>%
    dplyr::count(Survived)

surv <- train %>% dplyr::count(Survived) %>% filter(Survived == 1) %>% .$n
nosurv <- train %>% dplyr::count(Survived) %>% filter(Survived == 0) %>% .$n
surv/(surv+nosurv)*100

prop.table(table(train$Survived))


# 1.3 More about missing values -------------------------------------------
# visual of mice::md.pattern(combine)
combine %>% 
    select(-DataTag) %>% 
    aggr(., prop = FALSE, combined = TRUE, numbers = TRUE, 
         sortVars = TRUE, sortCombs = TRUE)

sum(is.na(combine$Ticket))
sum(is.na(combine$Cabin))


# 2.1 Individual features -------------------------------------------------
p_age = ggplot(train) +
    geom_freqpoly(mapping = aes(x = Age, color = Survived), binwidth = 1) +
    guides(fill=FALSE) +
    theme(legend.position = "none")

p_sex = ggplot(train, mapping = aes(x = Sex, fill = Survived)) +
    geom_bar(stat='count', position='fill') +
    labs(x = 'Sex') +
    scale_fill_discrete(name="Surv") +
    theme_few()

p_class = ggplot(train, mapping = aes(x = Pclass, fill = Survived, colour = Survived)) +
    geom_bar(stat='count', position='fill') +
    labs(x = 'Pclass') +
    theme(legend.position = "none")

p_emb = ggplot(train, aes(Embarked, fill = Survived)) +
    geom_bar(stat='count', position='fill') +
    labs(x = 'Embarked') +
    theme(legend.position = "none")

p_sib = ggplot(train, aes(SibSp, fill = Survived)) +
    geom_bar(stat='count', position='fill') +
    labs(x = 'SibSp') +
    theme(legend.position = "none")

p_par = ggplot(train, aes(Parch, fill = Survived)) +
    geom_bar(stat='count', position='fill') +
    labs(x = 'Parch') +
    theme(legend.position = "none")

p_fare = ggplot(train) +
    geom_freqpoly(mapping = aes(Fare, color = Survived), binwidth = 0.05) +
    scale_x_log10() +
    theme(legend.position = "none")


layout <- matrix(c(1,1,2,3,3,4,5,6,7),3,3,byrow=TRUE)
multiplot(p_age, p_sex, 
          p_fare, p_class, 
          p_emb, p_sib, p_par, 
          layout=layout)

train %>%
    group_by(Survived) %>%
    dplyr::summarise(median_age = median(Age, na.rm=TRUE))

train %>% 
    mutate(single = SibSp==0) %>% 
    add_count() %>% 
    group_by(single) %>% 
    dplyr::summarise(counts = n(), freq = n()/max(n))

train %>% 
    mutate(single = SibSp==0) -> x
prop.table(table(x$single))


# 2.2.1 Correlation overview ----------------------------------------------
train %>%
    select(-PassengerId, -Name, -Cabin, -Ticket, -DataTag) %>%
    mutate(Sex = fct_recode(Sex,
                            "1" = "male",
                            "0" = "female")) %>%
    mutate(Sex = as.integer(Sex),
           Pclass = as.integer(Pclass),
           Survived = as.integer(as.character(Survived)),
           Embarked = as.integer(Embarked)) %>%
    cor(use="complete.obs") %>%
    corrplot(type="lower", diag=FALSE)


# 2.2.2 Multi-feature comparisons -----------------------------------------
# 2.2.2.1 Pclass vs Fare 
ggplot(train, aes(Pclass, Fare, colour = Survived)) +
       geom_boxplot() +
       scale_y_log10()

train %>%
    ggplot(aes(Fare, fill=Pclass)) +
    geom_density(alpha = 0.5) +
    scale_x_log10() +
    facet_wrap(~ Survived, ncol = 1)

#2.2.2.2 Pclass vs Embarked
train %>%
    filter(Embarked %in% c("S","C","Q")) %>%
    ggplot() +
    geom_bar(aes(Embarked, fill = Pclass), position = "dodge") +
    facet_grid(~ Survived)

#2.2.2.3 Pclass vs Age and multi-dimensional plots
train %>%
    filter(Embarked %in% c("S","C","Q")) %>%
    ggplot(mapping = aes(Age, Fare, color = Survived, shape = Sex)) +
    geom_point() +
    scale_y_log10() +
    facet_grid(Pclass ~ Embarked)

#2.2.2.4 Age vs Sex
ggplot(train, aes(x=Age)) +
    geom_density(aes(fill = Survived), alpha = 0.5) +
    facet_wrap(~Sex)

#2.2.2.5 Pclass vs Sex
mosaicplot(table(train$Survived, train$Sex, train$Pclass),
           main = "Survival by Pclass and Sex", shade = TRUE)

#2.2.2.6 Parch vs SibSp
train %>%
    ggplot(aes(Parch, SibSp, color = Survived)) +
    geom_count()

#2.2.2.7 Parch vs Sex
train %>%
    ggplot() +
    geom_bar(aes(Parch, fill = Sex), position = "dodge") +
    scale_y_log10()

train %>%
    group_by(Parch, Sex) %>%
    count()

binom.test(1, 5, p = 577/(577+314))

#2.2.2.8 Age vs SibSp
train %>%
    mutate(SibSp = factor(SibSp)) %>%
    ggplot(aes(x=Age, color = SibSp)) +
    geom_density(size = 1.5)



# 2.3 Missing values imputation -------------------------------------------

# Embarked is missing
combine %>% 
    filter(Embarked == 'N/A')

combine %>%
    filter(Embarked != "N/A" & Pclass == 1 & Sex == "female") %>%
    group_by(Embarked, Pclass, Sex, Pclass, Parch, SibSp) %>%
    add_count() 

combine %>%
    filter(Embarked != "Q" & Pclass == 1 & Sex == "female") %>%
    group_by(Embarked, Pclass, Sex, Pclass, Parch, SibSp) %>%
    add_count() 

combine <- combine %>%
    mutate(Embarked = as.character(Embarked)) %>%
    mutate(Embarked = case_when(
        is.na(Embarked) ~ "C",
        TRUE ~ Embarked
    )) %>%
    mutate(Embarked = as.factor(Embarked))

# Fire missing
print(filter(combine, is.na(Fare)), width = Inf)
detach(package:Rmisc)
detach(package:plyr)
combine %>%
    ungroup() %>% 
    filter(!is.na(Fare)) %>%
    group_by(Pclass) %>% 
    summarise(med_fare = median(Fare)) %>% 
    filter(Pclass == 3) %>%
    .$med_fare ->
    med_fare_3 

combine <- combine %>%
    mutate(Fare = case_when(is.na(Fare) ~ med_fare_3,
                            TRUE ~ Fare))


# 3 Derived (engineered) features -----------------------------------------

combine <- mutate(combine,
                  fclass = factor(log10(Fare+1) %/% 1),
                  age_known = factor(!is.na(Age)),
                  cabin_known = factor(!is.na(Cabin)),
                  title_orig = factor(str_extract(Name, "[A-Z][a-z]*\\.")),
                  young = factor( if_else(Age<=30, 1, 0, missing = 0) | 
                                  title_orig %in% c('Master.','Miss.','Mlle.')),
                  child = Age<10,
                  family = SibSp + Parch,
                  alone = (SibSp == 0) & (Parch == 0),
                  large_family = (SibSp > 2) | (Parch > 3),
                  deck = if_else(is.na(Cabin),"U",str_sub(Cabin,1,1)),
                  ttype = str_sub(Ticket,1,1),
                  bad_ticket = ttype %in% c('1', '5', '6', '7', '8', 'A', 'F', 'W') )

tgroup <- combine %>%
    group_by(Ticket) %>%
    summarise(ticket_group = n()) %>%
    ungroup

combine <- left_join(combine, tgroup, by = "Ticket") %>%
    mutate(shared_ticket = ticket_group > 1)

combine <- combine %>%
    mutate(fare_eff = Fare/ticket_group,
           title = fct_lump(title_orig, n=4))

train <- combine %>% filter(!is.na(Survived))
test <- combine %>% filter(is.na(Survived))


# 3.1 Age-related parameters: age_known, young, child ---------------------
p1 <- train %>%
    ggplot(aes(age_known, fill = Survived)) +
    geom_bar(position = "fill")

p2 <- train %>%
    ggplot(aes(child, fill = Survived)) +
    geom_bar(position = "fill")

p3 <- train %>%
    ggplot(aes(young, fill = Survived)) +
    geom_bar(position = "fill")

p4 <- train %>%
    ggplot(aes(Age, fill = young)) +
    geom_density(alpha = 0.5)

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
Rmisc::multiplot(p2, p3, p4, p1, layout=layout)


plot_bar_fill_grid <- function(barx, filly, gridx, gridy){
    train %>%
        ggplot(aes_string(barx, fill = filly)) +
        geom_bar(position = "fill") +
        facet_grid(reformulate(gridy,gridx))
}

plot_bar_fill_grid("young", "Survived", "Sex", "Pclass")
plot_bar_fill_grid("age_known", "Survived", "Sex", "Pclass")


# 3.2 Family-related parameters: family, alone, large_family --------------
p1 <- train %>%
    mutate(family = as.factor(family)) %>%
    ggplot(aes(family, fill = family)) +
    geom_bar() +
    theme(legend.position = "none")

p2 <- train %>%
    ggplot(aes(alone, fill = Survived)) +
    geom_bar(position = "fill")

p3 <- train %>%
    mutate(family = as.factor(family)) %>%
    ggplot(aes(family, fill = Survived)) +
    geom_bar(position = "fill") +
    theme(legend.position = "none")

p4 <- train %>%
    ggplot(aes(large_family, fill = Survived)) +
    geom_bar(position = "fill")

layout <- matrix(c(1,1,2,3,3,4),2,3,byrow=TRUE)
Rmisc::multiplot(p1, p2, p3, p4, layout=layout)


p1 <- train %>%
    ggplot(aes(alone, fill = Survived)) +
    geom_bar(position = "stack") +
    facet_grid(Pclass ~ Sex) +
    theme(legend.position = "none")

p2 <- train %>%
    ggplot(aes(large_family, fill = Survived)) +
    geom_bar(position = "stack") +
    facet_grid(Pclass ~ Sex) +
    theme(legend.position = "none")

layout <- matrix(c(1,2),1,2,byrow=TRUE)
Rmisc::multiplot(p1, p2, layout=layout)


p1 <- train %>%
    filter(Pclass == 3) %>%
    ggplot(aes(alone, fill = Survived)) +
    geom_bar(position = "fill") +
    facet_wrap(~ Sex)

p2 <- train %>%
    filter(Pclass == 3) %>%
    ggplot(aes(large_family, fill = Survived)) +
    geom_bar(position = "fill") +
    facet_wrap(~ Sex)

layout <- matrix(c(1,2),1,2,byrow=TRUE)
Rmisc::multiplot(p1, p2, layout=layout)


# 3.3 Cabin- and ticket-related parameters: deck, cabin_known, tty --------

# Deck --------------------------------------------------------------------
#deck = if_else(is.na(Cabin),"U",str_sub(Cabin,1,1)),
p1 <- train %>%
    filter(deck != "U") %>%
    ggplot(aes(deck, fill = Pclass)) +
    geom_bar(position = "dodge") +
    coord_polar() +
    #theme(legend.position = "none") +
    scale_y_log10()

p2 <- train %>%
    filter(deck != "U") %>%
    ggplot(aes(deck, fill = Survived)) +
    geom_bar(position = "fill") +
    facet_wrap(~ Pclass, nrow = 3)

layout <- matrix(c(1,2),1,2,byrow=TRUE)
Rmisc::multiplot(p1, p2, layout=layout)


# cabin -------------------------------------------------------------------
p1 <- train %>%
    mutate(cabin_known = fct_recode(cabin_known, F = "FALSE", T = "TRUE")) %>%
    ggplot(aes(cabin_known, fill = Survived)) +
    geom_bar(position = "dodge") +
    facet_grid(Sex ~ Pclass) +
    scale_y_log10() +
    theme(legend.position = "none")

p2 <- train %>%
    mutate(cabin_known = fct_recode(cabin_known, F = "FALSE", T = "TRUE")) %>%
    ggplot(aes(cabin_known, fill = Survived)) +
    geom_bar(position = "fill") +
    facet_grid(Sex ~ Pclass) +
    theme(legend.position = "bottom")

layout <- matrix(c(1,2),1,2,byrow=TRUE)
Rmisc::multiplot(p1, p2, layout=layout)


# Ttype -------------------------------------------------------------------
#ttype = str_sub(Ticket,1,1),
p1 <- train %>%
    ggplot(aes(ttype, fill = ttype)) +
    geom_bar() +
    theme(legend.position = "none") +
    facet_wrap(~ Pclass, nrow=3)

av_surv <- train %>%
    group_by(Pclass, Survived) %>%
    count() %>%
    spread(key = Survived, value = n) %>%
    mutate(frac = `1`/(`0`+`1`))

p2 <- train %>%
    ggplot(aes(ttype, fill = Survived)) +
    geom_bar(position = "fill") +
    facet_wrap(~ Pclass, nrow = 3) +
    geom_hline(data = av_surv, aes(yintercept = frac), linetype=2)

layout <- matrix(c(1,2),1,2,byrow=TRUE)
Rmisc::multiplot(p1, p2, layout=layout)


# Bad ticket --------------------------------------------------------------
# function to extract binomial confidence levels
get_binCI <- function(x,n) as.list(setNames(binom.test(x,n)$conf.int, c("lwr", "upr")))

#bad_ticket = ttype %in% c('1', '5', '6', '7', '8', 'A', 'F', 'W') )
train %>%
    mutate(bad_ticket = factor(bad_ticket)) %>%
    group_by(bad_ticket, Survived) %>%
    count() %>%
    spread(Survived, n, fill = 0) %>%
    mutate(frac_surv = `1`/(`1`+`0`)*100,
           lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
           upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
    ggplot(aes(bad_ticket, frac_surv, fill = bad_ticket)) +
    geom_col() +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 1) +
    labs(y = "Survival fraction") +
    theme(legend.position = "none")


# Ticket group and Shared ticket ------------------------------------------
train %>%
    arrange(Ticket) %>%
    select(Ticket, ticket_group, shared_ticket, Name) %>%
    head(9) %>%
    tail(-3)


p1 <- train %>%
    group_by(Survived, shared_ticket) %>%
    count() %>%
    ggplot(aes(shared_ticket, n, fill = Survived)) +
    geom_col(position = "dodge") +
    geom_label(aes(label = n), position = position_dodge(width = 1)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p2 <- train %>%
    ggplot(aes(ticket_group, fill = Survived)) +
    geom_bar(position = "dodge") +
    theme(legend.position = "none") +
    scale_y_log10()

p3 <- train %>%
    ggplot(aes(shared_ticket, fill = Survived)) +
    geom_bar(position = "fill") +
    facet_wrap(~ Pclass) +
    theme(legend.position = "none")

p4 <- train %>%
    ggplot(aes(ticket_group, Fare)) +
    stat_summary(fun.data = "mean_cl_boot", col = "red")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
Rmisc::multiplot(p1, p2, p3, p4, layout=layout)

# combine %>% 
#     select(PassengerId, Pclass, Embarked, Fare, shared_ticket, ticket_group) %>% 
#     mutate(avgFare = Fare/ticket_group) %>% 
#     ss_send2excel()


# 3.4 Fare-related parameters: fclass, fare_eff ---------------------------

#3.4.1 fclass
train %>%
    ggplot(aes(fclass, Fare, color = Pclass)) +
    geom_sina(alpha = 0.5) +
    scale_y_log10() +
    #coord_flip() +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
    facet_zoom(xy = Pclass == 3)

#3.4.2 Fare_eff
train %>%
    group_by(Ticket) %>%
    summarise(ct = n(),
              sd_fare = sd(Fare)) %>%
    filter(ct > 1) %>%
    arrange(desc(sd_fare)) %>%
    head(3)


p1 <- train %>%
    filter(Fare>0) %>%
    ggplot(aes(Fare, Pclass, fill = Pclass)) +
    geom_density_ridges() +
    scale_x_log10(lim = c(3,1000)) +
    scale_fill_cyclical(values = c("blue", "red"))

p2 <- train %>%
    filter(fare_eff>0) %>%
    ggplot(aes(fare_eff, Pclass, fill = Pclass)) +
    geom_density_ridges() +
    scale_x_log10(lim = c(3,1000)) +
    labs(x = "Effective Fare") +
    scale_fill_cyclical(values = c("blue", "red"))

layout <- matrix(c(1,2),2,1,byrow=TRUE)
Rmisc::multiplot(p1, p2, layout=layout)


p <- train %>%
    filter(Fare>0) %>%
    mutate(log_fare = log10(Fare), log_fare_eff = log10(fare_eff)) %>%
    ggplot(aes(log_fare, log_fare_eff, color = Pclass)) +
    geom_jitter(size=2, width = 0.01, height = 0.01) +
    #geom_point(size=2) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 3, keywidth = 1, keyheight = 1))
ggMarginal(p, type="histogram", fill = "grey45", bins=20)


# 3.5 Title ---------------------------------------------------------------
p1 <- combine %>%
    group_by(title_orig) %>%
    count() %>%
    ggplot(aes(reorder(title_orig, -n, FUN = max), n, fill = title_orig)) +
    geom_col() +
    #scale_y_sqrt() +
    theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
    labs(x = "Original Titles", y = "Frequency")

p2 <- train %>%
    ggplot(aes(title, Age, fill = title)) +
    geom_violin() +
    theme(legend.position = "none") +
    labs(x = "Title groups")

p3 <- train %>%
    ggplot(aes(Survived, fill = title)) +
    geom_bar(position = "dodge") +
    labs(fill = "Title group")

layout <- matrix(c(1,1,2,3),2,2,byrow=TRUE)
Rmisc::multiplot(p1, p2, p3, layout=layout)


# 3.6 Overview ------------------------------------------------------------
train %>%
    select(-PassengerId, -Name, -Ticket, -Cabin, -title_orig) %>%
    mutate_all(as.numeric) %>%
    select(everything(), deck) %>%
    ggcorr(method = c("pairwise","spearman"), label = FALSE, angle = -0, hjust = 0.2) +
    coord_flip()



# write data for Tableau and Modeling --------------------------------------------------------------
write_csv(full_titanic, FILE_RAW_DATA_BASE)

# Clean up ----------------------------------------------------------------
rm(list = setdiff(ls(), VAR_BEFORE))
