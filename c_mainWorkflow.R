# !diagnostics off
library(tidyverse)
library(purrrlyr)
library(stringr)
library(microbenchmark)


# Set up dir ------------------------------------------------------------------
DIR_PRJBASE <- 'D:/daSpace/workshop/Kaggle/Titanic/shop'

DIR_SCRIPT <- file.path(DIR_PRJBASE, 'script')
setwd(DIR_SCRIPT)

DIR_INPUT <- file.path(DIR_PRJBASE, 'input')
DIR_MIDPUT <- file.path(DIR_PRJBASE, 'midput')
DIR_OUTPUT <- file.path(DIR_PRJBASE, 'output')


# Data steps --------------------------------------------------------------
FILE_SRC_DATA_RAW_TRAIN <- file.path(DIR_INPUT, 'train.csv')
FILE_SRC_DATA_RAW_TEST <- file.path(DIR_INPUT, 'test.csv')
FILE_SRC_DATA_RAW_TRUTH <- file.path(DIR_INPUT, 'titanic3.csv')

FILE_CLEAN_DATA_BASE_CSV <- file.path(DIR_MIDPUT, 'fullset.csv')
FILE_CLEAN_DATA_BASE_RDS <- file.path(DIR_MIDPUT, 'fullset.RDS')
FILE_CLEAN_DATA_GROUND_TRUTH <- file.path(DIR_MIDPUT, 'groundtruth.csv')


#  Data process protocal ------------------------------------------------------------------------
DATA_DEFALUT_Survived_Dummy <- 1

AGE_BIN_CUTOFF <- c(0,2,5,9,12,15,21,55,65,100)

DATA_TAG_TRAIN <- 'Train'
DATA_TAG_TEST <- 'Test'


# Load data ---------------------------------------------------------------
getCleanBaseDf <- function(reprocess = FALSE){
    if(reprocess){# Only need to process once
        source('d_prepareBase.R', local= TRUE)
    }
    
    return(readRDS(FILE_CLEAN_DATA_BASE_RDS)) 
}

#getCleanBaseDf(reprocess=TRUE)

getRawBaseDf <- function(){
    read_csv(FILE_CLEAN_DATA_BASE_CSV) %>% 
    select(- contains('_')) ->
    ret
    
    return(ret)
}


# Evaluation tool ---------------------------------------------------------
source('c_evaluation.R', local= TRUE)