# !diagnostics off
library(tidyverse, pos = 100)
library(microbenchmark, pos = 103)
library(yaml)

# Set up dir ------------------------------------------------------------------
FILE_NAME_PRJ_CFG <- 'projectcfg.txt'

prjCfg <- tryCatch(read_yaml(FILE_NAME_PRJ_CFG),
                   error=function(e) NULL)
stopifnot(!is.null(prjCfg))

DIR_PRJBASE <- prjCfg$project$root_dir

DIR_SCRIPT <- file.path(DIR_PRJBASE, prjCfg$project$script_dir)
DIR_INPUT <- file.path(DIR_PRJBASE, prjCfg$project$input_dir)
DIR_MIDPUT <- file.path(DIR_PRJBASE, prjCfg$project$input_dir)
DIR_OUTPUT <- file.path(DIR_PRJBASE, prjCfg$project$input_dir)

setwd(DIR_SCRIPT)

