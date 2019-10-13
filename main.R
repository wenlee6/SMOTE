## ===================================================================== ##
## Script: main_part1.r
##
## Purpose: This script acts as the wrapper script that runs all specific
##    components to load the data and build the  model 
##    (gradient boosting).
## 
## Author: Wen Jhe Lee
## Date: 22/02/2018
## 
## 
## ===================================================================== ##

# Update the path below to reflect your environment
setwd("C:/ACC");

# load SIU setup parameters
source("C:/ACC/include/siu_setup.R");

# Set seed for repeatabilit
set.seed(12345);

# Load libraries required for the model (versions used are given)

library(rmarkdown);
library(caret); # 6.0-76
library(ggplot2); # 2.2.1
library(stringr); # 1.2.0
library(psych); # 1.7.3.21
library(reshape2); # 1.4.2
library(mlbench); # 2.1-1
library(e1071); # 1.6-8
library(pROC); # 1.9.1
library(randomForest); # 4.6-12
library(xgboost); # 0.6-4
library(knitr); # 1.15.1
library(dplyr); # 0.5.0
library(DiagrammeR); # 0.9.0
library(Ckmeans.1d.dp); # 4.0.1
library(vcd); # 1.4-3
library(readr); # 1.1.0
library(tidyr); # 0.6.1
library(tibble); # 1.2
library(xlsx); # 0.5.7
library(corrplot); # 0.77
library(survey); # 3.31-5
library(mice);
library(randomForestSRC);
library(DMwR);


trim_outliers <- function(col, lower_cutoff, upper_cutoff) {
  limit_lower <- quantile(col, c(lower_cutoff) )
  limit_upper <- quantile(col, c(upper_cutoff) )
  col[which(col > limit_upper)] <- limit_upper
  col[which(col < limit_lower)] <- limit_lower
  return(col)
}

# 1. Load data sets
source("extract_data.R");

# 2. premodelling
source("premodelling_tasks.R");

# 3. running correlations
source("check_correlations.R");

#4 . running different models
source("model.R")




