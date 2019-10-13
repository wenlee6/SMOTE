## ===================================================================== ##
## Script: extract_dataset.r
##
## Purpose: extracts the raw data and does pre-processing
##  
## 
## Author: Wen -Jhe Lee
## Date: 22/02/2018
## 
## ===================================================================== ##


print("Running extract_dataset.R");


raw_data = read.csv(file='./Data/DataForExercise.csv')

analysis_dataset <- raw_data %>%
  rename(
    snz_uid = PersonACCId
    ,ethnicity = ethnicity_last_claim
    ,region = location_tla_last_claim
    ,work = work_last_claim
    , age = age_at_extraction_date
    , dep_index = Areaunit_score
    ,year = acci_year
  ) %>%

  mutate (
    ## Reorder pop_ind
    snz_uid =factor(snz_uid)
     ,gym = factor(ifelse(num_gym_all > 0, "Yes", "No"))
     ,wgt = factor(ifelse(num_wgt_all > 0, "Yes", "No"))
     ,back = factor(ifelse(back_sprain_all > 0 ,"Yes","No"))
    , rotator = factor(ifelse(rotator_sprain_all > 0 ,"Yes","No"))
    ,shoulder_sprain = factor(ifelse(rotator_sprain_all > 0 ,"Yes","No"))
    ,kneeleg_sprain = factor(ifelse(kneeleg_sprain_all > 0 ,"Yes","No"))
    ,thoracic_sprain = factor(ifelse(thoracic_sprain_all > 0 ,"Yes","No"))
    ,hipthigh_sprain = factor(ifelse(hipthigh_sprain_all  > 0 ,"Yes","No"))
    ,arm_sprain = factor(ifelse(arm_sprain_all  > 0 ,"Yes","No"))
    ,soft_tissue = factor(ifelse(soft_tissue_all  > 0 ,"Yes","No"))
    ,lower_back = factor(ifelse(lower_back_all  > 0 ,"Yes","No"))
    ,shoulder = factor(ifelse(shoulder_all > 0 ,"Yes","No"))
    ,neck = factor(ifelse(neck_all > 0 ,"Yes","No"))
    ,knee = factor(ifelse(knee_all > 0 ,"Yes","No"))
    ,high_leg_thigh = factor(ifelse(high_leg_thigh_all > 0 ,"Yes","No"))
    ,upper_back_spine = factor(ifelse(upper_back_spine_all > 0 ,"Yes","No"))
    ,ankle = factor(ifelse(ankle_all > 0 ,"Yes","No"))
    ,fracture = factor(ifelse(fracture_all > 0 ,"Yes","No"))
    ,dep_index = factor(dep_index)
    ,target = as.numeric(ifelse(y!="Y",0,1))
  
)


# Set the ID column name for child dataset
idcol <- "snz_uid"

# Create lists of numeric and categorical variables
catcols_analysis <- names( analysis_dataset[ , sapply( analysis_dataset, is.factor) ] %>% select(-one_of(idcol)) )
suppressWarnings(intcols_analysis<- names(analysis_dataset[ , sapply( analysis_dataset, is.integer) ] %>% select(-one_of(idcol)) ) )
numcols_analysis<- names( analysis_dataset[ , !names(analysis_dataset) %in% c(catcols_analysis, intcols_analysis, idcol)] )



print("Completed extract_dataset.R")