
## ===================================================================== ##
## Script: premodelling_tasks.r
##
## Purpose: Code for pre-tests on data, transformations, split into 
##          train/validation and visualizations.
## 

## ===================================================================== ##

print("Running premodelling_task.R");


missing_values <- analysis_dataset%>% summarise_all( funs(sum(is.na(.)) / n() ) ) %>% 
  gather(key = "feature", value = "missing_pct") %>%
  arrange(missing_pct)
ggplot(data = missing_values, aes(x = reorder(feature, -missing_pct), y = missing_pct )) + 
  geom_bar(stat="identity", fill="red") + coord_flip()
names(analysis_dataset)[colSums(is.na(analysis_dataset)) > 0];# Print columns with NA
# Age, Work type, Deprivation Index, arm_sprain_all, arm_sprain

#For all numerics, get the distribution histograms before imputation and preprocessing
analysis_dataset %>% select_if(is.numeric) %>% gather() %>% filter(!is.na(value)) %>%
  ggplot( aes(value) ) + facet_wrap(~ key, scales = "free") + geom_histogram()


#Nearly 40% missing will not use this variable
analysis_dataset<- analysis_dataset %>% select( -arm_sprain_all, -arm_sprain)



################################ Imputation and Preprocessing ################################

##analysis_dataset_imputed <- mice(analysis_dataset,m=1,maxit=10,meth='pmm',seed=12345)
##ran out of memory here but this is how i would impute


analysis_dataset <- na.omit(analysis_dataset)

# Validate that there are no "NA" values in the data
checkdata <- analysis_dataset[rowSums(is.na(analysis_dataset)) > 0, ]
ifelse(nrow(checkdata) > 0, print("Rows with Missing data found"), print("No Missing Data") );

#trimming age
#analysis_dataset_imputed <- analysis_dataset %>% 
#  mutate(age = trim_outliers(age, 0.001, 0.999)
#  )



# Split labels and covariates into different dataframes.


print(table(analysis_dataset_imputed$target))



#rare even therefore using smote to divide into training and testing


# Split labels and covariates into different dataframes.
y <- analysis_dataset_imputed[ , "y"];

# Remove target
X <- analysis_dataset_imputed[, !names(analysis_dataset_imputed) %in% c("snz_uid","target")];



# Remove all columns where std deviation is zero, ie, there is no variation, and
# print the columns dropped.
X_num <- X[ ,sapply(X, is.numeric)]
fullcols <- as.list(names(X_num));
X_num <- X_num[, apply(X_num, 2, function(x){sd(x)!=0} )];
dropcols <- fullcols[!fullcols %in% as.list(names(X_num))];
X <- X[ ,!names(X) %in% dropcols];


index <- createDataPartition(y, p = 0.7, list = FALSE)
train_data <- X[index, ]
test_data  <- X[-index, ]

##########################2. Visualizations and descr. stats #############################################

# Separate out the numeric/int variables from categorical, for correlations analysis
analysis_dataset_imputed$exit_status <- analysis_dataset_imputed$y
num <- sapply(analysis_dataset_imputed, is.numeric);
apps_data_num <- analysis_dataset_imputed[ , num];
apps_data_num$target<- analysis_dataset_imputed$y;
fac <- sapply(analysis_dataset_imputed, is.factor);
apps_data_fac <- analysis_dataset_imputed[ , fac];
apps_data_fac$actual_class <- as.factor(analysis_dataset_imputed$y);

# Function to create and save histograms for all numeric variables in the dataset
create_hist <- function(data) {
  cols <- colnames(data);
  
  for(i in 1:ncol(data)){
    ggplot(data, aes(x=data[ , i], fill=as.factor(target))) + 
      geom_histogram(aes(y=..count..  ), position="dodge") + 
      xlab(as.character(cols[i])) ;
    ggsave(filename = paste("./output/plots/hist_",as.character(cols[i]),".tiff"), device="tiff");
  }
}
create_hist(apps_data_num);


print("Finished premodelling_task.R");

