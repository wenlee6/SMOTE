

## ===================================================================== ##
## Script: check_correlations.r
##
## Purpose: Code for removing correlations that are highly correlated to each other 
##          
## 
## ===================================================================== ##

print("Running check_correlations.R")

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return( cormat);
}

;

# Set global variables
correlation_cutoff_absvalue = 0.6;

# Check correlations and plot correlation matrix
correlationMatrix <- cor(X_num);
upper_tri <- get_upper_tri(correlationMatrix);
melted_cormat <- melt(correlationMatrix);

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) + geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), 
                       space = "Lab", name="Pearson\nCorrelation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed();

ggsave(filename = paste("./output/plots/correlationmatrix.tiff"), device=tiff);

# Find candidate attributes to remove
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=correlation_cutoff_absvalue);
print(names(X_num)[highlyCorrelated]);

# COMMENT OUT AS REQUIRED !! Drop the variables not required from the dataset on the basis of correlation.
dropcols <- names(X_num)[highlyCorrelated];
train_data <- train_data[ ,!names(train_data) %in% dropcols];
test_data<- test_data[ ,!names(test_data) %in% dropcols];

print("Running check_correlations.R")
