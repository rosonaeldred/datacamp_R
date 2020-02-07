#https://www.datacamp.com/courses/introduction-to-credit-risk-modeling-in-r
library(gmodels) #for CrossTable
library(ggplot2) #to save plots, maybe
library(stats) #for IQR

##############################
# CH1, section 1, Introduction
##############################

#import data
loan_data <- readRDS("data/loan_data_ch1.rds")
head(loan_data,10)

#examing Categorical variables legels, need gmodels for CrossTable
CrossTable(loan_data$home_ownership)

#Use loan_status as second variable to examing relationship with response variable
CrossTable(loan_data$home_ownership, loan_data$loan_status, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE)
# prop.r = TRUE and the rest false gives you the row-wise proportion
#what do we see here? Other and Mortgage have higher default rates 

##############################
# CH1, section 2, histograms and outliers
##############################

##### Histograms
png("gfx/creditrating_hist-int_rate.png")
hist(loan_data$int_rate, main="Hist of interest rate", xlab="Interest Rate")
dev.off()
# Frequency on Y axes

png("gfx/creditrating_hist-ann_inc.png")
hist_income <-hist(loan_data$annual_inc, main="Hist of Annual Income", xlab="Annual Income")
dev.off()
#lopsided, one bar

hist_income$breaks

#change breaks, using sqrt of number of observations, e.g. 
n_breaks <-sqrt(nrow(loan_data))

png("gfx/creditrating_hist-ann_inc_sqrtn-breaks.png")
hist_income_n <- hist(loan_data$annual_inc, breaks=n_breaks, main="Hist of Annual Income", xlab="Annual Income")
dev.off()

#check scatter plot for outliers 
#see here one huge salary of 6 Million 
png("gfx/creditrating_scatter-ann_inc.png")
plot(loan_data$annual_inc, ylab="Annual Income")
dev.off()

### Outliers
# Expert opinion: more than 3Million is a lot. Rule of thumb? 
index_outlier_expert <-which(loan_data$annual_inc>3000000)
loan_data_expert <-loan_data[-index_outlier_expert,]

# rule of thumb: outlier if bigger than Q3+1.5*!QR (Tukey Bracket)
#need library(states) for IQR
outlier_cutoff <-quantile(loan_data$annual_inc, 0.75)+1.5*IQR(loan_data$annual_inc)
index_outlier_ROT <- which(loan_data$annual_inc>outlier_cutoff)
loan_data_ROT <- loan_data[-index_outlier_ROT,]

# Histograms without Outliers
png("gfx/creditrating_hist-expert-ann-inc.png")
hist(loan_data_expert$annual_inc, sqrt(nrow(loan_data_expert)),xlab="Annual income expert judgement")
dev.off()

png("gfx/creditrating_hist-ROT-ann-inc.png")
hist(loan_data_ROT$annual_inc, sqrt(nrow(loan_data_ROT)),xlab="Annual income ROT")
dev.off()

###### Bivariate plot. First var X axis, second Y axis. 
# lets you see 2D outliers. See same 6 Million one 
png("gfx/creditrating_scatter_emplength-vs-annualinc.png")
plot(loan_data$emp_length, loan_data$annual_inc, xlab="Employment length", ylab="Annual income")
dev.off()

######################### 
# Exercises Ch1 Sec 2

# Create histogram of loan_amnt: hist_1
hist_1 <- hist(loan_data$loan_amnt)

# Print locations of the breaks in hist_1
hist_1$breaks

# Change number of breaks and add labels: hist_2
hist_2 <- hist(loan_data$loan_amnt, breaks = 200, xlab = "Loan amount", main = "Histogram of the loan amount")


### Outliers in Age 
# Plot the age variable
plot(loan_data$age, ylab="Age")

# Save the outlier's index to index_highage
index_highage <-which(loan_data$age>122)

# Create data set new_data with outlier deleted
new_data <- loan_data[-index_highage, ]

# Make bivariate scatterplot of age and annual income
plot(loan_data$age, loan_data$annual_inc, xlab = "Age", ylab = "Annual Income")


##############################
# CH1, section 3, missing data and coarse classification 
# int_rate and emp_length has NAs
##############################

## looking for number of NAs: 
summary(loan_data$emp_length)

################
#Delete row/column, replace or keep 
#Delete Row: 
index_NA <- which(is.na(loan_data$emp_length)) 
loan_data_no_NA <- loan_data[-c(index_NA),]

#Delete Column: 
loan_data_delete_employ <-loan_data
loan_data_delete_employ$emp_length <-NULL

###############
# Replace / median imputation 
index_NA <- which(is.na(loan_data$emp_length)) 
loan_data_replace <-loan_data
#replace with median from non-NA entries: 
loan_data_replace$emp_length[index_NA] <-median(loan_data$emp_length,na.rm=TRUE)


###############
# Keeping NAs via coarse classification with bins 

######################### 
# Exercises Ch1 Sec 3

##### Delete missing data
# Look at summary of loan_data
summary(loan_data$int_rate)

# Get indices of missing interest rates: na_index
na_index <-which(is.na(loan_data$int_rate)) 

# Remove observations with missing interest rates: loan_data_delrow_na
loan_data_delrow_na <- loan_data[-c(na_index), ]

# Make copy of loan_data
loan_data_delcol_na <- loan_data

# Delete interest rate column from loan_data_delcol_na
loan_data_delcol_na$int_rate <- NULL

##### Replace missing data
# Compute the median of int_rate
median_ir <- median(loan_data$int_rate, na.rm=TRUE)

# Make copy of loan_data
loan_data_replace <- loan_data

# Replace missing interest rates with median
loan_data_replace$int_rate[c(na_index)] <-median_ir

# Check if the NAs are gone
summary(loan_data_replace$int_rate)


### bins
# Make the necessary replacements in the coarse classification example below 
loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))

loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"

loan_data$ir_cat <- as.factor(loan_data$ir_cat)

# Look at your new variable using plot()
plot(loan_data$ir_cat)


##############################
# CH2, section 1, Data splitting and confusion matrices
##############################
## reload data
loan_data <- readRDS("data/loan_data_ch2.rds")

######################### 
# Exercises Ch2 Sec 1

##### Splitting the data set
# Set seed of 567
set.seed(567)

# Store row numbers for training set: index_train
index_train <- sample(1:nrow(loan_data), 2/3*nrow(loan_data))

# Create training set: training_set
training_set <- loan_data[index_train, ]

# Create test set: test_set
test_set <- loan_data[-index_train, ]

##### Creating a confusion matrix 
# Create confusion matrix
#In this example, assume that you have run a model and stored the predicted outcomes 
#in a vector called model_pred. 
#You want to see how the model performed so you will construct a confusion matrix. 
#You will compare the actual loan status column (loan_status) to the predicted values 
#(model_pred), using the table() function, where the arguments are the true values 
#and the predicted values. 
conf_matrix <-table(test_set$loan_status, model_pred)

# Compute classification accuracy
(conf_matrix[1,"0"]+conf_matrix[2,"1"])/(conf_matrix[1,"0"]+conf_matrix[2,"1"]+conf_matrix[1,"1"]+conf_matrix[2,"0"])

# Compute sensitivity
(conf_matrix[2,"1"])/(conf_matrix[2,"1"]+conf_matrix[2,"0"])

##############################
# CH2, section 2, logistic regression model, intro
##############################
# regression model with output between 0 and 1
  
#Fitting a logistic model in R
log_model <- glm(loan_status~age, family="binomial", data=training_set)
log_model

#Coefficients:
#  (Intercept)          age  
#-1.924527    -0.005997  

######################### 
# Exercises Ch2 Sec 2

#####Basic logistic regression
#In the video, you looked at a logistic regression model including the variable age as a predictor. Now, you will include a categorical variable, and learn how to interpret its parameter estimates.
# When you include a categorical variable in a logistic regression model in R, you will obtain a parameter estimate for all but one of its categories. This category for which no parameter estimate is given is called the reference category. The parameter for each of the other categories represents the odds ratio in favor of a loan default between the category of interest and the reference category. Don't worry if this doesn't make complete sense to you yet, you'll do more exercises on this later on!

# Build a glm model with variable ir_cat as a predictor
log_model_cat <-glm(loan_status~ir_cat, family="binomial", data=training_set)

# Print the parameter estimates 
log_model_cat

# Look at the different categories in ir_cat using table()
table(loan_data$ir_cat)


#####Multiple variables in a logistic regression model
# Build the logistic regression model
log_model_multi <- glm(loan_status~age+ir_cat+grade+loan_amnt+annual_inc,family="binomial",  data=training_set)

# Obtain significance levels using summary()
summary(log_model_multi)

##############################
# CH2, section 3, Logistic regression model: predicting the probability of default
##############################

### An example with age and home ownership
log_model_small <- glm(loan_status ~age+home_ownership, family="binomial", data=training_set)
log_model_small

### Making predictions in R with a test case
test_case <- as.data.frame(test_set[1,])
test_case
predict(log_model_small, newdata=test_case)
# output of predict is NOT the probability of default, but the predictor

predict(log_model_small, newdata=test_case, type="response")

######################### 
# Exercises Ch2 Sec 3

#####Predicting the probability of default
# Build the logistic regression model
# Build the logistic regression model
predictions_all_small <- predict(log_model_small, newdata = test_set, type = "response")

#> range(predictions_all_small)
#[1] 0.08148754 0.15533685
# Look at the range of the object "predictions_all_small"
range(predictions_all_small)

#####Making more discriminative models

# Change the code below to construct a logistic regression model using all available predictors in the data set
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)

# Make PD-predictions for all test set elements using the the full logistic regression model
predictions_all_full <-predict(log_model_full, newdata = test_set, type = "response")

# Look at the predictions range
range(predictions_all_full)
#[1] 6.471741e-06 5.436561e-01

##############################
# CH2, section 4, Evaluating the logistic regression model result
##############################

# Confusion matrix ? Need cutoff or threshold value

######################### 
# Exercises Ch2 Sec 4
#using ifelse() 
#ifelse(predictions > 0.3, 1, 0)

# The code for the logistic regression model and the predictions is given below
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")

# Make a binary predictions-vector using a cut-off of 15%
pred_cutoff_15 <- ifelse(predictions_all_full >0.15,1,0)

# Construct a confusion matrix
table(test_set$loan_status, pred_cutoff_15)

##### Comparing Link functions for a given cut-off 
##The observed outcome (default versus non-default) is stored in true_val in the console.
# Fit the logit, probit and cloglog-link logistic regression models
log_model_logit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,family = binomial(link = logit), data = training_set)
log_model_probit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,family = binomial(link = probit), data = training_set)
log_model_cloglog <-glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,family = binomial(link = cloglog), data = training_set)

# Make predictions for all models using the test set
predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")
predictions_probit <- predict(log_model_probit, newdata = test_set, type = "response")
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response")

# Use a cut-off of 14% to make binary predictions-vectors
cutoff <- 0.14
class_pred_logit <- ifelse(predictions_logit > cutoff, 1, 0)
class_pred_probit <- ifelse(predictions_probit > cutoff, 1, 0)
class_pred_cloglog <- ifelse(predictions_cloglog > cutoff, 1, 0)

# Make a confusion matrix for the three models
  tab_class_logit <- table(true_val,class_pred_logit)
tab_class_probit <- table(true_val,class_pred_probit)
tab_class_cloglog <- table(true_val,class_pred_cloglog)

# Compute the classification accuracy for all three models
acc_logit <- sum(diag(tab_class_logit)) / nrow(test_set)
acc_probit <- sum(diag(tab_class_probit)) / nrow(test_set)
acc_cloglog <- sum(diag(tab_class_cloglog)) / nrow(test_set)


##############################
# CH3 Sec 1 Decision trees
##############################

######################### 
# Exercises Ch3 Sec 1 : computing gini gain 

# The Gini-measure of the root node is given below
gini_root <- 2 * 89 / 500 * 411 / 500

# Compute the Gini measure for the left leaf node
gini_ll <-2*(401/446)*(45/446)

# Compute the Gini measure for the right leaf node
gini_rl <-2*(10/54)*(44/54)

# Compute the gain
gain <- gini_root - 446 / 500 * gini_ll - 54 / 500 * gini_rl

# compare the gain-column in small_tree$splits with our computed gain, multiplied by 500, and assure they are the same
small_tree$splits
improve <- gain * 500

##############################
# CH3 Sec 2 Building decision trees using rpart
##############################
######################### 
# Exercises Ch3 Sec 2 : 

##### Undersampling 

#The training set has been undersampled for you, such that 
#1/3 of the training set consists of defaults, and 
#2/3 of non-defaults. 
#The resulting data set is available in your workspace and named undersampled_training_set

# Load package rpart in your workspace.
library(rpart)

# first attempt with tree 
tree_fromlecture <- rpart(loan_status ~ ., method = "class",data =  training_set)


# Change the code provided in the video such that 
#a decision tree is constructed using the undersampled training set.
#Include rpart.control to relax the complexity parameter to 0.001.
tree_undersample <- rpart(loan_status ~ ., method = "class",control = rpart.control(cp = 0.001), data =undersampled_training_set)

# Plot the decision tree
#Add a second argument uniform = TRUE to get equal-sized branches.
plot(tree_undersample, uniform=TRUE)

# Add labels to the decision tree
text(tree_undersample)


##### Changing the prior probabilities
# Change the code below such that a tree is constructed with adjusted prior probabilities.
tree_prior <- rpart(loan_status ~ ., method = "class",data = training_set, parms = list(prior=c(0.7, 0.3)), control = rpart.control(cp = 0.001))

#WARNING: Next to code lines MUST BE RUN TOGETHER; not in sequence
# Plot the decision tree
plot(tree_prior, uniform=TRUE)

# Add labels to the decision tree
text(tree_prior)

#####Including a loss matrix
# Change the code below such that a decision tree is constructed using a loss matrix penalizing 10 times more heavily for misclassified defaults.
tree_loss_matrix <- rpart(loan_status ~ ., method = "class",data =  training_set, parms = list(loss = matrix(c(0, 10, 1, 0), ncol=2)), control = rpart.control(cp = 0.001))

# Plot the decision tree
plot(tree_loss_matrix, uniform=TRUE)

# Add labels to the decision tree
text(tree_loss_matrix)
  
######################### 
# Exercises Ch3 Sec 3 :  pruning trees
# CAUTION, tree_prior not here 
##### Via complexity
# tree_prior is loaded in your workspace

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_prior)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized.
printcp(tree_prior)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_prior$cptable[ , "xerror"])

# Create tree_min
tree_min <- tree_prior$cptable[index, "CP"]
#  Prune the tree using tree_min
ptree_prior <- prune(tree_prior, cp = tree_min)

# Use prp() to plot the pruned tree
prp(ptree_prior)

#####Pruning the tree with the loss matrix
#In this exercise, you will prune the tree that was built using a loss matrix 
#in order to penalize misclassified defaults more than misclassified non-default

# set a seed and run the code to construct the tree with the loss matrix again
set.seed(345)
tree_loss_matrix<- rpart(loan_status ~ ., method = "class", data = training_set,parms = list(loss=matrix(c(0, 10, 1, 0), ncol = 2)), control = rpart.control(cp = 0.001))

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_loss_matrix)


# Prune the tree using cp = 0.0012788
ptree_loss_matrix <-prune(tree_loss_matrix, cp=0.0012788)

# Use prp() and argument extra = 1 to plot the pruned tree
prp(ptree_loss_matrix, extra=1)

#####One final tree using more options
# set a seed and run the code to obtain a tree using weights, minsplit and minbucket
set.seed(345)
tree_weights <- rpart(loan_status ~ ., method = "class",data = training_set,
                      control = rpart.control(minsplit =5, minbucket = 2, cp = 0.001), weights=case_weights)

# Plot the cross-validated error rate for a changing cp
plotcp(tree_weights)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_weights$cp[ , "xerror"])

# Create tree_min
tree_min <- tree_weights$cp[index, "CP"]

# Prune the tree using tree_min
ptree_weights <-prune(tree_weights,cp=tree_min)

# Plot the pruned tree using the rpart.plot()-package

prp(ptree_weights, extra=1)


#####Confusion matrices and accuracy of our final trees
# Make predictions for each of the pruned trees using the test set.
pred_undersample <- predict(ptree_undersample, newdata = test_set,  type = "class")
pred_prior <-predict(ptree_prior, newdata = test_set,  type = "class")
pred_loss_matrix <-predict(ptree_loss_matrix, newdata = test_set,  type = "class")
pred_weights <-predict(ptree_weights, newdata = test_set,  type = "class")

# construct confusion matrices using the predictions.
confmat_undersample <- table(test_set$loan_status, pred_undersample)
confmat_prior <- table(test_set$loan_status, pred_prior)
confmat_loss_matrix <- table(test_set$loan_status, pred_loss_matrix)
confmat_weights <- table(test_set$loan_status, pred_weights)

# Compute the accuracies
acc_undersample <- sum(diag(confmat_undersample)) / nrow(test_set)
acc_prior <-sum(diag(confmat_prior))/ nrow(test_set)
acc_loss_matrix <-sum(diag(confmat_loss_matrix))/ nrow(test_set)
acc_weights <-sum(diag(confmat_weights))/ nrow(test_set)
