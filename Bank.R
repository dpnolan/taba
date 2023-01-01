# Peter Nolan x22154116
# 
# Code for Terminal Assessment by Assignment (TABA)
# 
# Bank telemarketing analysis
#
#See Google Colab workbook at   
# https://colab.research.google.com/gist/dpnolan/7bf9b9b342490bfba31fb761510f9c19/tata-time-series-analysis.ipynb?authuser=3#scrollTo=-VpUAwwDymKr
# 
# Github depo https://github.com/dpnolan/taba 

####################
# Section 1 - Load data
####################
# Go to the working directory, import and summarise the supplied input data

getwd()
setwd("/Users/peternolan/Documents/Post Grad/NCI/Courses/Statistics/TABA/Bank")
list.files()
bank=read.csv('Bank.csv')
bank=data.frame(bank)
summary(bank)
bank_clean<-bank

# bank holds the imported data
# bank_clean is the data that is processed

####################
# Section 2 - Pre-processing
####################

# Note that balance is in the dataset but not in the document
# I include it in the analysis here

# Convert the categorical variables to factors in R
job_factor<-factor_clean(bank$job)
summary(job_factor)
bank_clean<-cbind(bank_clean,job_factor)

marital_factor<-factor(bank$marital)
summary(marital_factor)
bank_clean<-cbind(bank_clean,marital_factor)

education_factor<-factor(bank$education)
summary(education_factor)
bank_clean<-cbind(bank_clean,education_factor)

default_factor<-factor(bank$default)
summary(default_factor)
bank_clean<-cbind(bank_clean,default_factor)

housing_factor<-factor(bank$housing)
summary(housing_factor)
bank_clean<-cbind(bank_clean,housing_factor)

loan_factor<-factor(bank$loan)
summary(loan_factor)
bank_clean<-cbind(bank_clean,loan_factor)

# month is converted to factor but preserving the defined calendar order
month_factor<-factor(bank$month,levels=c('jan','feb','mar','apr','may',
                                         'jun','jul','aug','sep','oct','nov','dec'))
summary(month_factor)
bank_clean<-cbind(bank_clean,month_factor)

contact_factor<-factor(bank$contact)
summary(contact_factor)
bank_clean<-cbind(bank_clean,contact_factor)

poutcome_factor<-factor(bank$poutcome)
summary(poutcome_factor)
bank_clean<-cbind(bank_clean,poutcome_factor)

#y is the binary outcome variable
y_factor<-factor(bank$y)
summary(y_factor)
bank_clean<-cbind(bank_clean,y_factor)
# Counts
# no   yes 
# 39,922  5,289 
# 5289/(39922+5289)=  0.1169848 approx = 11.7%, 
# Similar to the original study by Moro et al. 2014
# The classes are significantly imbalanced, 
# so simply constantly predicting y='no' will be correct about 88.3% of the time

# pdays - values for -1, we assume are NA, no recorded value
bank_clean['pdays'][bank_clean['pdays'] == -1]<-NA
pdays<-bank_clean['pdays']
#How many NA and how many real values are there? 8,257 real, 36,954 NAs
length(bank_clean$pdays)-sum(is.na(bank_clean$pdays))
summary(bank_clean$pdays)

summary(bank_clean)

# Delete the now factorised variables from the working data i.e.  
library('dplyr')
bank_clean <- dplyr::select(bank_clean,-job,-marital,-education,-default,-housing,-loan,
                     -contact, -poutcome,-month,-day,-pdays,-y)


# bank clean contains the dependent variables plus y_factor, the binary dependent variable
independent_variables <- dplyr::select (bank_clean,-y_factor)
 
# Print the column names of our working data for visual inspection of the columns remaining
colnames(bank_clean)
colnames(independent_variables)

# Get summary statistics for our working dataframe
summary(bank_clean)


####################
# Section 3 - Descriptive statistics and graphing the variables
####################
library(ggplot2) 
library(scales)

dev.off() # might be needed to clear the plot buffer

# Violin plots for the numerical variables
# Is the y='yes' population associated with particular common characteristics?
# Code adapted from Chang, 2019, section 6.8 and 6.9
# White dot shows the median
# Black area shows a box plot with top and bottom edges at 75 and 25 percentile levels



# Graph 1 - customer age in yes and no datasets
hw_p <- ggplot(bank_clean, aes(x = y_factor, y=age, fill=y_factor  ))
hw_p + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + 
  stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)
# 'yes' population skews more, 0.87 v 0.59, towards the older and younger extreme,
# than the 'no' population

mean(bank_clean['age'][bank_clean['y_factor']=='yes']) # 41.67
mean(bank_clean['age'][bank_clean['y_factor']=='no']) # 40.84
# mean age is older for 'yes'
median(bank_clean['age'][bank_clean['y_factor']=='yes']) # 38 
median(bank_clean['age'][bank_clean['y_factor']=='no']) # 39
# median age is younger for 'yes'
skewness(bank_clean['age'][bank_clean['y_factor']=='yes']) # 0.87
skewness(bank_clean['age'][bank_clean['y_factor']=='no']) # 0.59
# skew much greater for 'yes'
kurtosis(bank_clean['age'][bank_clean['y_factor']=='yes']) # 0.28
kurtosis(bank_clean['age'][bank_clean['y_factor']=='no']) # 0.052
# kurtosis much greater for 'yes'

# Graph 2 - relationship balance in yes and no datasets
hw_p <- ggplot(bank_clean, aes(x = y_factor, y=balance, fill=y_factor  ))
hw_p + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + 
  stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)
# 'yes' population has bigger balance on the relationship
mean(bank_clean['balance'][bank_clean['y_factor']=='yes'])
mean(bank_clean['balance'][bank_clean['y_factor']=='no'])
# Greater mean balance, 1804 v 1304, for 'yes' 

# Graph 3 - relationship balance in yes and no datasets with a limit on the y-axis
hw_p <- ggplot(bank_clean, aes(x = y_factor, y=balance, fill=y_factor  ))
hw_p + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + ylim(0,12500)
stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)
mean(bank_clean['balance'][bank_clean['y_factor']=='yes'])
mean(bank_clean['balance'][bank_clean['y_factor']=='no'])
median(bank_clean['balance'][bank_clean['y_factor']=='yes'])
median(bank_clean['balance'][bank_clean['y_factor']=='no'])
skewness(bank_clean['balance'][bank_clean['y_factor']=='yes'])
skewness(bank_clean['balance'][bank_clean['y_factor']=='no'])
kurtosis(bank_clean['balance'][bank_clean['y_factor']=='yes'])
kurtosis(bank_clean['balance'][bank_clean['y_factor']=='no'])
# 'yes' population has bigger balance on the relationship, 
# mean 1804 v 1303 EUR, median 733 v 417 EUR
# 'yes' population has far fewer zero or very low balance values than the 'no' population

# Graph 4 - last contact duration, in seconds in yes and no datasets
hw_p <- ggplot(bank_clean, aes(x = y_factor, y=duration, fill=y_factor  ))
hw_p + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + 
  stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)
mean(bank_clean['duration'][bank_clean['y_factor']=='yes'])
mean(bank_clean['duration'][bank_clean['y_factor']=='no'])
median(bank_clean['duration'][bank_clean['y_factor']=='yes'])
median(bank_clean['duration'][bank_clean['y_factor']=='no'])
# duration, yes has fewer short conversations, 537 seconds (about 9 mins) v 221 (under 4 mins)
# median 426 v 164, 7 mins against 2.5


# Graph 5 - number of contacts during this campaign for this client in yes and no datasets
hw_p <- ggplot(bank_clean, aes(x = y_factor, y=campaign,fill=y_factor  ))
hw_p + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + 
  stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)
mean(bank_clean['campaign'][bank_clean['y_factor']=='yes'])
mean(bank_clean['campaign'][bank_clean['y_factor']=='no'])
median(bank_clean['campaign'][bank_clean['y_factor']=='yes'])
median(bank_clean['campaign'][bank_clean['y_factor']=='no'])

dev.off()

# Graph 5.1 - y axis limit on number of contacts during this campaign for this client in yes and no datasets
hw_p <- ggplot(bank_clean, aes(x = y_factor, y=campaign,fill=y_factor  ))
hw_p + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + ylim(0,15)
  stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)
# 'yes' population has lower number of contacts during this campaign, mean - 2.14 v 2.85


# Graph 6 - number of contacts before this campaign for this client in yes and no datasets
  hw_q <- ggplot(bank_clean, aes(x = y_factor, y=previous,fill=y_factor  ))
hw_q + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + 
  stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)

# Graph 6.1 y axis limited with number of contacts before this campaign for this client in yes and no datasets
hw_q <- ggplot(bank_clean, aes(x = y_factor, y=previous,fill=y_factor  ))
hw_q + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + ylim(0,10)
stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)
mean(bank_clean['previous'][bank_clean['y_factor']=='yes'])
mean(bank_clean['previous'][bank_clean['y_factor']=='no'])
median(bank_clean['previous'][bank_clean['y_factor']=='yes'])
median(bank_clean['previous'][bank_clean['y_factor']=='no'])
# 'yes' population saw more contacts before this campaign, 1.17 v 0.5, 
# but median for both is zero contacts before this campaign

####################
# Bar charts for the categorical variables
####################
# Is the y='yes' population associated with particular common characteristics?
# Main code source is Rhys (2020), section 4.2.3

# Graph 7 - Show breakdown of month_factor by 'yes' and 'no' population
ggplot(data=bank_clean,aes(x=y_factor, fill=month_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# In the 'yes' population, Feb, Mar, Apr, Oct are bigger proportion
# May is much bigger in the 'no' population


# Graph 8 - Show breakdown of job_factor by 'yes' and 'no' population
ggplot(data=bank_clean,aes(x=y_factor, fill=job_factor)) +
  geom_bar(position = "fill", colour='black') +
  scale_y_continuous(labels = scales::percent)
# 'yes' population has less job=blue-collar, more retired, some more student 

library(patchwork)

# Graph 9 - Show breakdown of marital_factor by 'yes' and 'no' population
g9<-ggplot(data=bank_clean,aes(x=y_factor, fill=marital_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)

# Graph 10 - Show breakdown of education_factor by 'yes' and 'no' population
g10<-ggplot(data=bank_clean,aes(x=y_factor, fill=education_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# more third-level education and fewer primary, 
# which we would expect as the more educated are more affluent customers, 

g9 + g10 

# Graph 11- Show breakdown of default_factor by 'yes' and 'no' population
g11<-ggplot(data=bank_clean,aes(x=y_factor, fill=default_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# defaulted customers are a very small percentage of both 'yes' and 'no'

# Graph 12 - Show breakdown of housing_factor by 'yes' and 'no' population
g12<-ggplot(data=bank_clean,aes(x=y_factor, fill=housing_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# 'yes' customers are significantly less likely to have a mortgage
# They may be saving for a house purchase?  

g11+g12

# Graph 13- Show breakdown of loan_factor by 'yes' and 'no' population
g13<-ggplot(data=bank_clean,aes(x=y_factor, fill=loan_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# 'has personal loan' more likely for 'no' customers


# Graph 14 - Show breakdown of contact_factor by 'yes' and 'no' population
g14<-ggplot(data=bank_clean,aes(x=y_factor, fill=contact_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# more cell phones and less unknown among 'yes' customers

g13+g14

# Show breakdown of poutcome_factor by 'yes' and 'no' population
g15<-ggplot(data=bank_clean,aes(x=y_factor, fill=poutcome_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# Previous success with the client is significantly higher among 'yes' customers 

g15

####################
# Section 4 - Model Fitting
####################

# all = variables in our dataset

glm.all<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
              + previous + job_factor + marital_factor + education_factor + default_factor 
              + housing_factor + loan_factor + contact_factor + poutcome_factor, 
              data=bank_clean, 
              family = binomial)
summary(glm.all)

# Null deviance: 32631  on 45210  degrees of freedom
# Residual deviance: 21578  on 45170  degrees of freedom
# AIC: 21660
# Shows Wald statistics and the associated probability 
# At 5% significance, two sided prob factor, NOT significant are 
# age, previous, job=unknown or unemployed, marital=single, default=yes, 
# contact=telephone or other, poutcome = unknown

#coefficients
coef(glm.all)
# odds ratios 
exp(coef(glm.all))
# the odds that a customer with poutcome=success is 9.968 times 
# than the odds a customer with the same characteristics apart from that will buy the offering
# target these customers for sales efforts
# Month effects odds ratios are Mar is 15.171 odds ratio, Sept 7.256, Oct 7.79, Dec 6.18 
# Target customers at these times

# Third-level educated (1.46), retired (1.29) and student (1.47) customers 
# show favourable odds ratios

# Diagnosis of the logistic regression
summary(residuals(glm.all))
plot(residuals(glm.all))

colnames(bank_clean)
#re-run including pdays in the regression

#add back p_days into the data
bank_clean<-cbind(bank_clean,pdays)
colnames(bank_clean)
glm.all2<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
               + previous + job_factor + marital_factor + education_factor + default_factor 
               + housing_factor + loan_factor + contact_factor + poutcome_factor
               +pdays, 
               data=bank_clean,
               family = binomial)

summary(glm.all2)
# Null deviance: 8919.8  on 8256  degrees of freedom
# Residual deviance: 5809.6  on 8215  degrees of freedom
# (36954 observations deleted due to missingness)
# AIC: 5893.6
# 
# Most observations are deleted because 36,000 NAs in pdays field
# AIC and deviance are  much lower
# pdays is nowhere near significance in this regression
# Many other variables drop out also

# Let's drop pdays variable as insignificant
bank_clean<-select(bank_clean,-pdays)
colnames(bank_clean)

glm.reduced<-glm(y_factor ~balance + month_factor + duration + campaign 
                + job_factor + marital_factor + education_factor +
               + housing_factor + loan_factor + contact_factor + poutcome_factor, 
               data=bank_clean,
               family = binomial)
# age, previous, pdays, default are excluded

summary(glm.reduced)

# Null deviance: 32631  on 45210  degrees of freedom
# Residual deviance: 23046  on 45194  degrees of freedom
# AIC: 23080, much worse than with glm.all

performance_hosmer(glm.reduced, n_bins = 10)
# Hosmer-Lemeshow Goodness-of-Fit Test
#
# Chi-squared: 516.531
# df:   8    
# p-value:   0.000
# 
# Summary: model does not fit well.
#
# H-L statistics still do not fit well

# User-selected variables, those that I identified in the graphical analysis as possibly 
# significantly different between 'yes' and 'no' customers? 
glm.user<-glm(y_factor ~ month_factor + duration 
               + previous +  poutcome_factor + contact_factor + housing_factor + education_factor,
               data=bank_clean,
               family = binomial)
summary(glm.user)

# Null deviance: 32631  on 45210  degrees of freedom
# Residual deviance: 21909  on 45188  degrees of freedom
# AIC: 21955, still worse than the glm.all model
performance_hosmer(glm.user, n_bins = 10)
# Hosmer-Lemeshow Goodness-of-Fit Test
#
# Chi-squared: 503.889
# df:   8    
# p-value:   0.000
# 
# Summary: model does not fit well.
#
# Still does not show significance for the overall model
# 

####################
# Section 5 - Automated model selection
####################

# Define models with (1) no variables, and (2) all variables

null_model<-glm(y_factor~1,data=independent_variables,family=binomial)
full_model<-glm(y_factor~. , data=independent_variables,family=binomial)

glm.fits3<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
               + previous + job_factor + marital_factor + education_factor + default_factor 
               + housing_factor + loan_factor + contact_factor + poutcome_factor,
               data=bank_clean,
               family = binomial)

library(MASS)

glm_step_AIC <- stepAIC(full_model,trace = TRUE, direction= "both")

summary(glm_step_AIC)

step_model <- step(null_model, 
                   scope = list(lower = null_model,
                                upper = full_model),
                   direction = "both")

summary(step_model)
plot(step_model)


####################
# Section 6 - Classification
####################

# How do our models perform in classifying the 'yes' or 'no' customers     
# based on the glm.all model?
# This follows Hastie et al. (2021), section 4.3 and 4.7.2.

summary(y_factor)
# no   yes 
# 39,922  5,289 
# 5289/(39922+5289)=  0.1169848 approx = 11.7%
# Similar to the original study by Moro et al. (2014)
# 
# The classes are significantly imbalanced, with many more 'no' consumers than 'yes'
# Naive classifier constantly predicting 'no' will be correct about 88.3% of the time
#
# Here, first we calculate using the 50% threshold, then with the true population mean of 11.7%

###################
# Classification using the 50% threshold
###################

contrasts(y_factor)

glm.probs<-predict(glm.all,type='response')
# Extract probabilities estimated as the dependent variable in the logistic regression
glm.probs[1:10]
# Display some probabilities for inspection

length(glm.probs)         
# Check that there are the correct number of probs, one for each observation input

glm.pred<-rep("no",length(glm.probs))
# Create the matrix for predictions, populated with 'no' glm.pred

glm.pred[glm.probs > 0.5]= 'yes'
# To begin with, make the simple assumption of a 50% threshold value,
# so an observation with prob, the y output, > 50% is assumed to be 
# a 'yes' customer prediction
# The class imbalance falsifies this, so let's look at the consequences
#
# glm.pred[glm.probs > 0.117]= 'yes'
glm.pred<-factor(glm.pred)

summary(glm.pred)
# with the 50% threshold, our model predications are
#  no     yes 
# 42,389  2,822 
# Our observations show
# no      yes 
# 39,922  5,289 
# So, this model classification underestimates the 'yes' customers

# Print out the statistics for the confusion matrix
table(glm.pred, y_factor)

# threshold = 0.5
#             y_factor
# glm.pred    no   yes
# no        38937  3452
# yes         985  1837
# TPR = TP / (TP + FN ) = 1837/(1837+3452) 
# =34.73% sensitivity, very bad
# TNR = TN / (TN + FP) = 38937 / (38937 + 985) = 97.53%
# = very accurate for the 'no' customers
# 
# threshold = 0.117, matching the proportion of 'yes' in the dataset
#             y_factor
# glm.pred    no   yes
# no        33597  905
# yes       6325   4384
# TPR = TP / (TP + FN ) = 43847/(4384+905) 
# =82.9% sensitivity
# TNR = TN / (TN + FP) = 33597 / (33597 + 6325) 
# = 84.16% specificity

# Using the method in caret library
library(caret)
library(e1071)
pred_classf<-as.factor(glm.pred) 

cm<-confusionMatrix(y_factor,pred_classf,positive = "yes")
cm 
# Produces the came Confusion Matrix as above

attributes(cm)
summary(y_factor)
summary(pred_classf)
sensitivity(pred_classf,y_factor,positive='yes')
# Sensitivity, the total positive rate, 
# matches the manual calculation of 34.73% above

specificity(pred_classf,y_factor,negative='no')
# Specificity, the total negative rate
# matches the manual calculation of 97.53% above

precision(pred_classf,y_factor,positive='yes')
recall(pred_classf,y_factor,negative='no')

###################
# Classification using the population threshold
###################
# Redo the classification, altering the threshold to match the population % of 'yes' customers
# as described in Branco et al. (2017)
glm.pred<-rep("no",length(glm.probs))
# Create the matrix for predictions, populated with 'no' glm.pred
glm.pred[glm.probs > 0.117]= 'yes'
glm.pred<-factor(glm.pred)
summary(glm.pred)

# Our predictions
# no      yes 
# 34,502 10,709

# Our observations show
# no      yes 
# 39,922  5,289 
# So, this model classification overestimates the 'yes' customers

# Print out the statistics for the confusion matrix
table(glm.pred, y_factor)

# threshold = 0.117, matching the proportion of 'yes' in the dataset
#             y_factor
# glm.pred    no   yes
# no        33597  905
# yes       6325   4384
# TPR = TP / (TP + FN ) = 43847/(4384+905) 
# =82.9% sensitivity
# TNR = TN / (TN + FP) = 33597 / (33597 + 6325) 
# = 84.16%


pred_classf<-as.factor(glm.pred)
cm2<-confusionMatrix(y_factor,pred_classf,positive = "yes")
cm2 
# Produces the came Confusion Matrix as above

attributes(cm2)
summary(y_factor)
summary(pred_classf)
sensitivity(pred_classf,y_factor,positive='yes')
# Sensitivity, the total positive rate, 
# matches the manual calculation of 82.9% above

specificity(pred_classf,y_factor,negative='no')
# Specificity, the total negative rate
# matches the manual calculation of 84.16% above

precision(pred_classf,y_factor,positive='yes')
recall(pred_classf,y_factor,negative='no')


####################
# Section 7 - Section ROC curve
####################
# This section follows Lantz (2015), chapter 10

install.packages('ROCR')
library(ROCR)

pred <- prediction(predictions = glm.probs,
                   labels =y_factor)

perf<-performance(pred, measure='tpr',x.measure='fpr')

# Plot the ROC curve  
plot(perf, main = "ROC curve for logistic regression sales classifier",
     col = "red", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)
# So, calculated AUC is around 90.8%, 

# Test the fitted model
#################
# H-L test
install.packages('glmtoolbox')
library(glmtoolbox)
hltest(glm.all)
# Statistic =  517.1731 
# degrees of freedom =  8 
# p-value =  < 2.22e-16 

install.packages('performance')
library(performance)
performance_hosmer(glm.all, n_bins = 10)
# Chi-squared: 518.110
# df:   8    
# p-value:   0.000
# Summary: model does not fit well.


# Nagelkerke pseudo R2
install.packages('fmsb')
library(fmsb)
NagelkerkeR2(glm.all)
# $R2=0.4218619

r2_nagelkerke(glm.all)
# Nagelkerke's R2 
# 0.4218619
      
install.packages('DescTools')
library('DescTools')
PseudoR2(glm.all,which='CoxSnell') #21.687%
PseudoR2(glm.all,which='Nagelkerke') #42.19%

PseudoR2(glm.all,which='all')
# AIC=21,660.35
# BIC=22,017.83
# log likelihood = -10,789.17
# null log likelihood = -16,315.48

###################################
# Section 8 - Train/test split and cross-validation
###################################

# Create train and test datasets
set.seed(1)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), size=nrow(bank_clean),replace=TRUE, prob=c(0.7,0.3))
train  <- bank_clean[sample, ]
test   <- bank_clean[!sample, ]

glm.train<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
               + previous + job_factor + marital_factor + education_factor + default_factor 
               + housing_factor + loan_factor + contact_factor + poutcome_factor, 
               data=train, 
               family = binomial)

summary(glm.train)

glm.probs<-predict(glm.train,type='response')
# Extract probabilities estimated as the dependent variable in the logistic regression
glm.probs[1:10]
# Display some probabilities for inspection

length(glm.probs)         
# Check that there are the correct number of probs, one for each observation input

glm.pred<-rep("no",length(glm.probs))
# Create the matrix for predictions, populated with 'no' glm.pred

glm.pred[glm.probs > 0.113]= 'yes'
glm.pred<-factor(glm.pred)

summary(glm.pred)
# Our observations show
# no      yes 
# 24,003  7,652

# Print out the statistics for the confusion matrix
table(glm.pred, train$y_factor)

#lm.pred    no    yes
# no     23385   618
# yes     4576   3076

performance_hosmer(glm.train, n_bins = 10)
PseudoR2(glm.train,which='all')

####################
# Section 9 - Oversampling cases
###################
# This is another method for classification in imbalanced data sets 
# recommended by Branco et al. (2017), namely oversample the under-represented class.  
# Here I resample the 'yes' customers to generate the same observations as the 
# 'no' customers 


cases_yes<-bank_clean[bank_clean$y_factor=='yes',]
summary(cases_yes)
colnames(cases_yes)
dim(cases_yes)[1]

cases_no<-bank_clean[bank_clean$y_factor=='no',]
summary(cases_no)
dim(cases_no)[1]

# Count the difference sizes of the classes, then resample the 'yes' cases to match the
# number of observations of the 'no' cases
case_differences<-( (dim(cases_no)[1])-(dim(cases_yes)[1]) )
oversampled_cases<-slice_sample(cases_yes,n=case_differences,replace=TRUE)

cases_yes<-rbind(cases_oversampled,cases_yes)
summary(cases_yes)
cases<-rbind(cases_no,cases_yes)
dim(cases)

glm.oversampled<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
               + previous + job_factor + marital_factor + education_factor + default_factor 
               + housing_factor + loan_factor + contact_factor + poutcome_factor, 
               data=cases, 
               family = binomial)
summary(glm.oversampled)

glm.probs<-predict(glm.oversampled,type='response')
glm.probs[1:10]
length(glm.probs)         

glm.pred<-rep("no",length(glm.probs))
glm.pred[glm.probs > 0.5]= 'yes'
glm.pred<-factor(glm.pred)
summary(glm.pred)

summary(cases$y_factor)
# 39922 each of yes and no cases

# Print out the statistics for the confusion matrix
table(glm.pred, cases$y_factor)
# glm.pred    no     yes
# no        33,859  7131
# yes       6,063   32,791

32791 / (32791+7131)
#sensitivity = 82.13767%, compared to 82.9% above 
# TPR = TP / (TP + FN )

33859 / (33859+6063)
# specificity = 84.8%, compared to 84.16% above
# TNR = TN / (TN + FP) 

hltest(glm.fits5)
# Statistic =  362017820 
# degrees of freedom =  9 
# p-value =  < 2.22e-16 
# Still not significant for the fitted model overall

performance_hosmer(glm.fits5, n_bins = 10)
# Chi-squared: 3794.805
# df:   8    
# p-value:   0.000
# Summary: model does not fit well.


# Nagelkerke pseudo R2
NagelkerkeR2(glm.fits5)
# $R2 = 0.5985811

r2_nagelkerke(glm.fits5)
# Nagelkerke's R2 
# 0.5985811

PseudoR2(glm.fits5,which='CoxSnell') # 44.89%
PseudoR2(glm.fits5,which='Nagelkerke') # 59.86%
PseudoR2(glm.fits5,which='all')
# AIC = 63,189.93
# BIC = 63,570.73
# log likelihood = -31,553.96
# null log likelihood = -55,343.64
