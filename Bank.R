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

# Section 1
# Go to the working directory, import and summarise the supplied input data

getwd()
setwd("/Users/peternolan/Documents/Post Grad/NCI/Courses/Statistics/TABA/Bank")
list.files()
bank=read.csv('Bank.csv')
bank=data.frame(bank)
summary(bank)

# bank holds the imported data
# bank_clean is the data that is processed
bank_clean<-bank

# Pre-processing
####################
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

month_factor<-factor(bank$month,levels=c('jan','feb','mar','apr','may','jun','jul','aug',
                                          'sep','oct','nov','dec'))
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
# no   yes 
# 39922  5289 
# 5289/(39922+5289)
#  0.1169848 approx = 11.7%, similar to the original study by Moro et al. 2014
# The classes are significantly imbalanced, 
# so simply constantly predicting y='no' will be correct about 88.3% of the time

bank_clean<-cbind(bank_clean,y_factor)

# pdays - values for -1, we assume are NA, no recorded value
bank_clean['pdays'][bank_clean['pdays'] == -1]<-NA

#How many NA and how many real values are there? 8,257 real, 36,954 NAs
length(bank_clean$pdays)-sum(is.na(bank_clean$pdays))
summary(bank_clean$pdays)

# Delete the now factorised variables from the 
library('dplyr')
bank_clean <- select ( bank_clean,-job,-marital,-education,-default,-housing,
-loan,-contact, -poutcome,-pdays,-month,-day,-pdays,-y)

independent_variables <- select (bank_clean,-y_factor,)

# Print the column names of our working data frame so as to
colnames(bank_clean)
colnames(independent_variables)

# Get summary statistics for our working dataframe
summary(bank_clean)

# Descriptive statistics and graphing the variables
####################
library(ggplot2) 
library(scales)

# Violin plots for the numerical variables
# Is the y='yes' population associated with particular common characteristics?
# Code adapted from Chang, 2019, section 6.8 and 6.9
# White dot shows the median
# Black area shows a box plot with top and bottom edges at 75 and 25 percentile 

hw_p <- ggplot(bank_clean, aes(x = y_factor, y=age, fill=y_factor  ))
hw_p + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + 
  stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)
# 'yes' population skews more, 0.87 v 0.59, towards the older and younger extreme,
# than the 'no' population

mean(bank_clean['age'][bank_clean['y_factor']=='yes'])
mean(bank_clean['age'][bank_clean['y_factor']=='no'])
median(bank_clean['age'][bank_clean['y_factor']=='yes'])
median(bank_clean['age'][bank_clean['y_factor']=='no'])
skewness(bank_clean['age'][bank_clean['y_factor']=='yes'])
skewness(bank_clean['age'][bank_clean['y_factor']=='no'])
kurtosis(bank_clean['age'][bank_clean['y_factor']=='yes'])
kurtosis(bank_clean['age'][bank_clean['y_factor']=='no'])

hw_p <- ggplot(bank_clean, aes(x = y_factor, y=balance, fill=y_factor  ))
hw_p + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + 
  stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)
# 'yes' population has bigger balance on the relationship

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
# 'yes' population has lower number of contacts during this campagin, mean - 2.14 v 2.85

hw_p <- ggplot(bank_clean, aes(x = y_factor, y=previous,fill=y_factor  ))
hw_p + geom_violin() +
  geom_boxplot(width = .1, 
               fill = "black", 
               outlier.colour = NA) + 
  stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 2.5)

hw_p <- ggplot(bank_clean, aes(x = y_factor, y=previous,fill=y_factor  ))
hw_p + geom_violin() +
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

# Bar charts for the categorical variables
# Is the y='yes' population associated with particular common characteristics?
# Main code source is Rhys (2020), section 4.2.3

# Show breakdown of month_factor by 'yes' and 'no' population
ggplot(data=bank_clean,aes(x=y_factor, fill=month_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# In the 'yes' population, Feb, Mar, Apr, Oct are bigger proportion
# May is much bigger in the 'no' population

# Show breakdown of job_factor by 'yes' and 'no' population
ggplot(data=bank_clean,aes(x=y_factor, fill=job_factor)) +
  geom_bar(position = "fill", colour='black') +
  scale_y_continuous(labels = scales::percent)
# 'yes' population has less job=blue-collar, more retired, some more student 

# Show breakdown of marital_factor by 'yes' and 'no' population
# Source is Rhys (2020), section 4.2.3
ggplot(data=bank_clean,aes(x=y_factor, fill=marital_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)

# Show breakdown of education_factor by 'yes' and 'no' population
# Source is Rhys (2020), section 4.2.3
ggplot(data=bank_clean,aes(x=y_factor, fill=education_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# more third-level education and fewer primary, 
# which we would expect as the more educated are more affluent customers, 


# Show breakdown of default_factor by 'yes' and 'no' population
# Source is Rhys (2020), section 4.2.3
ggplot(data=bank_clean,aes(x=y_factor, fill=default_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# defaulted customers are a very small percentage of both 'yes' and 'no'

# Show breakdown of housing_factor by 'yes' and 'no' population
# Source is Rhys (2020), section 4.2.3
ggplot(data=bank_clean,aes(x=y_factor, fill=housing_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# 'yes' customers are significantly less likely to have a mortgage
# may be saving for a house purchase?  

# Show breakdown of loan_factor by 'yes' and 'no' population
ggplot(data=bank_clean,aes(x=y_factor, fill=loan_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# 'has personal loan' more likely for 'no' customers

# Show breakdown of contact_factor by 'yes' and 'no' population
ggplot(data=bank_clean,aes(x=y_factor, fill=contact_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# more cell phones and less unknown among 'yes' customers

# Show breakdown of poutcome_factor by 'yes' and 'no' population
ggplot(data=bank_clean,aes(x=y_factor, fill=poutcome_factor)) +
  geom_bar(position = "fill",colour='black') +
  scale_y_continuous(labels = scales::percent)
# Previous success with the client is significantly higher among 'yes' customers 



#Model Fitting
#####################

glm.fits1<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
              + previous + job_factor + marital_factor + education_factor + default_factor 
              + housing_factor + loan_factor + contact_factor + poutcome_factor, 
              data=bank_clean, 
              family = binomial)
summary(glm.fits1)

# Null deviance: 32631  on 45210  degrees of freedom
# Residual deviance: 21578  on 45170  degrees of freedom
# AIC: 21660
# Shows Wald statistics and the associated probability 
# At 5% significance, two sided prob factor, NOT significant are 
# age, previous, job=unknown or unemployed, marital=single, default=yes, 
# contact=telephone or other, poutcome = unknown

#coefficients
coef(glm.fits1)
# odds ratios 
exp(coef(glm.fits1))
# the odds that a customer with poutcome=success is 9.968 times 
# than the odds a customer with the same characteristics apart from that will buy the offering
# target these customers for sales efforts
# Month effects odds ratios are Mar is 15.171 odds ratio, Sept 7.256, Oct 7.79, Dec 6.18 
# Target customers at these times

# Third-level educated (1.46), retired (1.29) and student (1.47) customers 
# show favourable odds ratios

# Diagnosis of the logistic regression
summary(residuals(glm.fits1))
plot(residuals(glm.fits1))


#re-run including pdays in the regression
glm.fits2<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
               + previous + job_factor + marital_factor + education_factor + default_factor 
               + housing_factor + loan_factor + contact_factor + poutcome_factor
               +pdays, 
               data=bank_clean,
               family = binomial)

summary(glm.fits2)
# Null deviance: 8919.8  on 8256  degrees of freedom
# Residual deviance: 5809.6  on 8215  degrees of freedom
# (36954 observations deleted due to missingness)
# AIC: 5893.6
# Most observations are deleted
# AIC and deviance are  much lower
# pdays is nowhere near significance in this regression
# Many other variables drop out also
# Let's drop pdays

# Define models with (1) no variables, and (2) all variables

null_model<-glm(y_factor~1,data=independent_variables,family=binomial)
full_model<-glm(y_factor~. , data=independent_variables,family=binomial)

glm.fits3<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
               + previous + job_factor + marital_factor + education_factor + default_factor 
               + housing_factor + loan_factor + contact_factor + poutcome_factor,
               data=bank_clean,
               family = binomial)
start_time <- Sys.time()
glm_back <- step(glm.fits3,direction='both',trace = TRUE)
end_time <- Sys.time()
elapsed = end_time - start_time

summary(glm_back)

summary(y_factor)
contrasts(y_factor)
glm.probs<-predict(glm.fits1,type='response')
glm.probs[1:10]
length(glm.probs)         

glm.pred<-rep("no",length(glm.probs))
glm.pred[glm.probs > 0.5]= 'yes'
glm.pred[glm.probs > 0.117]= 'yes'
glm.pred<-factor(glm.pred)
summary(glm.pred)

# Print out the statistics for the confusion matrix
table(glm.pred, y_factor)

# threshold = 0.5
#             y_factor
# glm.pred    no   yes
# no        38937  3452
# yes         985  1837
# TPR = TP / (TP + FN ) = 1837/(1837+3452) 
# =34.73% sensitivity
# TNR = TN / (TN + FP) = 38937 / (38937 + 985) = 97.53%
#
# threshold = 0.117, matching the proportion of 'yes' in the dataset
#             y_factor
# glm.pred    no   yes
# no        33597  905
# yes       6325   4384
# TPR = TP / (TP + FN ) = 43847/(4384+905) 
# =82.9% sensitivity
# TNR = TN / (TN + FP) = 33597 / (33597 + 6325) = 84.16%


mean(glm.pred==y_factor)

library(caret)
library(e1071)
pred_classf<-as.factor(glm.pred) 

cm<-confusionMatrix(y_factor,pred_classf,positive = "yes")
cm
attributes(cm)
summary(y_factor)
summary(pred_classf)
sensitivity(pred_classf,y_factor,positive='yes')
specificity(pred_classf,y_factor,negative='no')
precision(pred_classf,y_factor,positive='yes')
recall(pred_classf,y_factor,negative='no')

install.packages('ROCR')
library(ROCR)

pred <- prediction(predictions = glm.probs,
                   labels =y_factor)

perf<-performance(pred, measure='tpr',x.measure='fpr')
  
plot(perf, main = "ROC curve for logistic regression sales classifier",
     col = "red", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)

# Test the fitted model
#################
# H-L test
install.packages('glmtoolbox')
library(glmtoolbox)
hltest(glm.fits1)
# Statistic =  517.1731 
# degrees of freedom =  8 
# p-value =  < 2.22e-16 

install.packages('performance')
library(performance)
performance_hosmer(glm.fits1, n_bins = 10)
# Chi-squared: 518.110
# df:   8    
# p-value:   0.000
# Summary: model does not fit well.


# Nagelkerke pseudo R2
install.packages('fmsb')
library(fmsb)
NagelkerkeR2(glm.fits1)
# $R2=0.4218619

r2_nagelkerke(glm.fits1)
# Nagelkerke's R2 
# 0.4218619
      
install.packages('DescTools')
library('DescTools')
PseudoR2(glm.fits1,which='CoxSnell') #21.687%
PseudoR2(glm.fits1,which='Nagelkerke') #42.19%

PseudoR2(glm.fits1,which='all')
# AIC=21,660.35
# BIC=22,017.83
# log likelihood = -10,789.17
# null log likelihood = -16,315.48


# Automated search of logistic regression parameters

# Train/test split and cross-validation
###################################

# Create train and test datasets
set.seed(1)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), size=nrow(bank_clean),replace=TRUE, prob=c(0.7,0.3))
train  <- bank_clean[sample, ]
test   <- bank_clean[!sample, ]

glm.fits4<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
               + previous + job_factor + marital_factor + education_factor + default_factor 
               + housing_factor + loan_factor + contact_factor + poutcome_factor, 
               data=train, 
               family = binomial)
summary(glm.fits4)

performance_hosmer(glm.fits4, n_bins = 10)
PseudoR2(glm.fits4,which='all')


# Oversampling cases
###################

cases_yes<-bank_clean[bank_clean$y_factor=='yes',]
summary(cases_yes)
colnames(cases_yes)
dim(cases_yes)[1]

cases_no<-bank_clean[bank_clean$y_factor=='no',]
summary(cases_no)
dim(cases_no)[1]

case_differences<-( (dim(cases_no)[1])-(dim(cases_yes)[1]) )
oversampled_cases<-slice_sample(cases_yes,n=case_differences,replace=TRUE)

cases_yes<-rbind(cases_oversampled,cases_yes)
summary(cases_yes)
cases<-rbind(cases_no,cases_yes)

glm.fits5<-glm(y_factor ~ age + balance + month_factor + duration + campaign 
               + previous + job_factor + marital_factor + education_factor + default_factor 
               + housing_factor + loan_factor + contact_factor + poutcome_factor, 
               data=cases, 
               family = binomial)
summary(glm.fits5)

glm.probs<-predict(glm.fits5,type='response')
glm.probs[1:10]
length(glm.probs)         

glm.pred<-rep("no",length(glm.probs))
glm.pred[glm.probs > 0.5]= 'yes'
#glm.pred[glm.probs > 0.117]= 'yes'
glm.pred<-factor(glm.pred)
summary(glm.pred)

summary(y_factor)
# 5289/(39922+5289) = 11.69848% 'yes' 

# Print out the statistics for the confusion matrix
table(glm.pred, cases$y_factor)

32791 / (32791+7131)
#sensitivity = 82.13767% 

38937 / (38937+6963)
# specificity = 84.83% 

# threshold = 0.5
#             y_factor
# glm.pred    no   yes
# no        38937  3452
# yes         985  1837
# TPR = TP / (TP + FN ) = 1837/(1837+3452) 
# =34.73% sensitivity
# TNR = TN / (TN + FP) = 38937 / (38937 + 985) = 97.53%
#
# threshold = 0.117, matching the proportion of 'yes' in the dataset
#             y_factor
# glm.pred    no   yes
# no        33597  905
# yes       6325   4384
# TPR = TP / (TP + FN ) = 43847/(4384+905) 
# =82.9% sensitivity
# TNR = TN / (TN + FP) = 33597 / (33597 + 6325) = 84.16%

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
