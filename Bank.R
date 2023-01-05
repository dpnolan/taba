# Peter Nolan x22154116
# 
# Code  Terminal Assessment by Assignment (TABA)
# 
# Bank telemarketing analysis
#
#See Google Colab workbook and all the working files at 
#
# Github depo https://github.com/dpnolan/taba 

# Contents 
# B Bank Telesales data analysis 
# B.1 Load,
# B.2 Preprocess the data
# B.3 Descriptive statistics and graphing the variables
# B.4 Model fitting
# B.5 Automated Model Selection
# B.6 Classification
# B.7 ROC Curve 
# B.8 Oversampling the yes cases  
# B.9 Cross-validation

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
job_factor<-factor(bank$job)
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
#  Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)               -3.560e+00  1.850e-01 -19.246  < 2e-16 ***
#  age                        1.104e-04  2.203e-03   0.050 0.960033    
#  balance                    1.291e-05  5.136e-06   2.513 0.011965 *  
#  month_factorfeb            9.131e-01  1.221e-01   7.475 7.70e-14 ***
#  month_factormar            2.719e+00  1.485e-01  18.319  < 2e-16 ***
#  month_factorapr            1.174e+00  1.198e-01   9.802  < 2e-16 ***
#  month_factormay            7.307e-01  1.165e-01   6.273 3.54e-10 ***
#  month_factorjun            1.532e+00  1.262e-01  12.142  < 2e-16 ***
#  month_factorjul            3.400e-01  1.178e-01   2.886 0.003896 ** 
#  month_factoraug            4.394e-01  1.170e-01   3.757 0.000172 ***
#  month_factorsep            1.982e+00  1.472e-01  13.459  < 2e-16 ***
#  month_factoroct            2.053e+00  1.393e-01  14.738  < 2e-16 ***
#  month_factornov            3.034e-01  1.228e-01   2.471 0.013477 *  
#  month_factordec            1.821e+00  1.966e-01   9.261  < 2e-16 ***
#  duration                   4.186e-03  6.450e-05  64.889  < 2e-16 ***
#  campaign                  -8.520e-02  1.001e-02  -8.510  < 2e-16 ***
#  previous                   9.694e-03  6.424e-03   1.509 0.131316    
#  job_factorblue-collar     -3.138e-01  7.260e-02  -4.322 1.55e-05 ***
#  job_factorentrepreneur    -3.627e-01  1.255e-01  -2.889 0.003863 ** 
#  job_factorhousemaid       -4.994e-01  1.364e-01  -3.663 0.000250 ***
#  job_factormanagement      -1.641e-01  7.326e-02  -2.240 0.025076 *  
#  job_factorretired          2.554e-01  9.708e-02   2.631 0.008521 ** 
#  job_factorself-employed   -2.961e-01  1.119e-01  -2.645 0.008159 ** 
#  job_factorservices        -2.259e-01  8.399e-02  -2.689 0.007162 ** 
#  job_factorstudent          3.846e-01  1.088e-01   3.536 0.000407 ***
#  job_factortechnician      -1.713e-01  6.887e-02  -2.487 0.012892 *  
#  job_factorunemployed      -1.771e-01  1.117e-01  -1.585 0.112881    
#  job_factorunknown         -3.271e-01  2.333e-01  -1.402 0.160872    
#  marital_factormarried     -1.797e-01  5.889e-02  -3.051 0.002277 ** 
#  marital_factorsingle       9.270e-02  6.723e-02   1.379 0.167962    
#  education_factortertiary   3.808e-01  7.524e-02   5.061 4.18e-07 ***
#  education_factorsecondary  1.816e-01  6.473e-02   2.805 0.005033 ** 
#  education_factorunknown    2.524e-01  1.038e-01   2.431 0.015046 *  
#  default_factoryes         -1.372e-02  1.627e-01  -0.084 0.932812    
#  housing_factoryes         -6.877e-01  4.365e-02 -15.757  < 2e-16 ***
#  loan_factoryes            -4.305e-01  5.998e-02  -7.176 7.16e-13 ***
#  contact_factortelephone   -1.657e-01  7.517e-02  -2.205 0.027478 *  
#  contact_factorunknown     -1.588e+00  7.247e-02 -21.911  < 2e-16 ***
#  poutcome_factorother       2.115e-01  8.959e-02   2.361 0.018246 *  
#  poutcome_factorsuccess     2.299e+00  7.969e-02  28.855  < 2e-16 ***
#  poutcome_factorunknown    -6.150e-02  6.042e-02  -1.018 0.308737    
#
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

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
plot(glm.all)
# Save down the Cook's distance diagram that shows outliers are not significant, 
# although one comes close

library(car)

vif(glm.all)
#GVIF Df GVIF^(1/(2*Df))
#age              2.181062  1        1.476842
#balance          1.042316  1        1.020939
#month_factor     2.778305 11        1.047543
#duration         1.123955  1        1.060167
#campaign         1.082122  1        1.040251
#job_factor       4.153994 11        1.066871
#previous         1.272448  1        1.128028
##marital_factor   1.444198  2        1.096243
#education_factor 2.256349  3        1.145252
#housing_factor   1.414825  1        1.189464
#default_factor   1.016560  1        1.008246
#loan_factor      1.064586  1        1.031788
#contact_factor   1.866724  2        1.168880
#poutcome_factor  1.507077  3        1.070753

# All values < 10, so the variance heteroskedasticity should not be a problem for our model


#################
# Test the fitted model with Pseudo R2 measures
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


#################
# Test the fitted model with pdays added back
#################

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

# Coefficient, standard error, Z, probability score 
#  pdays                      4.497e-04  3.121e-04   1.441 0.149536   
# Many other variables drop out also


# Null deviance: 8919.8  on 8256  degrees of freedom
# Residual deviance: 5809.6  on 8215  degrees of freedom
# (36954 observations deleted due to missingness)
# AIC: 5893.6
# Most observations are deleted because 36,000 NAs in pdays field
# AIC and deviance are  much lower
# pdays is nowhere near significance in this regression

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

library(MASS)

glm_step_AIC <- stepAIC(full_model,trace = TRUE, direction= "both")

summary(glm_step_AIC)

step_model <- step(null_model, 
                   scope = list(lower = null_model,
                                upper = full_model),
                   direction = "both")

summary(step_model)

# step_model with lowest AIC of 21,656 is 
# -y_factor ~ duration + poutcome_factor + month_factor + contact_factor + 
#  housing_factor + job_factor + campaign + loan_factor + marital_factor + 
#  education_factor + balance + previous
#  i.e. age and default_factor drop out

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

glm.probs<-predict(step_model,type='response')
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
# with the 50% threshold, our model predictions are
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
# no        38938  3453
# yes         984  1836
# TPR = TP / (TP + FN ) = 1836/(1836+3453)
# =34.71% sensitivity, very bad
# TNR = TN / (TN + FP) = 38938 / (38938 + 984) = 97.535%
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
# matches the manual calculation of 34.71% above

specificity(pred_classf,y_factor,negative='no')
# Specificity, the total negative rate
# matches the manual calculation of 97.535% above

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
# no        33596  905
# yes       6326   4384
# TPR = TP / (TP + FN ) = 4384/(4384+905)
# =82.9% sensitivity
# TNR = TN / (TN + FP) = 33596 / (33596 + 6325)
# = 84.16%

pred_classf<-as.factor(glm.pred)
cm2<-confusionMatrix(y_factor,pred_classf,positive = "yes")
cm2 
# Produces the came Confusion Matrix as above

summary(y_factor)
summary(pred_classf)
sensitivity(pred_classf,y_factor,positive='yes')
# Sensitivity, the total positive rate, 
# matches the manual calculation of 82.9% above

specificity(pred_classf,y_factor,negative='no')
# Specificity, the total negative rate
# matches the manual calculation of 84.15% above

precision(pred_classf,y_factor,positive='yes')
recall(pred_classf,y_factor,negative='no')

####################
# Section 7 - Section ROC curve
####################
# This section follows Lantz (2015), chapter 10

dev.off() # clear the graphics buffer

# run AFTER 
# 1. Estimating the step_model in section 5
# 2. Executing the'Classification using the population threshold' above

install.packages('ROCR')
library(ROCR)

pred <- prediction(predictions = glm.probs,
                   labels =y_factor)

perf<-performance(pred, measure='tpr',x.measure='fpr')

# Plot the ROC curve  
plot(perf, main = "ROC curve for logistic regression sales classifier",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)
# So, calculated AUC is around 90.8%, a 'good' level of significance

####################
# Section 8 - Oversampling the 'yes' cases
###################
# This is another method for classification in imbalanced data sets 
# recommended by Branco et al. (2017), namely oversample the under-represented class.  
# Here I resample the 'yes' customers, adding simple duplicates to the dataset 
# to generate the same observations as the 'no' customers 

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

# no        33597  905
# yes       6325   4384

#sensitivity = 82.13767%, compared to 82.9% above 
# TPR = TP / (TP + FN )

33859 / (33859+6063)
# specificity = 84.8%, compared to 84.16% above
# TNR = TN / (TN + FP) 

hltest(glm.oversampled)
# Statistic =  362017820 
# degrees of freedom =  9 
# p-value =  < 2.22e-16 
# Still not significant for the fitted model overall

library(performance)
performance_hosmer(glm.oversampled, n_bins = 10)
# Chi-squared: 3794.805
# df:   8    
# p-value:   0.000
# Summary: model does not fit well.

# Nagelkerke pseudo R2
NagelkerkeR2(glm.oversampled)
# $R2 = 0.5985811

r2_nagelkerke(glm.oversampled)
# Nagelkerke's R2 
# 0.5985811

PseudoR2(glm.oversampled,which='CoxSnell') # 44.89%
PseudoR2(glm.oversampled,which='Nagelkerke') # 59.86%
PseudoR2(glm.oversampled,which='all')
# AIC = 63,189.93
# BIC = 63,570.73
# log likelihood = -31,553.96
# null log likelihood = -55,343.64


#################
# Section 9 - cross validation
#################

library(caret)
# https://cran.r-project.org/web/packages/caret/caret.pdf
# https://topepo.github.io/caret/index.html
# Code example adapted from https://daviddalpiaz.github.io/r4sl/the-caret-package.html
# James et al. (2021), section 4.7.2

#set a seed so that results can be re-run 
set.seed(1)
#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)

#fit a logistic regression model and use k-fold CV to evaluate performance
cv.full <- train(y_factor~ age + balance + month_factor + duration + campaign 
                 + previous + job_factor + marital_factor + education_factor + default_factor 
                 + housing_factor + loan_factor + contact_factor + poutcome_factor,
                 data=bank_clean,
                 method='glm',
                 family='binomial',
                 trControl = ctrl)
print(cv.full)  
summary(cv.full)

# Null deviance: 32631  on 45210  degrees of freedom
# Residual deviance: 21578  on 45170  degrees of freedom
# AIC: 21660

# Coefficients on the k-fold results exactly match the sign and size on the the glm.all model above, 
# thus validating the estimates for glm.all
# This would likely make us happier to accept the glm.all model as a useful estimator
# Accuracy = 0.9017273 and Kappa = 0.4036007 are reported also.  

# Note, however, that there is no agreement and implementation on a method to estimate 
# degrees of freedom and t and F tests for cross validation estimates
# This method belongs more to the world of machine learning

# Redo the fit and k-fold CV with the reduced model from the step-wise analysis that drops
# age and default_factor
cv.step <- train(y_factor~ balance + month_factor + duration + campaign 
                 + previous + job_factor + marital_factor + education_factor
                 + housing_factor + loan_factor + contact_factor + poutcome_factor,
                 data=bank_clean,
                 method='glm',
                 family='binomial',
                 trControl = ctrl)
print(cv.step)

# Null deviance: 32631  on 45210  degrees of freedom
# Residual deviance: 21578  on 45172  degrees of freedom
# AIC: 21656
# Coefficients show only minor differences to glm.all 

