Stroke_Data <- read.csv("healthcare-stroke-data.csv")

Stroke_Data$avg_glucose_level
#Question 1
#a Cleaning the NA
Stroke_DataClean <- Stroke_Data[Stroke_Data$bmi != "N/A",] #Clean off the NA values check to see if it matches

#b changing some names and making factors
Stroke_DataClean$smoking_status[Stroke_DataClean$smoking_status == "formerly smoked"] <- "Formerly Smoked"
Stroke_DataClean$smoking_status[Stroke_DataClean$smoking_status == "never smoked"] <- "Never Smoked"
Stroke_DataClean$smoking_status[Stroke_DataClean$smoking_status == "smokes"] <- "Smokes"
Stroke_DataClean$smoking_status[Stroke_DataClean$smoking_status == "formerly smoked"] <- "Formerly Smoked"

Stroke_DataClean$gender <- factor(Stroke_DataClean$gender)
Stroke_DataClean$ever_married <- factor(Stroke_DataClean$ever_married)
Stroke_DataClean$work_type <- factor(Stroke_DataClean$work_type)
Stroke_DataClean$Residence_type <- factor(Stroke_DataClean$Residence_type)
Stroke_DataClean$bmi <- as.numeric(Stroke_DataClean$bmi)
Stroke_DataClean$smoking_status <- factor(Stroke_DataClean$smoking_status)

Stroke_DataClean$hypertension[Stroke_DataClean$hypertension == 0] <- "False"
Stroke_DataClean$hypertension[Stroke_DataClean$hypertension == 1] <- "True"
Stroke_DataClean$heart_disease[Stroke_DataClean$heart_disease == 0] <- "False"
Stroke_DataClean$heart_disease[Stroke_DataClean$heart_disease == 1] <- "True"
Stroke_DataClean$stroke[Stroke_DataClean$stroke == 0] <- "False"
Stroke_DataClean$stroke[Stroke_DataClean$stroke == 1] <- "True"

Stroke_DataClean$stroke <- factor(Stroke_DataClean$stroke)
Stroke_DataClean$hypertension <- factor(Stroke_DataClean$hypertension)
Stroke_DataClean$heart_disease <- factor(Stroke_DataClean$heart_disease)

str(Stroke_DataClean)


#Question 2
#a (Numeric 1)
#Numeric variable average glucose level (mg/dL)

mean(Stroke_DataClean$avg_glucose_level) # 105.305
sd(Stroke_DataClean$avg_glucose_level) #44.424
min(Stroke_DataClean$avg_glucose_level) #55.12
max(Stroke_DataClean$avg_glucose_level) #271.74
median(Stroke_DataClean$avg_glucose_level) #91.68

#b (Numeric 1)
hist(Stroke_DataClean$avg_glucose_level, main = "Glucose level histogram", breaks = seq(40, 275, 5), xlim = c(40, 275), xlab = "Glucose level (mg/dL)", col = "lightblue", ylab = "Number of patients")

#c (Numeric 1)
#The data is bi-modal. Thus, the data cannot be verified to be normally distributed. Here is proof:
qqnorm(Stroke_DataClean$avg_glucose_level)
qqline(Stroke_DataClean$avg_glucose_level) #Not even close
shapiro.test(Stroke_DataClean$avg_glucose_level) #Not even close p value < 2.2e-16



# (Categorical 1 hypertension)
#a
# Summary Statistics for Hypertension (compared with stroke in some code)
stroketable <- xtabs(~hypertension + stroke, data = Stroke_DataClean)
hypertensionproportions <- addmargins(prop.table(stroketable))
hypertensionproportions #Margin table to compare proportions between hypertension and stroke

#b
#Hypertension variable on its own.
hypertension_factor <- factor(Stroke_DataClean$hypertension)
hypertension_table <- table(hypertension_factor)
barplot(hypertension_table, ylim = c(0,5000), ylab = "Frequency", xlab = "Hypertension", main = "Number of Cases for Hypertension", col = c("Blue", "Pink"))

#c
#Cannot use normal distribution for this categorical variable since it is not a numeric vector.



# (Numerical 2 BMI)
#Summary statistics for BMI variable

#a
summary(Stroke_DataClean$bmi) #Notable values: Median 28.10, Minimum of 10.30, Max of 97.60, and Mean 28.89
sd(Stroke_DataClean$bmi) #7.854

#b
boxplot(Stroke_DataClean$bmi, xlab = "BMI", main = "Boxplot of BMIs", horizontal = T, col = "Orange")

#c
qqnorm(Stroke_DataClean$bmi)
qqline(Stroke_DataClean$bmi)
shapiro.test(Stroke_DataClean$bmi)
#P-value <2.2e-16 not even close to passing normality test
# Based on the QQ plot of the BMI variable, the data appears to be normal.
# Furthermore the Shapiro-Wilk test returns a significantly small p-value, so the
# data can be assumed as NOT normal



# (Categorical 2  work type)

#Some setup 
work_type <- Stroke_DataClean$work_type
work_factor <- factor(work_type)
work_table <- table(work_factor)

# (a) Compute appropriate summary statistics.
#frequency of each category
work_table
#671 children
#630 government job
#22 never worked
#2811 private
#775 self employed

# (b) 
#histogram of proportions of work_type
barplot(prop.table(work_table), ylab = "Proportion of work type", main = "Histogram for work type", col = c("Black", "Yellow"))

# (c) 
# Categorical data like this just cannot be normally distributed as its categories are discrete.



# (Categorical 3)
#a
#This is a frequency table for the data inside ever_married 
Freq_Married <- table(Stroke_DataClean$ever_married)
addmargins(Freq_Married)
#1705 said no
#3204 said yes

# this is a proportion table for the variable ever_married
Prop_Married <- prop.table(Freq_Married)
Prop_Married
#34.7% never marries
#65.3% Yes married before

#b
# Bar plot for the variable ever_married
barplot(Freq_Married, main = "Ever Married", xlab = "Answers", ylab = "Amount of Replies", col = c("Brown", "Red3"))

#c
#This piece of categorical data would not be appropriate to assess for normality. Thus, no this categorical data is not normal.




#Question 3



#Part A: Confidence test for 2 proportions

#The null hypothesis is that the proportion of people with Stroke and hypertension
#is the same as the proportion of people with Stroke and don't have hypertension.

#H0: p_withHypertension = p_withoutHypertension

#The alternative hypothesis states that the proportion of people with Stroke and hypertension
#is not the same as the proportion of people with Stroke and don't have hypertension.

#H1: p_withHypertension != p_withoutHypertension

addmargins(stroketable)
prop.test(x = c(60,149), n = c(451,4458), alternative = "two.sided", correct = FALSE)

#data:  c(60, 149) out of c(451, 4458)
#X-squared = 99.704, df = 1, p-value < 2.2e-16
#alternative hypothesis: two.sided
#95 percent confidence interval:
#  0.06783017 0.13139910
#sample estimates:
#  prop 1     prop 2 
#0.13303769 0.03342306

#p-value is <2.2e-16

#Decision: Reject the Null hypothesis using a significance level of .05
#Conclusion:There is a significant amount of evidence that the proportion
#of people with hypertension and has had a stroke is not the same as the proportion of people without hypertension and
#has had a stroke.



#Part B

#Performed a 95% confidence interval for Average glucose level in the patients 
t.test(Stroke_DataClean$avg_glucose_level, conf.level = 0.95)

#95% CI is: 104.0621  106.5482
#We are 95% confident that the true population average glucose level is in between
# 104.0621-106.5482 mg/dL




#Part C:  Create a regression model with at least one predictor

# predictor - bmi
# response - avg_glucose_level

# # i) Check the assumptions of the model

# Linear assumption
# create scatter plot to check if there is a linear relationship between avg_glucose_level and age
plot(Stroke_DataClean$age, Stroke_DataClean$avg_glucose_level, 
     xlab = 'age', ylab = 'avg_glucose_level', main = "Scatterplot of Age and Average Glucose Level")

#plot looks slightly linear as avg_glucose_levels increase with age

regline <- lm(avg_glucose_level ~ age, data=Stroke_DataClean) 
abline(regline, col = 'red')
plot(x = Stroke_DataClean$bmi, y = regline$residual)
#There is a clear pattern to the left, so the linear assumption is questionable


# Normally Distributed Errors assumption 
qqnorm(regline$residuals)
qqline(regline$residuals)
shapiro.test(regline$residuals)

#From qqplot, errors appear to NOT be normally distributed
#Shapiro Test: W = 0.86515, p-value < 2.2e-16
# errors = data
# H_0: The errors come from a normal distribution
# H_1:The errors do not come from a normal distribution.
# Decision: Reject H0 at alpha = 0.05
# Conclusion: The errors do not come from a normal distribution

# Equal Variance of Errors
#F-test for equal variance
par(mfrow=c(2,2))
plot(regline)
par(mfrow=c(1,1))
# plot in top left corner - residual vs fitted data are seem to have a pattern and not be
# equally distributed/clustered around y = 0
# Therefore, cannot assume equal variances

# ii) find the parameters for the model
regline

# Coefficients:
# (Intercept)      age  
#  85.3940       0.4645 

#ANSWER:
# beta_0 = intercept = 85.3940
# beta_1 = slope = 0.4645
# Regression line Yhat_i = 85.3940 + 0.4645 X_i

# iii) test whether the model explains variability in the response variable
summary(regline)

#ANSWER:
# t-value = 17.00   p-value = <2e-16
# F-statistic:   289 on 1 and 4907 DF,  p-value: < 2.2e-16

# In this case, the p-value is very small, so
# Concluded that beta_1 is not 0 and there is a 
# linear relationship between Age and Average Glucose Level


# iv) if significant, test whether each variable explains variability in the response variable
# Is Age a useful variable in explaining Average Glucose Level?

# H_0:  beta_1 = 0
# H_1:  beta_1 != 0

#ANSWER:
# t is 17.00, p-value < 2e-16 
# Reject the null hypothesis.
# conclude beta_1 is not 0.

#Yes, age is a useful variable in explaining average_glucose_level

# Out of all the (numeric) variables, age and glucose level are the only ones that seem to have some sort
# of linear relationship. All others show to be not linearly related. Even with the regression model, 
# the assumptions are not fully met.
# Therefore, even though the regression model indicates that there is a linear relationship between age and avg
# glucose level, I am a bit skeptical about using age as a predictor for avg glucose level.




#Part D

#Anova testing for differences in average glucose level by smoking status

## ASSUMPTIONS

# Normality for each group
Data_regmodel<- lm(Stroke_DataClean$avg_glucose_level~Stroke_DataClean$smoking_status)
residual <- Data_regmodel$residual
Data2 <- cbind(Stroke_DataClean, residual)

# Split up dataset by group
Data2Unknown <- Data2$residual[Data2$smoking_status == "Unknown"]
Data2Never <- Data2$residual[Data2$smoking_status == "Never Smoked"]
Data2Former <- Data2$residual[Data2$smoking_status == "Formerly Smoked"]
Data2Smoker <- Data2$residual[Data2$smoking_status == "Smokes"]


# Normality Check for Unknown
qqnorm(Data2Unknown)
qqline(Data2Unknown)
shapiro.test(Data2Unknown)
#Not normal, small p-value 


# Normality Check for Never
qqnorm(Data2Never)
qqline(Data2Never)
shapiro.test(Data2Never)
#Not normal, small p-value 


# Normality Check for Formerly smokes
qqnorm(Data2Former)
qqline(Data2Former)
shapiro.test(Data2Former)
#Not normal, small p-value 


# Normality Check for Smoker
qqnorm(Data2Smoker)
qqline(Data2Smoker)
shapiro.test(Data2Smoker)
#Not normal, small p-value 

# All failed the normality assumption


#Normality for Overall Model
# Use when too many groups or sample sizes are small
qqnorm(Data_regmodel$residual)
qqline(Data_regmodel$residual)
shapiro.test(Data_regmodel$residual)
#Tiny P-value, not normal


# Residual vs group averages plots assumption

# Distribution of Residual By Group
boxplot(Data2$residual~Data2$smoking_status,xlab = "Smoking status",ylab = "Residual",main = "Distribution of Residual by Group")
# All groups are well behaved

#Overall Residual Plot check
plot(Data_regmodel$fitted.values, Data_regmodel$residuals, xlab = "Predicted Deflection Values", ylab = "Residual",main = "Overall Residual Plot")
abline(h = 0, lty = 2)
#Highly biased. Assumption failed.


#Homogeneity of variance test
# Hypotheses
# H_0: sigma_Unknown^2 = sigma_Never^2 = sigma_Formerly^2 = sigma_Smoker^2 
# H_1: not all variances are the same
install.packages("car")
library(car)
leveneTest(Data_regmodel)
#Small P-value <2.2e-16. Cannot assume equal variances.


##ANOVA TEST

# Hypotheses from Mean Model
# H_0: mu_Unknown = mu_Never = mu_Formerly = mu_Smoker
# H_1: at least one of the means is different

# ANOVA method

Data_aovmodel <- aov(Stroke_DataClean$avg_glucose_level~Stroke_DataClean$smoking_status)
Data_aovmodel
summary(Data_aovmodel)

#                                  Df  Sum Sq Mean Sq F value   Pr(>F)    
#Stroke_DataClean$smoking_status    3  123526   41175   21.12 1.37e-13 ***
  #Residuals                       4905 9562520   1950 

# p-value is small, reject H_0.  Conclude at least one mean
# is different statistically


## Comparisons 
TukeyHSD(Data_aovmodel)
# Never vs Former: 0.016 p-value --> significantly different
# Smoker vs Former: 0.031 p-value --> significantly different
# Unknown vs Former: 0.000 p-value --> significantly different
# Smoker vs Never: 0.98 p-value --> NOT different
# Unknown vs Never: 0.000 p-value --> significantly different
# Unknown vs Smoker: 0.0002 p-value --> significantly different

#Note: While the tests show significant differences, these results may be biased because of the failed assumptions




