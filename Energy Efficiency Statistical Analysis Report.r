# Packages that will be required for this project
install.packages("tidyverse")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("rcompanion")
install.packages("RVAideMemoire")
install.packages("readxl")
install.packages("FSA")
install.packages("caret")
install.packages("lattice")
install.packages("car")
install.packages("nlme")
install.packages("lmtest")
install.packages("sandwich")

library(tidyverse)
library(corrplot)
library(ggplot2)
library(rcompanion)
library(RVAideMemoire)
library(readxl)
library(FSA)
library(caret)
library(lattice)
library(car)
library(nlme)
library(lmtest)
library(sandwich)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# DATA EXPLORATION

# we first have to read the excel data into a data frame and name it Energy_Efficiency

Energy_Efficiency <- read_excel("Energy_Efficiency_Data.xlsx")

# then we perform some exploratory data analysis to first look at and understand the data we have

head(Energy_Efficiency)   # head show us the first 6 rows of the data

tail(Energy_Efficiency)   # tail show us the last 6 rows of the data

names(Energy_Efficiency)   # names show us all the column names in the data

summary(Energy_Efficiency)   #summary show us the statistical variations in the various columns of the data set

str(Energy_Efficiency)   #str shows us the data types associated with each column as well as the number of rows and columns in the data set which is 768 rows and 10 columns

any(is.na(Energy_Efficiency))   #is.na helps to check if there are missing values in the dataset

#plotting random columns just to explore and see how they relate
plot(Energy_Efficiency$`Relative Compactness`, Energy_Efficiency$`Surface Area`)   # the plot shows there is a negative linear correlation between surface area and relative compactness and there's also an outlier.

plot(Energy_Efficiency$`Heating Load`, Energy_Efficiency$`Cooling Load`)   # the plot shows there is a positive linear correlation between heating and cooling load

plot(Energy_Efficiency$`Surface Area`, Energy_Efficiency$`Heating Load`)   # the plot shows that there is a negative correlation between heating load and surface area

plot(Energy_Efficiency$`Glazing Area`, Energy_Efficiency$`Cooling Load`)   # the plot also shows a positive correlation 

plot(Energy_Efficiency$`Relative Compactness`, Energy_Efficiency$`Heating Load`)   # the plot shows a positive correlation between relative compactness and heating load 

hist(Energy_Efficiency$`Heating Load`)   # this show that there is a skew to the right in the distribution of heating load 

hist(Energy_Efficiency$`Cooling Load`)   # this looks like a more uniform distribution but also skewed to the right 

boxplot(Energy_Efficiency$`Glazing Area Distribution`)   #this helps identify outliers in glazing area distribution, which contain none 

par(mfrow = c(1,1))

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# CORRELATION ANALYSIS

#Now we want to see the correlation between all continuous variables which include 
# Heating Load, Cooling Load, Relative Compactness, Surface Area, Wall Area, Roof Area

# remove glazing area distribution, glazing area, orientation, overall height because they are categorical variables and not continuous
# we are using tidyverse to remove variables
# the pipe character "%>%" is a very good way to chain 
continuous_var <- Energy_Efficiency %>% select(-`Glazing Area Distribution`, -`Glazing Area`, -Orientation, -`Overall Height`)

head(continuous_var, 5)   # Display the first 5 observations of new dataset 

round(cor(continuous_var), digits = 2)   # Correlation for all continuous variables and rounded to 2 decimals

round(cor(continuous_var, method = "spearman"), digits = 2)   # Computing the correlation for all continuous variables using spearman and rounded to 2 decimals because of the nature of the data observed while performing exploratory data analysis

corrplot(cor(continuous_var, method = "spearman"), method = "number", type = "upper")   # Using corr plot for an improved correlation matrix and view of our correlation, type = "upper" shows only upper side of the matrix

# INTERPRETATION OF CONTINUOUS VARIABLES AGAINST HEATING AND COOLING LOAD
# Relative Compactness has a positive correlation between heating and cooling load, which implies that if relative compactness increase, heating and cooling load will also increase, same thing when it decrease. As for the strength of the relationship, it is a strong one as the values are closer to 1, being 0.62 and 0.65 respectively
# Surface Area has a negative correlation between heating and cooling load, which implies that if surface area increase, heating and cooling load will decrease as the vary in opposite directions. As for the strength of the relationship, it is a strong one as the value is closer to -1, being -0.62 and -0.65 respectively
# Wall Area has a positive correlation between heating and cooling load, as explained above. As for the strength of the relationship, it is a weak one, meaning as one variable increases, theres no tendency in the other variable increasing or decreasing as the value is independent and closer to 0 with 0.46 and 0.41 respectively
# Roof Area has a negative correlation between heating and cooling load, as explained above. As for the strength of the relationship, it is a strong one as the value is closer to -1, being -0.75 and -0.76 respectively
# Heating and Cooling load has a positive and very strong relationship, with a correlation of 0.97

# Next, we want to see the correlation, heating and cooling loads, have on categorical variables using ANOVA
# Assumptions 1-3 are already fulfilled so we move on to assumption 4 to check for significant outliers 
boxplot(`Heating Load` ~ `Glazing Area Distribution`, data = Energy_Efficiency, names = c("Uniform", "North", "East", "South", "West", "Unknown"),
        xlab = "Types of Glazing Area Distributions", ylab = "Heating Load",
        main = "Heating Load of Glazing Area Distribution")   
#There is no significant outlier

# For assumption 5, we implement the Shapiro-Wilk test
byf.shapiro(`Heating Load` ~ `Glazing Area Distribution`, data = Energy_Efficiency)
#Since all the p-values are less than 0.05, they failed the normality test and therefore not normally distributed.
# Instead, we make use of the Kruskal-Wallis test since normality is violated in multiple groups and Kruskal-Wallis test is a non-parametric alternative

kruskal.test(`Heating Load` ~ `Glazing Area Distribution`, data = Energy_Efficiency)
# p-value is far below 0.05 therefore one group differs significantly 
# so we do a post-hoc test next

dunnTest(`Heating Load` ~ factor(`Glazing Area Distribution`), data = Energy_Efficiency, method = "holm")
# from the test, looking at P.adj results, we can see that all significant differences involves Group 1 and Groups 2-6 are statistically similar, therefore
# Post-hoc Dunn tests revealed that Glazing Area Distribution category 1 differed significantly (p < 0.001) from all other categories (2–6).
#This suggests that the Heating Load is uniquely influenced when glazing is distributed equally on all sides, but configurations 2–6 produce similar performance.


# We repeat the process for Cooling load
# Assumptions 1-3 are already fulfilled so we move on to assumption 4 to check for significant outliers 
boxplot(`Cooling Load` ~ `Glazing Area Distribution`, data = Energy_Efficiency, names = c("Uniform", "North", "East", "South", "West", "Unknown"),
        xlab = "Types of Glazing Area Distributions", ylab = "Cooling Load",
        main = "Cooling Load of Glazing Area Distribution")
#There is no significant outlier

# For assumption 5, we implement the Shapiro-Wilk test
byf.shapiro(`Cooling Load` ~ `Glazing Area Distribution`, data = Energy_Efficiency)
#Since all the p-values are less than 0.05, they failed the normality test and therefore not normally distributed.
# Instead, we make use of the Kruskal-Wallis test since normality is violated in multiple groups and Kruskal-Wallis test is a non-parametric alternative

kruskal.test(`Cooling Load` ~ `Glazing Area Distribution`, data = Energy_Efficiency)
# p-value is far below 0.05 therefore one group differs significantly 
# so we do a post-hoc test next

dunnTest(`Cooling Load` ~ factor(`Glazing Area Distribution`), data = Energy_Efficiency, method = "holm")
# from the test, looking at P.adj results, we can see that all significant differences involves Group 1 and Groups 2-6 are statistically similar, therefore
# Post-hoc Dunn tests revealed that Glazing Area Distribution category 1 differed significantly (p < 0.01) from all other categories (2–6).
#This suggests that the Cooling Load is uniquely influenced when glazing is distributed equally on all sides, but configurations 2–6 produce similar performance.


# Assumptions 1-3 are already fulfilled so we move on to assumption 4 to check for significant outliers 
boxplot(`Heating Load` ~ `Glazing Area`, data = Energy_Efficiency, names = c("0%", "10%", "25%", "40%"),
        xlab = "Types of Glazing Area", ylab = "Heating Load",
        main = "Heating Load of Glazing Area")
#There is no significant outlier

# For assumption 5, we implement the Shapiro-Wilk test
byf.shapiro(`Heating Load` ~ `Glazing Area`, data = Energy_Efficiency)
#Since all the p-values are less than 0.05, they failed the normality test and therefore not normally distributed
# Instead, we make use of the Kruskal-Wallis test since normality is violated in multiple groups and Kruskal-Wallis test is a non-parametric alternative

kruskal.test(`Heating Load` ~ factor(`Glazing Area`), data = Energy_Efficiency)
# from the result, we can see that it is a highly significant result as the p-value is much lower than 0.001
# Next we run a post-hoc test

dunnTest(`Heating Load` ~ factor(`Glazing Area`), data = Energy_Efficiency, method = "holm")
# from the test, we can see that all comparisons are statistically significant because all adjusted p-values are less than 0.05.
# This implies that increasing the glazing area consistently increases the heating load, with 0%, 10%, 25%, and 40% glazing resulting 
# in statistically distinct heating performance. Thus, glazing area exerts a strong and monotonic influence on heating load


# We repeat the process for Cooling load
# Assumptions 1-3 are already fulfilled so we move on to assumption 4 to check for significant outliers 
boxplot(`Cooling Load` ~ `Glazing Area`, data = Energy_Efficiency, names = c("0%", "10%", "25%", "40%"),
        xlab = "Types of Glazing Area", ylab = "Cooling Load",
        main = "Cooling Load of Glazing Area")
#There is no significant outlier

# For assumption 5, we implement the Shapiro-Wilk test
byf.shapiro(`Cooling Load` ~ `Glazing Area`, data = Energy_Efficiency)
#Since all the p-values are less than 0.05, they failed the normality test and therefore not normally distributed
# Instead, we make use of the Kruskal-Wallis test since normality is violated in multiple groups and Kruskal-Wallis test is a non-parametric alternative

kruskal.test(`Cooling Load` ~ factor(`Glazing Area`), data = Energy_Efficiency)
# from the result, we can see that it is a highly significant result as the p-value is much lower than 0.05
# Next we run a post-hoc test

dunnTest(`Cooling Load` ~ factor(`Glazing Area`), data = Energy_Efficiency, method = "holm")
# from the test, we can see that all comparisons are statistically significant because all adjusted p-values are less than 0.05.
# This implies that increasing the glazing area consistently increases the cooling load, with 0%, 10%, 25%, and 40% glazing resulting 
# in statistically distinct cooling performance. Thus, glazing area exerts a strong and monotonic influence on heating load


# Assumptions 1-3 are already fulfilled so we move on to assumption 4 to check for significant outliers 
boxplot(`Heating Load` ~ `Orientation`, data = Energy_Efficiency, names = c("North", "East", "South", "West"),
        xlab = "Types of Orientation", ylab = "Heating Load",
        main = "Heating Load of Orientation")
#There is no significant outlier

# For assumption 5, we implement the Shapiro-Wilk test
byf.shapiro(`Heating Load` ~ `Orientation`, data = Energy_Efficiency)
#Since all the p-values are less than 0.05, they failed the normality test and therefore not normally distributed
# Instead, we make use of the Kruskal-Wallis test since normality is violated in multiple groups and Kruskal-Wallis test is a non-parametric alternative

kruskal.test(`Heating Load` ~ factor(`Orientation`), data = Energy_Efficiency)
# from the test, we can see that the p value is 0.99 which is extremely large, therefore it has no statistical difference across the 4 orientation categories
# This suggests that heating performance is independent of the direction the building faces


# Assumptions 1-3 are already fulfilled so we move on to assumption 4 to check for significant outliers 
boxplot(`Cooling Load` ~ `Orientation`, data = Energy_Efficiency, names = c("North", "East", "South", "West"),
        xlab = "Types of Orientation", ylab = "Cooling Load",
        main = "Cooling Load of Orientation")
#There is no significant outlier

# For assumption 5, we implement the Shapiro-Wilk test
byf.shapiro(`Cooling Load` ~ `Orientation`, data = Energy_Efficiency)
#Since all the p-values are less than 0.05, they failed the normality test and therefore not normally distributed
# Instead, we make use of the Kruskal-Wallis test since normality is violated in multiple groups and Kruskal-Wallis test is a non-parametric alternative

kruskal.test(`Cooling Load` ~ factor(`Orientation`), data = Energy_Efficiency)
# from the test, we can see that the p value is 0.94 which is extremely large, therefore it has no statistical difference across the 4 orientation categories
# This suggests that cooling performance is also independent of the direction the building faces


# Assumptions 1-3 are already fulfilled so we move on to assumption 4 to check for significant outliers 
boxplot(`Heating Load` ~ `Overall Height`, data = Energy_Efficiency, names = c("3.5m", "7.0m"),
        xlab = "Types of Height", ylab = "Heating Load",
        main = "Heating Load of Each Height")
# There are a few outliers but its not significant, assumption 4 is checked

# For assumption 5, we implement the Shapiro-Wilk test
byf.shapiro(`Heating Load` ~ `Overall Height`, data = Energy_Efficiency)
#Since all the p-values are less than 0.05, both groups failed the normality test and therefore not normally distributed. So we can not use t-test 
# because of this, we have to use a non parametric alternative to t-test which is wilcoxon rank sum

wilcox.test(`Heating Load` ~ factor(`Overall Height`),  data = Energy_Efficiency)
# from the test, we can see that there's a highly significant difference in heating load between the two groups as the p value is far below 0.05


boxplot(`Cooling Load` ~ `Overall Height`, data = Energy_Efficiency, names = c("3.5m", "7.0m"),
        xlab = "Types of Height", ylab = "Cooling Load",
        main = "Cooling Load of Each Height")
# There are a few outliers but its not significant, assumption 4 is checked

# For assumption 5, we implement the Shapiro-Wilk test
byf.shapiro(`Cooling Load` ~ `Overall Height`, data = Energy_Efficiency)
#Since all the p-values are less than 0.05, they failed the normality test and therefore not normally distributed in either the two height groups 
# because of this, we have to use a non parametric alternative to t-test which is wilcoxon rank sum

wilcox.test(`Cooling Load` ~ factor(`Overall Height`), data = Energy_Efficiency)
# from the test, we can see that there's a highly significant difference in cooling load between the two groups as the p value is far below 0.05


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# REGRESSION ANALYSIS
  # MULTIPLE LINEAR REGRESSION

#OBJECTIVE :
# 1. Predict Heating Load(HL) based on design variables
# 2. Predict Cooling Load(CL) based on design variables
# Dependent variable: Heating Load and Cooling Load(continuous)
# Independent variables(continuous): Relative Compactness, Surface Area, Wall Area, Roof Area
# Independent variables(Categorical): Overall Height(ordinal), Orientation(nominal), Glazing Area(ordinal), Glazing Area Distribution(nominal)
  
  #For each predictor, regression will test:
  #Null hypothesis H₀:
  #The predictor has no effect on Heating Load (β = 0)
#Alternative hypothesis H₁:
  #The predictor does significantly affect Heating Load (β ≠ 0)
#And for the model overall:
  #Model F-test H₀:
  #All β = 0 (model has no explanatory power)
#Model F-test H₁:
  #At least one β ≠ 0 (model significantly explains Heating Load)


model1 <- lm(`Heating Load` ~ `Relative Compactness` + `Surface Area` + `Wall Area` + `Roof Area` +
               `Overall Height` + factor(`Orientation`) + `Glazing Area` + factor(`Glazing Area Distribution`), data = Energy_Efficiency)
model2 <- lm(`Cooling Load` ~ `Relative Compactness` + `Surface Area` + `Wall Area` + `Roof Area` + 
               `Overall Height` + factor(`Orientation`) + `Glazing Area` + factor(`Glazing Area Distribution`), data = Energy_Efficiency)

summary(model1)
summary(model2)
# INTERPRETATION
# Model 1 has an overall quality of 92.15% (R² = 0.9215), meaning it is a very stable model with F-test p <2.2e-16 meaning that the model is statistically significant
# Relative Compactness has a strong negative effect on heating load, while Overall Height and Glazing Area both have a strong positive effect and Glazing Area Distribution 
# The rest are not significant 
# Model 2 has an overall quality of 88.9% (R² = 0.8893), meaning it is a very strong and highly significant model
# Relative compactness, Overall Height and Glazing Area are all significant predictors affecting Cooling Load with some levels of Glazing Area Distribution(2,3,5 marginally) while Surface Area has a weak significant level of p = 0.066
# The rest are not significant 




# Next, we check if the fitted model meets the MLR assumptions
# 1. Linearity
 # we will check the linearity by examining a scatter plot between heating and cooling load and the independent variables
data.frame(colnames(Energy_Efficiency))   #we use this to check the column numbers for the scatterplot

pairs(Energy_Efficiency[,c(9,1,2,3,4,5,6,7,8)], lower.panel = NULL, pch = 19,cex = 0.2)   #This draws a scatterplot to show linearity with heating load 

pairs(Energy_Efficiency[, c(10,1,2,3,4,5,6,7,8)], lower.panel = NULL, pch = 19, cex = 0.2)   # This draws a scatterplot to show linearity with cooling loads    
# from the plots, they both satisfy the linearity assumptions 

# 2. Residuals Independence

plot(model1, 1)    # we will check the assumption by examining a scatterplot of “residuals versus fits” for heatin load,  the correlation should be approximately 0

plot(model2, 1)    # we will check the assumption by examining a scatterplot of “residuals versus fits” for heatin load,  the correlation should be approximately 0
# The residual independence plots for both heating and cooling loads show systematic patterns and clustering rather than random dispersion around 0 and it indicates violations of the independence and linearity
# The curvature tells us that the relationship between the predictors and response is non linear, therefore the current linear model may be inadequate

# Now we try to transform the dependent variables using log

model1_log <- lm(log(`Heating Load`) ~ `Relative Compactness` + `Surface Area` + `Wall Area` + `Roof Area` + `Overall Height` + factor(`Orientation`) + `Glazing Area` + factor(`Glazing Area Distribution`), data = Energy_Efficiency)
par(mfrow = c(2,2))   # This partitions the plots and helps us put 4 different plots together and now we use it to check the residual independence, normality of residuals, equal variances of the residuals and influential points
plot(model1_log)

model2_log <- lm(log(`Cooling Load`) ~ `Relative Compactness` + `Surface Area` + `Wall Area` + `Roof Area` + `Overall Height` + factor(`Orientation`) + `Glazing Area` + factor(`Glazing Area Distribution`), data = Energy_Efficiency)
par(mfrow = c(2,2))   # SAME AS ABOVE 
plot(model2_log)
# We can see that the regression model is now statistically acceptable and valid, so we move on to the multicollinearity check


vif(model1)    #To check there is no multicollinearity

vif(model2)    #SAME AS ABOVE
# from the result, we can see that there is serious multicollinearity, meaning we have to remove redundant variables

model_reduced1 <- lm(`Heating Load` ~ `Relative Compactness` + `Overall Height` + factor(`Orientation`) + 
                       `Glazing Area` + factor(`Glazing Area Distribution`), data = Energy_Efficiency) # Since Surface Area, Wall Area, Roof Area and Relative Compactness are all geometrically related, we just pick one out of them to use which is relative compactness as its the most significant

model_reduced2 <- lm(`Cooling Load` ~ `Relative Compactness` + `Overall Height` + factor(`Orientation`) + 
                       `Glazing Area` + factor(`Glazing Area Distribution`), data = Energy_Efficiency) #SAME AS ABOVE

vif(model_reduced1)   #To check there is no multicollinearity in the reduced model

vif(model_reduced2)   # SAME AS ABOVE
# We can see that the results are all below 5 in GVIF which means that there is no harmful correlation between predicators, coefficients are stable and the model is now statistically interpretable.


# NOW WE HAVE TO VALIDATE OUR MODEL BY CHECKING ASSUMPTIONS AGAIN AND COMPARING PERFORMANCE TO ORIGINAL MODEL
par(mfrow = c(2,2))
plot(model_reduced1)
plot(model_reduced2)
# from the graph, the assumptions are violated so we have to repeat the transformation process again using log

# first we transform the model using log 
log_model_reduced1 <- lm(log(`Heating Load`) ~ `Relative Compactness` + `Overall Height` + 
                           factor(`Orientation`) + `Glazing Area` + factor(`Glazing Area Distribution`), data = Energy_Efficiency)
par(mfrow = c(2,2)) 
plot(log_model_reduced1)

log_model_reduced2 <- lm(log(`Cooling Load`) ~ `Relative Compactness` + `Overall Height` + 
                           factor(`Orientation`) + `Glazing Area` + factor(`Glazing Area Distribution`), data = Energy_Efficiency)
par(mfrow = c(2,2)) 
plot(log_model_reduced2)
# from the plot, we can see that Cooling Load met the residual independence and normality of residuals assumptions and also addressed multicollinearity but Heating Load still violates assumptions 
# Therefore we move on to try another transformation on heating load using Square root

#Next we transform Heating Load using square root
sqr_model_reduced1 <- lm(sqrt(`Heating Load`) ~ `Relative Compactness` + `Overall Height` + 
                           factor(`Orientation`) + `Glazing Area` + factor(`Glazing Area Distribution`), data = Energy_Efficiency)
par(mfrow = c(2,2))
plot(sqr_model_reduced1)
# from the plot, we can see that Heating Load met the residual independence and normality of residuals assumptions and also addressed multicollinearity but still violates homoscedasticity assumptions 

# Finally we check homoscedasticity assumptions for both models
bptest(sqr_model_reduced1)   # this is a Breusch-Pagan test to see if our heating load model is homoscedastic or heteroscedastic

bptest(log_model_reduced2)   # this is a Breusch-Pagan test to see if our cooling load model is homoscedastic or heteroscedastic
# since both p-values are extremely small, we reject the null hypothesis and fix using robust standard errors

# So now we apply HC1 Robust Standard Errors to our heating and cooling models
coeftest(sqr_model_reduced1, vcov = vcovHC(sqr_model_reduced1, type = "HC1"))
coeftest(log_model_reduced2, vcov = vcovHC(log_model_reduced2, type = "HC1"))
# The Heating Load square root transformed regression model revealed that Relative Compactness, Overall Height, Glazing Area, and Glazing Area Distribution significantly influence Heating Load (p < 0.05). Relative Compactness exhibited a significant negative relationship, while Overall Height and Glazing Area showed significant positive effects. Orientation was not statistically significant. Robust standard errors confirmed the reliability of these results despite the presence of heteroscedasticity
# The Cooling Load log transformed regression model revealed that Relative Compactness, Overall Height, Glazing Area, and Glazing Area Distribution are significant predictors of Cooling Load. Relative Compactness significantly reduces cooling demand, while Overall Height and Glazing Area increase it. Orientation was not significant. The use of robust standard errors ensured valid inference despite violations of the homoscedasticity assumption

# The final regression model for Heating Load, after square-root transformation and assumption validation, is given by:
# √HEATING LOAD = 2.2157 − 3.4217(RC) + 2.5241(OH) + 0.2666(GA) + 0.4973(GAD2) + 0.4906(GAD3) + 0.4648(GAD4) + 0.4840(GAD5) + 0.4618(GAD6)

# Back-transforming gives: HL = [predicted √HL]²

# The final model for Cooling Load, after log transformation, is:
# log(COOLING LOAD) = 2.2709 − 1.0788(RC) + 0.9094(OH) + 0.0837(GA) + 0.0877(GAD2) + 0.0844(GAD3) + 0.0674(GAD4) + 0.0832(GAD5) + 0.0710(GAD6)

# Back-transforming gives: CL = e^(predicted log(CL))

# Where HL = Heating Load, RC = Relative Compactness, OH = Overall Height, GA = Glazing Area and GAD = Glazing Area Distribution(category 2-6)

#To conclude the regression model, we fit the final models clearly

model_HL_final <- lm(sqrt(`Heating Load`) ~ `Relative Compactness` + `Overall Height` + `Glazing Area` + 
                       factor(`Glazing Area Distribution`), data = Energy_Efficiency)
model_CL_final <- lm(log(`Cooling Load`) ~ `Relative Compactness` + `Overall Height` + `Glazing Area` + 
                       factor(`Glazing Area Distribution`), data = Energy_Efficiency)

summary(model_HL_final)
summary(model_CL_final)

# REPORT 
# This study employed multiple linear regression to model Heating and Cooling Loads of buildings based on structural characteristics. After resolving assumption violations through appropriate transformations, addressing multicollinearity, and homoscedasticity, final reduced models were established. 
# Both models demonstrated strong explanatory power, with Adjusted R² values exceeding 91% (Heating Load model adjusted R² = 93.3% and Cooling Load model adjusted R² = 91.5%)
# Relative Compactness consistently reduced both Heating and Cooling Loads, while Overall Height and Glazing Area significantly increased energy demand. Glazing Area Distribution categories also exhibited strong positive effects. Orientation showed no statistically significant impact in either model.
# These findings highlight the critical role of building geometry and glazing configuration in optimizing energy efficiency and can guide sustainable architectural design decisions.


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# HYPOTHESIS TESTING 

# HYPOTHESIS TEST 1: Does the pattern of glazing distribution affect the heating load?
  # H₀: The mean heating load is equal across all six levels of glazing area distribution
  # H₁: At least one group mean of Heating Load is significantly different from the others.

# For this, we will be making use of One-Way ANOVA 
  # Dependent Variable: Heating Load 
  # Independent Variable: Glazing Area Distribution
  
par(mfrow = c(1, 1))  # to reset the screen partitioning 

#Assumptions 1-3 are already met so we move to assumption 4
  boxplot(`Heating Load` ~ `Glazing Area Distribution`, data = Energy_Efficiency, names = c("Uniform", "North", "East", "South", "West", "Unknown"),
          xlab = "Types of Glazing Area Distributions", ylab = "Heating Load",
          main = "Heating Load of Glazing Area Distribution")
  # There are no significant outliers

#Assumption 5 now to check normality
byf.shapiro(`Heating Load` ~ `Glazing Area Distribution`, data=Energy_Efficiency)
#Since all the p-values are less than 0.05, they failed the normality test and therefore not normally distributed.
# Instead, we make use of the Kruskal-Wallis test since normality is violated in multiple groups and Kruskal-Wallis test is a non-parametric alternative

kruskal.test(`Heating Load` ~ `Glazing Area Distribution`, data = Energy_Efficiency)
# p-value is far below 0.05 therefore one group differs significantly and we reject the null hypothesis
# so we do a post-hoc test to see which category exactly differs significantly

dunnTest(`Heating Load` ~ factor(`Glazing Area Distribution`), data = Energy_Efficiency, method = "holm")
# from the test, looking at P.adj results, we can see that all significant differences involves Group 1 and Groups 2-6 are statistically similar, therefore
# Post-hoc Dunn tests revealed that Glazing Area Distribution category 1 differed significantly (p < 0.001) from all other categories (2–6).
#This suggests that the Heating Load is uniquely influenced when glazing is distributed equally on all sides, but configurations 2–6 produce similar performance.

# Now lets confirm the homogenity of the test using levene test

levene_result <- leveneTest(`Heating Load` ~ factor(`Glazing Area Distribution`), data = Energy_Efficiency, center = median)
print(levene_result)
# From the result, we therefore reject the null hypothesis
# Since both the normality and homogeneity of variance assumptions were violated, the Kruskal-Wallis Test, a non-parametric test robust to both these violations, was the most appropriate and reliable technique for comparing the central tendency (median) of Heating Load across the different distribution groups


# HYPOTHESIS TEST 2: Is there a difference in Cooling Load between highly compact and loosely compact buildings?
   # H₀: The mean Cooling Load for buildings with high relative compactness is equal to that of buildings with low relative compactness
   # H₁: The mean Cooling Load is significantly different between the two compactness groups

# For this, we will be making use of Independent two sample T-test
   # Dependent Variable: Cooling Load 
   # Independent Variable: Relative Compactness

# First we convert into 2 groups, high compactness vs low compactness
Energy_Efficiency$compact_grp_med <- ifelse(
  Energy_Efficiency$`Relative Compactness` >= median(Energy_Efficiency$`Relative Compactness`),
  "high", "low")
Energy_Efficiency$compact_grp_med <- factor(Energy_Efficiency$compact_grp_med, levels=c("low","high"))

# Next we check the normality within each group using shapiro-wilk and also the equality of variance 
by(Energy_Efficiency$`Cooling Load`, Energy_Efficiency$compact_grp_med, shapiro.test)   #normality

leveneTest(`Cooling Load` ~ compact_grp_med, data = Energy_Efficiency)   # homogeneity of variance (levene)
# Since they are both violated, we use the wilcoxon rank sum test
wilcox.test(`Cooling Load` ~ compact_grp_med, data = Energy_Efficiency)

# Since p-value is far smaller than 0.05, you reject the null hypothesis

# HYPOTHESIS TEST 3: For a given building design, is the Heating Load equivalent to the Cooling Load?
   # H₀: The mean difference between Heating Load and Cooling Load across the set of buildings is zero
   # H₁: The mean difference is not zero (i.e., one load is consistently higher or lower than the other

# For this, we will be making use of Paired two sample t-test
   # Dependent Variables: Heating and Cooling Load 
   
# first we test whether one load is systematically higher than the other
Energy_Efficiency$Load_Difference <- Energy_Efficiency$`Heating Load` - Energy_Efficiency$`Cooling Load`

summary(Energy_Efficiency$Load_Difference)
# the mean(-2.266) is negative which means the cooling load is generally higher

shapiro.test(Energy_Efficiency$Load_Difference)    #to check the normality 
# from the p-value we see that it is not normally distributed so we use wilcoxon signed rank test

wilcox.test(Energy_Efficiency$`Heating Load` , Energy_Efficiency$`Cooling Load` , paired = TRUE, alternative = "two.sided")
# Since p-value is < 0.05 we reject the null hypothesis


# HYPOTHESIS TEST 4: Is Glazing Area positively associated with Cooling Load?
   # H₀: The population correlation between Glazing Area and Cooling Load is 0 (no association)
   # H₁: The population correlation between Glazing Area and Cooling Load is greater than 0 (positive association)

# For this, we will be making use of Spearman Correlation Test(one-sided)
   # Dependent Variable: Cooling Load 
   # Independent Variable: Glazing Area

plot(Energy_Efficiency$`Glazing Area`, Energy_Efficiency$`Cooling Load`,
     xlab="Glazing Area", ylab="Cooling Load", main="Glazing Area vs Cooling Load")

cor.test(Energy_Efficiency$`Glazing Area`, Energy_Efficiency$`Cooling Load`, method = "spearman", alternative = "greater")
# Since the test has a p-value of 6.069e-16, which is less than 0.05 then we reject the null hypothesis









