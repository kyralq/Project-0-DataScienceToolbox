install.packages("psych")
install.packages("dplyr")
install.packages("plyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("multcomp")
install.packages("rpivotTable")
install.packages("caret")


cardio <- read.csv("cardio_train.csv", sep = ";")
head(cardio)
summary(cardio)

## To begin tidying up the data, we remove the identity column

cardio1 <- data[ , 2:13]
head(cardio1)

library(dplyr)
## This removes any biologically impossible data points, in which the systolic blood pressure is lower
## than the diastolic blood pressure
ap_cleaned <- cardio1 %>% filter(ap_hi > ap_lo)
nrow(ap_cleaned)

## removes around 1300 rows

## we also get rid of anomalies in the height and weight

height_cleaned <- ap_cleaned %>% filter(ap_cleaned$height >= 140 & ap_cleaned$height <= 220)
weight_cleaned <- height_cleaned %>% filter(height_cleaned$weight >= 30)
ap_cleaned2 <- weight_cleaned %>% filter(weight_cleaned$ap_lo >= 30 & weight_cleaned$ap_lo <= 170)
cleaned_cardio <- ap_cleaned2 %>% filter(ap_cleaned2$ap_hi >= 70 & ap_cleaned2$ap_hi < 250)

head(cleaned_cardio)
summary(cleaned_cardio)

## In the above, we can see that some categorical variables have been treated like they are continuous
## so we change these now. 

cols = c("gender", "cholesterol", "gluc", "smoke", "alco", "active", "cardio")
cleaned_cardio[cols] = lapply(cleaned_cardio[cols], factor)
summary(cleaned_cardio)

## I want to model an individual's susceptibility to cardiovascular disease, so I will split the data
##into training data, validation data, and testing data. 

## This is to fit a model, then fine tune the parameters, then test the model with another part of the data set

write.csv(train, "train.csv", row.names = FALSE)
write.csv(test,  "test.csv",  row.names = FALSE)
write.csv(valid, "valid.csv", row.names = FALSE)
 
 ##skipped the csv loading step by creating the training, validation, and test data in memory.
 
cols = c("gender", "cholesterol", "gluc", "smoke", "alco", "active", "cardio")
train[cols] <- lapply(train[cols], factor)
test[cols]  <- lapply(test[cols], factor)
valid[cols] <- lapply(valid[cols], factor)

## Now we begin EDA on the training data

summary(train)

## There are 4524 female observations, and 2489 male observations in the data set, a significant difference. 
## The data may be biased because of the unequal gender distribution

## There is an approximate 50:50 split of patients with cardiovascular disease, and patients without
##cardiovascular disease. So the dependent variable "cardio" is evenly distributed, so is a reliable 
##method to evaluate model performance.

library(corrplot)
## We identify the distribution of the continuous variables

ggplot(train, aes(x=age)) + geom_density()

ggplot(train, aes(x=height)) + 
  geom_histogram(aes(y=after_stat(density)),
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")


ggplot(train, aes(x=weight)) + 
  geom_histogram(aes(y=after_stat(density)),
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

##The above 3 plots demonstrate that height, weight, and age are not normally distributed, but are all 
##slightly skewed. We need to take this into account when selecting a model, as linear models assume normality. 

train.corr <- cor(train[, c(1, 3, 4, 5, 6)])
train.corr

corrplot(train.corr, method = "circle")

## this scatter visualisation gives us an idea of the correlations between two pairs of continuous variables
## blood pressure (highs and lows) height and weight. 
## We can see a clear correlation between ap_hi and ap_low, and height and weight.

## Our EDA on training data helps us come up with hypotheses

##Hypothesis 1: Do people with different cholesterol levels have different values of mean weight? Are cholesterol and weight correlated?

oneway.test(train$weight~train$cholesterol, var.equal = TRUE)
boxplot(train$weight~train$cholesterol)

## H0: the mean weight for people having different cholesterol levels is the same. Our test outputs a p-value
##of < 2.2e-16, which is smaller than 0.05. We reject the null hypothesis and conclude that there is a difference 
##in the mean weight among people with various cholesterol levels. 

##from the box plots, we can see that median, upper and lower quartile all increase as cholesterol level
##rises from 1 (normal) to 3 (well above normal). This implies that the independent variables of 
##cholesterol and weight are positively correlated.

##Hypothesis 2: Is there a correlation between independent variables height and weight?

##Correlated covariates are incorportated into the model as interaction terms

library(psych)
corr.test(train$height, train$weight)

##We return a correlation coefficient between height and weight of 0.31. Hence the two variables are 
##moderately correlated. We decide to include a new independent variable Body Mass Index 2 (BMI) to 
##substitute independent variables height and weight, where BMI = weight/(height^2).

## Hypothesis 3: Is there a correlation between systolic blood pressure (ap_high) and diastolic blood pressure (ap_low)

corr.test(train$ap_hi, train$ap_lo)

##The correlation test outputs a correlation coefficient of 0.74, implying a strong correlation between the
##two variables. Therefore we include an interaction term in the model. 

##Hypothesis 4: Will gender affect someone’s smoking habit?

library(rpivotTable)

rpivotTable(train, rows = c("gender"), cols = c("smoke"))

chisq.test(train$gender, train$smoke, correct=FALSE)

##We conduct a Chi-Squared test with a contingency table, against the null hypothesis that gender and smoking habit are
## independent, whereas the alternative hypothesis is that there is a correlation. The p-value is less than 2.2e-16, 
##which is smaller than 0.05. There is sufficient evidence at 5% significance level to reject the null hypothesis 
##and conclude that there is a correlation between gender and smoking habit.

##Now we've performed some tests, we will begin to transform variables

## Firstly, transform age in days to age in years

train$age_y <- NA
train$age_y <- train$age / 365
 
test$age_y <- NA
test$age_y <- test$age / 365
 
valid$age_y <- NA
valid$age_y <- valid$age / 365

## Introduce a BMI term as decided earlier

train$BMI <- NA
train$BMI <- (train$weight/ ((train$height/100)^2))
 
test$BMI <- NA
test$BMI <- (test$weight/ ((test$height/100)^2))
 
valid$BMI <- NA
valid$BMI <- (valid$weight/ ((valid$height/100)^2))

## Introduce interation terms, one for gender and smoking habit, one for Systolic blood pressure and diastolic blood pressure
## and one for cholesterol and BMI - all terms with correlations proven by our
##previous testing

## Use Backwards stepwise selection

## This selects the variables that give the best performing model

full_model <- glm(cardio~age_y + gender + BMI + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + (ap_hi*ap_lo) + (gender*smoke) + (cholesterol * BMI), data = train, family = "binomial")
step(full_model, direction="backward")

## so we have a logistic regression model with the terms; age_y, gender, BMI, ap_hi, ap_lo, cholesterol, gluc, smoke, alco, active, ap_loap_hi, gendersmoke.
## We have included two interaction terms


## This is our logistic regression model.
lm1 <- glm(cardio~age_y + gender + BMI + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + (ap_hi*ap_lo) + (gender*smoke), data = train, family = "binomial")
summary(lm1)

## We look at it's prediction accuracy on our training data with a confusion matrix

train$prob <- predict(lm1, train, type = "response")
train$pred <- NA
train$pred[train$prob >= 0.50] <- "Yes"
train$pred[train$prob < 0.50] <- "No"
table(train$pred, train$cardio)

## We can calculate accuracy with the following equation:
##  (2418 + 2630) / 6933 x 100% = 72.81%.

## Using a logistic regression model requires certain assumptions, which we will now double check

## independence - respondents in the data set are individuals at medical institutions, so the cases are independent 
## linearity - we can visualise this with a scatter plot


train$logitp <- log(train$prob/(1-train$prob))

ggplot(train, aes(BMI, logitp))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess")

ggplot(train, aes(age, logitp))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess")

ggplot(train, aes(ap_lo, logitp))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess")

ggplot(train, aes(ap_hi, logitp))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess")

## these scatter plots show how the continuous independent variables BMI, age, ap_hi, and ap_low
## are quite linearly correlated with the predicted 'cardio' outcome in logit scale. 
## For ap_lo we have relative linearity in the common range in the middle, but not at the extreme ends

##Multicollinearity - we introduce interaction terms based on EDA to reduce the influence of 
## multicollinearity in the logistic regression model.

## Large sample size - The train data consists of 6933 rows, which is considered as a large sample.

## Cross-validation on the validation data. 
## The accuracy rate is 72.57%, very similar to the rate on the training data. Therefore we haven't overfitted our model
## to the training data, and the predictive power is good. There is no fine tuning required, or any need to include additional
## variables in the logistic regression model. 

## We can proceed to evaluate the model performance on the test data

library(caret)

fitControl <- trainControl(method="cv", number=10)
set.seed(8)
model.lr <- train(cardio ~ ., 
                  data = valid,
                  method = "glm",
                  family=binomial(),
                  trControl = fitControl)
model.lr

prob <- predict(lm1, test, type = "response")
test$pred <- NA
test$pred[prob >= 0.50] <- "Yes"
test$pred[prob < 0.50] <- "No"
table(test$pred, test$cardio)

##The accuracy on test data is 72.20%. Recall is 68.60%. Precision is 73.03%. F-measure is 0.707
