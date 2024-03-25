# PS7

library(readr)
library(modelsummary)
library(dplyr)
library(mice)

wages <- read_csv("ProblemSets/PS7/wages.csv")

wages1 <- wages[complete.cases(wages[, c("hgc", "tenure")]), ]

# Produce a summary table using modelsummary
summary_table1 <- modelsummary(lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages1))
summary_table1

# Produce linear model object of initial model
model1 <- lm(logwage ~ hgc + college + tenure + +I(tenure^2) + age + married, data = wages1)

# Perform listwise deletion on the logwage variable

# Subset of data with missing logwage values
wages2 <- wages1[!is.na(wages1$logwage), ]

# Fit the regression model
model2 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages2)

# Perform mean imputation
wages3 <- transform(wages1, logwage = ifelse(is.na(logwage), mean(logwage, na.rm = TRUE), logwage))

# Fit the regression model
model3 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages3)

# Predict logwage for missing values based on the original model
wages4 <- transform(wages1, logwage = ifelse(is.na(logwage), predict(model1, newdata = wages1), logwage))

# Fit the regression model
model4 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages4)

# Perform multiple imputation
wages5 <- mice(wages1, m = 5, printFlag = FALSE)

# Estimate model
model5 <- with(wages5, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married)) 

# Pool results
model5 <- mice::pool(model5)

# Summarize
modelsummary(model5)

summary_table2 <- modelsummary(list(model1,model2,model3,model4,model5), stars = TRUE, output="PS7Models.csv")
summary_table2