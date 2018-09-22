INCOME<- read.csv("Lab 1 2018/income.csv",header=TRUE)

attach(INCOME)

# Q1 - Creating of income vs. bachelors with the least square fit line through it
plot(MedInc,BachPlus_Pct,xlab="CT median household income", ylab="Percentage o/adults w/bachelor degree or higher")
hist(MedInc,breaks=35)
hist(BachPlus_Pct,breaks=35)

# Q2 - Estimate the following linear model, m1: MedInc = b_0+b_1*BachPlus_Pct. 
# a) Goodness of fit (using R^2 and ANOVA).
# a) Validity of this regression model (using diagnostic plots and statistics).


# Q2 - step 1: model

model=lm(MedInc~BachPlus_Pct)
print(model)

# We find b_0 and b_1
model$coefficients
b_0 <- model$coefficients[1]
b_1 <- model$coefficients[2]

# Q2 - step 2: R^2
# We can look up R^2 using summary(model):
rsquared_model <- summary(model)$r.squared 
# 0.1409696

