# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # PACKAGES # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# I use the ROCR package to make a colourful graph (ROC)!
install.packages("pROC")
library(pROC)
install.packages("ROCR")
library(ROCR)

library(readr)
install("LogisticDx") 
library(LogisticDx)
library(pROC)
install.packages("caTools")
library(caTools)

install.packages("car")
library(car)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

MNY <- read.csv("MichelinNY.csv", header=TRUE)
str(MNY)
summary(MNY)

par(mfrow=c(2,2))
plot(MNY$Food)
plot(MNY$Decor)
plot(MNY$Service)
plot(MNY$Price)

par(mfrow=c(2,2))
plot(MNY$Food,jitter(MNY$InMichelin,.1))
plot(MNY$Decor,jitter(MNY$InMichelin,.1))
plot(MNY$Service,jitter(MNY$InMichelin,.1))
plot(MNY$Price,jitter(MNY$InMichelin,.1))

# At first glace, dependent variables seem pretty evenly spread, no patterns. Likely no transformations recommended.


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # Generating possible models # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Just variables:
MNY_indep <- subset(MNY, select = c("Food","Decor","Service","Price"))


# The resulting output suggest that the variables Food and Price might be useful to help us predict InMichelin, but that Decon and Service may not be
# Before we go any further, check cor() of independent variable candidates:
cor(MNY_indep)


# First attempts: Make a bunch of models, we will run diagnostics later to fine tune the choice, and use AIC as a first way to select candidate models
# The Restaurant.Name variable is not relevant to our InMicheline variable
MNY2 <- subset(MNY, select = c("InMichelin","Food","Decor","Service","Price"))

#We do a trial logistic model including all the variables:
mny <- glm(InMichelin ~ ., data=MNY2, family=binomial)
summary(mny)

# However we noted that Price seems highly correlated with Service and Decor.
mny2 <- glm(InMichelin ~ Price + Decor + Food, data=MNY2, family=binomial)
summary(mny2)

# Out of curiosty, check what happnes when Price is removed:
mny3 <- glm(InMichelin ~ Service + Decor + Food, data=MNY2, family=binomial)
summary(mny3)

# I am now convinced that taking decor and food OR food and price may both be good options, but 'decor and food' yield a highe AUC.
mny4 <- glm(InMichelin ~  Decor + Food, data=MNY2, family=binomial)
summary(mny4) # This has a lower AIC, so this model seems better than mny3, but not better than mny2

# It looks like models without Price are doing worse (higher AICs), so we try some other models that include Price
# Decor is also not showing as significant, so we try mny2, but without Decor
mny5 <- glm(InMichelin ~  Price + Food, data=MNY2, family=binomial)
summary(mny5) # This model has a marginally lower AIC than mny2, but almost identical


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# The values in the correlation matrix were a bit high, so we also check VIFs before moving on
vif2 <- vif(mny2)
vif2
vif5 <- vif(mny5)
vif5
# These values are ok, so we proceed with the rest of the diagnostics

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # Comparing possible models # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# We take our two candidate models mny2 and mny5 and compare them using our diagnostic tools

# I check the accuracy of predictions of mny2 on our data set: 
predictionsMNY2 <- predict(mny2, type="response") 
str(predictionsMNY2)
table(MNY$InMichelin, predictionsMNY2>0.5)
(80+52)/nrow(MNY) # The accuracy using a threshold value of 0.5 is not bad (0.804878) ...
# ... but let's explore other possible threshold values using the ROC curve.

# I check the accuracy of predictions of mny5 on our data set: 
predictionsMNY5 <- predict(mny5, type="response") 
str(predictionsMNY5)
table(MNY$InMichelin, predictionsMNY5>0.5)
(79+52)/nrow(MNY) # The accuracy using a threshold value of 0.5 is not bad (0.7987805) ...
# ... THIS IS ALMOST the SAME AS mny2, marginally worse, so mny5 seems like a good choice


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ROC and threshold values
# a) mny2

# ROC curve - Option 1 (as in class)
roc(MNY$InMichelin,predict(mny2),ci=TRUE,ci.alpha=.95,plot=TRUE, auc.polygon=TRUE, max.auc.polygon = TRUE, grid = TRUE, print.auc=TRUE)

# ROC curve - Option 2 (ROCR and caTools packages)
# Prediction function
ROCRpred = prediction(predictionsMNY2, MNY$InMichelin)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# From the ROC plot, it looks like 0.6 is a good value to pick if we want to keep false positives pretty low.

# I check the accuracy of the model mny2 model for a threshold level of 0.6:

table(MNY$InMichelin, predictionsMNY2>0.6)
(84+46)/nrow(MNY)
# I get an accuracy of 0.7926829, which is very close to the accuracy level above (for 0.5) 0.804878
# They are both good (similar) choices, depending on what our goals for the model are.

# b) mny5

# ROC curve - Option 1 (as in class)
roc(MNY$InMichelin,predict(mny5),ci=TRUE,ci.alpha=.95,plot=TRUE, auc.polygon=TRUE, max.auc.polygon = TRUE, grid = TRUE, print.auc=TRUE)

# ROC curve - Option 2 (ROCR and caTools packages)
# Prediction function
ROCRpred = prediction(predictionsMNY5, MNY$InMichelin)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# From the ROC plot, it looks like 0.6 is a good value to pick if we want to keep false positives pretty low.

# I check the accuracy of the model mny5 model for a threshold level of 0.6:

table(MNY$InMichelin, predictionsMNY5>0.6)
(84+46)/nrow(MNY)
# I get the same accuracy of 0.7926829, so in this sense, the choice of 0.6 makes no difference for either model.




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# MORE DIAGNOSTICS (residuals)


# mny2

# We should have run diagnostics first, but we'll do them now, an dsee if anything needs to be changed afterwards, based on the results:
mny2.dev_residuals = residuals(mny2, type="deviance") #deviance residuals
mny2.pearson_residuals = residuals(mny2, type="pearson") #pearson residuals

# We check the spread of the deviance residuals:
par(mfrow=c(1,2))
plot(predictionsMNY2,mny2.dev_residuals)
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")

# We check the spread of the Pearson residuals residuals:
plot(predictionsMNY2,mny2.pearson_residuals)
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")

sum(mny2.dev_residuals^2) 
sum(mny2.pearson_residuals^2) # To count how many 'large' residuals we have

sum(abs(mny2.dev_residuals>2)) # To count how many 'large' residuals we have
sum(abs(mny2.pearson_residuals>2)) # To count how many 'large' residuals we have

# Note here that we have a few points with residuals s.t. abs(residual)>2, uh oh.
# Notably, the outliers with pearson residual lower than -5 and -15 may be of concern.
# We do not have the context to know if these cases should be deleted, but we assume we do, just for the exercise

# We do the same thing for mny5

# Residuals:
mny5.dev_residuals = residuals(mny5, type="deviance") #deviance residuals
mny5.pearson_residuals = residuals(mny5, type="pearson") #pearson residuals

# We check the spread of the deviance residuals:
par(mfrow=c(1,2))
plot(predictionsMNY5,mny5.dev_residuals)
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")

# We check the spread of the Pearson residuals residuals:
plot(predictionsMNY5,mny5.pearson_residuals)
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")

sum(mny5.dev_residuals^2) 
sum(mny5.pearson_residuals^2) # To count how many 'large' residuals we have

sum(abs(mny5.dev_residuals>2)) # To count how many 'large' residuals we have
sum(abs(mny5.pearson_residuals>2)) # To count how many 'large' residuals we have

# Here too we have a two observations with huge pearson residuals, again lower than -5 and -20. These are likely the same observations that caused problems for mny2.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # REMOVE OUTLIERS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# By now, we can see that mny2 and mny5 seem to be pretty good models so far, but we want to remove those two observations with huge residuals (assuming we knew enough context to justify this).
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# First, we need to find the observations that have the two large residuals
which(mny5.pearson_residuals<(-5))
# It is entries 11 and 14 of MNY, let's look at those briefly:
MNY[11,] # Observation: Arabelle
MNY[14,] # Observation: Atelier

# We now run everything again, but on the new set
finalMNY <- MNY[-c(11,14), ]


# We create our two new logistic models

mny6 <- glm(InMichelin ~ Price + Decor + Food, data=finalMNY, family=binomial)
mny7 <- glm(InMichelin ~  Price + Food, data=finalMNY, family=binomial)

summary(mny6) # Note AIC: 136.83
summary(mny7) # Note AIC: 135.51


s <- summary(mny7)
capture.output(s, file = "mny7.txt")

# mny7 is performing slightly better than muny6

# Run the same diagnostics as before:

# We take our two candidate models mny6 and mny7 (that are identical to mny2 and mny5, respectively, on finalMNY
# and compare them using our diagnostic tools

# I check the accuracy of predictions of mny6 on our data set: 
predictionsMNY6 <- predict(mny6, type="response", newdata= finalMNY) 
str(predictionsMNY6)
table(finalMNY$InMichelin, predictionsMNY6>0.5)
(77+55)/nrow(finalMNY) 
# Remember that the accuracy using a threshold value of 0.5 for mny2 was not bad (0.804878) ...
# mny6 has accuracy = 0.8148148 which is an improvement over mny2

# I check the accuracy of predictions of mny7 on our data set: 
predictionsMNY7 <- predict(mny7, type="response") 
str(predictionsMNY7)
table(finalMNY$InMichelin, predictionsMNY7>0.5)
(77+55)/nrow(finalMNY) # mny7 has the same accuracy (0.8148148) as mny5


# ROC and threshold values
# a) mny6

# ROC curve - Option 1 (as in class)
roc(finalMNY$InMichelin,predict(mny6),ci=TRUE,ci.alpha=.95,plot=TRUE, auc.polygon=TRUE, max.auc.polygon = TRUE, grid = TRUE, print.auc=TRUE)

# ROC curve - Option 2 (ROCR and caTools packages)
# Prediction function
ROCRpred = prediction(predictionsMNY6, finalMNY$InMichelin)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# From the ROC plot, it looks like 0.6 is a good value to pick if we want to keep false positives pretty low.

# I check the accuracy of the model mny6 model for a threshold level of 0.6:

table(finalMNY$InMichelin, predictionsMNY6>0.6)
(84+46)/nrow(finalMNY)
# I get an accuracy of 0.7926829, which is very close to the accuracy level above (for 0.5) 0.804878
# They are both good (similar) choices, depending on what our goals for the model are.

# b) mny7

# ROC curve - Option 1 (as in class)
roc(finalMNY$InMichelin,predict(mny7),ci=TRUE,ci.alpha=.95,plot=TRUE, auc.polygon=TRUE, max.auc.polygon = TRUE, grid = TRUE, print.auc=TRUE)


# ROC curve - Option 2 (ROCR and caTools packages)
# Prediction function
ROCRpred = prediction(predictionsMNY7, finalMNY$InMichelin)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# From the ROC plot, it looks like 0.6 is a good value to pick if we want to keep false positives pretty low.

# I check the accuracy of the model mny7 model for a threshold level of 0.6:

table(finalMNY$InMichelin, predictionsMNY7>0.6)
(81+48)/nrow(finalMNY)
# I get the same accuracy of 0.7926829, so in this sense, the choice of 0.6 makes no difference for either model.

# mny6

# We should have run diagnostics first, but we'll do them now, an dsee if anything needs to be changed afterwards, based on the results:
mny6.dev_residuals = residuals(mny6, type="deviance") #deviance residuals
mny6.pearson_residuals = residuals(mny6, type="pearson") #pearson residuals

# We check the spread of the deviance residuals:
par(mfrow=c(1,2))
plot(predictionsMNY6,mny6.dev_residuals)
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")

# We check the spread of the Pearson residuals residuals:
plot(predictionsMNY6,mny6.pearson_residuals)
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")


# We still have some pretty large residuals, but they are not as concerning as before.

sum(mny6.dev_residuals^2) 
sum(mny6.pearson_residuals^2) # To count how many 'large' residuals we have

sum(abs(mny6.dev_residuals>2)) # To count how many 'large' residuals we have
sum(abs(mny6.pearson_residuals>2)) # To count how many 'large' residuals we have

# Additionally the peorsan and deviation residuals are more reasonable.

# We do the same thing for mny7

# Residuals:
mny7.dev_residuals = residuals(mny7, type="deviance") #deviance residuals
mny7.pearson_residuals = residuals(mny7, type="pearson") #pearson residuals

# We check the spread of the deviance residuals:
par(mfrow=c(1,2))
plot(predictionsMNY7,mny7.dev_residuals)
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")

# We check the spread of the Pearson residuals residuals:
plot(predictionsMNY7,mny7.pearson_residuals)
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")

sum(mny7.dev_residuals^2) 
sum(mny7.pearson_residuals^2) # To count how many 'large' residuals we have

sum(abs(mny7.dev_residuals>2)) # To count how many 'large' residuals we have
sum(abs(mny7.pearson_residuals>2)) # To count how many 'large' residuals we have

# Similarly to mny6, we still have some outliers, but in general the plots and residuals are better than before.


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # FINAL MODEL SELECTION # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # We take model mny7 with data finalMNY # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Diagnostic plots form the logisticDx library:
install.packages("LogisticDx") 
library(LogisticDx)

#store diagnostics into a variable
mny7.diagnostic = dx(mny7)
plot(mny7.diagnostic$P,mny7.diagnostic$h)
abline(v=avgh)
plot(mny7.diagnostic$h,mny7.diagnostic$sdr)
plot(mny7.diagnostic)

par(mfrow=c(2,2))
avgh=2*mean(mny7.diagnostic$h)
avgh2 = 2*2/sum(is.na(finalMNY$InMichelin)==FALSE)
plot(mny7.diagnostic$h[mny7.diagnostic$h > avgh],mny7.diagnostic$sdr[mny7.diagnostic$h > avgh])
plot(mny7.diagnostic$h[mny7.diagnostic$h > avgh2],mny7.diagnostic$sdr[mny7.diagnostic$h > avgh2])



#
#
#

m.diagnostic = dx(mny7)
plot(m.diagnostic$P,m.diagnostic$h)
plot(m.diagnostic$h,m.diagnostic$sdr)
plot(mny7)

avgh=2*mean(m.diagnostic$h)
avgh2 = 2*2/sum(is.na(finalMNY$InMichelin)==FALSE)
plot(m.diagnostic$h[m.diagnostic$h > avgh],m.diagnostic$sdr[m.diagnostic$h > avgh])
plot(m.diagnostic$h[m.diagnostic$h > avgh2],m.diagnostic$sdr[m.diagnostic$h > avgh2])

