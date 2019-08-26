install.packages("car")
library(car)
library(alr3)

traveltimes <- read.csv("traveltimes.csv", header=TRUE)

str(traveltimes)
summary(traveltimes)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # TRANSFORMATION FINAL CHOICE # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

traveltimes$log.mtt <- log(traveltimes$mtt)
traveltimes$log.Area <- log(traveltimes$Area)
traveltimes$log.O5 <- log(traveltimes$O5)

model <- lm(log.mtt ~ log.Area + log.O5 + D5, data=traveltimes)
summary(model)

s <- summary(model)
capture.output(s, file = "model.txt")
# 0.6434

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # CHECKING RELATIONSHIPS BETWEEN VARIABLES# # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

data6 <-traveltimes[c("log.Area","log.O5","D5")]
c <- cor(traveltimes[c("log.Area","log.O5","D5")])
plot(data6)

capture.output(c, file = "cor.txt")

par(mfrow=c(2,2))
hist(traveltimes$log.mtt) # Just out of curiosity
hist(traveltimes$log.Area)
hist(traveltimes$log.O5)
hist(traveltimes$D5)
dev.off()

# The histograms look pretty good, but check for normality w/shapiro.test
shapiro.test(traveltimes$log.mtt) # good!
shapiro.test(traveltimes$log.Area) # good!
shapiro.test(traveltimes$log.O5) # good!
shapiro.test(traveltimes$D5) # Not very normal (reject hypothesis), but this is ok for lm

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Diagnostic plots: # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

par(mfrow=c(2,2))
plot(model)

# We check the relationship of the standardized resiuals to X
model.stdres <- MASS::stdres(model)
plot(traveltimes$log.mtt,model.stdres, xlab="mtt (X)", ylab = "Standardized residuals (m1.stdres)", main = "(X) - Standardized Residuals")
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")
dev.off()
cor(traveltimes$mtt,model.stdres)

# And also of the residuals to the fitted values for Y
plot(fitted(model), resid(model))
dev.off()
# The spread looks pretty good, but a bit more sparse for larger values on the X axis (fitted)

# We see that there are no large residuals & no large standardized residuals
# We check the h_(ii) values to make sure no outliers need to be dealt with 
# Not that from the plot (above) it doesn't seem like any outliers are too problematic!

# Chequing for outliers (we can see)
hat.values<- hatvalues(model)
h.threshold <- 2*mean(hat.values)
bad.outliers <- (abs(hat.values)>h.threshold)
sum(bad.outliers)
high.resid <- (abs(model$residuals) > 2)
sum(high.resid)
bad.outliers <- ifelse(bad.outliers == FALSE, FALSE,
                       ifelse(high.resid == FALSE, FALSE, TRUE))
sum(bad.outliers) # We have NO bad outliers!
mean(traveltimes$mtt)


# Plot r_i versus each X variable
par(mfrow=c(2,2))
plot(traveltimes$log.Area,model.stdres, ylab="Standardized Residuals")
plot(traveltimes$log.O5,model.stdres, ylab="Standardized Residuals")
plot(traveltimes$D5,model.stdres, ylab="Standardized Residuals")
dev.off()
# The residuals seem to be pretty evenly spead. For Area and D5, we see some very high values of x, but they have residuals that are close to zero.

# A picture of the linear model to get an idea
par(mfrow=c(1,1))
plot(model$fitted.values,traveltimes$log.mtt,xlab="Fitted Values", ylab="mtt (Y)")
abline(lsfit(model$fitted.values,traveltimes$log.mt))
dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Check Added Variable Plots and Variable Inflation Foctors # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

par(mfrow=c(2,2))
avPlot(model,variable="log.Area",ask=FALSE)
avPlot(model,variable="log.O5",ask=FALSE)
avPlot(model,variable="D5",ask=FALSE)

vif <- vif(model)
capture.output(vif, file = "vif.txt")

# All three are <5, good!

# Finally, since although cor(log.O5,D5) is still high after choosing it to minimize 
# cor(O#,D#), we note that since log.05 and D5 coefficients that may cancel each other 
# out somewhat, I just want to check that if they were each added separately to the 
# model (without the other), that the resulting model would still give both variables
# coefficients of the same sign than model.

cor(log.O5,D5)
modeltest1 <- lm(log(mtt) ~ log.Area + D5, data=traveltimes)
modeltest2 <- lm(log(mtt) ~ log.Area + log.O5, data=traveltimes)
model$coefficients
modeltest1$coefficients
modeltest2$coefficients

# We note that even without log.O5, D5 has a negative coefficient in modeltest1, and
# note that even without D5, log.O5 has a positive coefficient in modeltest2. 
# This is what we wanted!


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# m1 <- lm(Defective ~ Temperature+Density+Rate)
# model <- lm(log.mtt ~ log.Area + log.O5 + D5, data=traveltimes)

loessfit1 <- loess(log.mtt ~ log.Area,degree=1,span=2/3)
loessfit2 <- loess(model$fitted.values ~ log.Area,degree=1,span=2/3)
xx <- seq(min(log.Area),max(log.Area),length=100)
par(mfrow=c(1,2))
plot(log.Area,log.mtt,xlab="log.Area, x1", ylab="log(mtt), Y")
lines(xx,predict(loessfit1,data.frame(log.Area=xx)))
plot(log.Area,model$fitted.values,ylab=expression(hat(Y)),xlab="log.Area, x1")
lines(xx,predict(loessfit2,data.frame(log.Area=xx)))

par(mfrow=c(2,2))
mmp(model,log.Area)
mmp(model,log.O5)
mmp(model,D5)
mmp(model,model$fitted.values,xlab="Fitted Values")




# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - # - # ROUGH WORK: - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # Are any of the independent variables too closely related? # # # # # # # # # # #

cor(traveltimes[c("Area","Workers.Density","MeanO","HighwayDensity","FD","O5","O50")])
cor(traveltimes[c("Area","Workers.Density","MeanO","VarianceD","VarianceO","HighwayDensity","FD","O5","O50")])
cor(traveltimes[c("O5","O25","O50","O75","O95","D5","D25","D50","D75","D95")])
cor(traveltimes[c("O5","O25","O50","O75","O95","D5","D25","D50","D75","D95")])
cor(traveltimes[c("O25","O50","D25","D50")])

data2 <-traveltimes[c("log.Area","O5","O25","O50","O75","O95")]
cor(traveltimes[c("log.Area","O5","O25","O50","O75","O95")])
plot(data2)

data3 <-traveltimes[c("log.Area","O5","O25","O50","O75","O95")]
cor(traveltimes[c("log.Area","D5","D25","D50","D75","D95")])
plot(data3)

data4 <-traveltimes[c("log.Area","O25","D5")]
cor(traveltimes[c("log.Area","O5","D5")])
plot(data4)

data5 <-traveltimes[c("log.Area","O5","D5")]
cor(traveltimes[c("log.Area","O5","D5")])
plot(data5)

data6 <-traveltimes[c("log.Area","log.O5","D5")]
cor(traveltimes[c("log.Area","log.O5","D5")])
plot(data6)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # (Some) DIAGNOSTICS and fitting, PRE-TRANSFORMATION# # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

model1 <- lm(mtt~Area + Workers.Density + MeanO + HighwayDensity + FD + O5 + O50, data=traveltimes )
model2 <- lm(mtt~Area + Workers.Density + MeanO + HighwayDensity + FD + D5 + D25+ D50+D75+ D95, data=traveltimes )
model3 <- lm(mtt~Area + FD , data=traveltimes )

model4 <- lm(mtt~Area + O25 + CT, data=traveltimes )
# Adjusted R-squared:  0.5179

model5 <- lm(mtt~Area + O25 + CT + MeanO, data=traveltimes )
# Adjusted R-squared:  0.5148

model6 <- lm(mtt~Area + O25 + CT + VarianceO, data=traveltimes )
cor(traveltimes[c("Area" , "O25", "CT", "VarianceO")])
# Adjusted R-squared:  0.5449 

model7 <- lm(mtt~Area + O25 + CT + VarianceO + D5, data=traveltimes )
# Adjusted R-squared:  0.5902
cor(traveltimes[c( "O25", "D5")])

model8 <- lm(mtt~Area + O25 + CT + D25, data=traveltimes )
# Adjusted R-squared:  0.6506

cor(traveltimes[c("Area" , "O25", "CT", "D25")])

model <- lm(mtt ~ Area + O25 + D5 + FD, data=traveltimes)
# Adjusted R-squared:  0.5723 
cor(traveltimes[c("Area","O25","D5","FD")])
summary(model)

# We check the relationship of the standardized resiuals to X
model.stdres <- MASS::stdres(model)
plot(traveltimes$mtt,model.stdres, xlab="mtt (X)", ylab = "Standardized residuals (m1.stdres)", main = "(X) - Standardized Residuals")
abline(h=2, lwd=1, col="blue",lty= "dashed")
abline(h=-2, lwd=1, col = "blue",lty= "dashed")
abline(h=4, lwd=1, col = "red",lty= "dashed")
dev.off()
cor(traveltimes$mtt,model.stdres)

# And also of the residuals to the fitted values for Y
plot(fitted(model), resid(model))
dev.off()
# The spread looks pretty good, but a bit more sparse for larger values on the X axis (fitted)

# We see that there are no large residuals & no large standardized residuals
# We check the h_(ii) values to make sure no outliers need to be dealt with 
# Not that from the plot (above) it doesn't seem like any outliers are too problematic!

# Chequing for outliers (we can see)
hat.values<- hatvalues(model)
h.threshold <- 4/42
bad.outliers <- (abs(hat.values)>h.threshold)
high.resid <- (abs(model$residuals) > 2)
sum(high.resid)
bad.outliers <- ifelse(bad.outliers == FALSE, FALSE,
                       ifelse(high.resid == FALSE, FALSE,
                              TRUE))
sum(bad.outliers) # We have one potential bad outlier, entry 19
traveltimes$City[19] #Philadelphia
mean(traveltimes$mtt)
traveltimes$mtt[19]
# Unfortunately we don't have the context to see if we should exclude this outlier or not, but at least we learned a bit about the data set!

model.data <- traveltimes[c("mtt","Area","O25","D5","FD")]
summary(model.data)
str(model.data)
plot(model.data, which=1)

# Plot r_i versus each X variable
par(mfrow=c(2,2))
plot(model.data$Area,model.stdres, ylab="Standardized Residuals")
plot(model.data$O25,model.stdres, ylab="Standardized Residuals")
plot(model.data$D5,model.stdres, ylab="Standardized Residuals")
plot(model.data$FD,model.stdres, ylab="Standardized Residuals")
dev.off()
# The residuals seem to be pretty evenly spead. For Area and D5, we see some very high values of x, but they have residuals that are close to zero.

# A picture of the linear model to get an idea
par(mfrow=c(1,1))
plot(model$fitted.values,model.data$mtt,xlab="Fitted Values", ylab="mtt (Y)")
abline(lsfit(model$fitted.values,model.data$mtt))
dev.off()


lm(formula = mtt^(-0.0554) ~ I(log(Area)) + I(O25^0.0548) + 
     I(D5b^0.2572), data = traveltimes)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # TRANSFORMATION# # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 

# We get an error when we try to transform D5 because of the zero entries. 
traveltimes$D5b <- ifelse(traveltimes$D5==0,0.00001,D5) 
summary(traveltimes)
# We fix by adding a minimal amount to D5 into column D5b

summary(powerTransform(cbind(mtt,Area,O25,D5b,FD)~1, data=traveltimes))
transf.model <- lm(mtt^(-0.115)~I(Area^-0.074)+I(O25^0.043)+I(D5b^0.2538)+I(FD^8.0676),data=traveltimes)
summary(transf.model)
# R^2 of 0.6127, which is an improvement over our first model

summary(powerTransform(cbind(mtt,Area,O25,D5b)~1, data=traveltimes))
transf.model2 <- lm(mtt^(-0.0554)~I(Area^-0.0064)+I(O25^0.0548)+I(D5b^0.2572),data=traveltimes)
summary(transf.model2)
# Gives us an R^2 of 0.624 (0.6515) (Note that this is better than the R^2 of trans.model1)
par(mfrow=c(2,2))
plot(transf.model2)
plot(box.cox(income, .26), box.cox(education, .42))

# Note improvements in the Scale-Location and Residuals vs Fitted Plots


# To get an idea of diagnostics for this new model:
tmtt <- mtt^(-0.0554)
par(mfrow=c(1,1))
plot(transf.model2$fitted.values,tmtt,xlab="Fitted Values (transformed)", ylab="mtt (transformed) (Y)")
abline(lsfit(transf.model2$fitted.values,tmtt))
dev.off()


tmodel.stdres <- MASS::stdres(transf.model2)
plot(tmtt,tmodel.stdres, xlab="mtt (X)", ylab = "Standardized residuals (m1.stdres)", main = "(X) - Standardized Residuals")


par(mfrow=c(2,3))
hist(traveltimes$D5b)
hist(traveltimes$Area)
hist(traveltimes$O25)
hist(traveltimes$D5b^0.2572)
hist(traveltimes$Area^-0.0064)
hist(traveltimes$O25^0.0548)


# # # # # # # # # Looking at the histograms of the x variables, it looks like O25 was more normal before transforming it

summary(powerTransform(cbind(mtt,Area,O25,D5b)~1, data=traveltimes))
transf.model3 <- lm(mtt^(-0.0554)~I(Area^-0.0064)+O25+I(D5b^0.2572),data=traveltimes)
summary(transf.model3)
# Gives us an R^2 of 0.624 (Note that this is better than the R^2 of trans.model1)
par(mfrow=c(2,2))
plot(transf.model3)
# 0.6406
# But the intercept is no longer meaningful

hist(mtt)

# # # # # # # # # Looking at the histograms of the x variables, it looks like O25 was more normal before transforming it

summary(powerTransform(cbind(mtt,Area,O25,D5b)~1, data=traveltimes))
transf.model4 <- lm(mtt^(-0.0554)~I(Area^-0.0064),data=traveltimes)
summary(transf.model4)
# Gives us an R^2 of 0.624 (Note that this is better than the R^2 of trans.model1)
par(mfrow=c(2,2))
plot(transf.model3)
# 0.6406
# But the intercept is no longer meaningful

hist(mtt)


# These two plots of fitted and actual Y versus temperature can be combined into one
par(mfrow=c(1,1))
mmp(model,Area)

transf.model5 <- lm(log(mtt) ~ log(Area) + O25 + D5, data=traveltimes)
summary(transf.model5)
# 0.6024
par(mfrow=c(2,2))
plot(model)

transf.model6 <- lm(log(mtt) ~ log(Area) + O25 + D5, data=traveltimes)
summary(model6)
par(mfrow=c(2,2))
plot(model)

par(mfrow=c(1,2))
hist(traveltimes$Area)
hist(I(log(traveltimes$Area)))

par(mfrow=c(2,2))
hist(mtt,breaks=11)
hist(I(mtt^(-0.0554)),breaks=11)
hist(I(log(mtt)),breaks=11)
dev.off()

lm(formula = mtt^(-0.0554) ~ I(log(Area)) + I(O25^0.0548) + 
     I(D5b^0.2572), data = traveltimes)


