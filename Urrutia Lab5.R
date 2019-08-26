# Adapted from the script "Lab 4 Demo.R" uploaded to the Lab 5 zip file:

setwd("~/src/JPG1400/Lab+5+2018")

library(sp)
library(spdep)
library(classInt)
library(RColorBrewer)
library(maptools)

# Loading file:

columbus <- readShapeSpatial("columbus.shp")

# Bivariate plots

X<-as.data.frame(cbind(columbus$CRIME,columbus$INC, columbus$HOVAL))
colnames(X) <- c( 'CRIME', 'INC', 'HOVAL')
cor(X)
summary(X)
plot(X)

# Creating Boxplots and histograms of variables:

par(mfrow=c(2,2))

boxplot(X, main = "Boxplots of variables")

densCRIME <- density(columbus$CRIME)
hist(columbus$CRIME, breaks=40, probability=TRUE, main = "Histogram of CRIME")
lines(densCRIME)
rug(columbus$CRIME)

densINC <- density(columbus$INC)
hist(columbus$INC, breaks=40, probability=TRUE, main = "Histogram of INC")
lines(densINC)
rug(columbus$INC)


densHOVAL <- density(columbus$HOVAL)
hist(columbus$HOVAL, breaks=40, probability=TRUE, main = "Histogram of HOVAL")
lines(densHOVAL)
rug(columbus$HOVAL)

dev.off()


# Cloropleth maps of the variables


nclr<-5
plotvar <- columbus$CRIME
class <- classIntervals(plotvar, nclr, style = "quantile",dataPrecision = 2)
plotclr <- brewer.pal(nclr, "Reds")
colcode <- findColours(class, plotclr, digits = 3)
plot(columbus, col = colcode, pch = 19, axes = T,cex=1.5)
title(main = "Thefts Per 1000 Households")
legend("topleft", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8)

nclr<-5
plotvar <- columbus$INC
class <- classIntervals(plotvar, nclr, style = "quantile",dataPrecision = 2)
plotclr <- brewer.pal(nclr, "Blues")
colcode <- findColours(class, plotclr, digits = 3)
plot(columbus, col = colcode, pch = 19, axes = T,cex=1.5)
title(main = "Median household income (in $1000's)")
legend("topleft", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8)

nclr<-5
plotvar <- columbus$HOVAL
class <- classIntervals(plotvar, nclr, style = "quantile",dataPrecision = 2)
plotclr <- brewer.pal(nclr, "Greens")
colcode <- findColours(class, plotclr, digits = 3)
plot(columbus, col = colcode, pch = 19, axes = T,cex=1.5)
title(main = "Median housing value (in $1000's)")
legend("topleft", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8)

# Creating a neighbourhood file with queen connectivity (with the poly2nb function)

columbus_queen_nb<-poly2nb(columbus, queen=TRUE)
summary(columbus_queen_nb)

# Creating listw object with columbus_queen_nb

colqueen<-nb2listw(columbus_queen_nb, style="W")
summary(colqueen)

# Calculating Moran's I for the three variables:

#Calculate Moran's I

moranCRIME <- moran.test(columbus$CRIME, colqueen)
moranINC <- moran.test(columbus$INC, colqueen)
moranHOVAL <- moran.test(columbus$HOVAL, colqueen)
moranCRIME
moranINC
moranHOVAL

# Creating an OLS regression model
olsres<-lm(columbus$CRIME~columbus$INC+columbus$HOVAL)
summary(olsres)

sum1 <- summary(olsres)
capture.output(sum1, file = "olsres.txt")
moran.test(resid(olsres), colqueen)

# Creating a second model - OLS regression with a spatial lag
wy <- lag(colqueen, columbus$CRIME)
olsres2<-lm(columbus$CRIME~wy+columbus$INC+columbus$HOVAL)
moran.test(resid(olsres2), colqueen)
summary(olsres2)

# Creating a third model - ML regression with a spatial lag
slres<-lagsarlm(columbus$CRIME~columbus$INC+columbus$HOVAL, listw=colqueen)
moran.test(resid(slres), colqueen)
summary(slres)

# (Out of curiosity, I checked the Moran's I for the residuals and made a cloropleth of the residuals 
# to see if there were any evident patterns, as this would also show what is not accounted for in the 
# models, particularly for the first model!)

par(mfrow=c(1,3))
nclr<-5
plotvar <- resid(olsres)
class <- classIntervals(plotvar, nclr, style = "quantile",dataPrecision = 2)
plotclr <- brewer.pal(nclr, "Reds")
colcode <- findColours(class, plotclr, digits = 3)
plot(columbus, col = colcode, pch = 19, axes = T,cex=1.5)
title(main = "Map of residuals for the first model")
legend("topleft", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8)

nclr<-5
plotvar <- resid(olsres2)
class <- classIntervals(plotvar, nclr, style = "quantile",dataPrecision = 2)
plotclr <- brewer.pal(nclr, "Reds")
colcode <- findColours(class, plotclr, digits = 3)
plot(columbus, col = colcode, pch = 19, axes = T,cex=1.5)
title(main = "Map of residuals for the second model")
legend("topleft", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8)

nclr<-5
plotvar <- resid(slres)
class <- classIntervals(plotvar, nclr, style = "quantile",dataPrecision = 2)
plotclr <- brewer.pal(nclr, "Reds")
colcode <- findColours(class, plotclr, digits = 3)
plot(columbus, col = colcode, pch = 19, axes = T,cex=1.5)
title(main = "Map of residuals for the third model")
legend("topleft", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8)


# Task 5 (question 4) - Creating a spatially lagged dependent model using the function errorsarlm() 

errorres<-errorsarlm(columbus$CRIME~columbus$INC+columbus$HOVAL, listw=colqueen)
moran.test(resid(errorres), colqueen)
LMerror <- summary(errorres)
capture.output(LMerror, file = "LMerror.txt")


# Computing Lagrange Multipler tests for use with the flowchart to pick between model types

res<-lm.LMtests(olsres, listw=colqueen, test="all")
tres<-t(sapply(res, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tres)<-c("Statistic", "df", "p-value")
printCoefmat(tres)

LM <- printCoefmat(tres)

capture.output(LM, file = "LM.txt")


