setwd("~/src/JPG1400/Lab 4 Demo")

# First we add the packages used in our lab demo:
install.packages(c("pgirmess", "rgdal", "spdep", "maptools", "classInt", "RColorBrewer"))

require(maptools)
require(RColorBrewer)
require(classInt)
require(spdep)
require(rgdal)
require(pgirmess)

# 1: Loading and Plotting

SLCCT <- readOGR(dsn = '.', layer = "SLC_CT2010") #directory and then name of shapefile w/o extension (.shp)

?classIntervals
?brewer.pal
brewer.pal.info

nclr <- 5 #number of colors
plotvar <- SLCCT$HIS_PCT #variable of interest
class <- classIntervals(plotvar, nclr, style = "jenks",dataPrecision = 2)
plotclr <- brewer.pal(nclr, "Blues") 
colcode <- findColours(class, plotclr, digits = 3) 
plot(SLCCT, col = colcode, border = "black", axes = T) 
title(main = "Proportions of Hispanic Population per per census track in Salt Lake City") #add title
legend("bottomleft", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.7) #add legend
# This map looks good with dimensions 5-5.5 in by 8-8.2 in

summ <- summary(SLCCT$HIS_PCT)
capture.output(summ, file = "summary.txt")

hist(SLCCT$HIS_PCT)

install.packages(ggplot2)
library(ggplot2)

hist(SLCCT$HIS_PCT)

# 2: Neighbour Lists

?poly2nb

#Rook/queen differ in the dneigh variable

  coords <- coordinates(SLCCT)
  SLCnb_queen <- poly2nb(SLCCT, queen = TRUE)
  SLCnb_rook<-poly2nb(SLCCT, queen=FALSE)
  dneigh <- diffnb(SLCnb_rook, SLCnb_queen)
  plot(SLCCT, border="grey")
  plot(SLCnb_queen, coords, add=TRUE)
  plot(dneigh, coords, add=TRUE, col="red")
  title(main=paste("Differences (red) in queen adjacencies (black)",
                   "and rook adjacencies", sep="\n"))
# This map looks good with dimensions 6in by 6in

  
# 3: Creating Weight Matrices from Neighbor Lists
  
?nb2listw
  
SLCnb_rook[1]
SLCnb_queen[1]
  
SLCnb_rook_W<-nb2listw(SLCnb_rook)
SLCnb_queen_W<-nb2listw(SLCnb_queen)
SLCnb_rook_B<-nb2listw(SLCnb_rook, style = "B")
SLCnb_queen_B<-nb2listw(SLCnb_queen, style = "B")

SLCnb_rook_W$weights[1]
SLCnb_queen_W$weights[1]
SLCnb_rook_B$weights[1]
SLCnb_queen_B$weights[1]

# 4: Global Moran's I

moran_rookW<-moran.test(SLCCT$HIS_PCT,listw=SLCnb_rook_W)
moran_queenW<-moran.test(SLCCT$HIS_PCT,listw=SLCnb_queen_W)
moran_rookB<-moran.test(SLCCT$HIS_PCT,listw=SLCnb_rook_B)
moran_queenB<-moran.test(SLCCT$HIS_PCT,listw=SLCnb_queen_B)

moran_rookW
moran_queenW
moran_rookB
moran_queenB

cor_his <-correlog(coordinates(SLCCT),SLCCT$HIS_PCT,method="Moran") 
plot(cor_his)

# 5: Moran Scatterplot

par(mfrow=c(2,1))
lmpPop<-moran.plot(SLCCT$Pop2010, listw=SLCnb_queen_W)
lmpHis<-moran.plot(SLCCT$HIS_PCT, listw=SLCnb_queen_W)
dev.off()
# lmpPop
# lmpHis

# We check the Moran's I statistics for Pop2010 and for HIS_PCT
moran_Pop<-moran.test(SLCCT$Pop2010,listw=SLCnb_queen_W)
moran_His<-moran.test(SLCCT$HIS_PCT,listw=SLCnb_queen_W)

moran_Pop
moran_His


# 6: Local Moran

lm_his <- localmoran(SLCCT$HIS_PCT, listw=SLCnb_queen_W) #queen connectivity matrix
infl <- lm_his[,5]<0.05
x <- SLCCT$HIS_PCT
lhx <- cut(x, breaks=c(min(x), mean(x), max(x)), labels=c("L","H"), include.lowest=TRUE)
wx <- lag(SLCnb_queen_W, SLCCT$HIS_PCT)
lhwx <- cut(wx, breaks=c(min(wx), mean(wx), max(wx)), labels=c("L", "H"), include.lowest=TRUE)
lhlh <- interaction(lhx, lhwx, infl, drop=TRUE)
cols <- rep(1, length(lhlh))
cols[lhlh == "L.L.TRUE"] <- 2
cols[lhlh == "H.L.TRUE"] <- 3
cols[lhlh == "L.H.TRUE"] <- 4
cols[lhlh == "H.H.TRUE"] <- 5
plot(SLCCT, col=brewer.pal(5, "Accent")[cols], border = "black", axes = T)
legend("topright", legend=c("None", "LL", "HL", "LH", "HH"), fill=brewer.pal(5, "Accent"), bty="n", cex=0.8, y.intersp=0.8)


lg_his <- localG(SLCCT$HIS_PCT, listw=nb2listw(include.self(SLCnb_queen), style="B")) # queen binary matrix
lhg <- cut(lg_his, breaks=c(min(lg_his), -1.96, 1.96, max(lg_his)), labels=c("Cold","Insig.", "Hot"), include.lowest=TRUE)
cols <- rep(1, length(lhlh))
cols[lhg == "Cold"] <- 2
cols[lhg == "Hot"] <- 3
plot(SLCCT, col=brewer.pal(3, "Accent")[cols], border = "black", axes = T)
legend("topright", legend=c("Insignificant", "Cold", "Hot"), fill=brewer.pal(3, "Accent"), bty="n", cex=0.8, y.intersp=0.8)
title(main = "Census Tract Population Count Hotspots")

# Bonferroni adjustment
alpha <- 0.05/211
new_alpha <- 1 - alpha
sd(new_alpha)
zcrit <- qnorm(new_alpha,mean=0,sd=1)
# the new critical value is 3.495072 standard deviations away from the mean

# Bonferroni adjusted G stat:
lg_his <- localG(SLCCT$HIS_PCT, listw=nb2listw(include.self(SLCnb_queen), style="B")) # queen binary matrix
lhg <- cut(lg_his, breaks=c(min(lg_his), -3.495072, 3.495072, max(lg_his)), labels=c("Cold","Insig.", "Hot"), include.lowest=TRUE)
cols <- rep(1, length(lhlh))
cols[lhg == "Cold"] <- 2
cols[lhg == "Hot"] <- 3
plot(SLCCT, col=brewer.pal(3, "Accent")[cols], border = "black", axes = T)
legend("topright", legend=c("Insignificant", "Cold", "Hot"), fill=brewer.pal(3, "Accent"), bty="n", cex=0.8, y.intersp=0.8)
title(main = "Census Tract Population Count Hotspots, \n Bonferroni adjusted")

# 7. EXTRA:
# Out of curiosity: Histograms of distributions of LMI's:
# For variable Pop2010

par(mfrow=c(1,2))
lmPop <- localmoran(SLCCT$Pop2010, listw=SLCnb_queen_W)
lmPop
printCoefmat(data.frame(lmPop[oid,], row.names=afcon$name[oid]),
             check.names=FALSE)
hist(lmPop[,5])
mean(lmPop[,1])
sum(lmPop[,1])/Szero(SLCnb_queen_W)

# For variable HIS_PCT

lmHis <- localmoran(SLCCT$HIS_PCT, listw=SLCnb_queen_W)
lmHis
printCoefmat(data.frame(lmHis[oid,], row.names=afcon$name[oid]),
             check.names=FALSE)
hist(lmHis[,5])
mean(lmHis[,1])
sum(lmHis[,1])/Szero(SLCnb_queen_W)
dev.off()


