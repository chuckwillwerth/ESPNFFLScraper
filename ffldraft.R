install.packages("png")
library(png)
setwd("c:/temp")
img = readPNG("mcgangbang.png")
r = as.raster(img[,,1:3])
r[img[,,2] == 0] = "white"
ffl = read.csv("fflr.csv")
#ffl$Price = as.numeric(substring(as.character(ffl$Price),2)) 
lmdl = lm(Pts ~ Pos + Price, data=ffl)
ffl$Predicted = predict(lmdl)
mycolors = rainbow(12)
mycolors[3]="maroon1"
plot(0:36,ylim=c(0,36),xlim=c(3,25),type="n", xlab="Predicted points based on linear regression on Position and Auction Price",
     ylab="Actual Points", main="Chuck Took A Data Analytics MOOC And Has A Problem")
rasterImage(r,3,0,25,36)
points(ffl$Pts ~ ffl$Predicted, pch=as.numeric(ffl$TEAM), 
       col=mycolors[c(ffl$TEAM)], xlab="Predicted points based on Position and Auction Price", ylab="Actual Points", main="Chuck Took An Online Data Analytics Course")
segments(0,0,35,35)
legend(3.1,34.5,levels(ffl$TEAM), 
       pch=seq_along(levels(factor(ffl$TEAM))),
       text.col=mycolors[seq_along(levels(factor(ffl$TEAM)))],
       col=mycolors[seq_along(levels(factor(ffl$TEAM)))], 
       cex=.7,
       bg="transparent",
       text.font=2)
text(ffl$Predicted+.2, ffl$Pts, ffl$Name, cex=.7, adj=c(0,0), col=mycolors[c(ffl$TEAM)], font=2)
text(24.9,0.1,"BAD",adj=c(1,0),cex=1,col="red", font=2)
text(3.1,35.9,"GOOD",adj=c(0,1),cex=1,col="green3", font=2)
