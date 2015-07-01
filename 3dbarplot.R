## Load packages
library(rgl)
library(ggplot2)

#### START OF FUNCTIONS

## Functions modified from the "demo(hist3d)" examples in the rgl package:
# library(rgl)
# demo(hist3d)
## Would it have killed the author to comment their code?

## Draws a single "column" or "stack".
## X and Y coordinates determine the area of the stack
## The Z coordinate determines the height of the stack
stackplot.3d<-function(x,y,z=1,alpha=1,topcol="#078E53",sidecol="#aaaaaa"){
  
  ## These lines allow the active rgl device to be updated with multiple changes
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))
  
  ## Determine the coordinates of each surface of the stack and its edges
  x1<-c(rep(c(x[1],x[2],x[2],x[1]),3),rep(x[1],4),rep(x[2],4))
  z1<-c(rep(0,4),rep(c(0,0,z,z),4))
  y1<-c(y[1],y[1],y[2],y[2],rep(y[1],4),rep(y[2],4),rep(c(y[1],y[2],y[2],y[1]),2))
  x2<-c(rep(c(x[1],x[1],x[2],x[2]),2),rep(c(x[1],x[2],rep(x[1],3),rep(x[2],3)),2))
  z2<-c(rep(c(0,z),4),rep(0,8),rep(z,8) )
  y2<-c(rep(y[1],4),rep(y[2],4),rep(c(rep(y[1],3),rep(y[2],3),y[1],y[2]),2) )
  
  ## These lines create the sides of the stack and its top surface
  rgl.quads(x1,z1,y1,col=rep(sidecol,each=4),alpha=alpha)
  rgl.quads(c(x[1],x[2],x[2],x[1]),rep(z,4),c(y[1],y[1],y[2],y[2]),
            col=rep(topcol,each=4),alpha=1) 
  ## This line adds black edges to the stack
  rgl.lines(x2,z2,y2,col="#000000")
}
# Example:
#stackplot.3d(c(-0.5,0.5),c(4.5,5.5),3,alpha=0.6)

## Calls stackplot.3d repeatedly to create a barplot
# x is a constant distance along x axis
# y is the depth of column
# z is the height of column
barz3d<-function(x,y,z,alpha=1,topcol="#078E53",sidecol="#aaaaaa",scaley=1,scalez=1){
  ## These lines allow the active rgl device to be updated with multiple changes
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))

  ## Plot each of the columns
  n=length(x)
  breaks.x = seq(0,n-1)
  for(i in 1:n){
    stackplot.3d(c(breaks.x[i],breaks.x[i]+1),c(0,-y[i])*scaley,z[i]*scalez,alpha=alpha,topcol=topcol)
  }
  ## Set the viewpoint
  rgl.viewpoint(theta=30,phi=25)
}
# Example
#barz3d(x=LETTERS[1:4],y=c(0.8,0.2,0.9,0.15),z=c(0.11,0.75,0.89,0.16),alpha=0.4,scaley=2,scalez=2)

#### END OF FUNCTIONS

## Example data:
# 4 mutations in 100 samples
# VAF range is from 0 to 1
# A is frequent and usually subclonal
# B is infrequent and usually clonal
# C is frequent and usually clonal
# D is infrequent and usually subclonal

Avaf=rnorm(80,0.1,0.05)
Bvaf=rnorm(20,0.8,0.1)
Cvaf=rnorm(90,0.9,0.05)
Dvaf=rnorm(15,0.15,0.05)

## Summarize data in new object
vafsum=data.frame(median=sapply(list(Avaf,Bvaf,Cvaf,Dvaf),median),
                  proportion=sapply(list(Avaf,Bvaf,Cvaf,Dvaf),function(x){length(x)/100}))
rownames(vafsum)=c(LETTERS[1:4])

## Code to produce coxcomb/polar coordinate plot adapted from:
## http://robinlovelace.net/r/2013/12/27/coxcomb-plots-spiecharts-R.html
## https://github.com/Robinlovelace/lilacPlot
pos = 0.5 * (cumsum(vafsum$proportion) + cumsum(c(0, vafsum$proportion[-length(vafsum$proportion)])))
p = ggplot(vafsum, aes(x=pos)) + geom_bar(aes(y=median), width=vafsum$proportion, color = "black", stat = "identity") + scale_x_continuous(labels = rownames(vafsum), breaks = pos) # Linear version is ok
p + coord_polar(theta = "x")
# (ignore warnings thrown)

## A traditional boxplot
boxplot(Avaf,Bvaf,Cvaf,Dvaf,names=LETTERS[1:4])

## A barplot where height represents median VAF and the color of the bar represents
## how many samples contain each mutation
barplot(vafsum$median,names=LETTERS[1:4],col=rgb(0.1,0.1,0.1,vafsum$proportion))

## Our new 3D barplot function
barz3d(x=LETTERS[1:4],y=vafsum$proportion,z=vafsum$median,alpha=0.4,scaley=2,scalez=2)
rgl.snapshot("3dbarplot.png", fmt = "png", top = TRUE )
