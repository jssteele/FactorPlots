df <- data.frame(x=c(1,3,-3,-2), y=c(2,5,2,1),z=c(1,7,4,1))

library(rgl)
open3d()
plot3d(df,col=3,type="p", radius=0.5)
# type s is for sphere!
plot3d(df,col=rgb(1,0,0.3),alpha=0.5, add=T,type="s",radius=1)

open3d()
x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x, y)
plot3d(x, y, z, col = rainbow(1000))