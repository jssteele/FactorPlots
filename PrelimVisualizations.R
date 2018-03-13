# preliminary visualizations for factor based parceling
# Joel Steele Mar 3 2018
library(rgl)

# Example data from
# Raykov and Marcoulides Chapter 8 of Intro to Multivariate Analysis
inpcor = structure(
  c(1, 0.521190565099232, 0.471889619653499, -0.000799684818387502,
    -0.0256440460173457, 0.0175922532701324, 0.521190565099232, 1,
    0.53392331880074, -0.034501693182966, -0.0364910768210528, -0.0158892058798154,
    0.471889619653499, 0.53392331880074, 1, -0.0100671038254988,
    -0.0400203899591411, 0.0390242775019308, -0.000799684818387502,
    -0.034501693182966, -0.0100671038254988, 1, 0.42972033498741,
    0.36877142077544, -0.0256440460173457, -0.0364910768210528, -0.0400203899591411,
    0.42972033498741, 1, 0.42678031712179, 0.0175922532701324, -0.0158892058798154,
    0.0390242775019308, 0.36877142077544, 0.42678031712179, 1),
  .Dim = c(6L, 6L),
  .Dimnames = list(c("extravsn", "neurotic", "agreeabl",
                     "algebra", "geomet", "trigono"),
                   c("extravsn", "neurotic", "agreeabl",
                     "algebra", "geomet", "trigono")))

# function to normalize vectors to length of 1
norm2one = function(vec){
  nv = vec
  #normalize the vector to one
  nv = nv/sqrt(sum(nv^2))
  return(nv)
}

# orthogonal basis identification (component extraction), but bear with me
# using a correlation matrix
esol2 = eigen(inpcor) # eigenvalue decomposition
# we retain 3 factors to allow for 3d visualization
fpat = esol2$vectors[,1:3] %*% diag(sqrt(esol2$values[1:3])) # factor pattern

# set up the plotting parameters
op = par3d(family='sans')
# unit sphere 30% opacity via alpha
spheres3d(0,0,0,radius = 1,col='light blue',alpha=.3)
axes3d(c("x--","y--","z-+"),cex=0.8)

# how many indicators
nvars = nrow(fpat)
# how many factors
nfact = ncol(fpat)
# to this point the factor pattern entries are like points in space
# below these points are turned into vectors anchored at zero
for(i in 1:nvars){ # for this indicator
  # normalize vector to length of one
  nv = rbind(0,norm2one(fpat[i,]))
  # draw the vectors
  segments3d(nv,lwd=1,col='blue') 
  # points at the end of vectors
  points3d(xyz.coords(t(nv[-1,])),pch=19,cex=1.5) 
  # vector label
  text3d(xyz.coords(t(nv[-1,])), text=rownames(inpcor)[i],adj=c(.5,.7))
}


# plot the parcels using polygon3d
# for polygons the coordinates must loop around
# These represent the columns of the pattern matrix that comprise a parcel
parcel1 = c(1:3,1) # c(1,2,3,1)
parcel2 = c(4:6,4) # c(4,5,6,4)

# plot the polygons using parcel vectors to select which columns of the factor
# pattern matrix represent parcels and will therefore get a shape.
# Note if there are more than three indicators for a parcel the above vectors
# will need to change.
nrmpat = t(apply(fpat,1,norm2one))
polygon3d(xyz.coords(nrmpat[parcel1,]),col='red')
polygon3d(xyz.coords(nrmpat[parcel2,]),col='purple')

# reset plotting parameters
par3d(op)
