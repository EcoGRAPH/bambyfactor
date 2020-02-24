rm(list=ls())

set.seed(1333)

library(ggplot2)
library(mgcv)
library(dplyr)

# everything until the !!!!!! is just creation of a random data set with distributed lags that depend on some factor variable
# set up parameters for a model
nfactorlevels <- 3    # how many factor levels?
nobsperlevel  <- 500  # how many observations per factor level?
envlaglen     <- 100  # how many columns in the lagged environmental data?
sigmaerr      <- 1    # what's the variance on the artificial data?

# create a data frame with artificial environmental data
mydf <- expand.grid(faclev = 1:nfactorlevels,
                    nobs   = 1:nobsperlevel)
mydf$envcov <- matrix(rnorm(n=nrow(mydf)*envlaglen,
                            mean=0,
                            sd=1),
                      nrow=nrow(mydf),
                      ncol=envlaglen)

# figure out which lags correspond to which factor level (randomly)
faclevs <- data.frame(faclev = unique(mydf$faclev))
faclevs$dlstart  <- floor(runif(n=nrow(faclevs)) * envlaglen/2)
faclevs$dlstop   <- floor(runif(n=nrow(faclevs)) * envlaglen/2) + envlaglen/2
faclevs$dlcoef   <- rnorm(n=nrow(faclevs))
mydf <- left_join(mydf, faclevs, by="faclev")

faclevs

# create the outcome observations
mydf$Y <- 0
for (currow in 1:nrow(mydf)) {
  
  for (curdl in 1:ncol(mydf$envcov)) {
    
    if ((curdl >= mydf$dlstart[currow]) & (curdl <= mydf$dlstop[currow])) {
      
      mydf$Y[currow] <- mydf$Y[currow] + mydf$dlcoef[currow] * mydf$envcov[currow, curdl]
      
    }
    
  }
  
}
mydf$Y <- mydf$Y + rnorm(n=nrow(mydf),
                         mean=0,
                         sd=sigmaerr)
#View(mydf)







# this is where the actual regression starts

# create the lag matrix for the regression
# be lazy and copy over the environmental matrix
mydf$lagmat <- mydf$envcov
mydf$faclevmat <- mydf$envcov
# and then rewrite its columns
for (curcol in 1:ncol(mydf$envcov)) {
  
  mydf$lagmat[,curcol] <- curcol
  mydf$faclevmat[,curcol] <- mydf$faclev
  
}

# now create the environmental covariates by factor level
# this is where this approach gets really, really inefficient in terms of both memory and calculation
# there is one matrix per factor level
for (curlevel in unique(mydf$faclev)) {
  
  # create a redundant environmental matrix from the original lagged set
  mydf[paste("env_", curlevel, sep="")] <- mydf$envcov
  # if an observation is not from a factor with the current level, then change all its environmental data to 0 in that matrix
  for (currow in 1:nrow(mydf)) {
    
    mydf[currow, paste("env_", curlevel, sep="")] <- mydf[currow, paste("env_", curlevel, sep="")] * (mydf$faclev[currow] == curlevel)
    
  }
  
}

#colnames(mydf)
#View(mydf)

# now run the regression
envfacnames <- grep(x=colnames(mydf),
                    pattern="env_",
                    fixed=TRUE,
                    value=TRUE)
myformula <- paste("s(lagmat, by=", envfacnames, ", bs='tp', id=1)")
myformula <- paste(myformula, collapse=" + ", sep="")
myformula <- paste("Y ~ ", myformula)
myformula <- formula(myformula)
myformula

mygam <- bam(myformula, data=mydf)
summary(mygam)

plot(mygam, select=1)
plot(mygam, select=2)

# the better way to do it?
head(mydf)

# make sure this is a factor
mydf$faclev <- factor(mydf$faclev)
mydf$faclevmat <- factor(mydf$faclevmat)

mygam_byfactor <- bam(Y ~ 0 + faclev + s(lagmat, by=envcov, bs="tp") + s(lagmat, faclevmat, by=envcov, bs=c("tp", "re")),
                      data=mydf)

# mygam_byfactor <- bam(Y ~ 0 + faclev + s(x=lagmat, fac=faclevmat, by=envcov, bs="fs"),
#                       data=mydf)

tempdf <- model.matrix(mygam_byfactor)
#View(tempdf)
colnames(tempdf)

thisplot <- plot(mygam_byfactor, select=2)
tempdf   <- expand.grid(x=thisplot[[2]]$x,
                        y=thisplot[[2]]$y)
tempdf$fit <- thisplot[[2]]$fit
ggplot(tempdf) + geom_line(aes(x=x, y=fit, group=y, color=y))
faclevs

# mydf$preds_byfactor <- predict(mygam_byfactor)
# #ggplot(mydf) + geom_point(aes(x=Y, y=preds_byfactor, color=faclev))

summary(mygam)
summary(mygam_byfactor)

plot(mygam, select=1)
plot(mygam, select=2)

