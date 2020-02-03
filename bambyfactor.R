rm(list=ls())

library(ggplot2)
library(mgcv)

# set up parameters for a model
nfactorlevels <- 5    # how many factor levels?
nobsperlevel  <- 500  # how many observations per factor level?
envlaglen     <- 180  # how many columns in the lagged environmental data?
sigmasq       <- 1    # what's the variance on the artificial data?

# create a data frame with artificial environmental data
mydf <- expand.grid(faclev = 1:nfactorlevels,
                    nobs   = 1:nobsperlevel)
mydf$envcov <- matrix(rnorm(n=nrow(mydf)*envlaglen,
                            mean=0,
                            sd=1),
                      nrow=nrow(mydf),
                      ncol=envlaglen)

# create observations per 
                              