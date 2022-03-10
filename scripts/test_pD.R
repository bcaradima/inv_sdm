# calculate effective number of parameters according to Spiegelhalter et al (2002, 2014): see Appendix of Paper 1.
# Spiegelhalter, D. J., Best, N. G., Carlin, B. P., & Van Der Linde, A. (2002). Bayesian # measures of model complexity and fit. Journal of the royal statistical society: Series b (statistical methodology), 64(4), 583-639.
# Spiegelhalter, D. J., Best, N. G., Carlin, B. P., & Van der Linde, A. (2014). The deviance information criterion: 12 years on. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 76(3), 485-493.

# global variables:

workspaces <- c("model_UF0_example.RData","model_CF0_example.RData","model_CT0_example.RData", "model_CT1_example.RData" ,"model_CT2_example.RData")
# workspaces <- c("model_CT2_example.RData")
thinning <- 10

# deviance function:
# alpha <- alpha[i,]
# beta <- beta[i,,]
# gamma <- gamma[i,]
# x.lat <- x.lat[i,]
# beta.lat <- beta.lat[i,]

calc.deviance <- function(x,y,alpha,beta,gamma=NULL,x.lat=NULL,beta.lat=NULL,verbose=F) {
  n.taxa  <- ncol(y)
  n.sites <- nrow(y)
  
  if(length(alpha)!=n.taxa) stop("alpha of incorrect length")
  if(nrow(x)!=n.sites) stop("x of incorrect size")
  if(nrow(beta)!=ncol(x)) stop("dimensions of beta and x do not match")
  if(ncol(beta)!=n.taxa) stop("dimension of beta incorrect")
  
  z <- matrix(rep(alpha,n.sites),byrow=T,nrow=n.sites) + x %*% beta
  if ( !is.null(gamma) ) 
  {
    z <- z + matrix(rep(gamma,n.taxa),byrow=F,ncol=n.taxa) 
    if ( verbose ) cat("\nusing site effect\n")
  }
  if ( !is.null(x.lat) & !is.null(beta.lat) )
  {
    # z <- z + x.lat %*% beta.lat
    # Added for single versus multiple latent variables
    if (length(dim(x.lat)) > 1){
      z <- z + x.lat %*% beta.lat
    } else{
      z <- z + sapply(beta.lat, function(i){ i * x.lat })
    }
    
    if ( verbose ) cat("\nusing latent variables\n")
  }
  p <- 1/(1+exp(-z))
  
  dev <- sum(-2*log(ifelse(y==1, p, 1-p)), na.rm = TRUE)
  
  return(dev)
}

# do calculations:

for ( workspace in workspaces )
{
  # cleanup from previous calculation:
  
  if ( length(dim(gamma)) > 0 ) rm(gamma)  # CAUTION: "gamma" exists always, but is the gamma function unless overwritten!
  if ( exists("x.lat") ) rm(x.lat)
  if ( exists("beta.lat") ) rm(beta.lat)
  load(workspace)

  ind.select <- 1:floor(nrow(alpha)/thinning)*thinning
  #ind.select <- 5*1:500
  
  alpha.mean <- apply(alpha,2,mean)
  beta.mean  <- apply(beta,c(2,3),mean)
  if ( length(dim(gamma)) == 0 )  # gamma parameter does not exist, just gamma function
  {
    if ( !exists("x.lat") )
    {
      dev.sample <- rep(NA,length(ind.select))
      for ( i in 1:length(ind.select) )
      {
        dev.sample[i] <- calc.deviance(x=x,y=y,alpha=alpha[i,],beta=beta[i,,])
      }
      dev.meanpar <- calc.deviance(x=x,y=y,alpha=alpha.mean,beta=beta.mean,verbose=T)
    }
  }
  else
  {
    gamma.mean <- apply(gamma,2,mean)
    if ( !exists("x.lat") )
    {
      dev.sample <- rep(NA,length(ind.select))
      for ( i in 1:length(ind.select) ) 
      {
        dev.sample[i] <- calc.deviance(x=x,y=y,alpha=alpha[i,],beta=beta[i,,],
                                       gamma=gamma[i,siteIND])
      }
      dev.meanpar <- calc.deviance(x=x,y=y,alpha=alpha.mean,beta=beta.mean,
                                   gamma=gamma.mean[siteIND],verbose=T)
    }
    else
    {
      if (length(dim(x.lat)) > 2){
        # Assumes multiple latent variables
        x.lat.mean <- apply(x.lat,c(2,3),mean)
        beta.lat.mean <- apply(beta.lat,c(2,3),mean)
        dev.sample <- rep(NA,length(ind.select))
        for ( i in 1:length(ind.select) )
        {
          dev.sample[i] <- calc.deviance(x=x,y=y,alpha=alpha[i,],beta=beta[i,,],
                                         gamma=gamma[i,siteIND],x.lat=x.lat[i,,],beta.lat=beta.lat[i,,])
        }
        dev.meanpar <- calc.deviance(x=x,y=y,alpha=alpha.mean,beta=beta.mean,
                                     gamma=gamma.mean[siteIND],x.lat=x.lat.mean,beta.lat=beta.lat.mean,verbose=T)
      }
      else{
        x.lat.mean <- apply(x.lat,2,mean)
        beta.lat.mean <- apply(beta.lat,2,mean)
        dev.sample <- rep(NA,length(ind.select))
        for ( i in 1:length(ind.select) )
        {
          dev.sample[i] <- calc.deviance(x=x,y=y,alpha=alpha[i,],beta=beta[i,,],
                                         gamma=gamma[i,siteIND],x.lat=x.lat[i,],beta.lat=beta.lat[i,])
        }
        dev.meanpar <- calc.deviance(x=x,y=y,alpha=alpha.mean,beta=beta.mean,
                                     gamma=gamma.mean[siteIND],x.lat=x.lat.mean,beta.lat=beta.lat.mean,verbose=T)
      }
    }
  }

  pD.1 <- mean(dev.sample) - dev.meanpar
  pD.2 <- 0.5*var(dev.sample)

  cat("\n")
  cat("workspace:    ",workspace,"\n")
  cat("mean deviance:",mean(dev.sample),"\n")
  cat("dev. meanpar: ",dev.meanpar,"\n")
  cat("pD_1:         ",pD.1,"\n")
  cat("pD_2:         ",pD.2,"\n")
}
