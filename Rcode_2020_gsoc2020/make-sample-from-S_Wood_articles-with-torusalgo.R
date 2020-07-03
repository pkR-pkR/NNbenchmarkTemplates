
# using example, p21, https://pdfs.semanticscholar.org/98d7/5bc6614e1e5ed107e752e818bc1950b0a2d8.pdf

library(randtoolbox)

f0 <- function(x) 5*sin(2*pi*x)
f1 <- function(x) exp(3*x)-7
f2 <- function(x) 
  0.5*x^11*(10*(1 - x))^6 - 10*(10*x)^3*(1 - x)^10
f3 <- function(x)
  15*exp(-5*abs(x-1/2))-6
f4 <- function(x)
  2-(x <= 1/3)*(6*x)^3 - (x >= 2/3)*(6-6*x)^3 - (x > 1/3 & x < 2/3)*(8+2*sin(9*(x-1/3)*pi))

curve(f0, ylim=c(-10, 10))
curve(f1, add=TRUE, lty=2)
curve(f2, add=TRUE, lty=3)
curve(f3, add=TRUE, lty=4)
curve(f4, add=TRUE, lty=5)
abline(v=1:3/4, col="grey")
abline(v=1:4/5, lty=2, col="grey")


make_categ_data <- function(dist, linkinvfun, samplesize, nbcov, covlevels, otherparam)
{
  if(length(covlevels) != nbcov)
    stop("wrong covlevels and nbcov")
  
  xval <- lapply(1:nbcov, function(i)
      factor(sample(covlevels[[i]], size=samplesize, replace=TRUE)))
  
  xvalnum <- matrix(nrow=samplesize, ncol=nbcov)
  
  for(i in 1:nbcov)
  {
    m <- length(levels(xval[[i]]))
    xi <- as.numeric(xval[[i]]) / (m+1)
    j <- i %% 3
    if(j == 0)
    {
      #print(length(f0(xi)))
      xvalnum[,i] <- f0(xi)
    }
    if(j == 1)
    {
      #print(length(f1(xi)))
      xvalnum[,i] <- f1(xi)
    }
    if(j == 2)
    {
      #print(length(f2(xi)))
      xvalnum[,i] <- f2(xi)
    }
  }
  eta <- 1 + xvalnum %*% rep(1, nbcov)
  mu <- linkinvfun(eta)
  if(dist == "exp")
  {
    dist <- "gamma"
    otherparam <- 1
  }
  if(dist == "gamma")
    y <- rgamma(samplesize, shape = otherparam, rate = otherparam/mu)
  if(dist == "norm")
    y <- rnorm(samplesize, mean = mu, sd=otherparam)
  if(dist == "pois")
    y <- rpois(samplesize, lambda = mu)
  if(dist == "invgauss")
  {
    require("SuppDists")
    y <- SuppDists::rinvGauss(samplesize, nu = mu, lambda = 1/otherparam)
  }
  res <- cbind.data.frame(y, xval)
  colnames(res) <- c("y", paste0("V", 1:nbcov,"_"))
  res
}


make_cont_data <- function(dist, linkinvfun, samplesize, nbcov, otherparam,
                           mixingalpha=0.25)
{
  #call quasi RNG torus() from randtoolbox pkg
  xval <- torus(n= samplesize, dim = nbcov, mixed=TRUE)
  xvalnum <- matrix(nrow=samplesize, ncol=nbcov)
  
  for(i in 1:nbcov)
  {
    xi <- xval[,i]
    j <- i %% 5
    cat(i,j,"\n")
    if(j == 0)
      xvalnum[,i] <- f0(xi)
    if(j == 1)
      xvalnum[,i] <- f1(xi)
    if(j == 2)
      xvalnum[,i] <- f2(xi)
    if(j == 3)
      xvalnum[,i] <- f3(xi)
    if(j == 4)
      xvalnum[,i] <- f4(xi)
  }
  eta <- 1 + xvalnum %*% rep(1, nbcov)
  #eta <- 1 + xvalnum %*% c(0,0,0,0,0,1) manual check
  mu <- linkinvfun(eta)
  if(dist == "exp")
  {
    dist <- "gamma"
    otherparam <- 1
  }
  if(dist == "gamma")
    y <- rgamma(samplesize, shape = otherparam, rate = otherparam/mu)
  if(dist == "norm")
    y <- rnorm(samplesize, mean = mu, sd=otherparam)
  if(dist == "invgauss")
  {
    require("SuppDists")
    y <- SuppDists::rinvGauss(samplesize, nu = mu, lambda = 1/otherparam)
  }
  res <- cbind.data.frame(y, xval)
  colnames(res) <- c("y", paste0("V", 1:nbcov))
  res
}

controlplot <- function(data, plot.nr, plot.nc, plot.mar=c(4,4,2,1), plot.nobs=1000, var2round,...)
{
  if(var2round)
  {
    par(mfrow=c(plot.nr,plot.nc), mar=plot.mar)
    for(j in 1:(plot.nr*plot.nc))
    {
      Vjb <- round(data[,paste0("V",j)], 2)
      plot(sort(unique(Vjb)), tapply(data$y, Vjb, mean), 
         xlab=paste0("rounded variable ", j), ylab="y", ...)
    }
  }else
  {
    n <- min(plot.nobs, NROW(data)) 
    par(mfrow=c(plot.nr,plot.nc), mar=plot.mar)
    for(j in 1:(plot.nr*plot.nc))
      plot(data[1:n,paste0("V",j)], data$y[1:n], 
           xlab=paste0("original variable ", j), ylab="y",...)
  }
  
}

set.seed(123)
nrow <- 20000
ncov <- 6


#gamma example
mWoodG1 <- make_cont_data("gamma", abs, nrow, ncov,
                      otherparam = 0.25)
head(mWoodG1)
summary(glm(y~., data=mWoodG1, family=Gamma("log")))

#mean check
controlplot(mWoodG1, 2, ncov/2, var2round=TRUE)
#correlation check
round(cor(mWoodG1[,-1]), 2)
 
     

#normal example
mWoodN1 <- make_cont_data("norm", identity, nrow, ncov,
                     otherparam = 0.25, mixingalpha=0.25)
head(mWoodN1)
summary(glm(y~., data=mWoodN1, family=gaussian("identity")))

#mean check
controlplot(mWoodN1, 2, ncov/2, var2round=TRUE)
#correlation check
round(cor(mWoodN1[,-1]), 2)


#invgauss example
mWoodIG1 <- make_cont_data("invgauss", abs, nrow, ncov,
                     otherparam = .1)
head(mWoodIG1)
summary(glm(y~., data=mWoodIG1, family=inverse.gaussian("inverse")))

#mean check
controlplot(mWoodIG1, 2, ncov/2, var2round=TRUE)
#correlation check
round(cor(mWoodIG1[,-1]), 2)

#export to rda
if(FALSE)
{
  save(mWoodG1, file="mWoodG1.rda")
  save(mWoodN1, file="mWoodN1.rda")
  save(mWoodIG1, file="mWoodIG1.rda")
}