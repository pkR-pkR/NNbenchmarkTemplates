
# using example, p21, https://pdfs.semanticscholar.org/98d7/5bc6614e1e5ed107e752e818bc1950b0a2d8.pdf


f0 <- function(x) 5*sin(2*pi*x)
f1 <- function(x) exp(3*x)-7
f2 <- function(x) 
  0.5*x^11*(10*(1 - x))^6 - 10*(10*x)^3*(1 - x)^10

curve(f0, ylim=c(-10, 10))
curve(f1, add=TRUE, lty=2)
curve(f2, add=TRUE, lty=3)
abline(v=1:3/4, col="grey")
abline(v=1:4/5, lty=2, col="grey")


make_categ_data <- function(dist, linkinvfun, samplesize, nbcov, covlevels, otherparam)
{
  if(length(covlevels) != nbcov)
    stop("wrong covlevels and nbcov")
  
  
  f0 <- function(x) 5*sin(2*pi*x)
  f1 <- function(x) exp(3*x)-7
  f2 <- function(x) 
    0.5*x^11*(10*(1 - x))^6 - 10*(10*x)^3*(1 - x)^10
  
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

g1 <- make_categ_data("gamma", exp, 100, 3, list(LETTERS[1:3], letters[21:23], LETTERS[10:13]),
         otherparam = 3.5)
head(g1)
summary(glm(y~., data=g1, family=Gamma("log")))

make_cont_data <- function(dist, linkinvfun, samplesize, nbcov, otherparam)
{
  f0 <- function(x) 5*sin(2*pi*x)
  f1 <- function(x) exp(3*x)-7
  f2 <- function(x) 
    0.5*x^11*(10*(1 - x))^6 - 10*(10*x)^3*(1 - x)^10
  
  #make pairwise correlation
  alpha <- 0.25
  m <- floor(nbcov/2)
  xval <- sapply(1:m, function(i)
    runif(samplesize))
  xval <- cbind(xval, xval*alpha +(1-alpha) * sapply(1:m, function(i)
    runif(samplesize)))
  if(NCOL(xval) == nbcov-1)
    xval <- cbind(xval, runif(samplesize))
  
  #xval <- sapply(1:nbcov, function(i)
  #  runif(samplesize))
  
  xvalnum <- matrix(nrow=samplesize, ncol=nbcov)
  
  for(i in 1:nbcov)
  {
    xi <- xval[,i]
    j <- i %% 3
    if(j == 0)
    {
      xvalnum[,i] <- f0(xi)
    }
    if(j == 1)
    {
      xvalnum[,i] <- f1(xi)
    }
    if(j == 2)
    {
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
  if(dist == "invgauss")
  {
    require("SuppDists")
    y <- SuppDists::rinvGauss(samplesize, nu = mu, lambda = 1/otherparam)
  }
  res <- cbind.data.frame(y, xval, mu)
  colnames(res) <- c("y", paste0("V", 1:nbcov), "mu")
  res
}


#gamma example
g1 <- make_cont_data("gamma", abs, 1000, 4,
                      otherparam = 0.25)
head(g1)
summary(glm(y~V1+V2+V3+V4, data=g1, family=Gamma("log")))

par(mfrow=c(2,2), mar=c(4,4,2,1))
for(j in 1:4)
  plot(g1[,paste0("V",j)], g1$y)
     


#normal example
n1 <- make_cont_data("norm", function(x) x, 1000, 4,
                     otherparam = 0.25)
head(n1)
summary(glm(y~V1+V2+V3+V4, data=n1, family=gaussian("identity")))

par(mfrow=c(2,2), mar=c(4,4,2,1))
for(j in 1:4)
  plot(n1[,paste0("V",j)], n1$y, xlab=paste0("V",j))


#invgauss example
ig1 <- make_cont_data("invgauss", function(x) abs(x), 1000, 4,
                     otherparam = .02)
head(ig1)
summary(glm(y~V1+V2+V3+V4, data=ig1, family=inverse.gaussian("inverse")))

par(mfrow=c(2,2), mar=c(4,4,2,1))
for(j in 1:4)
  plot(ig1[,paste0("V",j)], ig1$y, xlab=paste0("V",j))



