getOption("repos")
options(repos = "https://cran.rstudio.com") # For packages installation

library('mvtnorm')
library('grf')
library('devtools')
library('crossEstimation')
library('balanceHD')
library('Synth')
library('glmnet')
library('rpart')
library('evtree')
library('stats')
library('caret')
library('glmnetUtils')
library('zeallot')
library('pbapply')
library('dplyr')

# ==================================================================
# Problem 2.1 ----------------- Policy Learning --------------------
# ==================================================================


# Simulation datasets
simDR <- as.data.frame(matrix(nrow=2500, ncol=200))
simIPW <- as.data.frame(matrix(nrow=2500, ncol=200))
B <- 200

for (j in 1:B)
{
  
  # Inside of a simulation
  
  # ------- [a] -------
  
  set.seed(1234)
  n <- 500
  
  x1 <- runif(n, min = -1, max = 1)
  x2 <- runif(n, min = -1, max = 1)
  X <- polym(x1, x2, degree=5, raw=T)
  e <- 1/(1+exp(-(x1+x2)))
  mu <- 2*exp(-(x1+x2))
  w <- 2*rbinom(n, 1, e) - 1
  tau <- 2/((1+exp(-4*x1))*(1+exp(-4*x2)))-0.4
  y <- rnorm(n, mean = (mu + tau*w/2), sd=1)
  df <- cbind.data.frame(X, e, mu, w, tau, y)
  
  
  # ------- [b] ------- 
  
  # CV both for alphas and lambdas in elastic net for e(x) --------------------
  
  lambda_grid <- seq(0, 1, 0.05)
  alpha_grid <- seq(0, 1, 0.05) 
  srchGrid <- expand.grid(.alpha = alpha_grid, .lambda = lambda_grid)
  fit <- cva.glmnet(X, e, alpha=alpha_grid, family="gaussian", nfolds=10)
  
  l <- list()
  mse <- list()
  
  # Find optimal alphas and lambdas
  for (i in 1:21) {
    f <- fit$modlist[[i]]
    best_lambda <- f$lambda.min
    l[[i]] <- best_lambda
    errs <- f$cvm
    lambdas <- f$lambda
    position <- which(lambdas %in% best_lambda)
    error <- mse[position]
    mse[[i]] <- error
  }
  
  l <- as.numeric(l)
  mse <- as.numeric(l)
  parameters <- cbind(alpha_grid, l, mse)
  min_mse <- min(mse)
  pos <- which(mse %in% min_mse)
  best_alpha <- parameters[pos]
  
  # -----------------------------------------------------------------------
  
  
  # Simple estimation of e(x)
  simple.fit <- glmnet(as.matrix(df[1:20]), as.matrix(df[21]), alpha=best_alpha, family="gaussian")
  e.hat <- predict(simple.fit, as.matrix(df[1:20]), type="response", s = best_lambda)
  
  # Inverse-Propensity Weightening Score
  G.IPW.hat <- w*y/e.hat
  abs.G.IPW.hat <- abs(G.IPW.hat)
  sign.G.IPW.hat <- sign(G.IPW.hat)
  
  
  # Cross-fitting estimation of e(x) and mu(x)
  m <- matrix(nrow = 100 , ncol = 20)
  colnames(m) <- c("e.hat.1", "e.hat.2", "e.hat.3", 'e.hat.4', 'e.hat.5',
                   "mu.hat.minus.1", "mu.hat.minus.2", "mu.hat.minus.3", 'mu.hat.minus.4', 'mu.hat.minus.5',
                   "mu.hat.plus.1", "mu.hat.plus.2", "mu.hat.plus.3", 'mu.hat.plus.4', 'mu.hat.plus.5',
                   "mu.hat.1", "mu.hat.2", "mu.hat.3", 'mu.hat.4', 'mu.hat.5')
  
  DR <- matrix(nrow = 0 , ncol = 1)
  
  for (i in 1:5){
    train <- sample_n(df, 400)
    test <- anti_join(df, train, by = "y")
    X.minus <- as.matrix(data.frame(test[1:20]), -1)
    X.plus <- as.matrix(data.frame(test[1:20]), 1)
    fit.e <- glmnet(as.matrix(train[1:20]), as.matrix(train[21]), alpha=0.95, family="gaussian")
    fit.mu <- glmnet(as.matrix(train[1:20]), as.matrix(train[22]), alpha=0.95, family="gaussian")
    pred.e <- predict(fit.e, as.matrix(test[1:20]), type="response", s = best_lambda)
    pred.mu.minus <- predict(fit.mu, X.plus, type="response", s = best_lambda)
    pred.mu.plus <- predict(fit.mu, X.minus, type="response", s = best_lambda)
    pred.mu <- predict(fit.mu, as.matrix(test[1:20]), type="response", s = best_lambda)
    m[, i] <- pred.e
    m[, i+5] <- pred.mu.plus
    m[, i+10] <- pred.mu.minus
    m[, i+15] <- pred.mu
    m <- as.data.frame(m)
    dr <- m[i+10] - m[i+5] + test[23]*(test[25] - m[i+15])/m[, i]
    dr <- as.matrix(dr)
    DR <- rbind(DR, dr)
  }
  
  DR <- as.data.frame(DR)
  colnames(DR) <- c('DR')
  
  G.DR.hat <- as.numeric(DR$DR)
  abs.G.DR.hat <- abs(G.DR.hat)
  sign.G.DR.hat <- sign(G.DR.hat)
  
  
  # ------- [c] ------- 
  
  data <- data.frame(X.plus, w, y)
  fit.DR <- rpart(sign.G.DR.hat ~ x1 + x2, data=data, weights=abs.G.DR.hat,
                  control=rpart.control(maxdepth=2))
  fit.IPW <- rpart(sign.G.IPW.hat ~ x1 + x2, data=data, weights=abs.G.IPW.hat,
                   control=rpart.control(maxdepth=2))
  
  ixs <- seq(-1, 1, length.out = 50) 
  X_test <- expand.grid(x1=ixs, x2=ixs)
  
  Y_test_DR <- predict(fit.DR, newdata=X_test)
  Y_test_IPW <- predict(fit.IPW, newdata=X_test)
  
  df_DR <- data.frame(X_test, Y_test_DR)
  df_IPW <- data.frame(X_test, Y_test_IPW)
  
  simDR[, j] = df_DR[, 3]
  simIPW[, j] = df_IPW[, 3]
  print(j) # To look at what stage of a simulation we are in
}

# ------- [d] ------- 

avgDR <- rowMeans(simDR)
avgIPW <- rowMeans(simIPW)

data <- data.frame(X_test, avgDR, avgIPW)
data$Tau <- sign(2/((1+exp(-4*data$x1))*(1+exp(-4*data$x2)))-0.4)

par(mar=c(3,4,2,2))
levelplot(avgDR ~ x1*x2, data=data, xlab="x1", pretty=T,
          col.regions = heat.colors(100)[length(heat.colors(100)):1],
          main="Doubly - Robust Estimator")

par(mar=c(3,4,2,2))
levelplot(avgIPW ~ x1*x2, xlab="x1", data=data, cuts = 50, pretty=T,
          col.regions = heat.colors(100)[length(heat.colors(100)):1],
          main="Inwerse - Propensity Weighting Estimator")

par(mar=c(3,4,2,2))
levelplot(Tau ~ x1*x2, data=data, xlab="x1", pretty=T,
          col.regions = heat.colors(100)[length(heat.colors(100)):1],
          main="Optimal Classifier")