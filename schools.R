library(rstan)

schools <- read.csv("schools.csv")
View(schools)
J <- nrow (schools)
y <- schools$estimate
sigma <- schools$sd


# sampling using stan
schools_fit <- stan(file = 'schools.stan', data=c("J","y","sigma"), iter = 1000, chains = 4)

print (schools_fit)
plot (schools_fit)

schools_sim <- extract (schools_fit, permuted=TRUE)
hist (schools_sim$tau)

# Gibbs sampler

theta_update <- function (){
  theta_hat <- (mu/tau^2 + y/sigma^2)/(1/tau^2 + 1/sigma^2)
  V_theta <- 1/(1/tau^2 + 1/sigma^2)
  rnorm (J, theta_hat, sqrt(V_theta))
}
mu_update <- function (){
  rnorm (1, mean(theta), tau/sqrt(J))
}
tau_update <- function (){
  sqrt(sum((theta-mu)^2)/rchisq(1,J-1))
}

chains <- 5
iter <- 1000
sims <- array (NA, c(iter, chains, J+2))
dimnames (sims) <- list (NULL, NULL,
                         c (paste ("theta[", 1:8, "]", sep=""), "mu", "tau"))
for (m in 1:chains){
  mu <- rnorm (1, mean(y), sd(y))
  tau <- runif (1, 0, sd(y))
  for (t in 1:iter){
    theta <- theta_update ()
    mu <- mu_update ()
    tau <- tau_update ()
    sims[t,m,] <- c (theta, mu, tau)
  }
}

monitor (sims)