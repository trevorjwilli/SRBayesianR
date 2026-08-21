#######################################
#-Statistical Rethinking Video 4 Code-#
#######################################

library(rethinking)


# Full luxury Bayes -------------------------------------------------------

data("Howell1")
d <- Howell1
d <- d[ d$age >= 18, ]
dat <- list(
  H = d$height,
  Hbar = mean(d$height),
  W = d$weight,
  S = d$male + 1
  
)

m_SHW_full <- quap(
  alist(
    # weight
    W ~ dnorm(mu, sigma),
    mu <- a[S] + b[S]*(H-Hbar),
    a[S] ~ dnorm(60, 10),
    b[S] ~ dunif(0,1),
    sigma ~ dunif(0,10),
    
    # height
    H ~ dnorm(nu, tau),
    nu <- h[S],
    h[S] ~ dnorm(160, 10),
    tau ~ dunif(0,10)
  ),
  data=dat
)

precis(m_SHW_full, depth=2)

# Total causal effect of S on W
post <- extract.samples(m_SHW_full)
Hbar <- dat$Hbar
n <- 1e4

with( post, {
  # Simulate W for S=1
  H_S1 <- rnorm(n, h[,1], tau)
  W_S1 <- rnorm(n, a[,1] + b[,1]*(H_S1-Hbar), sigma)
  
  # Simulate W for S=2
  H_S2 <- rnorm(n, h[,2], tau)
  W_S2 <- rnorm(n, a[,2] + b[,2]*(H_S1-Hbar), sigma)
  
  # Compute contrast
  W_do_S <<- W_S2 - W_S1
})

dens(W_do_S)
