#######################################
#-Statistical Rethinking Video 4 Code-#
#######################################

library(rethinking)


# Height, weight, sex model -----------------------------------------------

# Simulate observations of male and female height and weight
# S=1 female; S=2 male
sim_HW <- function(S,b,a) {
  N <- length(S)
  H <- ifelse(S==1, 150, 160) + rnorm(N,0,5)
  W <- a[S] + b[S]*H + rnorm(N,0,5)
  data.frame(S,H,W)
}

S <- rbern(100)+1
dat <- sim_HW(S, b=c(0.5,0.6),a=c(0,0))
head(dat)

# Causal effect of sex (Testing)

# Female sample
S <- rep(1,100)
simF <- sim_HW(S, b=c(0.5,0.6),a=c(0,0))

# Male sample
S <- rep(2, 100)
simM <- sim_HW(S, b=c(0.5,0.6),a=c(0,0))

# effect of sex (male-female)
mean(simM$W - simF$W)

# Now run the estimator with synthetic sample
S <- rbern(100) + 1
dat <- sim_HW(S, b=c(0.5,0.6),a=c(0,0))

# estimate posterior
m_SW <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a[S],
    a[S] ~ dnorm(60,10),
    sigma ~ dunif(0,10)
  ), data = dat
)
precis(m_SW, depth=2)

# Analyze real sample (Howell1)
data("Howell1")
d <- Howell1
d <- d[d$age>=18,]
dat <- list(
  W = d$weight,
  S = d$male + 1 # S=1 female, S=2 male
)

m_SW <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(60,10),
    sigma ~ dunif(0,10)
  ),
  data=dat
)
precis(m_SW, depth=2)

par(mfrow=c(2,1))
# Posterior means and predictions
# posterior mean W
post <- extract.samples(m_SW)
dens( post$a[,1], xlim=c(39,50), lwd=3,
      col=2, xlab="Posterior mean weight (kg)")
dens(post$a[,2], lwd=3, col=4, add=TRUE)

# Posterior W distributions (posterior predictive distributions)
W1 <- rnorm(1000, post$a[,1], post$sigma)
W2 <- rnorm(1000, post$a[,2], post$sigma)
dens(W1, xlim=c(20,70), ylim=c(0,0.085),
     lwd=3, col=2, xlab='posterior predicted weight (kg)')
dens(W2, lwd=3, col=4, add=TRUE)
par(mfrow=c(1,1))

# Causal contrast (in means)

mu_contrast <- post$a[,2] - post$a[,1]

dens(mu_contrast, xlim=c(3,10), lwd=3,
     col=1, xlab = "posterior mean weight contrast (kg)")

# Weight contrast

# posterior W distributions
W1 <- rnorm(1000, post$a[,1], post$sigma)
W2 <- rnorm(1000, post$a[,2], post$sigma)

# contrast
W_contrast <- W2 - W1
dens(W_contrast, xlim=c(-25,35), lwd=3, col=1, xlab="posterior wegith contrast (kg)")

# proportion above zero
sum(W_contrast > 0) / 1000
# Proportion below zero
sum(W_contrast < 0) / 1000

# What is the direct effect of S on W

# Simulation
S <- rbern(100) + 1
dat <- sim_HW(S, b=c(0.5, 0.5), a=c(0, 10))

# Analyze the sample
data(Howell1)
d <- Howell1
d <- d[ d$age >= 18, ]
dat <- list(
  W = d$weight,
  H = d$height,
  Hbar = mean(d$height),
  S = d$male + 1
)

m_SHW <- quap(
  alist(
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
    sigma ~ dunif(0,10)
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
precis(m_SHW, depth=2)



# Contrasts at each height
xseq <- seq(from=130, to=190, len=50) # Get sequence of heights to simulate over

muF <- link(m_SHW, data=list(S=rep(1,50),H=xseq,Hbar=mean(d$height)))
plot(d$height, d$weight, xlim=range(xseq), ylim=range(d$weight), xlab="height (cm)", ylab="weight (kg)")
lines(xseq, apply(muF,2,mean), lwd=3,col=2)

muM <- link(m_SHW, data=list(S=rep(2,50),H=xseq,Hbar=mean(d$height)))
lines(xseq, apply(muM,2,mean), lwd=3, col=4)


mu_contrast <- muF - muM
plot(NULL, xlim=range(xseq), ylim=c(-6, 8), xlab="height (cm)", ylab="weight contrast (F-M)")
for (p in c(0.5, 0.6, 0.7, 0.8, 0.9, 0.99))
  shade(apply(mu_contrast, 2, PI, prob=p), xseq)
abline(h=0, lty=2)
