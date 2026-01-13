#######################################
#-Statistical Rethinking Video 3 Code-#
#######################################


# Linear model Generative model -------------------------------------------

# function to simulate weights of individuals from height
sim_weight <- function(H,b,sd) {
  U <- rnorm(length(H), 0, sd)
  W <- b*H + U
  return(W)
}

H <- runif(200, min=130, max=170)
W <- sim_weight(H, b=0.5, sd=5)
plot(W ~ H, col=2, lwd=3)


# Quadratic approximation of model ----------------------------------------

m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*H,
    a ~ dnorm(0, 10),
    b ~ dunif(0, 1),
    sigma ~ dunif(0,10)
  ), data = list(W=W,H=H)
)


# Prior predictive simulation -------------------------------------------------

n <- 1e3
a <- rnorm(n,0,10)
b <- runif(n,0,1)
plot(NULL, xlim=c(130,170), ylim=c(50,90),
     xlab="height (cm)", ylab="weight (kg)")
for (j in 1:50) abline(a=a[j], b=b[j], lwd=2, col=2)


# 4 Validate --------------------------------------------------------------

# simulate a sample of 10 people
set.seed(93)
H <- runif(10, 130, 170)
W <- sim_weight(H, b=0.5, sd=5)

# Run the model
library(rethinking)
m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*H,
    a ~ dnorm(0, 10),
    b ~ dunif(0, 1),
    sigma ~ dunif(0,10)
  ), data = list(W=W,H=H)
)

# summary
precis(m3.1)


# 5 analyze with data -----------------------------------------------------

data("Howell1")
d2 <- Howell1[Howell1$age >=18,]
dat <- list(W=d2$weight, H=d2$height)
m3.2 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*H,
    a ~ dnorm(0, 10),
    b ~ dunif(0, 1),
    sigma ~ dunif(0,10)
  ), data = dat
)
precis(m3.2)

# Posterior predictive distributions
post <- extract.samples(m3.2)
plot(d2$height, d2$weight, col=2, lwd=3,
     xlab="height (cm)", ylab="weight (kg)")
for (j in 1:20) abline(a=post$a[j], b=post$b[j], lwd=1)

height_seq <- seq(130, 190, len=20)
W_postpred <- sim(m3.2, data=list(H=height_seq))
W_PI <- apply(W_postpred, 2, PI)
lines(height_seq, W_PI[1,], lty=2, lwd=2)
lines(height_seq, W_PI[2,], lty=2, lwd=2)



