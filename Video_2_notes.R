#######################################
#-Statistical Rethinking Video 2 Code-#
#######################################

library(rethinking)

# 3 design statistical way to produce estimate ----------------------------

# 4-sided Globe tossing example

sample <- c("W", "L", "W", "W", "W", "L", "W", "L", "W")
W <- sum(sample=="W") # number of W observed
L <- sum(sample=="L") # number of L observed
p <- c(0, 0.25, 0.5, 0.75, 1)
ways <- sapply(p, function(q) (q*4)^W * ((1-q)*4)^L)
prob <- ways/sum(ways)
d_post <- data.frame(p, ways, prob)


# 4 test 3 using 1 --------------------------------------------------------

# function to toss a globe covered p by water N times
sim_globe <- function(p=0.7, N=9) {
  sample(c("W", "L"), size=N, prob=c(p,1-p), replace=TRUE)
}

sim_globe()

replicate(sim_globe(p=0.5,N=9), n=10)

# Test the simulation on extreme settings
sim_globe(p=1, N=11) # Should only observe water

sum(sim_globe(p=0.5, N=1e4) == "W") / 1e4

# function to compute posterior distribution (estimator)
compute_posterior <- function(the_sample, poss=c(0,0.25,0.5,0.75,1)) {
  W <- sum(the_sample=="W") # number of W observed
  L <- sum(the_sample=="L") # number of L observed
  ways <- sapply(poss, function(q) (q*4)^W * ((1-q)*4)^L)
  post <- ways/sum(ways)
  bars <- sapply(post, function(q) make_bar(q))
  data.frame(poss, ways, post=round(post,3), bars)
}

compute_posterior(sim_globe())


# 5 - Analyze samples, summarize ------------------------------------------

# Sampling the posterior
post_samples <- rbeta(1e3, 6+1, 3+1)

dens( post_samples, lwd=4, col=2, xlab="proportion water", adj=0.1)
curve( dbeta(x, 6+1, 3+1), add=TRUE, lty=2, lwd=3)
dev.off()
plot.new()
# Posterior predictive distribution
post_samples <- rbeta(1e4, 6+1, 3+1)
pred_post <- sapply(post_samples, function(p) sum(sim_globe(p,10)=="W"))
tab_post <- table(pred_post)
for (i in 0:10) lines(c(i,i), c(0,tab_post[i+1]),lwd=4,col=4)
