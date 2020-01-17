#######################################################
# File name: "multilevel_script.R"
# Location: /Dropbox/Argentina/Analysis
# Dependency: "datamatched.Rdata" 
#######################################################

library(rjags)
load.module("glm")
library("plotrix")

######################
# LOAD R DATA FRAME #
######################

load("datamatched.Rdata")

n <- dim(datamatched)[1]

nvar.lev1 <- 9
nvar.lev2 <- 2

n.pollplace <- length(unique(datamatched$polling.place))

poll.place.ID <- as.numeric(datamatched$polling.place)

EV <- table(datamatched$polling.place, datamatched$EV)[, 2]
EV[EV>0] <- 1

outcome.lbls <- c("eselect.cand", "eval.voting", "easy.voting", "agree.evoting", "how.clean", "sure.counted", "capable.auth", "speed", "conf.secret")

n.outcomes <- length(outcome.lbls)

Y <- as.matrix(datamatched[, outcome.lbls])

age2 <- datamatched$age.group^2

X <- data.frame(datamatched$age.group, age2, datamatched$educ, datamatched$tech, datamatched$white.collar, datamatched$not.full.time, datamatched$male, datamatched$pol.info)

colnames(X) <- c("age.group", "age.group.sq", "educ", "tech", "white.collar", "not.full.time", "male", "pol.info")

X.stdr <- (X - matrix(apply(X, 2, mean), nrow = nrow(X), ncol = ncol(X), byrow = T)) / matrix(apply(X, 2, sd), nrow = nrow(X), ncol = ncol(X), byrow = T)

###############################
# BEGINNING OF BUGS/JAGS CODE #
###############################

modelString = "
model{
  for(i in 1:n) {
    y[i] ~ dbern(p.bound[i])
    p.bound[i]  <-  max(0, min(1, p[i]))

    logit(p[i])  <-  alpha[poll.place.ID[i]]
      + beta[poll.place.ID[i], 1] * age.group[i]
      + beta[poll.place.ID[i], 2] * age.group.sq[i]
      + beta[poll.place.ID[i], 3] * age.group[i] * educ[i]
      + beta[poll.place.ID[i], 4] * educ[i] 
      + beta[poll.place.ID[i], 5] * tech[i]
      + beta[poll.place.ID[i], 6] * white.collar[i]
      + beta[poll.place.ID[i], 7] * not.full.time[i]
      + beta[poll.place.ID[i], 8] * male[i]
      + beta[poll.place.ID[i], 9] * pol.info[i]
									
  }

  for (k in 1:n.pollplace) { 
    alpha[k] ~ dnorm(alpha.mean[k], prec.alpha)	
    alpha.adj[k] <- alpha[k] - mean(alpha[])
    alpha.mean[k]  <-  gamma[1]
      + gamma[2] * EV[k]

    for (j in 1:nvar.lev1) { 
      beta[k, j] ~ dnorm(beta.mean[j], prec.beta[j])
      beta.adj[k, j] <- beta[k, j] - mean(beta[, j])

  }}


  for (j in 1:nvar.lev1) { 
    beta.mean[j] ~ dnorm(0, 0.01) 
    sigma.beta[j] ~ dunif(0, 100)
    prec.beta[j] <-  pow(sigma.beta[j], -2)
  }

  for (j in 1:nvar.lev2) {   
    gamma[j] ~ dnorm(0, 0.01) 
  }

  sigma.alpha ~ dunif(0, 100)
  prec.alpha <-  pow(sigma.alpha, -2)

}
"
writeLines(modelString, con = "model.bug")

###########################################
# END OF BUGS CODE, PROCEED TO ESTIMATION #
###########################################

data <- NULL
logit.jags <- NULL
coda.samples <- NULL

# Estimate model using JAGS

parameters <- c("alpha", "alpha.adj", "beta", "beta.adj", "gamma", "beta.mean", "sigma.beta", "sigma.alpha")

inits <- list(list(.RNG.seed = c(43), .RNG.name = "base::Mersenne-Twister"),
			   list(.RNG.seed = c(54), .RNG.name = "base::Mersenne-Twister"))

for (i in 1:n.outcomes) {

data[[i]] <- list("n" = n, "poll.place.ID" = poll.place.ID, "n.pollplace" = n.pollplace, "nvar.lev1" = nvar.lev1, "nvar.lev2" = nvar.lev2, "y" = Y[,i], "age.group" = X.stdr$age.group, "age.group.sq" = age2, "educ" = X.stdr$educ, "tech" = X.stdr$tech, "white.collar" = X.stdr$white.collar, "not.full.time" = X.stdr$not.full.time, "male" = X.stdr$male, "pol.info" = X.stdr$pol.info, "EV"=EV)

logit.jags[[i]] <- jags.model("model.bug", data = data[[i]], inits = inits, n.chains = 2, n.adapt = 10000)

coda.samples[[i]] <- coda.samples(logit.jags[[i]], variable.names = parameters, n.iter = 10000, thin = 10)

}

#save(coda.samples, file = "coda.samples.Rdata")

load("coda.samples.Rdata")

###############################
# EFFECT OF ELECTRONIC VOTING #
###############################

alpha.samples <- NULL ; gamma.samples <- NULL ; beta.samples <- NULL
alpha.draws <- NULL ; gamma.draws <- NULL ; beta.draws <- NULL ; beta.samples.array <- NULL ; beta.samples.avgpp <- NULL

n.iters <- dim(coda.samples[[1]][[1]])[1] * 2

for (i in 1:n.outcomes){
	
alpha.samples[[i]] <- coda.samples[[i]][, 1:37]
gamma.samples[[i]] <- coda.samples[[i]][, c("gamma[1]", "gamma[2]")]
beta.samples[[i]] <- coda.samples[[i]][, 75:407]

alpha.draws[[i]] <- rbind(alpha.samples[[i]][[1]], alpha.samples[[i]][[2]])
gamma.draws[[i]] <- rbind(gamma.samples[[i]][[1]], gamma.samples[[i]][[2]])
beta.draws[[i]] <- rbind(beta.samples[[i]][[1]], beta.samples[[i]][[2]])
beta.samples.array[[i]] <- array(beta.draws[[i]], c(n.iters, n.pollplace, nvar.lev1))
beta.samples.avgpp[[i]] <- apply(beta.samples.array[[i]], c(1,3), mean)


}

X.median <- apply(X.stdr, 2, median)

# Create empty matrices & arrays for simulation
alpha.ev <- matrix(NA,nrow = n.outcomes, ncol = n.iters) ; alpha.tv <- matrix(NA,nrow = n.outcomes, ncol = n.iters)
beta.contrib <- matrix(NA,nrow = n.outcomes, ncol = n.iters)
xb.ev <- matrix(NA,nrow = n.outcomes, ncol = n.iters) ; xb.tv <- matrix(NA,nrow = n.outcomes, ncol = n.iters)
prob.ev <- matrix(NA,nrow = n.outcomes, ncol = n.iters) ; prob.tv <- matrix(NA, nrow = n.outcomes, ncol = n.iters)
effect <- matrix(NA,nrow = n.outcomes, ncol = n.iters)

for (i in 1:n.outcomes) {

for (j in 1:n.iters) {
	
alpha.ev[i, j] <- gamma.draws[[i]][j, 1]+ gamma.draws[[i]][j, 2] * 1

alpha.tv[i, j] <- gamma.draws[[i]][j, 1]+ gamma.draws[[i]][j, 2] * 0

beta.contrib[i,j] <- beta.samples.avgpp[[i]][j, 1] * X.median["age.group"]
+ beta.samples.avgpp[[i]][j, 2] * X.median["age.group.sq"]
+ beta.samples.avgpp[[i]][j, 3] * X.median["age.group"] * X.median["educ"] 
+ beta.samples.avgpp[[i]][j, 4] * X.median["educ"]
+ beta.samples.avgpp[[i]][j, 5] * X.median["tech"]
+ beta.samples.avgpp[[i]][j, 6] * X.median["white.collar"]
+ beta.samples.avgpp[[i]][j, 7] * X.median["not.full.time"]
+ beta.samples.avgpp[[i]][j, 8] * X.median["male"]
+ beta.samples.avgpp[[i]][j, 9] * X.median["pol.info"]

xb.ev[i, j] <- alpha.ev[i, j] + beta.contrib[i, j]

xb.tv[i, j] <- alpha.tv[i, j] + beta.contrib[i, j]

prob.ev[i, j] <- 1/(1 + exp(-xb.ev[i, j]))

prob.tv[i, j] <- 1/(1 + exp(-xb.tv[i, j]))

effect[i, j] <-  prob.ev[i, j] - prob.tv[i, j]

}}

effect.table <- cbind(apply(effect, 1, mean), apply(effect, 1, sd), apply(effect, 1, quantile, p = 0.05), apply(effect, 1, quantile, p=0.95))
rownames(effect.table) <- outcome.lbls

### Table A4 ###

print(effect.table[rev(order(effect.table[, 1])), ])

#########################
# PLOT EV COEFFFICIENTS #
#########################

EV.coefficients <- matrix(NA, nrow = n.iters, ncol = n.outcomes)

for (i in 1:n.outcomes) {

EV.coefficients[, i] <- gamma.draws[[i]][, 2]

}

var.names <- c("Select candidates electronically", "Evaluation voting experience", "Difficulty voting experience", "Agree subtitute TV by EV", "Elections in Salta are clean", "Sure vote was counted", "Qualification of poll workers", "Speed of voting process", "Confident ballot secret")

EV.coefficients.stats <- cbind(apply(EV.coefficients, 2, mean), apply(EV.coefficients, 2, quantile, p = 0.05), apply(EV.coefficients, 2, quantile, p = 0.95))

## Figure A4 ### 

par(oma=c(0, 9, 0, 0)) 

plotCI(y = 1:n.outcomes, x = rev(EV.coefficients.stats[, 1]), err = "x", li = rev(EV.coefficients.stats[, 2]), ui = rev(EV.coefficients.stats[, 3]), axes = FALSE, ann = FALSE, ylim = c(0.7, n.outcomes + 0.1), xlim = c(-2, 2), pch=1, pt.bg = par(cex = 0.9), sfrac = 0.005, lwd = 1)
axis(side = 1, cex.axis = 0.9)
axis(side = 2,las = 1, at = c(1:n.outcomes), cex.axis = 0.9, labels = rev(var.names), las = 2)
abline(v = 0, col = "grey63")




