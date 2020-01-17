########################################################################
# File name: "matching_script.R"
# Goal: Estimate effect of e-voting using matching
# Dependency: "datamatch.Rdata" 
########################################################################

library(MatchIt)
library(Zelig)
library(rbounds)

load("datamatch.Rdata")

outcomes <- datamatch[10:18]

outcomes.lbls <- names(outcomes)

n.outcomes <- dim(outcomes)[2]

#_________________________________ Table 1 _________________________________#

tab1 <- matrix(NA, nrow = n.outcomes, ncol = 6)
rownames(tab1) <- outcomes.lbls
colnames(tab1) <- c("N", "prop.all", "prop.ev", "prop.tv", "diff", "pvalue")

for (i in 1:n.outcomes) {
  tab1[i, 1] <- length(na.omit(outcomes[, i]))
  tab1[i, 2] <- prop.table(table(outcomes[, i]))[2] * 100	
  tab1[i, 3:4] <- rev(prop.table(table(outcomes[, i], datamatch$EV), 2)[2, ]) * 100
  tab1[i, 5] <- tab1[i, 3] - tab1[i, 4]	
  tab1[i, 6] <- prop.test(table(outcomes[, i], datamatch$EV)[2, ], n = apply(table(outcomes[, i], datamatch$EV), 2, sum))$p.value
}

tab1 <- tab1[rev(order(tab1[, "diff"])), ]

### Table 1 ###

print(tab1, digits = 4)

#___________________________________________________________________________#

# Drop observations with missing values in covariates

datamatch[, 10:18][is.na(datamatch[, 10:18]) == "TRUE"] <- 99999
datamatch <- na.omit(datamatch)

#__________________________ Table 2, pre-matching __________________________#

EV <- datamatch[2]

covariates <- datamatch[c("age.group", "educ", "white.collar", "not.full.time", "male", "tech", "pol.info")]
covariate.lbls <- names(covariates)

n.covariates <- dim(covariates)[2]

tab2.pre <- matrix(NA, nrow = n.covariates, ncol = 4)
rownames(tab2.pre) <- covariate.lbls
colnames(tab2.pre) <- c("ev", "tv", "diff", "pvalue")

tab2.pre[, 1:2] <- cbind(apply(covariates[EV == 1,], 2, mean), apply(covariates[EV == 0,], 2, mean))
tab2.pre[, 3] <- tab2.pre[, 1] - tab2.pre[, 2]

for (i in c(1, 2, 6, 7)){
  tab2.pre[i, 4] <- ks.boot(covariates[, i][EV == 1], covariates[, i][EV == 0], nboots = 500)$ks.boot.pvalue
}
for (i in c(3, 4, 5)){
  tab2.pre[i, 4] <- prop.test(table(covariates[, i], EV$EV), n = apply(table(covariates[,i],EV$EV),2, sum))$p.value
}

#__________________________ Table 3, pre-matching __________________________#

datamatch[datamatch == 99999] <- NA

outcomes.pre <- datamatch[10:18]

tab3.pre <- matrix(NA,nrow = n.outcomes,ncol = 5)
rownames(tab3.pre) <- outcomes.lbls
colnames(tab3.pre) <- c("N", "prop.ev", "prop.tv", "diff", "pvalue")

for (i in 1:n.outcomes) {
  tab3.pre[i, 1] <- length(na.omit(outcomes.pre[, i]))
  tab3.pre[i, 2:3] <- rev(prop.table(table(outcomes.pre[,i],datamatch$EV),2)[2,])*100
  tab3.pre[i, 4] <- tab3.pre[i, 2] - tab3.pre[i, 3]	
  tab3.pre[i, 5] <- prop.test(table(outcomes.pre[, i], datamatch$EV)[2, ], n = apply(table(outcomes.pre[, i], datamatch$EV), 2, sum))$p.value
}

datamatch[, 10:18][is.na(datamatch[, 10:18]) == "TRUE"] <- 99999

#__________________________ Matching (with MatchIt) ________________________#

print("Matching")

set.seed(36466)

m.out <- matchit(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + tech:pol.info + white.collar + not.full.time + male, caliper = 0.05, data = datamatch, method = "nearest", verbose = "TRUE")

#save(m.out, file = "m.out.Rdata")

print("Balance Improvement")
print(summary(m.out))

#pdf("balance.pdf")

#plot(m.out)
#plot(m.out, type = "hist")
#plot(m.out, type = "jitter")

#dev.off()

#___________________________________________________________________________#

# matched sample

datamatched <- match.data(m.out)
datamatched[datamatched == 99999] <- NA

save(datamatched, file = "datamatched.Rdata")

#__________________________ Table 2, post-matching _________________________#

EV.post <- datamatched[2]

covariates.post <- datamatched[, covariate.lbls]

tab2.post <- matrix(NA, nrow = n.covariates, ncol = 4)
rownames(tab2.post) <- covariate.lbls
colnames(tab2.post) <- c("ev", "tv", "diff", "pvalue")

tab2.post[, 1:2] <- cbind(apply(covariates.post[EV.post == 1, ], 2, mean), apply(covariates.post[EV.post == 0,], 2, mean))
tab2.post[, 3] <- tab2.post[, 1] - tab2.post[, 2]
for (i in c(1, 2, 6 , 7)){
  tab2.post[i, 4]<-ks.boot(covariates.post[,i][EV.post==1],covariates.post[,i][EV.post==0], nboots = 500)$ks.boot.pvalue
}
for (i in c(3, 4, 5)){
  tab2.post[i, 4] <- prop.test(table(covariates.post[, i], EV.post$EV), n = apply(table(covariates.post[, i], EV.post$EV),2 , sum))$p.value
}

tab2 <- cbind(tab2.pre, tab2.post)
tab2[3:5, c(1:3, 5:7)] <- tab2[3:5, c(1:3, 5:7)] * 100

### Table 2 ###

print(tab2, digits = 4)

#__________________________ Table 3, post-matching _________________________#

outcomes.post <- datamatched[10:18]

tab3.post <- matrix(NA, nrow = n.outcomes, ncol = 5)
rownames(tab3.post) <- outcomes.lbls
colnames(tab3.post) <- c("N", "prop.ev", "prop.tv", "diff", "pvalue")

for (i in 1:n.outcomes) {
  tab3.post[i, 1] <- length(na.omit(outcomes.post[, i]))
  tab3.post[i, 2:3] <- rev(prop.table(table(outcomes.post[, i], datamatched$EV), 2)[2, ]) * 100
  tab3.post[i, 4] <- tab3.post[i, 2] - tab3.post[i, 3]	
  tab3.post[i, 5] <- prop.test(table(outcomes.post[, i], datamatched$EV)[2, ], n = apply(table(outcomes.post[, i], datamatched$EV), 2, sum))$p.value
}

tab3 <- cbind(tab3.pre, tab3.post)

tab3 <- tab3[rev(order(tab3[, 9])), ]

### Table 3 ###

print(tab3, digits = 4)

#______________ Table 4, Post-matching, model-based adjustment _____________#

logit.out <- NULL
coeffs.and.sd <- NULL
tab4.top <- NULL
tab4.low <- NULL
sims.setx <- NULL
sims.out <- NULL

for (i in 1:n.outcomes) {
  logit.out[[i]] <- eval(parse(text = paste("logit.out[[", i, "]]<-zelig(" ,
  rownames(tab3)[i] ,
  "~ EV+age.group + I(age.group^2) + age.group:educ + educ + tech + white.collar + not.full.time + male + pol.info", 
  ", data = subset(datamatched, is.na(datamatched[, '",
  rownames(tab3)[i], 
  "']) != TRUE), model = 'logit')", sep = "")))

  coeffs.and.sd[[i]]<-summary(logit.out[[i]])$coefficients[, 1:2]

  tab4.low<-cbind(tab4.low,coeffs.and.sd[[i]])

  x.low <- setx(logit.out[[i]], EV  =  0)
  x.high <- setx(logit.out[[i]], EV  =  1)

  sims.setx[[i]] <- sim(logit.out[[i]], x  =  x.low, x1  =  x.high)
  sims.out[[i]] <- summary(sims.setx[[i]])$qi.stats$fd[1:2]

  tab4.top<-c(tab4.top, sims.out[[i]])
}

tab4.low <- tab4.low[c(rownames(tab4.low)[2:11], rownames(tab4.low)[1]), ]

tab4 <- rbind(tab4.top, tab4.low)

### Table 4 ###

print(tab4, digits = 2)

# print sampe sizes reported in last row of Table 4:

for (i in 1:n.outcomes) {
  print(dim(subset(datamatched, is.na(datamatched[, rownames(tab3)[i], ]) != TRUE))[1])
}

#_______________________ Table 5, Sensitivity analysis _____________________#

matched.pairs <- NULL
matched.pairs1 <- NULL
matched.pairs2 <- NULL
bin <- NULL
tab5 <- NULL

for (i in 1:n.outcomes) {
  matched.pairs[[i]] <- cbind(datamatch[row.names(m.out$match.matrix), rownames(tab3)[i]], datamatch[m.out$match.matrix, rownames(tab3)[i]])
  matched.pairs[[i]] <- data.frame(na.omit(matched.pairs[[i]]))
  matched.pairs1[[i]] <- subset(matched.pairs[[i]], matched.pairs[[i]][, 1] != matched.pairs[[i]][, 2] & matched.pairs[[i]][, 2] == 1)
  matched.pairs2[[i]] <- subset(matched.pairs[[i]], matched.pairs[[i]][, 1] != matched.pairs[[i]][, 2] & matched.pairs[[i]][, 2] == 0)
  bin[[i]] <- binarysens(x = dim(matched.pairs1[[i]])[1], y = dim(matched.pairs2[[i]])[1], Gamma = 3.0, GammaInc = 0.1)
}
   
# In the case of confidence in ballot secrecy, reverse the order of the counts in binarysens (because effect is negative)

bin[[9]]<-binarysens(y = dim(matched.pairs1[[i]])[1],x = dim(matched.pairs2[[i]])[1],Gamma = 3.0,GammaInc = 0.1)

for (i in 1:n.outcomes) {
  tab5<-cbind(tab5, as.matrix(bin[[i]]$bounds[, 2:3]))
}

### Table 5 ###

print(tab5, digits = 2)
