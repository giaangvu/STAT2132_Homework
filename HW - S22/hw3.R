### Yu's HW3 - Logistic Reg
library(MASS)
##1
setwd("/Users/giangvu/Desktop/STAT 2132 - Applied Stat Method 2/SPR22/hw")
bottle <- read.delim("CH14PR11.txt", header = F, sep = "")
colnames(bottle) <- c("X","n","Y")
#(a)
plot(bottle$X, bottle$Y/bottle$n,
     xlab = "Deposit",
     ylab = "Estimated Proportions")
#(b) aggregate data -> have to add weights
bottle.fit <- glm(Y/n~X, family = binomial(link="logit"), weights = n, data = bottle)
bottle.sum <- summary(bottle.fit)

beta.hat <- bottle.fit$coefficients
se <- bottle.sum$coefficients[,2]

#increase X by 10
alpha <- 0.05
exp(beta.hat[2]*10)
CI.beta <- exp(10*beta.hat[2] + 10*se[2]*c(qnorm(alpha/2), qnorm(1-alpha/2)))

#fit0 for deviance goodness of fit test
bottle.fit0 <- glm(Y/n~1,family = binomial(link="logit"),weights=n,data = bottle)
lr.stat <- bottle.fit0$deviance - bottle.fit$deviance
pvalue.H0.lr <- pchisq(q = lr.stat, df = 1, lower.tail = F) 
