
#a
#logit(y/m=pi)=B0 + B1*log2(x), Yi | Xi are indie, pi(log2(Xi)) = P(Yi = 1 | log2(Xi)) 
#B0 is log odds of Yi = 1 when log2(Xi) = 0
#B1 is the log odds ratio between log dose Xi +1 and log dose Xi.

#b
#create df
toxic <- data.frame(log2x = c(0,1,2,3,4,5),
                    mortality = c(0/7,2/9,3/8,5/7,7/9,10/11))

#fit logistic model #could use quasi well
toxic.fit <- glm(mortality ~ log2x, family = binomial(link = "logit"), data = toxic) 
#just use binomial link here, quasibin is for dispersion issue.
toxic.sum <- summary(toxic.fit)

#plot raw data
plot(toxic$log2x, toxic$mortality, xlab="Log Dose", ylab="Mortality Rate")

#plot fitted
lines(toxic$log2x, fitted(toxic.fit), col="red")
#model assumed in part a seems reasonable

#c
#(ii)
#new model with scaled log2x
toxic.r <- toxic
toxic.r$log2x <- toxic.r$log2x - 4
toxic.fit.r <- glm(mortality ~ 0+log2x, family = quasibinomial(link = "logit"), data = toxic.r)

#likelihood test
lr.stat <- toxic.fit.r$deviance - toxic.fit$deviance #H0: reduced model with log2(gam)=4 , H1: original model
pvalue.H0.lr <- pchisq(q = lr.stat, df = 1, lower.tail = F) #=> FTR H0, choose reduced model

