#Yu's 2132 HW9

#ONE WAY RANDOM EFFECT MODEL

filling <- read.delim("/Users/giangvu/Desktop/STAT 2132 - Applied Stat Method 2/SPR22/hw/filling.txt",
                      header = F, sep = "")
colnames(filling) <- c("Y", "machine", "carton")

filling.ranmod <- lmer(Y ~ (1|machine), data = filling)
summary(filling.ranmod)
#25.5
#r = 6, n = 20 (n here is replicates per machine, and we have balanced data)
#(b)
# Random effects:
#   Groups   Name        Variance Std.Dev.
# machine  (Intercept) 0.02134  0.1461  #this is sigma_mu
# Residual             0.03097  0.1760  #this is sigma
# Number of obs: 120, groups:  machine, 6

#Test H0: sigma_mu = 0 is equivalent to testing all 6 machines have same mu
#so can just use the usual anova model with lm to carry out F-test
#F stat = 14.785, p-val very small -> Reject H0
anova(lm(Y~as.factor(machine),data=filling))
#MSE = 0.03097
#MSTR = 0.45787

#(c) Y_bar.. +- t(5,0.975)*(sqrt(MSTR/rn))
mean(filling$Y) + c(qt(1-0.975,5)*sqrt(0.45787/(6*20)), qt(0.975,5)*sqrt(0.45787/(6*20)))

#25.6
#(a) ICC and its CI
0.1461/(0.1461+0.1760) #ICC
L <- (1/20) * ((0.45787/0.03097) * (1/qf(0.975,5,6*19)) - 1)
U <- (1/20) * ((0.45787/0.03097) * (1/qf(0.025,5,6*19)) - 1)
c(L/(1+L), U/(1+U)) #CI for ICC

#(b) sigma
0.03097 #estimate for sigma^2
c(6*19*0.03097/qchisq(0.975,6*19), 6*19*0.03097/qchisq(0.025,6*19)) #CI for sigma

#(c) sigma_mu
0.02134 #estimate

#(d) Satterthwaite CI for sigma_mu
L_hat = (0.45787-0.03097) / 20 #(MSTR - MSE)/n
df = (20^2 * L_hat^2) / ((0.45787^2/5)+(0.03097^2)/(6*19))
c(df*L_hat/qchisq(0.975,df), df*L_hat/qchisq(0.025,df)) #CI for sigma_mu


