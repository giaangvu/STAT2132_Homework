###HW5 - Yu Cheng
library(dplyr)
library(nnet)

setwd("/Users/giangvu/Desktop/STAT 2132 - Applied Stat Method 2/SPR22/hw")
ses <- read.delim("hsbdemo.txt",header = T, sep="")
ses$PROG <- as.factor(ses$PROG)
ses$SES <- as.factor(ses$SES)

#a
table(ses$SES,ses$PROG) #rows are SES, cols are school type

#b
ses %>% group_by(PROG) %>% summarise(mean_write=mean(WRITE)) 

#c
ses.c <- ses[ses$PROG!=3,]
ses.c$Yc <- ifelse(ses.c$PROG==2,0,1)
ses.c.fit <- glm(Yc~SES+WRITE, family=binomial(link = "logit"), data = ses.c)
summary(ses.c.fit)

#d
ses.d <- ses[ses$PROG!=1,]
ses.d$Yd <- ifelse(ses.d$PROG==2,0,1)
ses.d.fit <- glm(Yd~SES+WRITE, family=binomial(link = "logit"), data = ses.d)
summary(ses.d.fit)

#e
ses$Ye <- relevel(ses$PROG,ref=2)
ses.e.fit <- multinom(Ye~SES+WRITE,data = ses)
summary(ses.e.fit)

#f
ses.f.fit <- multinom(Ye~as.numeric(SES)+WRITE,data = ses)
summary(ses.f.fit)
