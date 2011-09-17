install.packages("Matching")
library(Matching)

##
## Matching
##

data(lalonde)

Y78 <- lalonde$re78
Tre <- lalonde$treat
logi <- glm(treat~., data=lalonde[,-9],family=binomial)

## default
summary(Match(Y=Y78, Tr=Tre,X=logi$fitted))

## 
summary(Match(Y=Y78, Tr=Tre,X=logi$fitted,M=2))

## caliper matching
summary(Match(Y=Y78, Tr=Tre,X=logi$fitted,caliper=T))

##
## kernel matching
## 

kmy <- lalonde$re74
ivec1 <- lalonde$treat
estp <- logi$fitted
km <- cbind(kmy,estp, ivec1)
km1 <- subset(km, ivec1==1)
km2 <- subset(km, ivec1==0)
km1x <- km1[,2]
km1y <- km1[,1]
km2x <- km2[,2]
km2y <- km2[,1]
bw1 <- 1.06*(nrow(km1))^(-0.2) * sd(km1x)
bw2 <- 1.06*(nrow(km2))^(-0.2) * sd(km2x)
esty1 <- ksmooth(x=km1x,y=km1y,kernel="normal",
                 bandwidth=bw1,x.points=km2x)
esty0 <- ksmooth(x=km2x,y=km2y,kernel="normal",
                 bandwidth=bw2,x.points=km1x)

head(esty1$y)

head(esty0$y)
