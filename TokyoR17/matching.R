##
## インストールと読み込み
##
install.packages("Matching")
library(Matching)
data(lalonde)

##
## 傾向スコアの算出
##
logi <- glm(treat~., data=lalonde[,-9],family=binomial)


## マッチングでNSWプログラムの効果を推定
nsw1 <- Match(Y=lalonde$re78, Tr=lalonde$treat,X=logi$fitted)
summary(nsw1)

## マッチングのペアの確認
lalonde2 <- lalonde
lalonde2$id <- 1:nrow(lalonde2)
lalonde2$score <- logi$fitted

pair.df <- cbind(lalonde2[nsw1$index.treated, c("id","score")],
                 lalonde2[nsw1$index.control, c("id","score")])

names(pair.df) <- c("t.id", "t.score", "c.id", "c.score")

head(pair.df)

## キャリパーマッチングでNSWプログラムの効果を推定
nsw2 <- Match(Y=lalonde$re78, Tr=lalonde$treat,X=logi$fitted, caliper=T)
summary(nsw2)





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
