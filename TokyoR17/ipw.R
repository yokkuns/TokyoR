##
## インストールと読み込み
##
library(Matching)
data(lalonde)

##
## 傾向スコアの算出
##
logi <- glm(treat~., data=lalonde[,-9],family=binomial)

##
## 傾向スコアの逆数で重みの作成
##
ivec1 <- lalonde$treat
ivec2 <- rep(1, nrow(lalonde)) - ivec1
ivec <- cbind(ivec1,ivec2)

iestp1 <- (ivec1/logi$fitted) * (length(ivec1)/sum(ivec1))
iestp2 <- (ivec2/logi$fitted) * (length(ivec2)/sum(ivec2))
iestp <- iestp1 + iestp2

##
## IPW推定量
##
ipwe <- lm(re78~ivec-1, weights=iestp, data=lalonde)

