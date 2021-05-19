# 오차의 등분산성
goose = read.table("goose.txt", header=T)
head(goose)
goose.lm = lm(photo~obsA, data=goose)
plot(goose.lm$fitted, goose.lm$resid, pch=19)
library(car)
ncvTest(goose.lm) # 스코어검정 p=2.22e-16으로 등분산 가정 기각

# 회귀모형의 선형성
tree = read.table("tree.txt", header=T)
head(tree,3)
tree.lm = lm(V~I(D^2)+D+H, data=tree) # plot(tree$D, tree.lm$resid)에서 비선형성 모양이 보임에 따라 수정
par(mfrow=c(1,2))
plot(tree$D, tree.lm$resid, pch=19)
plot(tree$H, tree.lm$resid, pch=19)

# 오차의 정규성
goose.lm = lm(photo~obsA, data=goose)
qqPlot(goose.lm)
library(mvnormtest)
goose.rstudent = rstudent(goose.lm)
shapiro.test(goose.rstudent)

# 반응변수의 변환
energy = read.table("energy.txt", header=T)
head(energy,3)
energy.lm = lm(Y~X, data=energy)
plot(energy.lm$fitted, energy.lm$resid, pch=19)
library(MASS)
boxcox(Y~X, data=energy, lambda=seq(-2,2,1/2), plotit=TRUE)

# Problem2
plot(tree.lm$fitted, tree.lm$resid, pch=19)
library(car)
ncvTest(tree.lm)
tree_norm = boxcox(V~D+H, data=tree, lambda=seq(-2,2,1/2), plotit=TRUE)
tree_lambda = tree_norm$x[which.max(tree_norm$y)]
tree_lm_norm = lm(V^tree_lambda~D+H, data=tree)
summary(tree_lm_norm)
plot(tree_lm_norm$fitted, tree_lm_norm$resid, pch=19)
ncvTest(tree_lm_norm)
tree.lm = lm(V~D+H, data=tree)
summary(tree.lm)

# Problem4
church = read.csv("p187.csv", header=T)
head(church)
plot(area~size, data=church)
church.lm = lm(area~size, data=church) # 비선형성
plot(church.lm$fitted, church.lm$resid, pch = 19)
library(car)
ncvTest(church.lm) # 등분산성 위배된다는 것 확인
qqPlot(church.lm)
library(mvnormtest)
church.rstudent = rstudent(church.lm)
shapiro.test(church.rstudent) # 정규성 만족
church_norm=boxcox(area~size, data=church, lambda = seq(-2,2,1/2), plotit=TRUE)
church_lambda = church_norm$x[which.max(church_norm$y)]
church_norm_lm = lm(area^church_lambda ~ size, data=church)
summary(church_norm_lm)
plot(church_norm_lm$fitted, church_norm_lm$resid, pch=19)
identify(church_norm_lm$fitted, church_norm_lm$resid) # 6번, 11번 값 벗어난 것으로 판단됨
outlierTest(church_norm_lm) # 하지만 outlierTest 시, 이상치는 없는 것으로 판단됨
