# 다항회귀모형(p.122)
tcrime = read.csv("tcrime.txt", header = T)
head(tcrime)
attach(tcrime)
plot(motor, tcratio, pch=19) # 2차 그래프
tcrime_lm = lm(tcratio~motor+I(motor^2), data = tcrime)
summary(tcrime_lm)

maraton = read.csv("maraton.txt", header = T)
head(maraton,2)
plot(maraton$sect, maraton$m1990, pch=19) # 3차 그래프
maraton_lm = lm(m1990~sect+I(sect^2)+I(sect^3), data=maraton)
summary(maraton_lm)

# 가변수 회귀 모형(p.127)
soup=read.table("soup.txt",header=T)
soup[c(1,15,16,27),]
soup$D = factor(soup$D, levels=c(0,1), label=c("Line0","Line1"))
plot(soup$X, soup$Y, type="n") # type="n"이란 아무것도 그리지 않는다는 뜻
points(soup$X[soup$D=="Line1"],soup$Y[soup$D=="Line1"],pch=17,col="BLUE")
points(soup$X[soup$D=="Line0"],soup$Y[soup$D=="Line0"],pch=19,col="RED")
legend("bottomright",legend=levels(soup$D),pch=c(19,17), col=c("RED","BLUE"))
soup_lm = lm(Y~X+D, data=soup) # 기울기는 동일, 절편만 차이
summary(soup_lm)
abline(27.28179, 1.23074, lty=2, col="RED")
abline(27.28179+53.1292, 1.23074, lty=2, col="BLUE")
soup2_lm = lm(Y~X+D+X:D, data=soup) #기울기와 절편 모두 차이 lm(Y~X*D)로 써도 됨
summary(soup2_lm) # X:DLine1에 대한 t0 유의확률이 0.183으로 교호작용 고려 하지 않는 모형 적합하는 것이 맞음


# Practice1 # 2차 다항회귀모형
boron = read.csv("p136.csv", header=T)
head(boron)
boron_lm = lm(Y~X1+X2+I(X1^2)+I(X2^2)+I(X1*X2), data = boron)
summary(boron_lm) # X1*X2의 t값 유의확률은 0.248로 제거하는 것이 맞다

# 단계별 회귀방법 사용
start_boron = lm(Y~1, data=boron)
full_boron = lm(Y~X1+X2+I(X1^2)+I(X2^2), data = boron)
step(start_boron, scope=list(upper=full_boron), data=boron, direction="both")
#step(start_boron, scope=list(upper=boron_lm), data=boron, direction="both") # 이 경우 X1*X2 가 가장 적합하다고 나온다
#summary(lm(Y~I(X1*X2), data=boron)) # R^2 = 0.3996
boron$X1X2 = boron$X1 * boron$X2
#plot(boron$X1X2, boron$Y, pch=19)
summary(lm(formula = Y ~ X1 + I(X2^2) + I(X1^2) + X2, data = boron))


# Practice2
economy = read.csv("p137_2.csv", header=T)
head(economy)
economy$war = ifelse(economy$year>=1941&economy$year<=1945, 1, 0)
plot(economy$income, economy$consumption, type="n")
points(economy$income[economy$war==0], economy$consumption[economy$war==0], pch=17, col="BLUE")
points(economy$income[economy$war==1], economy$consumption[economy$war==1], pch=19, col="RED")
economy_lm = lm(consumption~income+war, data=economy)
summary(economy_lm) #0.9237
economy2_lm = lm(consumption~income*war, data=economy)
summary(economy2_lm) # t검정에서 절편 제외 모든 설명변수가 유의하지 않음
abline(232.8515,0.2478,lty=2, col="BLUE")
abline(232.8515-60.7891,0.2478,lty=2,col="RED")
economy$war=factor(economy$war,levels=c(0,1), label=c("Line0","Line1")) # 이걸 해주지 않으면
legend("topleft",legend=levels(economy$war), pch=c(17,19), col=c("BLUE","RED") ) # legend를 그릴 수 없음
