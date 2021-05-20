# 이항자료와 비율에 대한 분석모형: 로지스틱 회귀모형
glider = read.csv("sugar_glider_binomial.csv", header=T)
head(glider,3)
logit_ml = glm(occurr~p_size_km+con_metric, family=binomial(link=logit), data=glider)
summary(logit_ml)
1-pchisq(68.994-54.661,2) # Null Deviance와 Residual Deviance 사이의 유의 차 확인
logit_m0 = glm(occurr~1, family=binomial(link=logit), data=glider)
anova(logit_m0, logit_ml, test="Chisq")
1-pchisq(54.661,47) # Residual Deviance의 모형 적합도 확인
logit_m2 = glm(occurr~p_size_km, family=binomial(link=logit), data=glider)
summary(logit_m2)
anova(logit_m2, logit_ml, test="Chisq") # con_metric을 추가하는 것은 유의하지 않음 
AIC(logit_m2, logit_ml) # AIC값 : logit_m2 : 59.7 / logit_ml : 60.7
library(MASS)
stepAIC(logit_ml, direction="both") # 변수선택

# 로지스틱 회귀모형의 해석
p_size = seq(20,230,1)
het_eta = predict(logit_m2, list(p_size_km=p_size), type="link")
par(mfrow=c(1,2))
plot(glider$p_size_km, glider$occurr, xlab="구획의 크기(x)", ylab="hat pi(x) \\ occurr", sub="(a)", pch=20)
lines(p_size, exp(het_eta)/(1+exp(het_eta)), lwd=1.5, col="red")
glider_g = read.csv("sugar_glider_binomial_g.csv")
plot(glider_g$p_size_med, glider_g$cases/glider_g$count, xlab="구획의 크기(x)", ylab="hat pi(x) \\ sample prop.", sub="(b)",pch=20,col="blue", ylim=c(0,1))
lines(p_size, exp(het_eta)/(1+exp(het_eta)), lwd=1.5, col="red")

#관심사건 수, 전체사건 수 - 관심사건 수
y = cbind(glider_g$cases, glider_g$count-glider_g$cases)
logit_mg = glm(y~glider_g$p_size_med, family=binomial(link=logit))
summary(logit_mg)

# 승산비
exp(coef(logit_m2))
exp(confint(logit_m2, parm="p_size_km",level=0.95))
x = 150
predict(logit_m2, list(p_size_km=x), type="response") # 출현할 확률
predict(logit_m2, list(p_size_km=x), type="link") # link 함수의 값

# (1-a)100% Wald 신뢰구간
vcov(logit_m2) # 분산-공분산 행렬
coef(logit_m2)
sqrt(vcov(logit_m2)[1,1]+x^2*vcov(logit_m2)[2,2]+2*x*vcov(logit_m2)[2,1])


# 사례-대조 연구
data(esoph)
head(esoph,3)
attach(esoph)
y = cbind(ncases,ncontrols)
levels(alcgp)
n.alcgp = factor(alcgp, ordered=FALSE)
levels(n.alcgp) = c("0-39","40-79","80-119","120+")
levels(tobgp)
n.tobgp = factor(tobgp, ordered=FALSE)
levels(n.tobgp) = c("0-9","10-19","20-29","30+")
levels(agegp)
n.agegp=factor(agegp, ordered=FALSE)
levels(n.agegp) = c("25-34","35-44","45-54","55-64","65-74","75+")
logit_tob = glm(y~n.tobgp, family=binomial(link=logit))
summary(logit_tob) # n.tobgp10-19, n.tobgp20-29 의 계수가 거의 일치
n3.tobgp=n.tobgp
levels(n3.tobgp)[2:3] = "10-29"
levels(n3.tobgp)
logit_tob3 = glm(y~n3.tobgp, family=binomial(link=logit))
summary(logit_tob3)
anova(logit_tob3, logit_tob, test="Chisq") # 유의확률 0.9441로 n3.tobgp로 진행
logit_alcgp = glm(y~n.alcgp, family=binomial(link=logit))
summary(logit_alcgp)
logit_agegp = glm(y~n.agegp, family=binomial(link=logit))
summary(logit_agegp) # n.agegp55-64, n.agegp65-74, n.agegp75+ 의 계수가 거의 비슷함
n4.agegp = n.agegp
levels(n4.agegp)[4:6] = "55+"
levels(n4.agegp)
logit_agegp4 = glm(y~n4.agegp, family=binomial(link=logit))
summary(logit_agegp4)
anova(logit_agegp4, logit_agegp, test="Chisq") # 유의확률 0.8809로 n4.agegp로 진행

logit_main = glm(y~n.alcgp+n3.tobgp+n4.agegp, family=binomial(link=logit))
summary(logit_main)
exp(coef(logit_main))
exp(confint(logit_main, parm="n.alcgp40-79",level=0.95))

# 로지스틱회귀모형 적합도의 시각적 검토
attach(glider)
logit_m2 = glm(occurr~p_size_km, family=binomial(link=logit))
plot(p_size_km, occurr, type="n",xlab="구획의 크기(x)", ylab="hat pi(x) \\ occurr")
rug(jitter(p_size_km[occurr==0]))
rug(jitter(p_size_km[occurr==1]), side=3)

# 확률 추정 곡선 그리기
x = seq(23,226,1)
hat.pi = predict(logit_m2, list(p_size_km = x), type="response")
lines(x,hat.pi,col="red",lwd=1.5)
cl.intr = cut(p_size_km,5)
table(cl.intr)
tapply(occurr, cl.intr, mean)
s.prop = tapply(occurr, cl.intr, mean)
s.prop = as.vector(s.prop)
md.size = tapply(p_size_km, cl.intr, median)
md.size = as.vector(md.size)
lines(md.size, s.prop, type="p", pch=20, cex=1.5, col="blue")
#표본 비율 +- 표준오차 구하고 그리기
se.p = sqrt(s.prop*(1-s.prop)/table(cl.intr))
up.p = s.prop+as.vector(se.p)
low.p = s.prop-as.vector(se.p)
for(i in 1:5){
  lines(c(md.size[i], md.size[i]), c(up.p[i],low.p[i]), col="blue")
}


# 프로빗 모형
glider = read.csv("sugar_glider_binomial.csv")
attach(glider)
probit_m = glm(occurr~p_size_km, family=binomial(link=probit))
summary(probit_m)
plot(p_size_km, occurr, type="n",xlab="구획의 크기(x)", ylab="hat pi(x) \\ occurr")
rug(jitter(p_size_km[occurr==0]))
rug(jitter(p_size_km[occurr==1]), side=3)
x = seq(23,226,1)
hat.pi.p = predict(probit_m, list(p_size_km=x), type="response") # 프로빗 모형
lines(x, hat.pi.p, col="blue", lty=1, lwd=1.5)
hat.pi.l = predict(logit_m2, list(p_size_km=x), type="response") # 로지스틱 회귀모형
lines(x, hat.pi.l, col="red", lty=2, lwd=1.5)
lines(md.size, s.prop, type="p", pch=20, cex=1.5, col="blue")
legend("bottomright", c("프로빗모형","로지스틱회귀모형"), lty=c(1,2),col=c("blue","red"), cex=0.7)
predict(probit_m, list(p_size_km = 150), type="response")


# 개수형 자료에 대한 분석 모형 : 로그 선형 모형
library(MASS)
data(Traffic)
head(Traffic,3)
attach(Traffic)
day = as.factor(day)
year = as.factor(year)
log_m = glm(y~limit+day+year, family=poisson(link=log))
summary(log_m)
log_m1 = glm(y~limit+day, family=poisson(link=log))
summary(log_m1)
exp(coef(log_m1, parm="limit"))
exp(confint(log_m1, parm="limityes", level=0.95))
par(mfrow=c(2,2))
plot(log_m1)

# 율에 대한 분석 모형 : 로그 선형 모형
melanoma = read.csv("melanoma.csv")
head(melanoma,3)
attach(melanoma)
log_rt = glm(cases~age+region-offset(log(total)), family=poisson(link=log))
summary(log_rt)
exp(coef(log_rt))
exp(confint(log_rt, parm="regionsouth", level=0.95))
