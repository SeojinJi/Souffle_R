# 해군병원의 인력자료(p.94)
hospital = read.csv("hospital.csv", header=T)
head(hospital)
hospital_lm = lm(Y~X1+X2+X3+X4+X5, data = hospital)
summary(hospital_lm)
anova(hospital_lm) 
# 결과상, X1,X3,X5는 회귀계수 추정값의 부호가 양이 나와야 하나, 음이 나온 것 확인
# 다중 공선성 의심
library(fmsb)
VIF(lm(X1~X2+X3+X4+X5, data = hospital)) #8227.274
VIF(lm(X2~X1+X3+X4+X5, data = hospital)) #11.7382
VIF(lm(X3~X1+X2+X4+X5, data = hospital)) #7416.641
VIF(lm(X4~X1+X2+X3+X5, data = hospital)) #19.57455
VIF(lm(X5~X1+X2+X3+X4, data = hospital)) #3.837986
cor(hospital[,-6]) # 독립변수들 간의 상관계수 : X1~X3의 상관계수는 0.999
# X1 제외하고 회귀계수 다시 구하기
summary(lm(Y~X2+X3+X4+X5, data = hospital))
VIF(lm(X2~X3+X4+X5, data = hospital)) #7.596641
VIF(lm(X3~X2+X4+X5, data = hospital)) #23.17376
VIF(lm(X4~X3+X2+X5, data = hospital)) #12.83022
VIF(lm(X5~X3+X4+X2, data = hospital)) #3.398222

# HALD의 자료(p.105)
hald = read.csv("hald.csv", header=T)
head(hald)
library(leaps)
all_lm = regsubsets(Y~.,data=hald) # 모든 가능한 회귀
(rs=summary(all_lm))
names(rs)
rs$rsq
rs$adjr2
rs$cp

start_lm = lm(Y~1, data = hald) # 절편만을 포함하는 모형
full_lm = lm(Y~., data=hald) # 전체를 포함하는 모형
step(start_lm, scope=list(lower=start_lm, upper=full_lm), direction="forward") # 앞으로부터 선택법
step(full_lm, data=hald, direction="backward") #뒤로부터 제거법
step(start_lm, scope=list(upper=full_lm), data=hald, direction="both") # 단계별 회귀방법

# Practice1
evaporation = read.csv("evaporation.csv",header =T)
head(evaporation)
evaporation_lm = lm(EVAP~MAXST+MINST+AVST+MAXAT+MINAT+AVAT, data = evaporation)
summary(evaporation_lm)
evap_co=cor(evaporation[,-1])
evap_co=evap_co[,-7]
evap_co

VIF(lm(MAXST~MINST+AVST+MAXAT+MINAT+AVAT, data=evaporation)) #21.96
VIF(lm(MINST~MAXST+AVST+MAXAT+MINAT+AVAT, data=evaporation)) #8.61
VIF(lm(AVST~MINST+MAXST+MAXAT+MINAT+AVAT, data=evaporation)) #27.93
VIF(lm(MAXAT~MINST+AVST+MAXST+MINAT+AVAT, data=evaporation)) #14.40
VIF(lm(MINAT~MINST+AVST+MAXAT+MAXST+AVAT, data=evaporation)) #10.09
VIF(lm(AVAT~MINST+AVST+MAXAT+MINAT+MAXST, data=evaporation)) #17.81

start = lm(EVAP~1, data = evaporation)
full = lm(EVAP~MAXST+MINST+AVST+MAXAT+MINAT+AVAT, data = evaporation)
step(start, scope=list(lower=start, upper=full), direction="forward")
result_lm = lm(EVAP~MAXST, data=evaporation)
summary(result_lm)
step(full, data=evaporation, direction="backward")
summary(lm(formula = EVAP ~ MAXST + MINST + MINAT, data = evaporation))

# Practice3
accident = read.csv("accident.csv", header=T)
start_ac = lm(Y~1, data = accident)
full_ac = lm(Y~., data = accident)

step(start_ac, scope=list(lower=start_ac, upper=full_ac), direction="forward") #앞으로부터 선택법
summary(lm(formula = Y ~ X9 + X1 + X4 + X8 + X12, data = accident)) #R^2 = 0.745

step(full_ac, data=evaporation, direction="backward") # 뒤로부터 제거법
summary(lm(formula = Y ~ X1 + X3 + X4 + X8 + X9 + X11, data = accident)) #R^2 = 0.7466

step(start_ac, scope=list(upper=full_ac), data=accident, direction="both")# 단계별 회귀방법
summary(lm(formula = Y ~ X9 + X1 + X4 + X8 + X12, data = accident)) #R^2 = 0.745

library(leaps)
all_ac = regsubsets(Y~., data=accident)
(rs_ac = summary(all_ac))
rs_ac$rsq
rs_ac$adjr2
rs_ac$cp

accident_co = cor(accident[,-1])
accident_co
