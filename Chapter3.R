# 산점도 (p.71)
exam = read.table("exam scores_2012.txt", header = T)
str(exam); attach(exam)
plot(mid, final)
summary(exam)
mid[is.na(mid)] = 0
final[is.na(final)] = 0
n=length(mid)
plot(mid, final, pch=20, xlim=c(-5,40), ylim=c(-5,40), col="blue",
     xlab="중간시험", ylab="기말시험", main = "통계적 사고")
# jittering 적용
plot(mid+runif(n,-0.5,0.5), final+runif(n,-0.5,0.5), pch=20, col=rainbow(5),
     xlim=c(-5,40),ylim=c(-5,40),xlab="중간시험",ylab="기말시험",main="통계적 사고")


# 이변량 밀도 (p.76)
exam = exam[!is.na(exam$mid) & !is.na(exam$final),]
plot(exam$mid, exam$final, pch=20, xlim=c(-5,40), ylim=c(-5,40),
     col="blue",xlab="중간시험",ylab="기말시험",main="통계적 사고")
library(KernSmooth)
density=bkde2D(exam, bandwidth=c(1,1))
par(new=T)
contour(density$x1,density$x2,density$fhat,xlim=c(-5,40),ylim=c(-5,40),
        col=heat.colors(7)[7:1],nlevels=7,lwd=2)


# n이 큰 자료의 산점도 (p.78)
library(ggplot2)
data(diamonds)
str(diamonds); attach(diamonds)
par(mfrow=c(1,2))
plot(carat,price,main="diamonds",xlim=c(-0.5,5.5),ylim=c(-1000,21000))
plot(carat,sqrt(price),main="diamonds",xlim=c(-0.5,5.5),ylim=c(0,160)) #비선형성 개선을 위해 반응변수에 제곱근
# n이 너무 커서 새까맣게 칠해져있는 것을 볼 수 있음
library(hexbin)
par(mfrow=c(1,2))
hexbinplot(sqrt(price)~carat,data=diamonds, main="diamonds",
           xlim=c(-0.5,5.5),ylim=c(0,160),xbins=25,aspect=1,colorkey=F)
hexbinplot(sqrt(price)~carat,data=diamonds, main="diamonds",
           xlim=c(-0.5,5.5),ylim=c(0,160),xbins=100,aspect=1,colorkey=F,
           colormap=function(n) magenta(n,225,25))


# 회귀적 관계 (p.81)
exam = read.table("exam scores_2012.txt", header = T)
str(exam); attach(exam)
plot(mid,final,pch=20,xlim=c(-5,40),ylim=c(-5,40), col="blue",
     xlab="중간시험",ylab="기말시험",main="통계적 사고")
abline(lm(final~mid),col="red")
diff=mean(final, na.rm=T)-mean(mid,na.rm=T)
abline(c(diff,1),lty="dotted")

data(diamonds)
attach(diamonds)
diamonds$sqrt.price = sqrt(price)
plot(sqrt(price)~carat, col="gray",main="diamonds",
     xlim=c(-0.5,5.5),ylim=c(0,160))
lines(lowess(diamonds$sqrt.price~diamonds$carat, f=0.1),lwd=2,col="blue")
lines(lowess(diamonds$sqrt.price~diamonds$carat,f=0.25),lwd=2,col="red",lty="dotted")

plot(price~carat, col="gray", main="diamonds",
     xlim=c(-0.5,5.5),ylim=c(-1000,21000))
lines(lowess(diamonds$price~diamonds$carat, f=0.1),lwd=2,col="blue")
lines(lowess(diamonds$price~diamonds$carat, f=0.25), lwd=2, col="red",lty="dotted")


# 모자이크 플롯(p.84)
data(Titanic)
str(Titanic)
addmargins(apply(Titanic,c(1,4),sum))
mosaicplot(~Class+Survived, data=Titanic,color=c("grey","red"))
mosaicplot(~Sex+Survived,data=Titanic,color=c("grey","red"))
mosaicplot(~Age+Survived, data=Titanic,color=c("grey","red"))
mosaicplot(~Class+Survived,data=as.table(Titanic[,"Male","Adult",]),
           color=c("grey","red"),main="Male+Adult") # 성인남성에 대해 클래스별 생존유무 확인
mosaicplot(~Class+Survived,data=as.table(Titanic[,"Female","Adult",]),
           color=c("grey","red"),main="Male+Adult") # 성인여성에 대해 클래스별 생존유무 확인
mosaicplot(~Class+Sex+Survived,dir=c("v","v","h"), data=Titanic[,,"Adult",],off=c(1,2),color=c("grey","red"),
           main="Adult")


# 나무지도(p.89)
library(treemap)
GNI_2010 = read.table("GNI-2010.txt",header=T)[1:104,]
str(GNI_2010)
treemap(GNI_2010, index=c("sector","item"),
        vSize="principal",vColor="yield", type="value", bg.labels="yellow",
        title="Portpolio Evaluation GNI.2010[1:104]")
GNI_2010$yield.total = GNI_2010$principal*as.numeric(GNI_2010$yield)
GNI_2010_a = aggregate(GNI_2010[,3:5],by=list(GNI_2010$sector),sum)
GNI_2010_a$yield.avg = GNI_2010_a$yield.total/GNI_2010_a$principal
treemap(GNI_2010_a,index=c("Group.1"),vSize="principal",vColor="yield.avg",
        type="value",bg.labels="yellow",title="Portpolio Evaluation GNI_2010[1:104]")


# Practice1
library(MASS)
data(geyser)
View(geyser)
plot(duration~waiting, data=geyser, xlim=c(40,115), ylim=c(0,6), pch=20,
     xlab="대기시간", ylab="분출시간", main="Old Faithful 간헐천", col="blue")
library(KernSmooth)
par(new=T)
density = bkde2D(geyser, bandwidth=c(5,0.5))
contour(density$x1,density$x2,density$fhat,xlim=c(40,115),ylim=c(0,6),
        col=rainbow(7)[7:1],nlevels=10,lwd=2)

#Practice2
n=100000
x = c(rnorm(n,-1,1),rnorm(2*n,0.5,0.5))
y = c(rnorm(n,-1,1),rnorm(2*n,0.5,0.5))
plot(y~x)
library(hexbin)
hexbinplot(y~x, colorkey=F)

#Practice3
data(UCBAdmissions)
str(UCBAdmissions)
addmargins(apply(UCBAdmissions, c(1,3), sum))
addmargins(apply(UCBAdmissions, c(2,3), sum))
mosaicplot(~Gender+Admit, data=UCBAdmissions, color = c("green","red"))
mosaicplot(~Dept+Gender+Admit, dir=c("v","v","h"),data=UCBAdmissions,color=c("green","red"), off=c(1,2), main="Dept별 UCBAdmissions")


#Practice4
GNI_2010b = read.table("GNI-2010.txt",header=T)[105:209,]
str(GNI_2010b)
treemap(GNI_2010b,index=c("sector","item"),vSize="principal",vColor="yield",
        type="value",bg.labels="yellow",title="Portpolio Evaluation GNI_2010[105:209]")
GNI_2010b$yield.total = GNI_2010b$principal*as.numeric(GNI_2010b$yield)
GNI_2010b_a=aggregate(GNI_2010b[,3:5],by=list(GNI_2010b$sector),sum)
GNI_2010b_a$yield.avg = GNI_2010b_a$yield.total/GNI_2010b_a$principal
treemap(GNI_2010b_a, index=c("Group.1"),vSize="principal",vColor="yield.avg",
        type="value",bg.labels="yellow",title="Portpolio Evaluation GNI_2010[105:209]")
