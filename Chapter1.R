# R 시각화 사례 (p.11-13)
kings = read.table("chosun kings.txt", header = T)
str(kings); attach(kings)
hist(period)
# 10년 단위를 5년 단위로 변경, (10,20]이 아니라, [10,20)으로 변경, 수평축, 수직축 변경
hist(period, xlim = c(0,60), ylim = c(0,10), nclass = 14, right=F,
     main = "조선왕조", xlab = "재위기간(년)", ylab = "빈도")
# 컬러 삽입 0-10은 lightblue, 10-40은 royalblue, 40-70은 navyblue
hist(period, xlim = c(0,60), ylim = c(0,10), nclass = 14, right=F,
     main = "조선왕조", xlab = "재위기간(년)", ylab = "빈도",
     col=c(rep("lightblue",2), rep("royalblue",6), rep("navyblue", 6)))

# R의 컬러 체계 (p.14)
color = c("#FF0000", "#FFFF00", "#00FF00", "#00FFFF", "#0000FF", "#FF00FF")
pie(rep(1,6), col = color, labels=color)
par(new=T)
pie(rep(1,1), col="white", radius=0.5, labels = "")

par(mfrow=c(5,1), mar=c(1,1,1,1))
n=10
barplot(rep(1,n),col=rainbow(n,alpha=1), axes=F, main="rainbow colors")
barplot(rep(1,n),col=heat.colors(n,alpha=1),axes=F, main="heat colors")
barplot(rep(1,n),col=terrain.colors(n,alpha=1),axes=F, main="terrain colors")
barplot(rep(1,n),col=topo.colors(n, alpha=1), axes=F, main="topo colors")
barplot(rep(1,n),col=cm.colors(n, alpha=1), axes=F, main="cyan-magenta colors")

library(RColorBrewer)
display.brewer.all(type="seq")

# R 그래프의 기본 요소 (p.20)
plot(c(0,0),c(28,60), type="n", xlim=c(0,28), ylim=c(-5,65),
     xlab="순서", ylab="재위기간", main="조선왕조") # type이 n이기 때문에 실제 점은 찍히지 않는다.
points(1:27, period, pch=15, col="red") # 점 찍기
segments(1:27, rep(0,27), 1:27, period, lwd=3, col="red") # 줄 긋기
#segments(x1,y1,x2,y2)는 점 (x1,y1)과 (x2,y2)를 잇는 선을 넣는다
abline(h=c(0,mean(period)),lty="dotted",lwd=1,col="blue")

# R 그래프의 기본 요소 - 사각형 넣기 (p. 25)
p = cumsum(period) #cumsum() : cumulative sums 누적합
plot(1:27,p,type="n",xlab="순서",ylab="누적연수", main="조선왕조")
rect(0,0,1,p[1],col="royalblue",border="navyblue")
for(i in 2:27)
  rect(i-1,p[i-1],i,p[i],col="royalblue",border="navyblue")
segments(0,0,27,518, lty="dotted")

polygon(c(0,0,1,1),c(0,p[1],p[1],0), col=rainbow(27)[1])
for(i in 2:27)
  polygon(c(i-1,i-1,i,i),c(p[i-1],p[i],p[i],p[i-1]),col=rainbow(27)[i])


# Practice1
pie(rep(1,12),col=rainbow(12),border="white", labels="", init.angle=90, clockwise=T)
par(new=T)
pie(rep(1,1),radius=0.5,col="white",border="white",labels="")
# 동일한 모형을 만들기 위해 90도 회전 및 시계방향 전환


# Practice2
plot(0:5,0:5,type="n",xlab="",ylab="")
for(i in 1:5)
  for(j in 1:5)
    rect(i-1,j-1,i,j,col=rainbow(25)[(j-1)*5+i])