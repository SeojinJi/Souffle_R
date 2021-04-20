# 원그래프 그리기 (p.33)
Blood = c("A","B","B","A","A","O","A","AB","O","O",
          "O","A","A","B","AB","A","O","B","A","B",
          "B","A","B","A","B","AB","B","A","O","AB",
          "O","B","A","B","A","O","B","A","A","A",
          "A","O","A","O","O","B","B","O","AB","A",
          "B","AB","B","O","O","O","AB","O","O","B",
          "A","A","O","A","B","O","A","O","B","O",
          "A","B","O","AB","B","B","A","O","B","A",
          "B","B","O","AB","B","A","AB","A","B","A",
          "A","O","O","A","A","O","AB","A","A","O")
Blood
blood_sort = sort(table(Blood), decreasing=T)
blood_sort
slices=c("red","blue","yellow","green")
pie(blood_sort, col=slices, radius=1, main="원그래프") # 라벨 표기가 없어도 A,B,O,AB는 보임

# 국회의원 선거 정당별 분포 원그래프 그리기(p.35)
require(grDevices) #grDevices : 색상팔레트 
pie_vote = c(0.5067, 0.0167, 0.0100, 0.0433, 0.4233)
names(pie_vote) = c("새누리 152명","선진 5명","무 3명","진보 13명","민주 127명")
par(mfrow=c(1,2))
pie(pie_vote)
pie(pie_vote, col=c("red3","blue","green3","magenta","yellow", main="19대 국회의원 선거"))
pie(pie_vote, col=c("red3","blue","green3","magenta","yellow", main="19대 국회의원 선거"))
par(new=T)
pie(c(1,1),radius=0.7, col="white",label=NA, border=NA)
text(0,0,"총 300석")


# 막대그래프 그리기(p.37)
Blood = c("A","B","B","A","A","O","A","AB","O","O",
          "O","A","A","B","AB","A","O","B","A","B",
          "B","A","B","A","B","AB","B","A","O","AB",
          "O","B","A","B","A","O","B","A","A","A",
          "A","O","A","O","O","B","B","O","AB","A",
          "B","AB","B","O","O","O","AB","O","O","B",
          "A","A","O","A","B","O","A","O","B","O",
          "A","B","O","AB","B","B","A","O","B","A",
          "B","B","O","AB","B","A","AB","A","B","A",
          "A","O","O","A","A","O","AB","A","A","O")
Blood
blood_sort = sort(table(Blood), decreasing=T)
blood_sort
slices=c("red","blue","yellow","green")
barplot(blood_sort, col=slices, main="혈액형별 막대그래프")

pie_vote = c(0.5067, 0.0167, 0.0100, 0.0433, 0.4233)
names(pie_vote) = c("새누리 152명","선진 5명","무 3명","진보 13명","민주 127명")
barplot(pie_vote, col=c("red3","blue","green3","magenta","yellow"), main="정당별 막대그래프")


# 히스토그램 그리기(p.39)
bile = read.table("bile.txt",header=T)
attach(bile)
str(bile)
n=length(담즙의과포화비율)
sort(담즙의과포화비율)
sort(담즙의과포화비율,decreasing=T)
par(mfrow=c(2,1))
hist(담즙의과포화비율,main=NULL,freq=F)
rug(jitter(담즙의과포화비율))
hist(담즙의과포화비율,main=NULL,right=F)
rug(jitter(담즙의과포화비율))

hist(담즙의과포화비율,prob=T,right=F,ylab="상대도수",main=NULL)
lines(density(담즙의과포화비율, bw=5),col="red")
rug(담즙의과포화비율, col="blue")

# 간헐온천 히스토그램 그리기(p.42)
eruption_length=c(4.37,3.87,4.00,4.03,3.50,4.08,2.25,4.70,
                  1.73,4.93,1.73,4.62,3.43,4.25,1.68,3.92,3.68,3.10,4.03,
                  1.77,4.08,1.75,3.20,1.85,4.62,1.97,4.50,3.92,4.35,2.33,
                  3.83,1.88,4.60,1.80,4.73,1.77,4.57,1.85,3.52,4.00,3.70,
                  3.72,4.25,3.58,3.80,3.77,3.75,2.50,4.50,4.10,3.70,3.80,
                  3.43,4.00,2.27,4.40,4.05,4.25,3.33,2.00,4.33,2.93,4.58,
                  1.90,3.58,3.73,3.73,1.82,4.63,3.50,4.00,3.67,1.67,4.60,
                  1.67,4.00,1.80,4.42,1.90,4.63,2.93,3.50,1.97,4.28,1.83,
                  4.13,1.83,4.65,4.20,3.93,4.33,1.83,4.53,2.03,4.18,4.43,
                  4.07,4.13,3.95,4.10,2.72,4.58,1.90,4.50,1.95,4.83,4.12)
n=length(eruption_length)
range(eruption_length)
sort(eruption_length) #정렬
mean(eruption_length)
var(eruption_length)
class1=seq(1.35,5.35,by=0.5)
class2=seq(1.5,5.0,by=0.5)
cat_class1 = cut(eruption_length, breaks=class1)
t1 = table(cat_class1)
cat_class2 = cut(eruption_length, breaks=class2)
t2 = table(cat_class2)

m1 = c(1.6,2.1,2.6,3.1,3.6,4.1,4.6,5.1)
f1 = c(16,12,2,5,20,30,21,1)
mean1 = sum(m1*f1)/sum(f1)
var1 = sum((m1-mean1)^2*f1)/sum(f1)

m2 = c(1.75,2.25,2.75,3.25,3.75,4.25,4.75)
f2 = c(24,5,3,8,25,27,15)
mean2 = sum(m2*f2)/sum(f2)
var2 = sum((m2-mean2)^2*f2)/sum(f2)

par(mfrow=c(1,2))
hist(eruption_length, breaks = class1, main="간헐온천 지속시간에 대한 히스토그램\n origin: 1.35",
     xlab = "간헐온천 지속시간")
hist(eruption_length, breaks = class2, main="간헐온천 지속시간에 대한 히스토그램\n origin: 1.5",
     xlab = "간헐온천 지속시간")

hist_func2=function(n)
{par(mfrow=c(n,2))
  for(i in 1:n)
  {class1 = seq(1.35,5.35,by=0.5/(2*i-1))
    class2 = seq(1.5,5.0, by=0.5/(2*i-1))
    hist(eruption_length, breaks=class1, probability=T, main="origin:1.35", xlab=NULL)
    hist(eruption_length, breaks=class2, probability=T, main="origin:1.35", xlab=NULL)
  }
}
hist_func2(3)
w = c(0.1,0.2,0.3,0.4,0.5)
for(i in 1:5)
{
  class1=seq(1.5,5.1,by=w[i])
  cat_class1 = cut(eruption_length, breaks = class1)
  table(cat_class1)
  cat("계급의 폭=",w[i],"\n")
  print(table(cat_class1))
}


# 상자그림(p.52)
boxplot(담즙의과포화비율,col="yellow",horizontal=T,main=NULL)
rug(담즙의과포화비율, col="blue")
boxplot(담즙의과포화비율~성별,notch=T, col="yellow",main=NULL)

population = read.csv("광역시-구 인구.csv",header=T)
attach(population)
str(population)
boxplot(Y2010)


# Practice1 (p.66)
student = c(36,57,20,15,7)
names(student) = c("20대","30대","40대","50대","60대")
par(mfrow=c(1,2))
pie(student, col=c("red3","blue","green3","magenta","yellow"))
student_f = student/sum(student)
barplot(student_f,col=c("red3","blue","green3","magenta","yellow"))
