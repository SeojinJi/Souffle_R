# R 시계열 객체의 생성 (p.106)
library(zoo)
econ1 = read.csv("gdp.csv", header = T)
year = seq(as.Date("1970-01-01"), as.Date("2013-12-01"), "quarter")
econ = zoo(econ1, year)
plot(econ/1000, ylab = "GDP(조 원)", xlab = "", col = 1:2, screens = 1)

# ggplot2의 사용(p.108)
library(ggplot2)
gdp_1 = read.csv("gdp.csv", header = T)
year = seq(as.Date("1970-01-01"), as.Date("2013-12-01"), "quarter")
gdp_kr = cbind(gdp_1, year)
ggplot(data = gdp_kr, aes(x=year)) +
  geom_line(aes(y=gdp/1000, colour="원계열")) +
  geom_line(aes(y=gdpsa/1000, colour = "계절조정계열")) +
  ylab("GDP(조 원)") + scale_color_hue("GDP") + theme(legend.position="bottom")

# 우리나라 종합주가지수의 선그래프 작성(p.110)
library(quantmod)
kospi = getSymbols("^KS11", auto.assign=FALSE)[,4] # Yahoo Finance로부터 종합주가지수 종가 데이터 가져오기
kospi = kospi[!is.na(kospi$KS11.Close)]
kospi$ma = runMean(kospi, n=200) # 200일 후방 이동평균선 작성
colnames(kospi) = c("종합주가지수","200일이평선")
autoplot(kospi, facets = NULL) + xlab("연도") + 
  theme(panel.background = element_rect(fill="white", colour="gray"), legend.position = "none")

# 우리나라 경기동행지수 순환변동치 선그래프 작성(p.112)
library(ggplot2)
library(scales)
library(xts)
cycle1 = read.csv("cycle.csv", header=T)
years = seq(as.Date("1970-01-01"), as.Date("2014-06-01"), "month")
cycle = xts(cycle1[,2],years)
# 경기 기준 순환일 데이터 가져와서 정리
refdate = read.csv("refdate1.csv", header=T)
yrng=range(cycle)
datebreaks = seq(as.Date("1970-01-01"),as.Date("2014-01-01"),"2 year")
# 그래프 그리기
p = autoplot(cycle, facets=NULL)+
  theme(panel.background = element_rect(fill="white", colour = "gray"), legend.position="bottom")+
  geom_rect(aes(NULL, NULL, xmin=as.Date(start), xmax = as.Date(end),fill=경기순환), ymin = yrng[1], ymax=yrng[2], data=refdate)+
  scale_fill_manual(values=alpha(c("yellow","darkblue"),0.1)) + ylab("") +xlab("")+
  geom_hline(yintercept=100, colour="gray")+
  geom_text(aes(x=as.Date(start), y=yrng[2], label=name1), data = refdate, size = 4, hjust=0.5, vjust=-0.5)+
  geom_text(aes(x=as.Date(end), y=yrng[2],label=name2), data = refdate, size=4, hjust=0.5, vjust=-0.5)+
  scale_x_date(breaks=datebreaks, labels=date_format("%Y"),expand=c(0.01,0.01))

# 제주시 강수량의 선그래프 (p.113)
climate_kr = read.csv("climate.csv",header=T)
year = seq(as.Date("2008-01-01"),as.Date("2013-12-01"), "month")
climate_kr=cbind(year, climate_kr)
ggplot(climate_kr, aes(x=year,y=jrain))+
  geom_area(colour="black",fill="blue",alpha=.2)+
  ylab("강수량(mm)")

# 기온 선그래프 작성(p.114)
climate_kr1 = read.csv("climate1.csv",header=T)
year=rep(seq(as.Date("2008-01-01"), as.Date("2013-12-01"), "month"),3)
climate_kr1 = cbind(year, climate_kr1)
h = ggplot(climate_kr, aes(x=year)) + facet_wrap(region~.) +
  geom_ribbon(aes(ymin=min, ymax = max), fill = "pink", alpha=.7)+
  geom_line(aes(y=mean), colour = "red") + theme_bw()
h + ylab("") +xlab("") + ylim(-12,33)

# 우리나라 연도별 경상수지의 막대그래프 작성(p.116)
cb = read.csv("cb.csv", header = T)
cb$total = cb$total/100
cb$pos = cb$total>=0
ggplot(cb, aes(x=year, y=total, fill=pos))+
  geom_bar(stat="identity",position="identity", colour="black", size=0.25)+
  scale_fill_manual(values=c("red","black"), guide=FALSE)+
  ylab("경상수지(억 달러)")+theme_bw()

# 연령대별 우리나라 인구 및 인구 비중 누적그래프(p.119)
library(reshape2)
library(plyr)
pop_kr1 = read.csv("krpop.csv",header=T)
pop_kr=melt(pop_kr1,id="age")
pop_kr$year = as.numeric(substr(pop_kr$variable,2,5))
ggplot(pop_kr, aes(x=year,y=value/10000, fill = age)) +
  geom_area(colour="black", size=.1, alpha=.4)+
  scale_fill_brewer(palette="Blues")+ylab("인구(만 명)")+
  scale_x_continuous(breaks=seq(1960,2060,5), expand=c(0,0)) +
  theme_bw()
# 비중 구하기
pop_kr_p = ddply(pop_kr, "year", transform, weight = value/sum(value)*100)
ggplot(pop_kr_p, aes(x=year, y=weight, fill=age))+
  geom_area(colour="black", size=.1, alpha=.4)+
  scale_fill_brewer(palette="Blues")+ylab("인구비중(%)")+
  scale_x_continuous(breaks=seq(1960,2060,10), expand=c(0,0)) +
  theme_bw()

# 우리나라 산업구조의 막대그래프 작성(p.121)
gdp_s1 = read.csv("gdp_sh.csv",header=T)
gdp_s=melt(gdp_s1, id="year")
names(gdp_s) = c("연도", "산업","비중")
ggplot(gdp_s, aes(x=연도,y=비중,fill=산업))+
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=seq(1970,2010,5))+
  theme(panel.background=element_rect(fill="white",colour="gray"), legend.position="bottom")+
  ylab("비중(%)")+xlab("")

# 우리나라 제조업 재고출하순환도(p.123)
library(scales)
library(xts)
inven1 = read.csv("inven_cy.csv", header = T)
year = seq(as.Date("1980-01-01"), as.Date("2014-06-01"),"month")
inven = xts(inven1[,2:3], year)
inven$출하지수증감률 = (inven$ship - lag(inven$ship, 12))/lag(inven$ship,12)*100
inven$재고지수증감률 = (inven$stock - lag(inven$stock, 12))/lag(inven$stock, 12)*100
inven_1 = inven[337:366]
ggplot(inven_1, aes(x=출하지수증감률,y=재고지수증감률)) + theme_bw()+
  geom_path()+ geom_point() + ylim(-21, 28) + xlim(-21, 28) +
  geom_text(aes(label=substr(index(inven_1),3,7)), size=3, hjust=-0.2, vjust=-0.3, colour="blue") +
  geom_hline(yintercept=0, colour="gray") + geom_vline(xintercept=0, colour="gray")

# 주가변동 채색달력 그래프 작성 (p.125)
library(quantmod); library(ggplot2); library(reshape2)
library(plyr); library(scales)
getSymbols("^KS11",src="yahoo")
KS11$주가변동 = abs((KS11$KS11.Close - lag(KS11$KS11.Close, 1))/lag(KS11$KS11.Close, 1)) * 100
dat = data.frame(date=index(KS11), KS11)
dat$year = as.numeric(as.POSIXlt(dat$date)$year + 1900)
dat$month = as.numeric(as.POSIXlt(dat$date)$mon + 1)
dat$monthf = factor(dat$month, levels=as.character(1:12), labels = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월"), ordered = TRUE)
dat$weekday = as.POSIXlt(dat$date)$wday
dat$weekdayf = factor(dat$weekday, levels=rev(1:7), labels = rev(c("월","화","수","목","금","토","일")))
dat$yearmonth = as.yearmon(dat$date)
dat$yearmonthf = factor(dat$yearmonth)
dat$week = as.numeric(format(dat$date, "%W"))
dat = ddply(dat, .(yearmonthf), transform, monthweek = 1+week-min(week))
ggplot(dat, aes(monthweek, weekdayf, fill=주가변동)) +
  geom_tile(colour="white") + facet_grid(year~monthf) +
  scale_fill_gradient(limits=c(0,12), low = "lightgray", high="darkred")+
  xlab("") +ylab("") +
  theme(panel.background = element_rect(fill="white", colour="gray"))
