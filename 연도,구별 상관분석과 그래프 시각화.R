library(readxl)
traffic<- read_excel("../상관분석.xlsx")
traffic
names(traffic) <- c('accident','protect','academy','population',
                    'baby','park','cars','cctv')
View(traffic)
traffic <- as.data.frame(traffic)
str(traffic)
summary(traffic)

##### 상관관계 분석 및 그래프 시각화 #####
cor(traffic)
plot(traffic)
library(psych)
pairs.panels(traffic)  
library(PerformanceAnalytics)
chart.Correlation(traffic)
library(corrplot)
corrplot(cor(traffic),method='ellipse') 

plot(accident ~., traffic, type='l', col ='red', main="어린이 교통사고 건수 대비 요인들 추세")
