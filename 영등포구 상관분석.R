library(readxl)
traffic_gu <- read_xlsx("../corr.xlsx")
traffic_gu
names(traffic) <- c('accident','protect','academy','population',
                    'child','park','cars','cctv')
View(traffic_gu)
traffic_gu <- as.data.frame(traffic_gu)
str(traffic_gu)
summary(traffic_gu)

##### 상관관계 분석 및 그래프 시각화 #####
cor(traffic_gu)
plot(traffic_gu)
library(psych)
pairs.panels(traffic_gu)  
library(PerformanceAnalytics)
chart.Correlation(traffic_gu)
library(corrplot)
corrplot(cor(traffic_gu),method='ellipse') 

plot(accident ~., traffic_gu, type='l', col ='red', main="어린이 교통사고 건수 대비 요인들 추세")
