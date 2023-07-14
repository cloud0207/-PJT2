library(dplyr)
library(tidyverse)
library(devtools)
library(readxl)
library(ggplot2)

data <- read.csv("../2013~2021 서울특별시 어린이보호구역 내 사고 제외 어린이 사고 데이터.csv")
View(data)

####어린이보호구역 외 영등포구####
영등포구 <- data %>% filter(str_detect(string=data$시군구,pattern="영등포구"))
단일로y <- 영등포구 %>% filter(영등포구$도로형태=="단일로 - 기타")
횡단보도y <- 영등포구 %>% filter(영등포구$도로형태=="단일로 - 횡단보도상")
교차로안y <- 영등포구 %>% filter(영등포구$도로형태=="교차로 - 교차로안")
교차로부근y <- 영등포구 %>% filter(영등포구$도로형태=="교차로 - 교차로부근")


road <- c(count(단일로y),count(횡단보도y),count(교차로안y),count(교차로부근y))
road <- as.numeric(road)
road_type <- c("단일로 - 기타","단일로 - 횡단보도상","교차로 - 교차로안","교차로 - 교차로부근")

b <- barplot(road, names=road_type, main = '영등포구 비어린이보호구역 내 어린이 사고발생 도로형태',
             xlab = '도로형태', ylab = '발생건수',ylim=(c(0,500)),xlim=(c(0,5)),
             cex.main=1.5, col = c('lightyellow','grey','grey','grey'))
legend('topright', c("단일로 - 기타","단일로 - 횡단보도상","교차로 - 교차로안","교차로 - 교차로부근"), 
       fill = c('yellow','grey','grey','grey'),cex=0.8)
text(x=b, y=road, labels=paste(road,"건"), col="black", cex=1, font=2)
abline(h=158, col="red", lty=2, lwd=1)



####어린이보호구역 외 강서구####
강서구 <- data %>% filter(str_detect(string=data$시군구,pattern="강서구"))
단일로k <- 강서구 %>% filter(강서구$도로형태=="단일로 - 기타")
횡단보도k <- 강서구 %>% filter(강서구$도로형태=="단일로 - 횡단보도상")
교차로안k <- 강서구 %>% filter(강서구$도로형태=="교차로 - 교차로안")
교차로부근k <- 강서구 %>% filter(강서구$도로형태=="교차로 - 교차로부근")


road <- c(count(단일로k),count(횡단보도k),count(교차로안k),count(교차로부근k))
road <- as.numeric(road)
road_type <- c("단일로 - 기타","단일로 - 횡단보도상","교차로 - 교차로안","교차로 - 교차로부근")

c <- barplot(road, names=road_type, main = '강서구 비어린이보호구역 내 어린이 사고발생 도로형태',
             xlab = '도로형태', ylab = '발생건수',ylim=(c(0,500)),xlim=(c(0,5)),
             cex.main=1.5, col = c('lightyellow','grey','grey','grey'))
legend('topright', c("단일로 - 기타","단일로 - 횡단보도상","교차로 - 교차로안","교차로 - 교차로부근"), 
       fill = c('yellow','grey','grey','grey'),cex=0.8)
text(x=b, y=road, labels=paste(road,"건"), col="black", cex=1, font=2)
abline(h=116, col="red", lty=2, lwd=1)





data2 <- read.csv("../2013~2021 서울특별시 어린이보호구역 내 어린이사고 데이터.csv")
View(data2)

####어린이보호구역 내 영등포구####
영등포구 <- data2 %>% filter(str_detect(string=data2$시군구,pattern="영등포구"))
단일로y <- 영등포구 %>% filter(영등포구$도로형태=="단일로 - 기타")
횡단보도y <- 영등포구 %>% filter(영등포구$도로형태=="단일로 - 횡단보도상")
교차로안y <- 영등포구 %>% filter(영등포구$도로형태=="교차로 - 교차로안")
교차로부근y <- 영등포구 %>% filter(영등포구$도로형태=="교차로 - 교차로부근")


road <- c(count(단일로y),count(횡단보도y),count(교차로안y),count(교차로부근y))
road <- as.numeric(road)
road_type <- c("단일로 - 기타","단일로 - 횡단보도상","교차로 - 교차로안","교차로 - 교차로부근")

b <- barplot(road, names=road_type, main = '영등포구 어린이보호구역 내 어린이 사고발생 도로형태',
             xlab = '도로형태', ylab = '발생건수',ylim=(c(0,50)),xlim=(c(0,5)),
             cex.main=1.5, col = c('lightyellow','grey','grey','grey'))
legend('topright', c("단일로 - 기타","단일로 - 횡단보도상","교차로 - 교차로안","교차로 - 교차로부근"), 
       fill = c('yellow','grey','grey','grey'),cex=0.8)
text(x=b, y=road, labels=paste(road,"건"), col="black", cex=1, font=2)
abline(h=3, col="red", lty=2, lwd=1)



####어린이보호구역 내 강서구####
강서구 <- data2 %>% filter(str_detect(string=data2$시군구,pattern="강서구"))
단일로k <- 강서구 %>% filter(강서구$도로형태=="단일로 - 기타")
횡단보도k <- 강서구 %>% filter(강서구$도로형태=="단일로 - 횡단보도상")
교차로안k <- 강서구 %>% filter(강서구$도로형태=="교차로 - 교차로안")
교차로부근k <- 강서구 %>% filter(강서구$도로형태=="교차로 - 교차로부근")


road <- c(count(단일로k),count(횡단보도k),count(교차로안k),count(교차로부근k))
road <- as.numeric(road)
road_type <- c("단일로 - 기타","단일로 - 횡단보도상","교차로 - 교차로안","교차로 - 교차로부근")

c <- barplot(road, names=road_type, main = '강서구 어린이보호구역 내 어린이 사고발생 도로형태',
             xlab = '도로형태', ylab = '발생건수',ylim=(c(0,50)),xlim=(c(0,5)),
             cex.main=1.5, col = c('lightyellow','grey','grey','grey'))
legend('topright', c("단일로 - 기타","단일로 - 횡단보도상","교차로 - 교차로안","교차로 - 교차로부근"), 
       fill = c('yellow','grey','grey','grey'),cex=0.8)
text(x=b, y=road, labels=paste(road,"건"), col="black", cex=1, font=2)
abline(h=14, col="red", lty=2, lwd=1)
