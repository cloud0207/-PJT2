library(dplyr)
library(tidyverse)
library(devtools)
library(readxl)
library(ggplot2)

data <- read.csv("../2013~2021 서울특별시 어린이보호구역 내 사고 제외 어린이 사고 데이터.csv")
data

월요일 <- data %>% filter(str_detect(string=data$요일,pattern="월요일"))
월요일 <- data %>% filter(data$요일=="월요일")
화요일 <- data %>% filter(str_detect(string=data$요일,pattern="화요일"))
수요일 <- data %>% filter(str_detect(string=data$요일,pattern="수요일"))
목요일 <- data %>% filter(str_detect(string=data$요일,pattern="목요일"))
금요일 <- data %>% filter(str_detect(string=data$요일,pattern="금요일"))
토요일 <- data %>% filter(str_detect(string=data$요일,pattern="토요일"))
일요일 <- data %>% filter(str_detect(string=data$요일,pattern="일요일"))

accident <- c(count(월요일),count(화요일),count(수요일),count(목요일),count(금요일),
              count(토요일),count(일요일))

accident <- as.numeric(accident)
week = c('월요일','화요일','수요일','목요일','금요일','토요일','일요일')

a <- barplot(accident, names=week, main = '비어린이보호구역 내 어린이 사고발생 요일',
        xlab = '요일', ylab = '발생건수',ylim=(c(0,3000)),xlim=(c(0,10)),
        cex.main=2, col = c('grey','grey','grey','grey','grey','lightyellow','grey'))
legend('topright', c('월요일','화요일','수요일','목요일','금요일','토요일','일요일'), 
       fill = c('grey','grey','grey','grey','grey','yellow','grey'),cex=0.8)
text(x=a, y=accident, labels=paste(accident,"건"), col="black", cex=1, font=2)
abline(h=1839, col="red", lty=2, lwd=1)

####영등포구####
영등포구 <- data %>% filter(str_detect(string=data$시군구,pattern="영등포구"))
월요일y <- 영등포구 %>% filter(영등포구$요일=="월요일")
화요일y <- 영등포구 %>% filter(영등포구$요일=="화요일")
수요일y <- 영등포구 %>% filter(영등포구$요일=="수요일")
목요일y <- 영등포구 %>% filter(영등포구$요일=="목요일")
금요일y <- 영등포구 %>% filter(영등포구$요일=="금요일")
토요일y <- 영등포구 %>% filter(영등포구$요일=="토요일")
일요일y <- 영등포구 %>% filter(영등포구$요일=="일요일")

accident_y <- c(count(월요일y),count(화요일y),count(수요일y),count(목요일y),count(금요일y),
              count(토요일y),count(일요일y))

accident_y <- as.numeric(accident_y)

b <- barplot(accident_y, names=week, main = '영등포구 비어린이보호구역 내 어린이 사고발생 요일',
             xlab = '요일', ylab = '발생건수',ylim=(c(0,250)),xlim=(c(0,10)),
             cex.main=2, col = c('grey','grey','grey','grey','grey','lightyellow','grey'))
legend('topright', c('월요일','화요일','수요일','목요일','금요일','토요일','일요일'), 
       fill = c('grey','grey','grey','grey','grey','yellow','grey'),cex=0.8)
text(x=b, y=accident_y, labels=paste(accident_y,"건"), col="black", cex=1, font=2)
abline(h=119, col="red", lty=2, lwd=1)



####강서구####
강서구 <- data %>% filter(str_detect(string=data$시군구,pattern="강서구"))
월요일k <- 강서구 %>% filter(강서구$요일=="월요일")
화요일k <- 강서구 %>% filter(강서구$요일=="화요일")
수요일k <- 강서구 %>% filter(강서구$요일=="수요일")
목요일k <- 강서구 %>% filter(강서구$요일=="목요일")
금요일k <- 강서구 %>% filter(강서구$요일=="금요일")
토요일k <- 강서구 %>% filter(강서구$요일=="토요일")
일요일k <- 강서구 %>% filter(강서구$요일=="일요일")

accident_k <- c(count(월요일k),count(화요일k),count(수요일k),count(목요일k),count(금요일k),
                count(토요일k),count(일요일k))

accident_k <- as.numeric(accident_k)

c <- barplot(accident_k, names=week, main = '강서구 비어린이보호구역 내 어린이 사고발생 요일',
             xlab = '요일', ylab = '발생건수',ylim=(c(0,180)),xlim=(c(0,10)),
             cex.main=2, col = c('grey','grey','grey','grey','grey','lightyellow','grey'))
legend('topright', c('월요일','화요일','수요일','목요일','금요일','토요일','일요일'), 
       fill = c('grey','grey','grey','grey','grey','yellow','grey'),cex=0.8)
text(x=c, y=accident_k, labels=paste(accident_k,"건"), col="black", cex=1, font=2)
abline(h=90, col="red", lty=2, lwd=1)
