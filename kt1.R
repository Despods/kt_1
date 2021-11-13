#Контрольная работа 1 Жданов Би-18
#Прогнозирование цен на Оптовом рынке энергомощности России. 

#Поставим нужные пакеты для графиков
install.packages('ggplot2')
install.packages('tidyverse')

#Включим их
library(ggplot2)
library(tidyverse)

#Грузим данные из таблицы RU_Electricity_Market_PZ_dayahead_price_volume
table <- read.csv("C:/Users/Виталий/Desktop/кт1/RU_Electricity_Market_PZ_dayahead_price_volume.csv", header = TRUE, sep = ",")

#Смотрим имена столбцов
names(table)

#Устанавливаем набор случайных величин,Делаем метод авторегрессии
set.seed(22)
fix(table)
names(table)
str(table)
table[, c(4)] <- sapply(table[, c(4)], as.numeric)

lm.fit = lm(price_eur~consumption_eur, data = table)
attach(table)

fix(table)

lm.fit # основная информация по моедели
summary(lm.fit) # p-значения, стандартные ошибки коэффициентов, а также коэф-т детерминации R^2, F-критерий для модели

lm.fit$coefficients
lm.fit$residuals
names(lm.fit)

coef(lm.fit)

confint(lm.fit)


predict(lm.fit, data.frame(price_eur=(c(5,10,15))),
        interval = "confidence")

predict(lm.fit, data.frame(lstat=(c(5,10,15))),
        interval = "prediction")


# отобразим результаты на простой диаграмме с линейной формулой
plot(consumption_eur, price_eur)
abline(lm.fit)



