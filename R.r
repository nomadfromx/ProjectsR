# Использование R на практике

# Одна из библиотек для R
https://plot.ly/r/
			  
#load(".RData")

Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_141')
# добавим работу с Excel
library("xlsx")
# загрузим данные по продажам
setwd("C:/Users/andrey.zvyagin.DIR/Downloads/Мое")
sales <- read.xlsx("sales_12225.xls", sheetIndex = 1)
# подготовим данные
# выбор только МР=12225
dates<-sales$SALEDATE[rep(which(sales$DIST_MOD_ID==12225))]
quantity<-sales$QUANTITY[rep(which(sales$DIST_MOD_ID==12225))]
# сортировка
quantity<-quantity[order(dates)]			
dates<-dates[order(dates)]		    
# data.frame
sales<-data.frame(dates,quantity)
# head(sales)

# график с апроксимацией
library(ggplot2)
library(dplyr)
require(splines)
sales %>%												# создаем конвейер
  ggplot(aes(x=dates,y=quantity))+						# по x - даты, по y - зависимвые от x продажи
  geom_point(alpha=0.5)+								# указываем, что хотим получить диаграмму рассеяния (scatter plot)
  stat_smooth(method = lm, formula = y ~ ns(x,12))+		# применяем натуральный сплайн (функция stat_smooth) 
														# и строим приближенную кривую, 
														# отображаемую поверх диаграммы рассеяния
  theme_bw()		

# Проверка на выбросы - ящик с усами
boxplot(quantity)
# Заберём индексы точек выбросов
ind <- which(quantity %in% boxplot.stats(quantity)$out)
# Сохраним координаты точек выбросов в отдельном dataframe
eject <- data.frame(dates=dates[ind], quantity=quantity[ind])
# Покажем их на графике
plot(dates,quantity,col='green', pch=18, ylim=c(0,max(quantity)))
points(eject$dates, eject$quantity, col='red',pch=18)
# Удалим выбросов
#dates <- dates[-ind]
#quantity <- quantity[-ind]		
#sales<-data.frame(dates,quantity)								

# добавим данные из Oracle Demantra
forecast <- read.xlsx("forecast_12225.xls", sheetIndex = 1)
forecast_s<-forecast$SALEDATE[rep(which(forecast$DIST_MOD_ID==12225))]
forecast_q<-forecast$QUANTITY[rep(which(forecast$DIST_MOD_ID==12225))]
forecast_q<-forecast_q[order(forecast_s)]		# сортировка
forecast_s<-forecast_s[order(forecast_s)]		# сортировка
forecast<-data.frame(dates=forecast_s,quantity=forecast_q)
#forecast

# вычислим прогноз
# 1. построим линейную регрессивную модель - натуральный сплайн 12 порядка
fit.lm<-lm(quantity~ns(dates,12),data=sales)
#summary(fit.lm)
# 2. сам прогноз
mypredict<-predict(fit.lm,data.frame(dates=forecast$dates))
#mypredict
			  
# добавим реальные продажи
real <- data.frame(dates=c("2017-07-31","2017-08-07","2017-08-14","2017-08-21","2017-08-28","2017-09-04"), 
                   quantity=c(1163,757,657,663,913,803))
real$dates<-strptime(real$dates,"%Y-%m-%d")	
sales<-data.frame(dates=sales$dates[1:length(sales$dates)-1],quantity=sales$quantity[1:length(sales$quantity)-1]) 
quantity<-quantity[order(sales$dates)]		# сортировка
dates<-dates[order(sales$dates)]			# сортировка
graph <- data.frame(dates=c(dates,forecast$dates,forecast$dates,forecast$dates),quantity=c(quantity,forecast$quantity,as.vector(mypredict),real$quantity))		   
style <- c(rep(1,length(dates)),rep(2,length(forecast$dates)),rep(3,length(mypredict)),rep(1,length(real$dates)))
# полный график
graph %>%													
   ggplot(aes(x=dates,y=quantity,colour=factor(style)),size=2)+
	ggtitle("Продажи по Роутер_4G_WIFI_MAIN")+
	labs(x="Даты",y="Количество")+
	geom_line()+
	geom_point(alpha=0.5,size=2)+
	scale_colour_discrete(name=NULL,breaks=c(1:3),labels=c("Текущие продажи","Oracle Demantra","Наш расчет"))+
	theme_bw()
# укрупнение	
graph[100:123,] %>%													
     ggplot(aes(x=dates,y=quantity,colour=factor(style[100:123])),size=2)+
     ggtitle("Продажи по Роутер_4G_WIFI_MAIN")+
     labs(x="Даты",y="Количество")+
     geom_point(alpha=0.5,size=2)+
	 geom_line()+
     scale_colour_discrete(name=NULL,breaks=c(1:3),labels=c("Текущие продажи","Oracle Demantra","Наш расчет"))+
     theme_bw()				   
			  
# Пример использования аналитики - HH.RU + Java
https://tver.hh.ru/search/resume?area=89&clusters=true&text=Java&pos=full_text&logic=normal&exp_period=all_time
https://tikhonov.shinyapps.io/cvmining/

			   
			   





