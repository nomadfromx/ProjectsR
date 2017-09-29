#load(".RData")

Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_141')
#install.packages("rJava")

# добавим работу с Excel
#install.packages("xlsx", dep = T)
library("xlsx")

# загрузим данные по продажам
setwd("C:/Users/andrey.zvyagin.DIR/Downloads/Мое")
sales1 <- read.xlsx("sales.xls", sheetIndex = 1); sales2 <- read.xlsx("sales.xls", sheetIndex = 2); sales3 <- read.xlsx("sales.xls", sheetIndex = 3)
#sales$SALE_DATE_W<-strptime(sales$SALE_DATE_W,"%Y-%m-%d")

# подготовим данные
dates1<-sales1$SALEDATE[rep(which(sales1$DIST_MOD_ID==12225))]; dates2<-sales2$SALEDATE[rep(which(sales2$DIST_MOD_ID==12225))]; dates3<-sales3$SALEDATE[rep(which(sales3$DIST_MOD_ID==12225))]; 
dates<-c(dates1,dates2,dates3) 
quantity1<-sales1$QUANTITY[rep(which(sales1$DIST_MOD_ID==12225))]; quantity2<-sales2$QUANTITY[rep(which(sales2$DIST_MOD_ID==12225))]; quantity3<-sales3$QUANTITY[rep(which(sales3$DIST_MOD_ID==12225))]
quantity<-c(quantity1,quantity2,quantity3)
quantity<-quantity[order(dates)]	# сортировка
dates<-dates[order(dates)]		    # сортировка
sales<-data.frame(dates,quantity)

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

# Проверка на выбросы
#boxplot(quantity)
# Заберём индексы точек выбросов
#ind <- which(quantity %in% boxplot.stats(quantity)$out)
# Сохраним координаты точек выбросов в отдельном dataframe
#eject <- data.frame(dates=dates[ind], quantity=quantity[ind])
# Покажем их на графике
#plot(dates,quantity,col='green', pch=18, ylim=c(0,max(quantity)))
#points(eject$dates, eject$quantity, col='red',pch=18)
# Удалим выбросов
#dates <- dates[-ind]
#quantity <- quantity[-ind]										

# добавим данные из Oracle Demantra
forecast <- read.xlsx("forecast.xls", sheetIndex = 1)
forecast_s<-forecast$SALEDATE[rep(which(forecast$DIST_MOD_ID==12225))]
forecast_q<-forecast$QUANTITY[rep(which(forecast$DIST_MOD_ID==12225))]
forecast_q<-forecast_q[order(forecast_s)]		# сортировка
forecast_s<-forecast_s[order(forecast_s)]		# сортировка
forecast<-data.frame(dates=forecast_s,quantity=forecast_q)
#forecast
#       dates quantity
#1 2017-07-31      672
#2 2017-08-07      738
#3 2017-08-14      672
#4 2017-08-21      680
#5 2017-08-28      718
#6 2017-09-04      639

# вычислим прогноз
# 1. построим линейную регрессивную модель
fit.lm<-lm(quantity~ns(dates,12),data=sales)
# summary(fit.lm)
# 2. сам прогноз
myforecast<-data.frame(dates=forecast$dates)
mypredict<-predict(fit.lm,myforecast)
#mypredict

# LOESS
#fit.loess<-loess(quantity ~ as.numeric(dates), control=loess.control(surface="direct"))
#mypredict<-predict(fit.loess,as.numeric(myforecast$dates))

# общий график
#graph <- data.frame(dates=c(dates,forecast$dates,forecast$dates),quantity=c(quantity,forecast$quantity,as.vector(mypredict)))
#style <- c(rep(1,length(dates)),rep(2,length(forecast$dates)),rep(3,length(mypredict)))
#graph %>%													
#    ggplot(aes(x=dates,y=quantity,colour=factor(style)),size=2)+
#	ggtitle("Продажи по Роутер_4G_WIFI_MAIN")+
#	labs(x="Даты",y="Количество")+
#	geom_line()+
#	geom_point(alpha=0.5,size=2)+
#	scale_colour_discrete(name=NULL,breaks=c(1:3),labels=c("Текущие продажи","Oracle Demantra","Наш расчет"))+
#	theme_bw()
# крупно последние даты	
#graph[90:118,] %>%													
#     ggplot(aes(x=dates,y=quantity,colour=factor(style[90:118])),size=2)+
#     ggtitle("Продажи по Роутер_4G_WIFI_MAIN")+
#     labs(x="Даты",y="Количество")+
#     geom_point(alpha=0.5,size=2)+
#	 geom_line()+
#     scale_colour_discrete(name=NULL,breaks=c(1:3),labels=c("Текущие продажи","Oracle Demantra","Наш расчет"))+
#     theme_bw()	
	 
# анализ предсказанной модели
# 1. чем больше точка - тем сильнее влияет это значение на построение прогноза
# 2. чем светлее точка - тем сильнее ошибку дает это значение
# по этой моделе видно, что самые светлые "ошибочные" точки - самые мальнькие (т.е. мало влияют на результат), кроме одной
#require(ggvis)
#data.frame(predict = predict(fit.lm),
#           resids  = residuals(fit.lm),
#           rstuds  = rstudent(fit.lm),
#           leverage = hatvalues(fit.lm)) %>%
#    ggvis(~predict, ~resids, fill = ~abs(rstuds), size = ~leverage) %>% 
#    layer_points() %>%
#    add_axis("x", title = "Предсказанные значения") %>%
#    add_axis("y", title = "Остатки")  %>%
#    add_legend("size", title = "Влияние на модель") %>% 
#    add_legend("fill", title = "Модуль стьюд.ост.", 
#               properties = legend_props(legend = list(y = 100)))	
			   
# добавим реальные продажи
real <- data.frame(dates=c("2017-07-31","2017-08-07","2017-08-14","2017-08-21","2017-08-28","2017-09-04"), 
                   quantity=c(1163,757,657,663,913,803))
real$dates<-strptime(real$dates,"%Y-%m-%d")	
sales<-data.frame(dates=sales$dates[1:length(sales$dates)-1],quantity=sales$quantity[1:length(sales$quantity)-1]) 
quantity<-quantity[order(sales$dates)]		# сортировка
dates<-dates[order(sales$dates)]			# сортировка
graph <- data.frame(dates=c(dates,forecast$dates,forecast$dates,forecast$dates),quantity=c(quantity,forecast$quantity,as.vector(mypredict),real$quantity))		   
style <- c(rep(1,length(dates)),rep(2,length(forecast$dates)),rep(3,length(mypredict)),rep(1,length(real$dates)))
#graph %>%													
#    ggplot(aes(x=dates,y=quantity,colour=factor(style)),size=2)+
#	ggtitle("Продажи по Роутер_4G_WIFI_MAIN")+
#	labs(x="Даты",y="Количество")+
#	geom_line()+
#	geom_point(alpha=0.5,size=2)+
#	scale_colour_discrete(name=NULL,breaks=c(1:3),labels=c("Текущие продажи","Oracle Demantra","Наш расчет"))+
#	theme_bw()
graph[100:123,] %>%													
     ggplot(aes(x=dates,y=quantity,colour=factor(style[100:123])),size=2)+
     ggtitle("Продажи по Роутер_4G_WIFI_MAIN")+
     labs(x="Даты",y="Количество")+
     geom_point(alpha=0.5,size=2)+
	 geom_line()+
     scale_colour_discrete(name=NULL,breaks=c(1:3),labels=c("Текущие продажи","Oracle Demantra","Наш расчет"))+
     theme_bw()				   
			   





