# ������������� R �� ��������

# ���� �� ��������� ��� R
https://plot.ly/r/
			  
#load(".RData")

Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_141')
# ������� ������ � Excel
library("xlsx")
# �������� ������ �� ��������
setwd("C:/Users/andrey.zvyagin.DIR/Downloads/���")
sales <- read.xlsx("sales_12225.xls", sheetIndex = 1)
# ���������� ������
# ����� ������ ��=12225
dates<-sales$SALEDATE[rep(which(sales$DIST_MOD_ID==12225))]
quantity<-sales$QUANTITY[rep(which(sales$DIST_MOD_ID==12225))]
# ����������
quantity<-quantity[order(dates)]			
dates<-dates[order(dates)]		    
# data.frame
sales<-data.frame(dates,quantity)
# head(sales)

# ������ � �������������
library(ggplot2)
library(dplyr)
require(splines)
sales %>%												# ������� ��������
  ggplot(aes(x=dates,y=quantity))+						# �� x - ����, �� y - ���������� �� x �������
  geom_point(alpha=0.5)+								# ���������, ��� ����� �������� ��������� ��������� (scatter plot)
  stat_smooth(method = lm, formula = y ~ ns(x,12))+		# ��������� ����������� ������ (������� stat_smooth) 
														# � ������ ������������ ������, 
														# ������������ ������ ��������� ���������
  theme_bw()		

# �������� �� ������� - ���� � �����
boxplot(quantity)
# ������ ������� ����� ��������
ind <- which(quantity %in% boxplot.stats(quantity)$out)
# �������� ���������� ����� �������� � ��������� dataframe
eject <- data.frame(dates=dates[ind], quantity=quantity[ind])
# ������� �� �� �������
plot(dates,quantity,col='green', pch=18, ylim=c(0,max(quantity)))
points(eject$dates, eject$quantity, col='red',pch=18)
# ������ ��������
#dates <- dates[-ind]
#quantity <- quantity[-ind]		
#sales<-data.frame(dates,quantity)								

# ������� ������ �� Oracle Demantra
forecast <- read.xlsx("forecast_12225.xls", sheetIndex = 1)
forecast_s<-forecast$SALEDATE[rep(which(forecast$DIST_MOD_ID==12225))]
forecast_q<-forecast$QUANTITY[rep(which(forecast$DIST_MOD_ID==12225))]
forecast_q<-forecast_q[order(forecast_s)]		# ����������
forecast_s<-forecast_s[order(forecast_s)]		# ����������
forecast<-data.frame(dates=forecast_s,quantity=forecast_q)
#forecast

# �������� �������
# 1. �������� �������� ������������ ������ - ����������� ������ 12 �������
fit.lm<-lm(quantity~ns(dates,12),data=sales)
#summary(fit.lm)
# 2. ��� �������
mypredict<-predict(fit.lm,data.frame(dates=forecast$dates))
#mypredict
			  
# ������� �������� �������
real <- data.frame(dates=c("2017-07-31","2017-08-07","2017-08-14","2017-08-21","2017-08-28","2017-09-04"), 
                   quantity=c(1163,757,657,663,913,803))
real$dates<-strptime(real$dates,"%Y-%m-%d")	
sales<-data.frame(dates=sales$dates[1:length(sales$dates)-1],quantity=sales$quantity[1:length(sales$quantity)-1]) 
quantity<-quantity[order(sales$dates)]		# ����������
dates<-dates[order(sales$dates)]			# ����������
graph <- data.frame(dates=c(dates,forecast$dates,forecast$dates,forecast$dates),quantity=c(quantity,forecast$quantity,as.vector(mypredict),real$quantity))		   
style <- c(rep(1,length(dates)),rep(2,length(forecast$dates)),rep(3,length(mypredict)),rep(1,length(real$dates)))
# ������ ������
graph %>%													
   ggplot(aes(x=dates,y=quantity,colour=factor(style)),size=2)+
	ggtitle("������� �� ������_4G_WIFI_MAIN")+
	labs(x="����",y="����������")+
	geom_line()+
	geom_point(alpha=0.5,size=2)+
	scale_colour_discrete(name=NULL,breaks=c(1:3),labels=c("������� �������","Oracle Demantra","��� ������"))+
	theme_bw()
# ����������	
graph[100:123,] %>%													
     ggplot(aes(x=dates,y=quantity,colour=factor(style[100:123])),size=2)+
     ggtitle("������� �� ������_4G_WIFI_MAIN")+
     labs(x="����",y="����������")+
     geom_point(alpha=0.5,size=2)+
	 geom_line()+
     scale_colour_discrete(name=NULL,breaks=c(1:3),labels=c("������� �������","Oracle Demantra","��� ������"))+
     theme_bw()				   
			  
# ������ ������������� ��������� - HH.RU + Java
https://tver.hh.ru/search/resume?area=89&clusters=true&text=Java&pos=full_text&logic=normal&exp_period=all_time
https://tikhonov.shinyapps.io/cvmining/

			   
			   





