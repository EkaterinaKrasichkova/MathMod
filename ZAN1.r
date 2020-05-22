#Красичкова Е.Д. 123 гр Вариант 9 
#Регион 73 - Ульяновская область
#Красичкова Екатерина – для региона 73 рассчитайте урожайность пшеницы в 2003 году, взяв для рассчета средние суммы активных температур за предыдущие 9 лет, с метеостанций на расстоянии от 70 до 210 км

# Проверка рабочей директории
setwd("E:/MatMod")
getwd()

#Установление пакетов
#install.packages("tidyverse")
library(tidyverse)
#install.packages("rnoaa")
library(rnoaa)

#Скачивание списка метеостанций. Делается один раз
#station_data = ghcnd_stations()
#Запись в файл для последующей работы.Делается один раз
#write.csv(station_data,"station_data.csv")
station_data = read.csv("station_data.csv")

# ФОРМИРОВАНИЕ СПИСКА МЕТЕОСТАНЦИЙ
#После получения списка всех станций, выберем из него список станций ближайших к Ульяновску,создав таблицу с именем региона и координатами его столицы
ulyanovsk = data.frame(id = "ULYANOVSK", latitude = 54.328240,  longitude = 48.386570)
ulyanovsk

#прочитаем справку команды meteo_nearby_stations 
? meteo_nearby_stations

#можно выбирать метеостанции в некотором фиксированном радиусе от Ульяновска
#или конечное число станций, которые имеют необходимые данные
#в заданный временной период, и выбрать переменные, которые обязательно должны быть в наличии
ulyanovsk_around = meteo_nearby_stations(lat_lon_df = ulyanovsk, station_data = station_data, limit = 100, var = c("PRCP", "TAVG"), year_min = 1994, year_max = 2003)
ulyanovsk_around

#write.csv(ulyanovsk_around, "ulyanovsk_around.csv")

#ulyanovsk_around это список единственным элементом которого является таблица, 
#содержащая идентификаторы метеостанций, отсортированых по их удаленности от Ульяновска 
#вспомним, как работать со списками

#1)очевидно что первым элементом таблицы будет
#идентификатор метеостанции Ульяновска, его то мы и попытаемся получить
ulyanovsk_id=ulyanovsk_around[["ULYANOVSK"]][["id"]][1]
ulyanovsk_id
summary(ulyanovsk_id)

#2)чтобы получить таблицу всех метеостанций вокруг Ульяновска нужно выбрать целиком первый объект из списка
ulyanovsk_table = ulyanovsk_around[[1]]
ulyanovsk_table
summary(ulyanovsk_table)


#3)выберем те метеостации, которые располагаются на расстоянии от 70 до 210
#ulyanovsk_table[,5]
ulyanovsk_table = filter (ulyanovsk_table, distance > 69 & distance < 211 )
#ulyanovsk_stations = ulyanovsk_table[ulyanovsk_table$ulyanovsk.distance>70 & ulyanovsk_table$ulyanovsk.distance<210] 
ulyanovsk_stations = ulyanovsk_table

#Список необходимых станций
str(ulyanovsk_stations)
ulyanovsk_stations$id

#Скачивание погодных данных для выбранных метеостанций
#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйте след. команду
meteo_tidy_ghcnd?
  all_ulyanovsk_data = meteo_tidy_ghcnd(stationid = ulyanovsk_id)
#all_ulyanovsk_data = meteo_tidy_ghcnd(stationid = ulyanovsk_id)


#посмотрим, что же скачивается

summary(all_ulyanovsk_data)

#необходимо взять средние суммы активных температур за 2003 г
#Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка

#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()

#Создадим объект, куда скачаем все данные всех метеостанций
all_ulyanovsk_meteodata = data.frame()

#Цикл для всех метеостанций

for(i in 1:16) 
{ 
  all_i  = meteo_tidy_ghcnd(stationid =  ulyanovsk_around[["ULYANOVSK"]][["id"]][i])
  
  #выберем нужные свойства 
  all_i = all_i[ ,c("id","date","tavg")] 
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном    #этапах цикла
  print(all_i)
  all_ulyanovsk_meteodata=rbind(all_ulyanovsk_meteodata, all_i)
}


#Записываем полученные результаты

#write.csv(all_ulyanovsk_meteodata,"all_ulyanovsk_meteodata.csv")

# считываем данные из файла all_ulyanovsk_meteodata.csv
all_ulyanovsk_meteodata = read.csv("all_ulyanovsk_meteodata.csv")
#посмотрим на данные
str(all_ulyanovsk_meteodata)
#видим, что дата записана в формате "1963-01-02"
#ищем библиотеку из tidyverse, которая может помочь с датой
library(lubridate)
# вытащить год
#проверим, что работает
y = year(all_ulyanovsk_meteodata$date); y
all_ulyanovsk_meteodata [,"year"]= year(all_ulyanovsk_meteodata$date)
#добавим месяц
all_ulyanovsk_meteodata [,"month"]= month(all_ulyanovsk_meteodata$date) 
#вытащить день от начала года
all_ulyanovsk_meteodata [,"day_of_the_year"]= yday(all_ulyanovsk_meteodata$date) 
#проверим результат
str(all_ulyanovsk_meteodata)    
#отфильтруем данные за 1993-2002
years_ulyanovsk_meteodata = filter (all_ulyanovsk_meteodata, year > 1992 & year < 2003 )   
#проверим результат
str(years_ulyanovsk_meteodata)
summary (years_ulyanovsk_meteodata)    
################## 5. Средняя (по годам и метеостанциям) сумма активных температур за месяц
#Изучаем формулу и видим, что единственное, что нужно расчитать
#- это сумму температур больше 5 град. по месячно, остальное в формуле-  константы

#### 1.  температурy нужно поделить на 10
years_ulyanovsk_meteodata[,"tavg"]= years_ulyanovsk_meteodata$tavg / 10
summary (years_ulyanovsk_meteodata)
#### 2. Превратим в нули все NA и где tavg больше 5 градусов

years_ulyanovsk_meteodata [is.na(years_ulyanovsk_meteodata$tavg), "tavg"] = 0
years_ulyanovsk_meteodata [years_ulyanovsk_meteodata$tavg<5, "tavg"] = 0

#проверяем, что температура получилась в или 0 или больше 5 градусов
summary(years_ulyanovsk_meteodata)
#### 3. суммарная температура за месяц за 9 лет для всех станций 
# группирую по метеостанциям, годам и месяцам

alldays= group_by(years_ulyanovsk_meteodata,id,year,month)

#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum

sumT_alldays_ulyanovsk = summarize(alldays, tsum = sum(tavg))
#Получилось - все года, все месяца присутствуют
# максимальная суммарная температура за месяц 748,4, то есть 748,4/30=24,9

summary(sumT_alldays_ulyanovsk)
748.4/30
#Сгруппирем данные по месяцам  
groups_ulyanovsk_months = group_by(sumT_alldays_ulyanovsk,month)
groups_ulyanovsk_months
#найду для всех метеостанций и ВСЕХ лет среднее по месяцам
sumT_months= summarize(groups_ulyanovsk_months , St = mean(tsum))
sumT_months



################## 6. Подготовка к расчету по формуле Урожая
### Ввод констант
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
y = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;
Kf = 300 # - коэффициент использования ФАР посевом;
Qj = 1600 # - калорийность урожая культуры; 
Lj = 2.2 #  - сумма частей основной и побочной продукции; 
Ej = 25 # - стандартная влажность культуры; 
# Рассчитаем Fi по месяцам
#Fi= afi+bfi∗y∗(St>5℃)
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)

#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
##  Расчитываем урожай как сумму по месяцам и думаем разумный ли он
Yield = sum(sumT_months$Yi)  
Yield
# Ответ: 17,3 ц/га 



