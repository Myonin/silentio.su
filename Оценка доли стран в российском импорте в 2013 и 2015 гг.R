# Оценка доли стран в российском импорте, сравнение 2013 и 2015 гг.,
# лидеры роста и аутсайдеры
# © silentio - gonzo data science lab | silentio.su - 22 сентября 2016 г.

library(dplyr)
library(tidyr)
library(ggplot2)
library(rworldmap)
library(grid)
library(gridExtra)

setwd("~/Документы/DATA")

# Данные с сайта Федеральной таможенной службы РФ
# Ссылка на страницу выгрузки данных: 
# http://stat.customs.ru/apex/f?p=201:2:1203052782525272::NO
# Прямая ссылка на данные:
# https://docs.google.com/spreadsheets/d/13no2a7WCsHkT4NVVJkMdbS06amrQjxLkwqaLRe__9Yw/edit?usp=sharing

# Загрузка данных
data_custom <- read.csv("Таможенная статистика внешторговли РФ.csv", 
                        sep = ";", dec = ",", as.is = T)
data_custom$Год <- gsub(" год", "", data_custom$Год)
data_custom$Год <- as.integer(data_custom$Год)

# Консолидация данных по округам (данные в целом по РФ)
data_custom <- data_custom %>% 
    group_by(Год, Направление.перемещения, Страна) %>%
    summarise(sum(Стоимость..долл..США))

# Разбивка направления перемещения на экспорт и импорт
data_custom <- data_custom %>% 
    spread(Направление.перемещения, `sum(Стоимость..долл..США)`)

# Вычисление торгового оборота (экспорт + импорт)
data_custom$Оборот <- rowSums(data_custom[,c(3,4)], na.rm = T)

# Вычисление суммарного торгового оборота по годам
sum_data_custom <- data_custom %>%
    group_by(Год) %>%
    summarise(sum(Оборот, na.rm = T), sum(Импорт, na.rm = T), 
              sum(Экспорт, na.rm = T))
colnames(sum_data_custom) <- c("Год", "Оборот", "Импорт", "Экспорт")
sum_data_custom <- subset(sum_data_custom, Год != "2016")
sum_data_custom <- gather(sum_data_custom, Оборот2, Значение, -c(Год, Оборот))

#Перевод долларов США в млрд долларов США
sum_data_custom$Значение <- round(sum_data_custom$Значение/(10^9), digits = 0)

# Импорт в 2015 г. упал на 42% по сравнению с 2013, экспорт - на 35%.

# Датасет по торговому обороту с другого раздела сайта:
# http://stat.customs.ru/apex/f?p=201:7:1904119482513115::NO
data_custom2 <- read.csv("Итоги внешней торговли РФ.csv", dec = ",")
data_custom2 <- data_custom2 %>%
    gather(Оборот, Значение, -Год)
data_custom2$Значение <- round(data_custom2$Значение/(1000), digits = 0)

# Данные с разных разделов сайта немного расходятся (?)

# График "Торговый оборот России со странами мира"
ggplot(data_custom2, aes(x = Год, y = Значение, colour = Оборот))+
    geom_line()+
    geom_point()+
    ggtitle("Торговый оборот России со странами мира")+
    geom_text(aes(label = paste0("$", Значение, " млрд")), 
              colour = "black", vjust = -1)+
    scale_x_continuous(name = "Год\n",
                       breaks = c(seq(2011, 2015, 1)), 
                       limits = c(2011, 2015))+
    scale_y_continuous(name = "млрд долларов США")+
    theme(legend.title = element_blank(),
          legend.position = "top",
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 14), 
          title = element_text(size = 16))
grid.text("Источник: Федеральная таможенная служба РФ",
          x = 0.02, y = 0.01, just = c("left", "bottom"), 
          gp = gpar(fontsize = 11))
grid.text("www.silentio.su",
          x = 0.98, y = 0.01, just = c("right", "bottom"), 
          gp = gpar(fontsize = 11))

# Получение геокода
data_custom$Код <- gsub("-.*", "", data_custom$Страна)
data_custom$Страна <- gsub(".*-", "", data_custom$Страна)

# Карта торгового оборота за 2013
data_custom_2013_trade <- subset(data_custom, Год == "2013",
                                  select = c(Код, Оборот))
GEO_data_custom_2013_trade <- joinCountryData2Map(data_custom_2013_trade, joinCode="ISO2", 
                                                  nameJoinColumn="Код")
mapCountryData(GEO_data_custom_2013_trade, 
               nameColumnToPlot="Оборот", 
               catMethod =  "fixedWidth",
               missingCountryCol = "lightblue", nameColumnToHatch = T,
               mapTitle="Торговый оборот России со странами мира в 2013 году")
mtext("доллары США\n
      Источник: Федеральная таможенная служба РФ © silentio.su",side = 1,line = 3)
rm(GEO_data_custom_2013_trade)

# Карта торгового оборота за 2015
data_custom_2015_trade <- subset(data_custom, Год == "2015",
                                 select = c(Код, Оборот))
GEO_data_custom_2015_trade <- joinCountryData2Map(data_custom_2015_trade, joinCode="ISO2", 
                                                  nameJoinColumn="Код")
mapCountryData(GEO_data_custom_2015_trade, 
               nameColumnToPlot="Оборот", 
               catMethod =  "fixedWidth",
               missingCountryCol = "lightblue", nameColumnToHatch = T,
               mapTitle="Торговый оборот России со странами мира в 2015 году")
mtext("доллары США\n
      Источник: Федеральная таможенная служба РФ © silentio.su",side = 1,line = 3)
rm(GEO_data_custom_2015_trade)

# Вычисление доли в импорте
data_custom_percents <- subset(data_custom, Год == "2013" | Год == "2015",
                               select = c("Год", "Страна", "Импорт"))
data_custom_percents$Percents <- 0
for (i in 1:nrow(data_custom_percents)){
    ifelse(data_custom_percents$Год[i] == "2013", 
           data_custom_percents$Percents[i] <- data_custom_percents$Импорт[i]/
               sum(data_custom_percents$Импорт[data_custom_percents$Год == "2013"],
                   na.rm = T),
           data_custom_percents$Percents[i] <- data_custom_percents$Импорт[i]/
               sum(data_custom_percents$Импорт[data_custom_percents$Год == "2015"],
                   na.rm = T))  
}

# Перевод в проценты
data_custom_percents$Percents <- round(data_custom_percents$Percents, digits = 4)*100

data_custom_percents_top10 <- data_custom_percents %>%
    group_by(Год) %>%
    arrange(desc(Percents)) %>%
    slice(1:10)

# График "ТОП 10 стран по импорту"
data_custom_percents_top10$Страна <- reorder(data_custom_percents_top10$Страна,
                                               data_custom_percents_top10$Percents)
ggplot(data_custom_percents_top10, aes(x = as.factor(Страна), y = Percents, 
                                       fill = as.factor(Год)))+
    geom_bar(stat = "identity", position = "dodge")+
    coord_flip()+
    ggtitle("Импорт в Российскую Федерацию. Топ-10 стран")+
    scale_x_discrete(name = " ")+
    labs(fill = "Годы:")+
    geom_text(aes(label = paste0(Percents, "%")), position=position_dodge(.9),
              hjust = 1.1, colour = "white")+
    scale_y_continuous(name = "доля в российском торговом обороте, %\n")+
    theme(axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 14), 
          title = element_text(size = 16))
grid.text("Источник: Федеральная таможенная служба РФ",
          x = 0.02, y = 0.01, just = c("left", "bottom"), 
          gp = gpar(fontsize = 11))
grid.text("www.silentio.su",
          x = 0.98, y = 0.01, just = c("right", "bottom"), 
          gp = gpar(fontsize = 11))

# Вычисление разницы доли в импорте 2013 г. и 2015 г. по странам
data_custom_percents$Страна <- as.factor(data_custom_percents$Страна)
data_custom_percents$Импорт <- NULL
data_custom_percents_top <- data_custom_percents %>%
    spread(Год, Percents)
data_custom_percents_top$Разница <- data_custom_percents_top$`2015`-
    data_custom_percents_top$`2013`
data_custom_percents_top$Рост <- data_custom_percents_top$Разница/
    data_custom_percents_top$`2013`
data_custom_percents_top <- data_custom_percents_top[,1:1:4]
colnames(data_custom_percents_top) <- c("Страна",
                                        "Доля в импорте, 2013 г., %",
                                        "Доля в импорте, 2015 г., %",
                                        "Разница между 2015 и 2013 гг.")

# Таблица "Топ-10 стран, которые нарастили долю импорте в Россию"
grid.text("Топ-10 стран, которые нарастили долю в российском импорте",
          x = 0.02, y = 0.93, just = c("left", "bottom"), 
          gp = gpar(fontsize = 16))
grid.table(arrange(data_custom_percents_top, 
                   desc(`Разница между 2015 и 2013 гг.`))[1:10,])
grid.text("Источник: Федеральная таможенная служба РФ",
          x = 0.02, y = 0.01, just = c("left", "bottom"), 
          gp = gpar(fontsize = 11))
grid.text("www.silentio.su",
          x = 0.98, y = 0.01, just = c("right", "bottom"), 
          gp = gpar(fontsize = 11))

# Таблица "Топ-10 стран, которые потеряли долю в российском импорте"
grid.text("Топ-10 стран, которые потеряли долю в торговом обороте с Россией",
          x = 0.02, y = 0.93, just = c("left", "bottom"), 
          gp = gpar(fontsize = 16))
grid.table(arrange(data_custom_percents_top, 
                   `Разница между 2015 и 2013 гг.`)[1:10,])
grid.text("Источник: Федеральная таможенная служба РФ",
          x = 0.02, y = 0.01, just = c("left", "bottom"), 
          gp = gpar(fontsize = 11))
grid.text("www.silentio.su",
          x = 0.98, y = 0.01, just = c("right", "bottom"), 
          gp = gpar(fontsize = 11))

