# Оценка зависимости уровня жизни украинцев от количества снесённых памятников 
# Ленина
# © silentio - gonzo data science lab | silentio.su - 27 сентября 2016 г.

library(dplyr)
library(tidyr)
library(ggplot2)
library(rworldmap)
library(grid)
library(gridExtra)

setwd("~/Документы/DATA")

# По данным культуролога Дмитрия Кудинова (сайт http://leninstatues.ru/leninopad),
# в 1991 г. на Украине было около 5,5 тыс. памятников Ленину, летом 2016 - 900, 
# на конец 2015 - около 1100.
# По данным Всемирного банка, в 1991 г. ВВП Украины был $189 млрд (в рыночных 
# ценах 2010 г.), 2015 г. - $121 млрд (в рыночных ценах 2010 г.).
ukrlenin_GDP <- data.frame(Year = c(1991, 2015, 1991, 2015),
                           key = c("памятники Ленину на Украине, шт.", 
                                   "памятники Ленину на Украине, шт.", 
                                   "ВВП Украины (в ценах 2010 г.), $ млрд",
                                   "ВВП Украины (в ценах 2010 г.), $ млрд"),
                           value = c(5500, 1100, 189, 121))
# Колонка с подписями
ukrlenin_GDP$labels <- c("5,5 тыс.", "1,1 тыс.", "$189 млрд", "$121 млрд")

# График "Чем меньше памятников Ленину на Украине - тем меньше ВВП"
ggplot(ukrlenin_GDP, aes(x = as.factor(Year), y = value, fill = key))+
    geom_bar(stat = "identity")+
    facet_grid(key ~ ., scales = "free" )+
    ggtitle("Чем меньше памятников Ленину на Украине - тем меньше ВВП")+
    labs(x = "Год\n",
         y = " ",
         fill = " ")+
    theme(legend.position = "top",
          legend.text = element_text(size = 15),
          axis.text.x = element_text(size = 15), 
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          title = element_text(size = 17))+
    geom_text(aes(label = labels), colour = "black", size = 7, vjust = 1.5)
grid.text("Источники: leninstatues.ru, World Bank",
          x = 0.02, y = 0.02, just = c("left", "bottom"), 
          gp = gpar(fontsize = 15, col = "dimgrey"))
grid.text("© silentio.su",
          x = 0.98, y = 0.02, just = c("right", "bottom"), 
          gp = gpar(fontsize = 15, col = "dimgrey"))

# Загрузка данных "Статистика ленинопада на Украине - leninstatues.ru.csv".
# Колонка Statues содержит данные по количеству снесённых памятников Ленина на
# Украине, источник: http://leninstatues.ru/leninopad, актуальность 2016.09.06
# Колонка USD.UAH содержит стоимость 1 доллара США в гривнах, срединная точка,
# источник: Oanda.com (https://www.oanda.com/lang/ru/currency/historical-rates/)
# Ссылка на данные:
# https://docs.google.com/spreadsheets/d/18mVC4beCz6NST_SEaVCVzQcEUOPkRBXxMkUx7bCsjsc/edit?usp=sharing
ukrlenin <- read.csv("Статистика ленинопада на Украине - leninstatues.ru.csv")

# Добавление колонки с данными по снесённым памятникам Ленина с аккумуляцией
ukrlenin$Statues.accum[1] <- ukrlenin$Statues[1]
for(i in 2:nrow(ukrlenin)){
    ukrlenin$Statues.accum[i] <- ukrlenin$Statues.accum[i-1]+ukrlenin$Statues[i]
}

# Корреляционный тест (Пирсон): cor = 0.9, p-value = 4.814e-15
# Прямая корреляционная зависимость кол-ва снесённых памятников и динамики курса
# доллара к гривне
cor.test(ukrlenin$Statues.accum, ukrlenin$USD.UAH)

# Объединение колонок
ukrlenin <- ukrlenin %>%
    gather(key, value, -Date)

# Русификация
ukrlenin$key <- as.factor(ukrlenin$key)
levels(ukrlenin$key) <- c("кол-во снесённых памятников Ленину по месяцам",
                          "кол-во снесённых памятников Ленину",
                          "курс доллара к гривне")

# Перевод колонки Date в формат даты
ukrlenin$Date <- as.Date(ukrlenin$Date, "%Y-%m-%d")
    
# График "Чем меньше памятников Ленину на Украине - тем дешевле гривна"
ggplot(filter(ukrlenin, key != "кол-во снесённых памятников Ленину по месяцам"), 
       aes(x = Date, y = value, colour = key))+
    geom_line(size = 1)+
    geom_point(size = 2)+
    facet_grid(key ~ ., scales = "free" )+
    ggtitle("Чем меньше памятников Ленину на Украине - тем дешевле гривна")+
    labs(x = "Дата\n",
         y = " ",
         colour = " ")+
    theme(legend.position = "top",
          legend.text = element_text(size = 15),
          axis.text.x = element_text(size = 15), 
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          title = element_text(size = 17))
grid.text("Источники: leninstatues.ru, НБУ",
          x = 0.02, y = 0.02, just = c("left", "bottom"), 
          gp = gpar(fontsize = 15, col = "dimgrey"))
grid.text("© silentio.su",
          x = 0.98, y = 0.02, just = c("right", "bottom"), 
          gp = gpar(fontsize = 15, col = "dimgrey"))
