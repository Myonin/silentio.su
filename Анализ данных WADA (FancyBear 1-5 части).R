# Анализ данных WADA, выложенных хакерской группой "FancyBear",
# части с 1 по 5, актуальность на 2016-09-23.
# © silentio - gonzo data science lab | silentio.su - 25 сентября 2016 г.

library(dplyr)
library(tidyr)
library(ggplot2)
library(rworldmap)
library(grid)
library(gridExtra)

setwd("~/Документы/DATA")

# Загрузка данных WADA, выложенных хакерской группой "FancyBear
# Ссылка на сайт с данными: https://fancybear.net/
# Ссылка на датасет:

# Загрузка данных
wada <- read.csv("WADA databases by FancyBear Part 1-5 2016-09-23.csv",
                 fileEncoding = "UTF-8")

# Датасет по количеству упоминаний стран в данных
wada_countries <- wada %>%
    count(Country) %>%
    arrange(desc(n))
# Перевод в проценты и округление до 2 знака
wada_countries$Percents <- round(wada_countries$n/sum(wada_countries$n)*100,
                                 digits = 2)

# График "Страны, спортсменам которых разрешено принимать допинг по медицинским
# показаниям"
ggplot(wada_countries[1:7,], aes(x = reorder(Country, Percents), 
                                 y = Percents))+
    geom_bar(stat = "identity")+
    coord_flip()+
    geom_label(aes(label = paste0(Percents, "%")), size = 5)+
    ggtitle(expression(atop("Страны с наибольшим числом \"больных\" спортсменов", 
                            atop(italic("страны, спортсменам которых разрешено принимать допинг по медицинским показаниям")))))+
    labs(x = " ",
         y = "% от суммарного количества спортсменов по всем странам\n")+
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          title = element_text(size = 16))
grid.text("Источники: WADA, FancyBear.net",
          x = 0.02, y = 0.02, just = c("left", "bottom"), 
          gp = gpar(fontsize = 14, col = "dimgrey"))
grid.text("© silentio.su",
          x = 0.98, y = 0.02, just = c("right", "bottom"), 
          gp = gpar(fontsize = 14, col = "dimgrey"))

# Датасет по количеству упоминаний спортивных дисциплин
wada_discipline <- wada %>%
    count(Discipline) %>%
    arrange(desc(n))
# Перевод в проценты и округление до 2 знака
wada_discipline$Percents <- round(wada_discipline$n/sum(wada_discipline$n)*100,
                                 digits = 2)

# График "Топ спортдисциплин с наибольшим числом \"больных\" спортсменов"
ggplot(wada_discipline[1:8,], aes(x = reorder(Discipline, Percents), 
                                 y = Percents))+
    geom_bar(stat = "identity")+
    coord_flip()+
    geom_label(aes(label = paste0(Percents, "%")), size = 5)+
    ggtitle(expression(atop("Cпортдисциплины с наибольшим числом \"больных\" спортсменов", 
                            atop(italic("дисциплины, в которых больше всего спортсменов с разрешением принимать допинг")))))+
    labs(x = " ",
         y = "% от суммарного количества спортсменов по всем дисциплинам\n")+
    scale_y_continuous(limits = c(0, 20))+
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          title = element_text(size = 16))
grid.text("Источники: WADA, FancyBear.net",
          x = 0.02, y = 0.02, just = c("left", "bottom"), 
          gp = gpar(fontsize = 14, col = "dimgrey"))
grid.text("© silentio.su",
          x = 0.98, y = 0.02, just = c("right", "bottom"), 
          gp = gpar(fontsize = 14, col = "dimgrey"))

# США: плавание - 12% всех разрешённых нарушений по стране, теннис - 12%
wada %>%
    filter(Country == "США") %>%
    count(Discipline) %>%
    arrange(desc(n)) %>%
    mutate(Percents = round(n/sum(n)*100,
                            digits = 2))

# Великобритания: велоспорт - 26.09%, гребля - 21.74%, хоккей на траве - 13.04%
wada %>%
    filter(Country == "Великобритания") %>%
    count(Discipline) %>%
    arrange(desc(n)) %>%
    mutate(Percents = round(n/sum(n)*100,
                 digits = 2))

# Канада: футбол - 36.36%, горный велосипед - 18.18%, плавание - 18.18%
wada %>%
    filter(Country == "Канада") %>%
    count(Discipline) %>%
    arrange(desc(n)) %>%
    mutate(Percents = round(n/sum(n)*100,
                            digits = 2))
