#Устанавливаем и подключаем сет нужных пакетов
install.packages("AER")
install.packages("sandwich")
install.packages("lmtest")
install.packages("car")
install.packages("stargazer")
install.packages("ggplot2")
install.packages("openintro")
install.packages("OIdata")
install.packages("gdata")
install.packages("doBy")
install.packages("plm")
install.packages("ivpack")
install.packages("Rcpp")
install.packages("GGally") #Для матрицы корреляции
install.packages("dplyr") #Для матрицы корреляции

library("Rcpp")
library("AER")
library("sandwich")
library("lmtest")
library("car")
library("stargazer")
library("ggplot2")
library("openintro")
library("OIdata")
library("gdata")
library("doBy")
library("plm") #Панельные данные
library("ivpack") #Инстурменты
library("tidyverse")
library("caret")
library("GGally") #Для матрицы корреляции
library("dplyr") #Для матрицы корреляции

# Стандартные ошибки 
clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

#Загрузим данные
D <- read.csv("DataSet.csv", sep=";", dec=",", header=TRUE)


# Описательные статистики
  stargazer(D, type="text", median=TRUE,
            digits=2, title="Property Data")

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Модели
#Модель pooled-regression
mifl_pool = plm(MIFL ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + DIV,
           data = D, index = c("Ticker","Time"), model="pooling")
summary(mifl_pool)

ltfl_pool = plm(LTFL ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + MTB + DIV,
                data = D, index = c("Ticker","Time"), model="pooling")
summary(ltfl_pool)

stfl_pool = plm(STFL ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + MTB + DIV,
                data = D, index = c("Ticker","Time"), model="pooling")
summary(stfl_pool)

wacc_pool = plm(WACC ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + MTB + DIV,
                data = D, index = c("Ticker","Time"), model="pooling")
summary(wacc_pool)


#Модель с фиксированными эффектами
mifl_fix = plm(MIFL ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + DIV,
           data = D, index = c("Ticker","Time"), model="within", effect="individual")
summary(mifl_fix)

ltfl_fix = plm(LTFL ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + MTB + DIV,
               data = D, index = c("Ticker","Time"), model="within", effect="individual")
summary(ltfl_fix)

stfl_fix = plm(STFL ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + MTB + DIV,
               data = D, index = c("Ticker","Time"), model="within", effect="individual")
summary(stfl_fix)

wacc_fix = plm(WACC ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + MTB + DIV,
               data = D, index = c("Ticker","Time"), model="within", effect="individual")
summary(wacc_fix)


#Модель со случайными эффектами
mifl_rndm = plm(MIFL ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + DIV,
           data = D, index = c("Ticker","Time"), 
           effect="twoways", model="random", random.method = "walhus")
summary(mifl_rndm)

ltfl_rndm = plm(LTFL ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + MTB + DIV,
                data = D, index = c("Ticker","Time"), 
                effect="twoways", model="random", random.method = "walhus")
summary(ltfl_rndm)

stfl_rndm = plm(STFL ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + MTB + DIV,
                data = D, index = c("Ticker","Time"), 
                effect="twoways", model="random", random.method = "walhus")
summary(stfl_rndm)

wacc_rndm = plm(WACC ~ SIZEBD + INDEPD + CEODUO + BIGF + WOMEN + PNDM + COMPSIZE + ROA + GROWTH + FATA + NDTS + MTB + DIV,
                data = D, index = c("Ticker","Time"), 
                effect="twoways", model="random", random.method = "walhus")
summary(wacc_rndm)


---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Сводная таблица со всеми переменными

#MIFL
stargazer(mifl_pool, mifl_fix, mifl_rndm, 
          title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled", "FE", "RE"), 
          df=FALSE, digits=4)

stargazer(mifl_pool, mifl_fix, mifl_rndm, 
          se=list(clse(mifl_pool), clse(mifl_fix), clse(mifl_rndm)), 
          title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled", "FE", "RE"), 
          df=FALSE, digits=4)

#LTFL
stargazer(ltfl_pool, ltfl_fix, ltfl_rndm, 
          title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled", "FE", "RE"), 
          df=FALSE, digits=4)

stargazer(ltfl_pool, ltfl_fix, ltfl_rndm, 
          se=list(clse(ltfl_pool), clse(ltfl_fix), clse(ltfl_rndm)), 
          title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled", "FE", "RE"), 
          df=FALSE, digits=4)

#STFL
stargazer(stfl_pool, stfl_fix, stfl_rndm, 
          title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled", "FE", "RE"), 
          df=FALSE, digits=4)

stargazer(stfl_pool, stfl_fix, stfl_rndm,
          se=list(clse(stfl_pool), clse(stfl_fix), clse(stfl_rndm)), 
          title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled", "FE", "RE"), 
          df=FALSE, digits=4)

#WACC
stargazer(wacc_pool, wacc_fix, wacc_rndm, 
          title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled", "FE", "RE"), 
          df=FALSE, digits=4)

stargazer(wacc_pool, wacc_fix, wacc_rndm,
          se=list(clse(wacc_pool), clse(wacc_fix), clse(wacc_rndm)), 
          title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled", "FE", "RE"), 
          df=FALSE, digits=4)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Показатели VIF
  
vif(mifl_pool)
vif(ltfl_pool)
vif(stfl_pool)
vif(wacc_pool)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##MIFL
#Тест на линейное ограничение для сравнения pooled и FE
plmtest(mifl_pool, effect="twoways",type="ghm")

#Тест Бреуша-Пагана для сравнения pooled и RE
bptest(mifl_pool, mifl_rndm)

#Тест Хаусмана для сравнения FE и RE
phtest(mifl_fix, mifl_rndm)


##LTFL
#Тест на линейное ограничение для сравнения pooled и FE
plmtest(ltfl_pool, effect="twoways",type="ghm")

#Тест Бреуша-Пагана для сравнения pooled и RE
bptest(ltfl_pool, ltfl_rndm)

#Тест Хаусмана для сравнения FE и RE
phtest(ltfl_fix, ltfl_rndm)


##STFL
#Тест на линейное ограничение для сравнения pooled и FE
plmtest(stfl_pool, effect="twoways",type="ghm")

#Тест Бреуша-Пагана для сравнения pooled и RE
bptest(stfl_pool, stfl_rndm)

#Тест Хаусмана для сравнения FE и RE
phtest(stfl_fix, stfl_rndm)


##WACC
#Тест на линейное ограничение для сравнения pooled и FE
plmtest(wacc_pool, effect="twoways",type="ghm")

#Тест Бреуша-Пагана для сравнения pooled и RE
bptest(wacc_pool, wacc_rndm)

#Тест Хаусмана для сравнения FE и RE
phtest(wacc_fix, wacc_rndm)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Сводная таблица для диплома
stargazer(mifl_fix, ltfl_pool, stfl_pool, wacc_fix,
            se=list(clse(mifl_fix), clse(ltfl_pool), clse(stfl_pool), clse(wacc_fix)), 
            title="Panel regressions", type="text", 
            column.labels=c("FE", "Pooled", "Pooled", "FE"), 
            df=FALSE, digits=2, out = "MODELS.html", summary = FALSE)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
  
##Корреляционная матрица перменных
Cor_data_1 <- D[,c(4:20)]
cor(Cor_data_1)
ggcorr(Cor_data_1, label = TRUE)

bptest(wacc_fix)













