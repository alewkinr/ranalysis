#install.packages("moments")
#install.packages("nortest")
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("ggline")
#install.packages("car")
#install.packages("magrittr") # only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%

library(magrittr) # need to run every time you start R and want to use %>%
library(reshape2)
library(bstats)
library(ggplot2)
library(rlms)
library(dplyr)
library(VIM)
library(moments)
library(nortest)
library(car)
library(lmtest)

rlms <- rlms_read("~/Desktop/RLMS/2017.sav")

names(rlms)
# Чистовой df
df <- data.frame(rlms$idind,rlms$v_diplom, rlms$v_marst, rlms$vj60,
                       rlms$vj71, rlms$vj72.172, rlms$vj262, rlms$vj170.1, rlms$vj4.1,rlms$vj6.2)
names(df) <- c('id','education', 'marital', 'salary', 'ad_education', 
               'kids', 'lang', 'dms', 'branch', 'worktime')

# Превращаем нужные переменные в факторные
df$id <- as.numeric(df$id)
df$education <- as.factor(df$education)
df$marital <- as.factor(df$marital)
df$ad_education <- as.factor(df$ad_education)
df$lang <- as.factor(df$lang)
df$dms <- as.factor(df$dms)
df$branch <- as.factor(df$branch)

df$salary <- as.numeric(df$salary)
df$kids <- as.numeric(df$kids)
df$worktime <- as.numeric(df$worktime)


#2.1	Анализ особенностей данных: потенциальные ошибки и пропущенные значения, группы и выбросы
#2.1.1. Анализ количественных данных
hist(df$salary)
hist(df$kids)
hist(df$worktime)

# удаляем пустые строки в количественных переменных
df_no_na <- na.omit(df[, c('id','salary', 'kids', 'worktime')])
# кол-во строк в новом DF
nrow(df_no_na)
# Смотрим саммари по кол-венным переменным
summary(df_no_na)
# Cчитаем и удаляем нулевые значения кол-венных признаках
nrow(df_no_na[(df_no_na$salary==0),])
nrow(df_no_na[(df_no_na$worktime==0),])

df_no_na <- df_no_na[!(df_no_na$salary==0),]
# считаем матожидание и дисперсию
sem<-function(x){sd(x)/sqrt(length(x))}
# Матожидание
sem(df_no_na$salary); sem(df_no_na$kids); sem(df_no_na$worktime)
#Дисперсия
sd(df_no_na$salary); sd(df_no_na$kids); sd(df_no_na$worktime)
#Добавляем коэфф. ассиметрии и эксцесса
skewness(df_no_na$salary); skewness(df_no_na$kids); skewness(df_no_na$worktime)
kurtosis(df_no_na$salary); kurtosis(df_no_na$kids); kurtosis(df_no_na$worktime)
# Проверка правила трех сигм
mean_salary <- mean(df_no_na$salary)
mean_worktime <- mean(df_no_na$worktime)
sd_salary <- sd(df_no_na$salary)
sd_worktime <- sd(df_no_na$worktime)
top_edge_sal = mean_salary+3*sd_salary
bot_edge_sal = mean_salary-3*sd_salary
top_edge_wokrtime = mean_worktime+3*sd_worktime
bot_edge_wokrtime = mean_worktime-3*sd_worktime
print(cbind(mean_salary, sd_salary, top_edge_sal,bot_edge_sal))
print(cbind(mean_worktime, sd_worktime, top_edge_wokrtime,bot_edge_wokrtime))
# Кол-во выбросов по правилу трех сигм признака Salary
length(df_no_na$salary[df_no_na$salary>top_edge_sal])
length(df_no_na$salary[df_no_na$salary<bot_edge_sal])

# Кол-во выбросов по правилу трех сигм признака worktime
length(df_no_na$worktime[df_no_na$worktime>top_edge_wokrtime])
length(df_no_na$worktime[df_no_na$worktime<bot_edge_wokrtime])


df_no_na <- df_no_na[(df_no_na$salary<top_edge_sal & df_no_na$salary>bot_edge_sal & df_no_na$worktime<top_edge_wokrtime & df_no_na$worktime>bot_edge_wokrtime),]

df_no_na$salary_log = log(df_no_na$salary)
df_no_na$worktime_log = log(df_no_na$worktime)

hist(df_no_na$salary_log, probability = TRUE)
hist(df_no_na$worktime_log, probability = TRUE)

# Собираем статистики по логарифмированным кол-венникам
summary(cbind(df_no_na$salary_log, df_no_na$worktime_log))
nrow(df_no_na)

# Проверка правила трех сигм
mean_salary <- mean(df_no_na$salary_log)
mean_worktime <- mean(df_no_na$worktime_log)
sd_salary <- sd(df_no_na$salary_log)
sd_worktime <- sd(df_no_na$worktime_log)
top_edge_sal = mean_salary+3*sd_salary
bot_edge_sal = mean_salary-3*sd_salary
top_edge_wokrtime = mean_worktime+3*sd_worktime
bot_edge_wokrtime = mean_worktime-3*sd_worktime
print(cbind(top_edge_sal,bot_edge_sal))
print(cbind(top_edge_wokrtime,bot_edge_wokrtime))

# Кол-во выбросов по правилу трех сигм признака Salary
length(df_no_na$salary_log[df_no_na$salary_log>top_edge_sal])
length(df_no_na$salary_log[df_no_na$salary_log<bot_edge_sal])

# Кол-во выбросов по правилу трех сигм признака worktime
length(df_no_na$worktime_log[df_no_na$worktime_log>top_edge_wokrtime])
length(df_no_na$worktime_log[df_no_na$worktime_log<bot_edge_wokrtime])

df_no_na <- df_no_na[(df_no_na$salary_log<top_edge_sal & df_no_na$salary_log>bot_edge_sal & df_no_na$worktime_log<top_edge_wokrtime & df_no_na$worktime_log>bot_edge_wokrtime),]
nrow(df_no_na)

hist(df_no_na$salary_log, probability = TRUE)
hist(df_no_na$worktime_log, probability = TRUE)

summary(cbind(df_no_na$salary_log, df_no_na$worktime_log))
print(c(sd(df_no_na$salary_log), sd(df_no_na$worktime_log)))

skewness(df_no_na$salary_log)
skewness(df_no_na$worktime_log)

kurtosis(df_no_na$salary_log)
kurtosis(df_no_na$worktime_log)

# чистый датафрейм
#cdf <- merge(df, df_no_na, by = 'id')
# бекапим данные
bkp_df <- df_no_na
bkp_df

#Берем только факторные фичи и удаляем пустые строки в количественных переменных
df_no_na <- na.omit(df[, c('id','education', 'marital', 'ad_education', 'kids','lang', 'dms', 'branch')])
nrow(df_no_na)

aggr(df[, c('id','education', 'marital', 'ad_education', 'kids', 'lang', 'dms', 'branch')])

# вырезаем lang
df_no_na <- na.omit(df[, c('id','education', 'marital', 'ad_education', 'kids', 'dms', 'branch')])
nrow(df_no_na)
aggr(df[, c('id','education', 'marital', 'ad_education', 'kids', 'dms', 'branch')])

#Смотрим саммари по факторным признакам (branch)
df_no_na$branch_bool[df_no_na$branch!=12] <- 0
df_no_na$branch_bool[df_no_na$branch==12] <- 1
summary(df_no_na$branch_bool)

df_no_na$branch_bool <- as.numeric(df_no_na$branch_bool)
hist(df_no_na$branch_bool)
df_no_na$branch_bool
df_no_na$branch_bool <- as.logical(df_no_na$branch_bool)

#Смотрим саммари по факторным признакам (education)
summary(df$education)
summary(df_no_na$education)
barplot(table(df_no_na$education), main = "Histogram of df_no_na$education")

#Смотрим саммари по факторным признакам (marital)
summary(df$marital)
summary(df_no_na$marital)
barplot(table(df_no_na$marital), main = "Histogram of df_no_na$marital")

#Смотрим саммари по факторным признакам (ad_education_bool)
summary(df$ad_education)
df_no_na$ad_education_bool[df_no_na$ad_education!=1] <- 0
df_no_na$ad_education_bool[df_no_na$ad_education==1] <- 1
df_no_na$ad_education_bool <- as.logical(df_no_na$ad_education_bool)
summary(df_no_na$ad_education_bool)
barplot(table(df_no_na$ad_education_bool), main = "Histogram of df_no_na$ad_education_bool")

#Смотрим саммари по факторным признакам (kids)
summary(df$kids)
df_no_na$kids_factor <- as.factor(df_no_na$kids)
summary(df_no_na$kids_factor)
barplot(table(df_no_na$kids_factor), main = "Histogram of df_no_na$kids_factor")

#Смотрим саммари по факторным признакам (dms)
summary(df$dms)
df_no_na$dms_bool[df_no_na$dms!=1] <- 0
df_no_na$dms_bool[df_no_na$dms==1] <- 1
df_no_na$dms_bool <- as.logical(df_no_na$dms_bool)
summary(df_no_na$dms_bool)
barplot(table(df_no_na$dms_bool), main = "Histogram of df_no_na$dms_bool")

# СКЛЕИВАЕМ ОДИН ДАТАСЕТ
first_df <- bkp_df[,c('id', "salary", "salary_log", "worktime", "worktime_log")]
second_df <- df_no_na[,c('id', "education", "marital", "ad_education_bool", "kids_factor", "dms_bool", "branch_bool")]
cdf <- merge(second_df, first_df, by = 'id')
nrow(cdf)
aggr(cdf)

#2.2 Анализ статистической связи
#2.2.1. Графический анализ пары "числовая зависимая переменная - качественная независимая переменная"

qplot(x=cdf$education, y=cdf$salary_log, geom=c("boxplot"), main = "education/salary")
qplot(x=cdf$marital, y=cdf$salary_log, geom=c("boxplot"), main = "marital/salary")
qplot(x=cdf$ad_education_bool, y=cdf$salary_log, geom=c("boxplot"), main = "ad_educatin_bool/salary")
qplot(x=cdf$kids_factor, y=cdf$salary_log, geom=c("boxplot"), main = "kids_factor/salary")
qplot(x=cdf$dms_bool, y=cdf$salary_log, geom=c("boxplot"), main = "dms_bool/salary")
qplot(x=cdf$branch_bool, y=cdf$salary_log, geom=c("boxplot"), main = "branch_bool/salary")

#2.2.2 Графический анализ пары "числовая зависимая переменная - числовая независимая переменная"
ggplot(cdf, aes(x=cdf$worktime_log, y=cdf$salary_log)) + geom_point(size=2, shape=23)+geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
qplot(data=cdf, worktime_log, salary_log) +stat_smooth(method = 'lm')

cdf_cor_pearson <- cor(cdf[,c("worktime_log","salary_log")], method = 'pearson')
cdf_cor_spearman <- cor(cdf[,c("worktime_log","salary_log")], method = 'spearman')
cdf_cor_pearson
cdf_cor_spearman

backup <- cdf

# 20% —> 1113 rows
y_test <- cdf[1:1113,]
nrow(y_test)

# 80% —> 4450 rows
y_train <- cdf[1113:5562,]
nrow(y_train)



#Модель 1
features <- y_train[,c("education", "marital", "ad_education_bool", "kids_factor", "dms_bool", "branch_bool", "worktime","worktime_log")]
fit_1 <- lm(y_train$salary_log~., features)
summary(fit_1)
predict_1 <- predict(fit_1, features)
rmse <- mean((y_train$salary_log - predict_1)^2)
print(rmse)

#Модель 2
fit_2 <- lm(data=y_train, salary_log~dms_bool+branch_bool+kids_factor+marital)
summary(fit_2)
predict_2 <- predict(fit_2, features)
rmse <- mean((y_train$salary_log - predict_2)^2)
print(rmse)

#Модель 3
features <- y_train[,c("education", "marital", "ad_education_bool", "kids_factor", "dms_bool", "branch_bool","worktime_log")]
fit_1 <- lm(y_train$salary_log~., features)
summary(fit_1)
predict_1 <- predict(fit_1, features)
rmse <- mean((y_train$salary_log - predict_1)^2)
print(rmse)

#General VIF
r2.1 <- summary(fit_1)$r.squared
vif1 <- 1 / (1 - r2.1)
vif1

#Итоги по модели:
summary(fit_1)$coefficients
confint(fit_1, level = 0.90)
vif(fit_1)


#Выбросы
r <- rstandard(fit_1)
plot(fitted(fit_1), r, ylim = c(-5, 5))
abline(h=0, lty = 2)
abline(h = c(-2, 2), lty = 2, col = "red")


r <- rstudent(fit_1)
plot(fitted(fit_1), r, ylim = c(-5, 5))
abline(h=0, lty = 2)
abline(h = c(-4.18, 4.18), lty = 2, col = "red")

max(abs(rstudent(fit_1)))


#Расстояние кука:
plot(cooks.distance(fit_1))
abline(h = 4/(4450 - 13 - 1), lty = 2)

#Тест Уайта
white <- bptest(fit_1, data=y_train, varformula= ~ worktime_log+I(worktime_log^2) +ad_education_bool+I(ad_education_bool^2)+dms_bool+I(dms_bool^2)+branch_bool+I(branch_bool^2))
white

#Оптимизация модели
#model1
summary(fit_1)
AIC(fit_1)
BIC(fit_1)

#model2
fit_2 <- lm(data=y_train, salary_log~dms_bool+branch_bool+marital+worktime_log)
summary(fit_2)
AIC(fit_2)
BIC(fit_2)

#model 3
fit_3 <- lm(data=y_train, salary_log~dms_bool+branch_bool+kids_factor+marital)
summary(fit_3)
AIC(fit_3)


#Проверка прогностических свойств
predict_1 <- predict(fit_1, features)
rmse <- mean((y_train$salary_log - predict_1)^2)
print(rmse)
