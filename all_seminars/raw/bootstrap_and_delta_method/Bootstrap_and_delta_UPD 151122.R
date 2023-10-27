# бутстрап- ДИ для медианной зарплаты
library("readxl")
data<-read_excel("wages.xlsx")
data$wage<-as.numeric(data$wage)
data$grp_def<-as.numeric(data$grp_def)
attach(data)

median(wage)
hist(wage)
###############################
#bootstrap руками делаем цикл
###############################
#число бутстраповских выборок
B<-1000
#размер бутстраповской выборки совпадает с размером исходной выборки
tip<-NULL 
#цикл нужно с чего-то начать, поэтому создаём пустой вектор,
# куда попадут все оценки
tip
# цикл for
for (i in 1:B){
   boot_sample<-data[sample(nrow(data), replace=TRUE),]
   # случайная выборка с возвращением
   tip[i]<-median(boot_sample$wage)
   # бутстраповская статистика для каждого i 
   cat("\r", i, "of", B)
  #счётчик итераций в цикле
}
 
tip
#распределение полученных оценок
hist(tip)

#95% confidence interval
tip_sorted<-sort(tip, decreasing = FALSE)
tip_sorted[26]
tip_sorted[975]

#альтернативно
quantile(tip, 0.025)
quantile(tip, 0.975)

##############################
#автоматически:
##############################
library(boot)
#вспомогательная функция - что именно считаем
bs = function(data, indices) {
  d = data[indices,] # allows boot to select sample 
  return(median(d$wage))
}
# bootstrapping with 1000 replications 
results = boot(
  data=data, 
  statistic=bs, 
  R=1000)

CI_auto<-boot.ci(results, type="basic")
CI_auto #сравним: руками мы получили 1 из автоматических вариантов



########## пример с регрессией
# в каком возрасте мах зп в час
model<-lm(wage ~ age + I(age^2) , data=data)
summary(model)
-model$coefficients[2]/(2*model$coefficients[3])

###############################
#bootstrap руками делаем цикл
###############################
#число бутстраповских выборок
B<-1000
#размер бутстраповской выборки совпадает с размером исходной выборки
tip<-NULL 
#цикл нужно с чего-то начать, поэтому создаём пустой вектор,
# куда попадут все оценки
tip
# цикл for
for (i in 1:B){
  boot_sample<-data[sample(nrow(data), replace=TRUE),]
  # случайная выборка с возвращением
  model<-lm(wage ~ age + I(age^2) , data=boot_sample)
  tip[i]<--model$coefficients[2]/(2*model$coefficients[3])
    # бутстраповская статистика для каждого i 
  cat("\r", i, "of", B)
  #счётчик итераций в цикле
}

tip
#распределение полученных оценок
hist(tip)

#95% confidence interval
tip_sorted<-sort(tip, decreasing = FALSE)
tip_sorted[26]
tip_sorted[975]

#альтернативно
quantile(tip, 0.025)
quantile(tip, 0.975)

# бутстрап автоматически
#вспомогательная функция - что именно считаем
bs = function(data, indices, formula) {
  d = data[indices,] # allows boot to select sample 
  model<-lm(wage ~ age + I(age^2) , data=d)
  return(-model$coefficients[2]/(2*model$coefficients[3]))
}
# bootstrapping with 1000 replications 
results = boot(
  data=data, 
  statistic=bs, 
  R=B, formula = wage ~ age + I(age^2) )
CI_auto<-boot.ci(results, type="perc")
CI_auto #сравним: руками мы получили 1 из автоматических вариантов


################
#Delta-method
################
install.packages("car")
library("car")
data$age2<-data$age^2
model<-lm(wage ~ age + age2 , data=data)
summary(model)
deltaMethod(model,"-age/(2*age2)")# age that maximizes the quadratic



##############################
#автоматически:
##############################
library(boot)
#вспомогательная функция - что именно считаем: точку вершины параболы
bs = function(data, indices, formula) {
  d = data[indices,] # allows boot to select sample 
  fit = lm(formula, data=d)
  return(-fit$coef[2]/(2*fit$coef[3]))
}
# bootstrapping with 1000 replications 
results = boot(
  data=mydata, 
  statistic=bs, 
  R=1000, 
  formula=y ~ age + I(age^2))
CI_auto<-boot.ci(results)
CI_auto #сравним: руками мы получили 1 из автоматических вариантов
CI95

################
#Delta-method
################
install.packages("car")
library("car")
help(deltaMethod)
m1<-lm(y ~ age + age2, data=mydata)
summary(m1)
deltaMethod(m1,"-age/(2*(age2))")# age that maximizes the quadratic
