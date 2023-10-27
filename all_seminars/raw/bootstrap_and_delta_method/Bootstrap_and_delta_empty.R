##################################
#Генерим искуственные данные заработок футболиста от возраста
##################################
set.seed(1234) #важно, чтобы все генерили одни и те же числа
age<-sample(18:39,25, replace = TRUE) #пусть в выборке 25 футболистов в возрасте от 18 до 39 лет
eps<-rnorm(25) # случайный шок
y<-4000+200*age-3*(age^2)+eps #истинная зависимость заработка от возраста
mydata<-data.frame(y=y, age=age, age2= age^2)
plot(y ~ age)

##############
#OLS estimates
##############
model1<-lm(y ~ age + I(age^2))
summary(model1)
tipping<-(-lm(y ~ age + I(age^2))$coef[2])/(2*lm(y ~ age + I(age^2))$coef[3])
tipping #после какого возраста заработок падает
#проблема: как посчитать доверительный интервал для "точки перелома"?


###############################
#bootstrap руками делаем цикл
###############################
#число бутстраповских выборок
#размер бутстраповской выборки совпадает с размером исходной выборки
tip<-NULL #цикл нужно с чего-то начать, поэтому создаём пустой вектор, куда попадут все оценки
tip
# цикл for

# случайная выборка с возвращением
# бутстраповская статистика
  #для каждого i оценили модель и посчитали отношение оценок коэффициентов
#  cat("\r", i, "of", B)
#счётчик итераций в цикле

 #распределение полученных оценок

#95% confidence interval



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
