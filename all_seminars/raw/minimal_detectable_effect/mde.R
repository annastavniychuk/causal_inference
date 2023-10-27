{
  library(dplyr) # работа с данными
  library(tidyr) # дополнительные функции по работе с данными
  library(boot) # бутстрап
  library(ggplot2) # графики
  library(svMisc) # для progress bar
}



# Симуляция выборки -------------------------------------------------------
ATE <- 0.5 # истинный эффект

generator <- function(N, alpha, ATE, gamma, seed) {
  # Симулирует выборку
  
  set.seed(seed) # для реплицируемоти задаем seed
  
  df <- data.frame(
    X = rnorm(N, 5, 4), # экзогенный регрессор
    e = rnorm(N, 0, 1), # остатки
    T = rbinom(N, 1, 0.5)) %>% # тритмент
    mutate(
      Y_a = alpha + 0 * T + gamma * X + e, # исход при истинном ATE = 0
      Y_b = alpha + ATE * T + gamma * X + e) # исход при истинном ATE ≠ 0
  
  return(df)
}

df1 <- generator(1000, 3, ATE, 0.5, 123) # большая выборка
df2 <- generator(100, 3, ATE, 0.5, 123) # маленькая выборка

# Посмотрим результаты регрессии на симулированной выборки
summary(lm(Y_a ~ T + X, data = df1))



# Бутстрап ----------------------------------------------------------------
### Бутстрап ручками для большой выборки
{
  B <- 1000 # количество бутстраповских подвыборок
  tip <- c() # цикл нужно с чего-то начать, поэтому создаём пустой вектор, куда попадут все оценки
  indices <- 1:nrow(df1) # вектор с номерами строк, которые будем сэмплировать
}

for (i in 1:B) { # для каждой псевдовыборки
  
  progress(value = i, max.value = B, progress.bar = TRUE, console = TRUE) # создание progress bar
  
  sampled_indices <- sample(indices, replace = TRUE) # сэмплируем индексы; размер бутстраповской выборки совпадает с размером исходной выборки
  boot_sample <- df1[sampled_indices,] # по индексам достаем нужные строки из данных
  
  boot_model <- lm(Y_b ~ T + X, data = boot_sample) # оцениваемая модель
  statistic <- coef(boot_model)[2] # бутстрапируемая статистика -- оценка ATE
  
  tip <- c(tip, statistic) # крепим к пустому вектору значения оцененной статистики
}

# Выборочные характеристики распределения оценок из бутстрапа аналогичны оценкам, полученным в исходной регрессии
mean(tip)
sd(tip)
summary(lm(Y_b ~ T + X, data = df1))


### Бутстрап автоматизированный
bootstrap <- function(data, indices, formula) {
  # здесь надо обязательно два аргумента
  # 1) данные, которые будут сэмплироваться (data)
  # 2) индексы наблюдений после сэмплирования (indices) (boot сам на каждой итерации будет подставлять сюда значения)
  
  boot_sample <- data[indices,] # ВАЖНАЯ СТРОКА -- она нужна, чтобы boot создал псевдовыборку 
  
  boot_model <- lm(formula, data = boot_sample) # оцениваемая модель
  statistic <- coef(boot_model)[2] # бутстрапируемая статистика -- оценка ATE
  
  return(statistic) # должна возвращать статистику, которую мы бутстрапируем
  # можно возвращать одновременно несколько статистик, если объединить их в вектор
}

{
  # Для большой выборки
  dist_tau_a_1 <- boot(
    data = df1, 
    statistic = bootstrap, # наша функция
    R = B, # количество псевдовыборок, которое будем использовать
    formula = Y_a ~ T + X, # см. аргумент функции bootstrap
    parallel = "multicore", ncpus = 4) # параметры для параллельных вычислений
  
  dist_tau_b_1 <- boot(
    data = df1, 
    statistic = bootstrap, # наша функция
    R = B, # количество псевдовыборок, которое будем использовать
    formula = Y_b ~ T + X, # см. аргумент функции bootstrap
    parallel = "multicore", ncpus = 4) # параметры для параллельных вычислений
  
  # Для маленькой выборки
  dist_tau_a_2 <- boot(
    data = df2, 
    statistic = bootstrap, # наша функция
    R = B, # количество псевдовыборок, которое будем использовать
    formula = Y_a ~ T + X, # см. аргумент функции bootstrap
    parallel = "multicore", ncpus = 4) # параметры для параллельных вычислений
  
  dist_tau_b_2 <- boot(
    data = df2, 
    statistic = bootstrap, # наша функция
    R = B, # количество псевдовыборок, которое будем использовать
    formula = Y_b ~ T + X, # см. аргумент функции bootstrap
    parallel = "multicore", ncpus = 4) # параметры для параллельных вычислений
}

# Результаты бутстрапа поместим в датафреймы
dist_tau_1 <- data.frame(tau_a = dist_tau_a_1$t[,1], tau_b = dist_tau_b_1$t[,1])
dist_tau_2 <- data.frame(tau_a = dist_tau_a_2$t[,1], tau_b = dist_tau_b_2$t[,1])

# Эмпирические (бутстраповские) распределения оценок сходятся к теоретическим
ggplot(dist_tau_1, aes(tau_a)) +
  geom_density() + # эмпирическая плотность
  geom_function(fun = dnorm, args = list(mean = mean(dist_tau_1$tau_a), # теоретическая плотность
    sd = sd(dist_tau_1$tau_a)), colour = "red")

ggplot(dist_tau_1, aes(tau_b)) +
  geom_density() + # эмпирическая плотность
  geom_function(fun = dnorm, args = list(mean = mean(dist_tau_1$tau_b), # теоретическая плотность
    sd = sd(dist_tau_1$tau_b)), colour = "red")



# Мощность теста на значимость --------------------------------------------
# Квантили распределения
ql <- qnorm(0.025, mean = 0, sd = sd(dist_tau_2$tau_a))
qu <- qnorm(0.975, mean = 0, sd = sd(dist_tau_2$tau_a))

# На маленькой выборке
{
  plot_df2 <- data.frame(x = seq(-1, 1.3, length.out = 1000))
  plot_df2$dist_tau_a <- sapply(plot_df2$x, FUN = function(x) {dnorm(x, 0, sd(dist_tau_2$tau_a))})
  plot_df2$dist_tau_b <- sapply(plot_df2$x, FUN = function(x) {dnorm(x, ATE, sd(dist_tau_2$tau_b))})
}

ggplot(plot_df2, aes(x)) +
  geom_line(aes(y = dist_tau_a), colour = "red") + # плотность при истинности H0
  geom_line(aes(y = dist_tau_b), colour = "blue") + # плотность при истинности H1
  geom_vline(xintercept = ql) + # нижняя критическая точка
  geom_vline(xintercept = qu) + # верхняя критическая точка
  geom_ribbon( # вероятность ошибки II рода
    data = subset(plot_df2, x <= qu), # накладываем ограничения по оси X
    aes(ymin = 0, ymax = dist_tau_b), # накладываем ограничения по оси Y
    fill = "green", # цвет заливки
    alpha = .5) # параметр прозрачности

# На большой выборке
{
  plot_df1 <- data.frame(x = seq(-1, 1.3, length.out = 1000))
  plot_df1$dist_tau_a <- sapply(plot_df1$x, FUN = function(x) {dnorm(x, 0, sd(dist_tau_1$tau_a))})
  plot_df1$dist_tau_b <- sapply(plot_df1$x, FUN = function(x) {dnorm(x, ATE, sd(dist_tau_1$tau_b))})
}

ggplot(plot_df1, aes(x)) +
  geom_line(aes(y = dist_tau_a), colour = "red") + # плотность при истинности H0
  geom_line(aes(y = dist_tau_b), colour = "blue") + # плотность при истинности H1
  geom_vline(xintercept = ql) + # нижняя критическая точка
  geom_vline(xintercept = qu) + # верхняя критическая точка
  geom_ribbon( # вероятность ошибки II рода
    data = subset(plot_df1, x <= qu), # накладываем ограничения по оси X
    aes(ymin = 0, ymax = dist_tau_b), # накладываем ограничения по оси Y
    fill = "green", # цвет заливки
    alpha = .5) # параметр прозрачности
  

### Переделаем график как следует
plot_df2 %>%
  pivot_longer(c("dist_tau_a", "dist_tau_b"), names_to = "name") %>% # преводим данные в длинный формат
  ggplot(aes(x)) +
  geom_line(aes(y = value, color = as.factor(name))) + # рисует сразу обе плотности и красит их в зависимости от значения переменной name
  geom_area( # заливка определенных областей на графике (отличется от geom_ribbon тем, что тут по умолчанию ymin = 0)
    data = subset(plot_df2, x <= qu & x >= ql), # накладываем ограничения по оси X
    aes(y = dist_tau_a), # накладываем ограничения по оси Y
    fill = "red", # цвет заливки
    alpha = .2) + # параметр прозрачности
  geom_area( # вероятность ошибки II рода
    data = subset(plot_df2, x <= qu), # накладываем ограничения по оси X
    aes(y = dist_tau_b), # накладываем ограничения по оси Y
    fill = "green", # цвет заливки
    alpha = .5) + # параметр прозрачности
  scale_color_discrete( # меняем легенду по группировочному параметру color
    name = "Распределение",
    labels = c("H0: tau = 0", "H1: tau = ATE ≠ 0")) +
  labs( # основные подписи на графике
    title = "Распределения оценок",
    subtitle = "Маленькая выборка", 
    y = "Плотность", 
    x = "Размер оцененного эффекта (tau)")







