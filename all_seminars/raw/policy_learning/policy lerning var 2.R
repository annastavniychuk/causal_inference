n=500
B=200
x <- seq(-1, 1, length.out = 50)
X_test <- expand.grid(x,x)
names(X_test) = c('V1','V2')

e = function(x){
  return(1/(1+exp(-(x[1]+x[2]))))
}
w = function(e_x){
  return(rbinom(1, size = 1, e_x))
}

mu = function(x){
  return(2*(exp(-(x[1]+x[2]))))
}

y = function(mu_x,tau_x,w_x){
  y_x = rnorm(1, mean = mu_x + (tau_x*w_x)/2, sd = 1)
  return(y_x)
}
tau = function(x){
  return(2/((1+exp(-4*x[1]))*(1+exp(-4*x[2])))-0.4)
}

generate_data = function(n){
  X = matrix(runif(n*2, min = -1, max = 1), n,2)
  e_X = apply(X, 1, FUN=e)
  W_X = as.vector(unlist(lapply(e_X, FUN=w)))
  W_X = replace(W_X, W_X==0, -1)
  mu_X = apply(X, 1, FUN=mu)
  tau_X = apply(X, 1, FUN=tau)
  Y_WX=c()
  for (i in 1:n)  Y_WX = c(Y_WX,y(mu_X[i],tau_X[i],W_X[i]))
  return(list(Y = Y_WX,
              W = W_X,
              X = X,
              e_X = e_X,
              tau = tau_X))
}
install.packages('polynom')
install.packages("caret")
library('polynom')
library(caret)
library(glmnet)
get_gamma_dr = function(mu_model_1,
                        mu_model_0,
                        e_model,
                        X, W, Y){
  mu_1 = predict(mu_model_1,
                 newx = X,
                 s=mu_model_1$lambda.min)
  mu_0 = predict(mu_model_0,
                 newx = X,
                 s=mu_model_0$lambda.min)
  e_x = predict(e_model,
                newx = X,
                s=e_model$lambda.min,type="response")
  gamma = mu_1 - mu_0 + W*(Y - (W*(1+W)/2*mu_1+W*(1-W)/2*mu_0))/e_x
  return(list(gamma=gamma,
              sign_gamma = sign(gamma)))
}
get_gamma_ipw = function(e_model,
                         X, Y, W){
  e_x = predict(e_model,
                newx = X,
                s=e_model$lambda.min,type="response")
  gamma = W*Y/e_x
  return(list(gamma=gamma,
              sign_gamma = sign(gamma)))
}
library("evtree")
library("rpart")
## accuracy: misclassification rate
mc <- function(obj,sg) 1 - mean(predict(obj) == sg)


for_each_simulation = function(n){
  #set.seed(1234)
  generated_data = generate_data(n)
  Y = generated_data[['Y']]
  W = generated_data[['W']]
  p = data.frame(poly(generated_data[['X']], degree = 5, raw=TRUE))
  X = data.frame(generated_data[['X']])
  X_with_pol = p
  data = data.frame(Y=Y, W=W, X=X_with_pol)
  flds <- createFolds(data[['Y']], k = 5, list = TRUE, returnTrain = FALSE)
  split_up <- lapply(flds, function(ind, dat) dat[ind,], dat = data)
  unlist(lapply(split_up, nrow))
  
  data_1 = split_up[[1]]
  data_2 = split_up[[2]]
  data_3 = split_up[[3]]
  data_4 = split_up[[4]]
  data_5 = split_up[[5]]
  all_data_list=list(list(data = data_1,
                          data_minus = data[-as.numeric(rownames(data_1)),]),
                     list(data = data_2,
                          data_minus = data[-as.numeric(rownames(data_2)),]),
                     list(data = data_3,
                          data_minus = data[-as.numeric(rownames(data_3)),]),
                     list(data = data_4,
                          data_minus = data[-as.numeric(rownames(data_4)),]),
                     list(data = data_5,
                          data_minus = data[-as.numeric(rownames(data_5)),]))
  gamma_i_dr = c()
  sign_gamma_dr = c()
  gamma_i_ipw = c()
  sign_gamma_ipw = c()
  for (i in 1:length(all_data_list)){
    mu_model_1 = cv.glmnet(as.matrix(all_data_list[[i]][['data_minus']][(all_data_list[[i]][['data_minus']]$W==1),-(1:2)]),
                           all_data_list[[i]][['data_minus']][all_data_list[[i]][['data_minus']]$W==1,1], nfolds=5,type.measure="mae")
    mu_model_0 = cv.glmnet(as.matrix(all_data_list[[i]][['data_minus']][all_data_list[[i]][['data_minus']]$W==(-1),-(1:2)]),
                           all_data_list[[i]][['data_minus']][all_data_list[[i]][['data_minus']]$W==(-1),1], nfolds=5,type.measure="mae")
    e_model = cv.glmnet(as.matrix(all_data_list[[i]][['data_minus']][,-(1:2)]),
                        all_data_list[[i]][['data_minus']][,2], nfolds=5,alpha=1,
                        family="binomial",type.measure = "mse")
    gamma_info_dr = get_gamma_dr(mu_model_1,
                                 mu_model_0,
                                 e_model,
                                 as.matrix(all_data_list[[i]][['data']][,-(1:2)]),
                                 all_data_list[[i]][['data']][,2],
                                 all_data_list[[i]][['data']][,1])
    gamma_info_ipw = get_gamma_ipw(e_model,
                                   as.matrix(all_data_list[[i]][['data']][,-(1:2)]),
                                   all_data_list[[i]][['data']][,1],
                                   all_data_list[[i]][['data']][,2])
    gamma_i_dr = c(gamma_i_dr,gamma_info_dr[['gamma']])
    sign_gamma_dr = c(sign_gamma_dr,gamma_info_dr[['sign_gamma']])
    gamma_i_ipw = c(gamma_i_ipw,gamma_info_ipw[['gamma']])
    sign_gamma_ipw = c(sign_gamma_ipw,gamma_info_ipw[['sign_gamma']])
  }
  gamma_i_dr = abs(gamma_i_dr)
  gamma_i_ipw = abs(gamma_i_ipw)
  for (i in 1:length(all_data_list)){
    if(i==1){
      data_full = all_data_list[[i]][['data']]}
    else{data_full = rbind(data_full,all_data_list[[i]][['data']])}
  }
  data_for_tree_dr = data.frame(cbind(data_full[,(3)],data_full[,(8)],
                                      sign_gamma_dr))
  data_for_tree_ipw = data.frame(cbind(data_full[,(3)],data_full[,(8)],
                                       sign_gamma_ipw))
  ev_ipw <- evtree(sign_gamma_ipw ~ ., data = data_for_tree_ipw,weights = gamma_i_ipw, minbucket = 10, maxdepth = 2)
  ev_dr <- evtree(sign_gamma_dr ~ ., data = data_for_tree_dr,weights = gamma_i_dr, minbucket = 10, maxdepth = 2)
  print(mc(ev_dr,data_for_tree_dr$sign_gamma_dr))
  print(mc(ev_ipw,data_for_tree_ipw$sign_gamma_ipw))
  return(cbind(sign(predict(ev_dr,X_test, type='response')),
               sign(predict(ev_ipw,X_test, type='response'))))
}

for(j in 1:B){
  result = for_each_simulation(n)
  if (j == 1){
    final_result = result} else {
      final_result = final_result + result}
  print(j)
}
final_result = final_result/k
X_test_dr = cbind(X_test,final_result[,1])
names(X_test_dr) = c('V1','V2','tau')
X_test_ipw = cbind(X_test,final_result[,2])
names(X_test_ipw) = c('V1','V2','tau')
library(ggplot2)
ggplot(data = X_test_dr, aes(x = V2, y = V1)) +
  geom_tile(aes(fill = tau))
ggplot(data = X_test_ipw, aes(x = V2, y = V1)) +
  geom_tile(aes(fill = tau))

X_test_true = cbind(X_test,sign(tau(X_test)))
names(X_test_true) = c('V1','V2','tau')
ggplot(data = X_test_true, aes(x = V2, y = V1)) +
  geom_tile(aes(fill = tau))