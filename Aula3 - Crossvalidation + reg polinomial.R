library(rsample)
library(dplyr)

rm(list = ls())

load("C:/Users/erald/Desktop/Faculdade/Aprendizagem de Maquina/dados_expectativa_renda.RData")
dados <- dados_expectativa_renda[,-1]
colnames(dados) <- c("y", "x")

set.seed(2023)

dados_splitted <- initial_split(dados, prop = 3/4)

cross_validation <- vfold_cv(training(dados_splitted), v = 11)

eqm <- NULL

for(i in 1:11){
  data_temp <- cross_validation$splits[[i]] %>% analysis()
  
  fit_temp <- lm(y~., data_temp)
  
  test_temp <- cross_validation$splits[[i]] %>% assessment()
  
  eqm[i] <-  apply((test_temp[,1] - predict(fit_temp, test_temp))^2, MARGIN = 2, mean)
  
  if(i > 1){
    data_temp <- cross_validation$splits[[i]] %>% analysis()
    
    for(j in 2:i){
      assign(paste0("x",j), data_temp$x^j)
      data_temp <- cbind(data_temp, get(paste0("x", j)))
    }
    colnames(data_temp) <- c("y", "x", paste0("x", 2:i))
    
    fit_temp <- lm(y~. , data = data_temp)
    
    test_temp <- cross_validation$splits[[i]] %>% assessment() 
    
    for(j in 2:i){
      assign(paste0("x", j), test_temp$x^j)
      test_temp <- cbind(test_temp, get(paste0("x", j)))
    }
    colnames(test_temp) <- c("y", "x", paste0("x", 2:i))
    
    eqm[i] <- mean((test_temp[,1] - predict(fit_temp, test_temp))^2)
  }
}

which(eqm == min(eqm))
min(eqm)

eqm















