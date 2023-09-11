rm(list = ls())

library(tidymodels)
library(FNN)
library(glue)
library(patchwork)

dados_expectativa_renda <- read_csv("machine_learning/dados_expectativa_renda.csv")
data <- dados_expectativa_renda[,-1]

knnregtest <- function(dados, K = 1L, TIDYMODELS = F){
  colnames(dados) <- c("y", "x")
  
  if(TIDYMODELS == T){
    receita <- recipe(y ~ x, data = dados)
    
    # Definindo o modelo
    modelo_knn <- nearest_neighbor(neighbors = K) |> 
      set_mode("regression") |> 
      set_engine("kknn")
    
    # Workflow
    ajuste_final <- 
      workflow() |> 
      add_model(modelo_knn) |> 
      add_recipe(receita) |> 
      fit(data = dados)
    
    # Retornando previsoes
    y_chapeu <- predict(ajuste_final, new_data = dados)
    
    dados <- dados |> 
      mutate(y_chapeu = y_chapeu$.pred)
  }
  else{
    fit <- knn.reg(dados$x, y = dados$y, k = K)
    
    dados <- dados %>% 
      mutate(y_chapeu = fit$pred)
  }
  
  dados %>%  
    ggplot() +
    geom_point(aes(x = x, y = y), size = 3) +
    geom_line(aes(x = x, y = y_chapeu), col = "red", alpha = 0.6, size = 1.3) +
    labs(title = "k-nearest neighbours", subtitle = glue("k = {K}")) +
    theme(
      title = element_text(face = "bold")
    )
}


p1 <- knnregtest(data, K = 1)
p2 <- knnregtest(data, K = 3)
p3 <- knnregtest(data, K = 10)
p4 <- knnregtest(data, K = 50)

(p1 + p2) / (p3 + p4)
