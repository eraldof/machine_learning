#Pagina 45 livro do rafael izbicki

library(tidymodels)
library(glmnet)
tidymodels::tidymodels_prefer()

rm(list = ls())
setwd("C:/Users/erald/Desktop/Faculdade/Aprendizagem de Maquina/machine_learning/")
insurance <- read.csv("insurance.csv")


#Transformando qualitativas em numéricas
df <- recipe(charges ~., data = insurance) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  prep() %>% juice() 

knitr::kable(df)

df <- initial_split(df, prop = 3/4)

trainer <- training(df)
tester <- testing(df)

#Setting Engine
modelo_mmo <- 
  linear_reg(penalty = 0, mixture = 0) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

modelo_ridge <- 
  linear_reg(penalty = tune::tune(), mixture = 0) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

modelo_lasso <- 
  parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>% 
  set_mode("regression") %>% 
  parsnip::set_engine("glmnet")

modelo_elastic <- 
  parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()) %>% 
  set_mode("regression") %>% 
  parsnip::set_engine("glmnet")


#workflow
all_wf <- 
  workflow_set(
    preproc = list(charges ~ . ),
    models = list(mmo = modelo_mmo, ridge = modelo_ridge, lasso = modelo_lasso, elastic = modelo_elastic), 
    cross = TRUE
  )

#cross-validation
set.seed(2023)
cv <- rsample::vfold_cv(trainer, v = 5L)

#metrics
metrica <- yardstick::metric_set(rmse)

#tunning
tunagem <- 
  all_wf %>%  
  workflow_map(
    seed = 2023, 
    verbose = TRUE,
    resamples = cv,
    grid = 50,
    metrics = metrica
  )

#melhores modelos
modelos_rank <- tunagem %>% rank_results()

melhor_mmo <- 
  tunagem %>% 
  extract_workflow_set_result("formula_mmo") %>% 
  select_best("rmse")

melhor_ridge <- 
  tunagem %>% 
  extract_workflow_set_result("formula_ridge") %>% 
  select_best("rmse")

melhor_lasso <- 
  tunagem %>% 
  extract_workflow_set_result("formula_lasso") %>% 
  select_best("rmse")

melhor_elastic <- 
  tunagem %>% 
  extract_workflow_set_result("formula_elastic") %>% 
  select_best("rmse")

finalizando_mmo <- 
  tunagem %>% 
  extract_workflow("formula_mmo") %>% 
  finalize_workflow(melhor_mmo) %>% 
  last_fit(split = df)

finalizando_ridge <- 
  tunagem %>% 
  extract_workflow("formula_ridge") %>% 
  finalize_workflow(melhor_ridge) %>% 
  last_fit(split = df)

finalizando_lasso <- 
  tunagem %>% 
  extract_workflow("formula_lasso") %>% 
  finalize_workflow(melhor_lasso) %>% 
  last_fit(split = df)

finalizando_elastic <- 
  tunagem %>% 
  extract_workflow("formula_elastic") %>% 
  finalize_workflow(melhor_elastic) %>% 
  last_fit(split = df)


knitr::kable(data.frame(row.names = c("MMO", "Ridge", "Lasso", "Elastic"), 
                        "RMSE" = as.numeric(c((finalizando_mmo  %>%  collect_metrics())[1,3],
                                              (finalizando_ridge %>% collect_metrics())[1,3],
                                              (finalizando_lasso %>% collect_metrics())[1,3],
                                              (finalizando_elastic %>% collect_metrics())[1,3])))
)




data_temp <- data.frame(ychapeu_mmo = (finalizando_mmo %>% collect_predictions())[2],
                        ychapeu_ridge = (finalizando_ridge %>% collect_predictions())[2],
                        ychapeu_lasso = (finalizando_lasso %>% collect_predictions())[2],
                        ychapeu_elastic = (finalizando_elastic %>% collect_predictions())[2],
                        y = (finalizando_elastic %>% collect_predictions())[4]
)

colnames(data_temp) <- c("ychapeu_mmo", "ychapeu_ridge", "ychapeu_lasso", "ychapeu_elastic", "y")
data_temp <- as_tibble(data_temp)


g1 <- ggplot(data_temp, aes(x = ychapeu_mmo, y = y)) +
  geom_point() + geom_line(aes(y = ychapeu_mmo), colour = "#191970", size = 1) +
  labs(title = "Modelo MMO", subtitle = "y_ajustado vs y_real")

g2 <- ggplot(data_temp, aes(x = ychapeu_ridge, y = y)) +
  geom_point() + geom_line(aes(y = ychapeu_ridge), colour = "#4169E1", size = 1) +
  labs(title = "Modelo ridge", subtitle = "y_ajustado vs y_real")

g3 <- ggplot(data_temp, aes(x = ychapeu_lasso, y = y)) +
  geom_point() + geom_line(aes(y = ychapeu_lasso), colour = "orange", size = 1) +
  labs(title = "Modelo lasso", subtitle = "y_ajustado vs y_real")

g4 <- ggplot(data_temp, aes(x = ychapeu_elastic, y = y)) +
  geom_point() + geom_line(aes(y = ychapeu_elastic), colour = "red", size = 1) +
  labs(title = "Modelo elastic", subtitle = "y_ajustado vs y_real")


(g1 + g2) / (g3 + g4)



