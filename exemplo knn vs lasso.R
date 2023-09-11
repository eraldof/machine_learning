rm(list = ls())

dados <- read_csv("machine_learning/Student_Performance.csv")
skimr::skim(dados)

dados <- recipe(`Performance Index`~. , data = dados) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  prep() %>% juice() 

df <- initial_split(dados, prop = 0.9)

trainer <- training(df)
tester <- testing(df)

receita <- recipe(`Performance Index` ~ ., data = trainer)

modelo_knn <- nearest_neighbor(neighbors = tune()) |> 
  set_mode("regression") |> 
  set_engine("kknn")

modelo_lasso <- 
  linear_reg(penalty = tune(), mixture = 1) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")


all_wf <- 
  workflow_set(
    preproc = list(`Performance Index` ~ .),
    models = list(knn = modelo_knn, lasso = modelo_lasso), 
    cross = TRUE
    )

cv <- vfold_cv(trainer, v = 20)

metrica <- metric_set(rmse)

tunagem <- 
  all_wf %>%  
  workflow_map(
    seed = 2023, 
    verbose = TRUE,
    resamples = cv,
    grid = 50,
    metrics = metrica
  )

modelos_rank <- tunagem %>% rank_results()

melhor_knn <- 
  tunagem %>% 
  extract_workflow_set_result("formula_knn") %>% 
  select_best("rmse")

melhor_lasso <- 
  tunagem %>% 
  extract_workflow_set_result("formula_lasso") %>% 
  select_best("rmse")


finalizando_knn <- 
  tunagem %>% 
  extract_workflow("formula_knn") %>% 
  finalize_workflow(melhor_knn) %>% 
  last_fit(split = df)

finalizando_lasso <- 
  tunagem %>% 
  extract_workflow("formula_lasso") %>% 
  finalize_workflow(melhor_lasso) %>% 
  last_fit(split = df)



melhor_knn$neighbors
melhor_lasso$penalty

knitr::kable(data.frame(row.names = c("KNN", "LASSO"), 
                        "RMSE" = as.numeric(c((finalizando_knn  %>%  collect_metrics())[1,3],
                                              (finalizando_lasso %>% collect_metrics())[1,3]))
))
