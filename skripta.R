install.packages("devtools")
library(devtools)
devtools::install_github("marinsokol5/dshelperr")
library(dshelperr)

rm(list = ls())
zaglav <- read_csv("ZAGLAV.csv")

glimpse(zaglav)
View(df_status_v3(zaglav))

djelatnosti <- read.csv("DJELATNOSTI2007.csv")

djelatnosti %<>% 
  remove_columns("OPIS_DJEL") %>%
  one_hot_encode("GRUPA", "RAZINA") %>% 
  characterize_columns("SIFRA")

zaglav %<>% left_join_v2(
  djelatnosti,
  name_for_the_right_table = "djelatnosti",
  by = c("NKD2007" = "SIFRA")
)

View(df_status_v3(djelatnosti_cleaned))
intersect(colnames(zaglav), colnames(djelatnosti_cleaned))

sifarnik_opcina <- read.csv("SIFARNIK_OPCINA.csv")
sifarnik_opcina %<>% 
  mutate(zadnji_broj = OPCINA_KBR - (OPCINA * 10)) %>% 
  remove_columns(
    "OPCINA_KBR",
    "AKTIVNOST",
    "NAZIV_OPC"
  ) %>% 
  one_hot_encode(
    "zadnji_broj",
    "NAZIV_ZUP",
    "VRSTA"
  )

zaglav %<>% left_join_v2(
  sifarnik_opcina
)

sifarnik_velicina <- read.csv("SIFARNIK_VELICINA.csv")
sifarnik_velicina %<>% one_hot_encode("OPIS_VEL")
zaglav %<>% left_join_v2(
  sifarnik_velicina,
  c("VEL"="SIF_VEL")
)

sifarnik_vlasnistva <- read.csv("SIFARNIK_VLASNISTVA.csv")
sifarnik_vlasnistva %<>% one_hot_encode("OPIS_VLASTI")
zaglav %<>% left_join_v2(sifarnik_vlasnistva, c("VLAST" = "SIF_VLASTI"))


tabl1 <- read.csv("TABL1.csv")
tabl2 <- read.csv("TABL2.csv")
tabl3 <- read.csv("TABL3.csv")
tabl4 <- read.csv("TABL4.csv")

tabl <- tabl1 %>% 
  inner_join(tabl2) %>% 
  inner_join(tabl3) %>% 
  inner_join(tabl4)

tabl %<>% remove_columns_with_one_unique_value(verbose = TRUE)
data <- zaglav %>% left_join(tabl)

data %<>%
  as.data.frame() %>% 
  remove_columns(
    "ID_IZVJESTAJA",
    "NKD2007",
    "ZUPANIJA",
    "OPCINA",
    "VRSTA_IZV",
    "DAT_STANJA",
    "DAT_STANJA_OD",
    "VEL",
    "VLAST"
  ) %>% one_hot_encode(
    "OZN_LIKV_STEC",
    "SIF_OBL_ORG",
    "SVRHA"
  ) %>% 
  remove_columns_with_one_unique_value(verbose = TRUE) %>% 
  replace_diacritic_signs_df()


saveRDS(data, "./data.rds")
data <- readRDS("data.rds")

rm(list = ls())
gc()

library(dshelperr)
data <- readRDS("data.rds")

blokirane_tvrtke <- read.csv("blokirane_tvrtke.csv")

blokirane_tvrtke %<>% 
  mutate(
    blocked = 1,
    GODINA = GODINA - 1
  )
data %<>% 
  left_join(blokirane_tvrtke) %>% 
  fill_NA_with_value(blocked, 0)

table(blokirane_tvrtke$GODINA)
table(data$GODINA)
table(data$blocked)

table(data$blocked, data$GODINA)

# data %<>% remove_columns("ID_TVRTKE", "GODINA")

test_data <- data %>% 
  filter(GODINA == 2015) %>%
  remove_columns("GODINA", "blocked")
solution <- test_data %>% select(ID_TVRTKE)
test_data %<>% remove_columns("ID_TVRTKE")

model_data <- data %>% 
  filter(GODINA != 2015) %>% 
  remove_columns("ID_TVRTKE", "GODINA")

model_labels <- model_data$blocked
model_data %<>% remove_columns("blocked")
table(model_labels)

train_percentage = 0.7
train_indexes <- sample(1:nrow(model_data), train_percentage * nrow(model_data))
train_data <- model_data[train_indexes, ]
train_labels <- model_labels[train_indexes]

validation_data <- model_data[-train_indexes, ]
validation_labels <- model_labels[-train_indexes]

train_dmatrix <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)
validation_dmatrix <- xgb.DMatrix(data = as.matrix(validation_data), label = validation_labels)

# ds <- df_status(data, print_results = F)
View(ds)
View(columns_classes(data))
model <- xgb.train(
  data = train_dmatrix,
  watchlist = list(train = train_dmatrix, validation = validation_dmatrix),
  early_stopping_rounds = 400,
  nrounds = 1000,
  print_every_n = 10,
  objective = "binary:logistic",
  eval_metric = "auc",
  params = list(
    eta = 0.1,
    subsample = 0.8,
    max_depth = 6,
    min_child_weight = 1,
    colsample_bytree = 0.5
  )
)
model$best_score

importance <- xgb.importance(model = model)
xgb.plot.importance(importance, top_n = 10)

opisi_aopa <- read_csv("OPISI_AOPA.csv")


prediction <- predict(model, as.matrix(test_data))
solution$Blocked <- prediction
solution %<>% rename_column("ID_TVRTKE", "Id")
write_csv(solution, "solution.csv")


prediction <- predict(model, validation_dmatrix)
prediction <- ifelse(prediction < 0.5, 0, 1)
caret::confusionMatrix(
  data = as.factor(validation_labels),
  reference = as.factor(prediction)
)

