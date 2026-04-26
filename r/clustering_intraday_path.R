############################################
### clustering_intraday_path.R
############################################

library(dplyr)
library(readr)
library(tibble)

BASE_DIR <- "C:/app/hts/minute_analysis"
DERIVED_DIR <- file.path(BASE_DIR, "data", "derived")

### 1. 데이터 로드 ----------------------------------------

path_wide <- readRDS(
  file.path(DERIVED_DIR, "intraday_path_wide_122630_ret_centered.rds")
)

### 2. clustering -----------------------------------------

mat <- path_wide |>
  select(-symbol, -date) |>
  as.matrix() |>
  scale()

set.seed(123)

k <- 4

fit <- kmeans(
  mat,
  centers = k,
  nstart = 50
)

### 3. 결과 테이블 ----------------------------------------

intraday_cluster_tbl <- path_wide |>
  select(symbol, date) |>
  mutate(
    intraday_cluster = fit$cluster
  )

print(intraday_cluster_tbl)

### 4. 저장 ----------------------------------------------

saveRDS(
  intraday_cluster_tbl,
  file.path(DERIVED_DIR, "intraday_cluster_tbl.rds")
)

readr::write_csv(
  intraday_cluster_tbl,
  file.path(DERIVED_DIR, "intraday_cluster_tbl.csv")
)

message("intraday clustering 완료")