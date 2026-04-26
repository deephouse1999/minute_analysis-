############################################
### clustering_daily_price.R
### 목적:
### 1) quantmod로 daily OHLC 수집
### 2) feature 생성
### 3) k-means clustering (state 정의)
### 4) 결과 저장
############################################

###a) 패키지 ------------------------------------------------------------

library(quantmod)
library(dplyr)
library(tibble)
library(zoo)
library(readr)


###b) 경로 설정 ---------------------------------------------------------

BASE_DIR <- "C:/app/hts/minute_analysis"

DERIVED_DIR <- file.path(BASE_DIR, "data", "derived")

dir.create(DERIVED_DIR, recursive = TRUE, showWarnings = FALSE)


###c) 데이터 수집 -------------------------------------------------------

get_daily_ohlc <- function(symbol = "^KS11",
                           start_date = "2005-01-01") {
  
  xts_data <- getSymbols(
    Symbols = symbol,
    src = "yahoo",
    from = start_date,
    auto.assign = FALSE
  )
  
  tibble(
    date  = as.Date(index(xts_data)),
    open  = as.numeric(Op(xts_data)),
    high  = as.numeric(Hi(xts_data)),
    low   = as.numeric(Lo(xts_data)),
    close = as.numeric(Cl(xts_data)),
    volume = as.numeric(Vo(xts_data))
  ) |>
    arrange(date)
}


###d) feature 생성 ------------------------------------------------------

make_daily_features <- function(df) {
  
  df |>
    arrange(date) |>
    mutate(
      prev_close = lag(close),
      
      ret_cc_1d = close / prev_close - 1,
      ret_oc_1d = close / open - 1,
      gap_ret   = open / prev_close - 1,
      
      range = (high - low) / open,
      body  = (close - open) / open,
      
      upper_shadow = (high - pmax(open, close)) / open,
      lower_shadow = (pmin(open, close) - low) / open,
      
      body_ratio = ifelse(range == 0, NA_real_, body / range),
      close_location = ifelse(
        high == low,
        NA_real_,
        (close - low) / (high - low)
      ),
      
      vol_5d  = rollapply(ret_cc_1d, 5, sd, fill = NA, align = "right"),
      vol_20d = rollapply(ret_cc_1d, 20, sd, fill = NA, align = "right"),
      
      ma_5  = rollapply(close, 5, mean, fill = NA, align = "right"),
      ma_20 = rollapply(close, 20, mean, fill = NA, align = "right"),
      
      ma_5_gap  = close / ma_5 - 1,
      ma_20_gap = close / ma_20 - 1,
      ma_slope_5 = ma_5 / lag(ma_5) - 1,
      
      next_ret_1d = lead(close) / close - 1
    )
}


###e) clustering --------------------------------------------------------

run_daily_clustering <- function(df,
                                 k = 4) {
  
  cluster_vars <- c(
    "ret_cc_1d",
    "ret_oc_1d",
    "gap_ret",
    "range",
    "body_ratio",
    "close_location",
    "vol_5d",
    "vol_20d",
    "ma_5_gap",
    "ma_20_gap",
    "ma_slope_5"
  )
  
  df_model <- df |>
    select(date, all_of(cluster_vars), next_ret_1d) |>
    filter(if_all(all_of(cluster_vars), ~ is.finite(.)))
  
  mat <- df_model |>
    select(all_of(cluster_vars)) |>
    scale()
  
  set.seed(123)
  
  fit <- kmeans(
    mat,
    centers = k,
    nstart = 50
  )
  
  df_model |>
    mutate(state_id = fit$cluster)
}


###f) state 요약 --------------------------------------------------------

make_state_summary <- function(df) {
  
  df |>
    group_by(state_id) |>
    summarise(
      n = n(),
      avg_ret_cc = mean(ret_cc_1d, na.rm = TRUE),
      avg_next_ret = mean(next_ret_1d, na.rm = TRUE),
      win_rate_next = mean(next_ret_1d > 0, na.rm = TRUE),
      avg_range = mean(range, na.rm = TRUE),
      avg_vol_20d = mean(vol_20d, na.rm = TRUE),
      avg_close_location = mean(close_location, na.rm = TRUE),
      avg_ma_20_gap = mean(ma_20_gap, na.rm = TRUE),
      .groups = "drop"
    )
}


###g) 실행 --------------------------------------------------------------

daily_tbl <- get_daily_ohlc("^KS11")

daily_feat <- make_daily_features(daily_tbl)

daily_state_tbl <- run_daily_clustering(daily_feat, k = 4)

state_summary <- make_state_summary(daily_state_tbl)

print(state_summary)


###h) 저장 --------------------------------------------------------------

saveRDS(
  daily_state_tbl,
  file.path(DERIVED_DIR, "daily_state_tbl.rds")
)

readr::write_csv(
  state_summary,
  file.path(DERIVED_DIR, "daily_state_summary.csv")
)