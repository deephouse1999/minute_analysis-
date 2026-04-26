############################################
### make_intraday_path.R
### 목적:
### 1) intraday_path rds 로드
### 2) 장중 수익률 path 생성
### 3) shape clustering용 normalized path 생성
### 4) wide matrix 생성
### 5) derived rds 저장
############################################

###a) 패키지 ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(zoo)


###b) 경로 설정 ---------------------------------------------------------

BASE_DIR <- "C:/app/hts/minute_analysis"

DERIVED_DIR <- file.path(BASE_DIR, "data", "derived")

if (!dir.exists(DERIVED_DIR)) {
  dir.create(DERIVED_DIR, recursive = TRUE)
}


###c) intraday path 로드 ------------------------------------------------

load_intraday_path <- function(symbol,
                               derived_dir = DERIVED_DIR) {
  
  path <- file.path(
    derived_dir,
    paste0("intraday_path_", symbol, ".rds")
  )
  
  if (!file.exists(path)) {
    stop("intraday_path 파일이 없어: ", path)
  }
  
  readRDS(path)
}


###d) path feature 생성 -------------------------------------------------

add_intraday_path_features <- function(path_tbl) {
  
  path_tbl |>
    mutate(
      symbol = as.character(symbol),
      date = as.character(date),
      time = sprintf("%06d", suppressWarnings(as.integer(time)))
    ) |>
    arrange(symbol, date, time) |>
    group_by(symbol, date) |>
    mutate(
      ###a. 기본 수익률 path
      day_open = first(open),
      ret_from_open = close / day_open - 1,
      
      ###b. 분 단위 변화율
      bar_ret = close / lag(close) - 1,
      
      ###c. 시간 index
      bar_index = row_number(),
      n_bar = n(),
      
      ###d. 하루 전체 방향
      final_ret = last(close) / first(open) - 1,
      final_direction = dplyr::case_when(
        final_ret > 0 ~ "UP",
        final_ret < 0 ~ "DOWN",
        TRUE ~ "FLAT"
      ),
      
      ###e. shape용 중심화
      ret_centered = ret_from_open - mean(ret_from_open, na.rm = TRUE),
      
      ###f. shape용 표준화
      ret_sd = sd(ret_centered, na.rm = TRUE),
      ret_norm = ifelse(
        is.na(ret_sd) | ret_sd == 0,
        0,
        ret_centered / ret_sd
      ),
      
      ###g. 방향 정렬 path
      direction_sign = dplyr::case_when(
        final_ret > 0 ~ 1,
        final_ret < 0 ~ -1,
        TRUE ~ 1
      ),
      ret_direction_aligned = ret_from_open * direction_sign
    ) |>
    ungroup()
}


###e) 품질 필터 ---------------------------------------------------------

filter_valid_intraday_days <- function(path_tbl,
                                       min_n_bar = 300,
                                       start_time_max = "091000",
                                       end_time_min = "150000") {
  
  quality_tbl <- path_tbl |>
    group_by(symbol, date) |>
    summarise(
      n_bar = n(),
      start_time = min(time, na.rm = TRUE),
      end_time = max(time, na.rm = TRUE),
      n_na_close = sum(is.na(close)),
      .groups = "drop"
    ) |>
    mutate(
      valid_day = n_bar >= min_n_bar &
        start_time <= start_time_max &
        end_time >= end_time_min &
        n_na_close == 0
    )
  
  valid_dates <- quality_tbl |>
    filter(valid_day) |>
    select(symbol, date)
  
  out <- path_tbl |>
    inner_join(valid_dates, by = c("symbol", "date"))
  
  list(
    path_tbl = out,
    quality_tbl = quality_tbl
  )
}


###f) path wide 생성 ----------------------------------------------------
### path_value:
### - "ret_from_open"           : 방향 포함 원본 누적수익률
### - "ret_centered"            : 하루 평균 제거
### - "ret_norm"                : 평균 제거 + 표준화
### - "ret_direction_aligned"   : 상승/하락 방향 정렬

make_path_wide <- function(path_tbl,
                           path_value = "ret_centered",
                           max_bar = 300) {
  
  if (!path_value %in% names(path_tbl)) {
    stop("path_value 컬럼이 없어: ", path_value)
  }
  
  path_tbl |>
    filter(bar_index <= max_bar) |>
    select(symbol, date, bar_index, value = all_of(path_value)) |>
    pivot_wider(
      names_from = bar_index,
      values_from = value,
      names_prefix = "t_"
    ) |>
    drop_na()
}


###g) path summary 생성 -------------------------------------------------

make_intraday_day_summary <- function(path_tbl) {
  
  path_tbl |>
    group_by(symbol, date) |>
    summarise(
      n_bar = n(),
      start_time = min(time, na.rm = TRUE),
      end_time = max(time, na.rm = TRUE),
      open_price = first(open),
      close_price = last(close),
      high_price = max(high, na.rm = TRUE),
      low_price = min(low, na.rm = TRUE),
      day_ret = last(close) / first(open) - 1,
      day_range = (max(high, na.rm = TRUE) - min(low, na.rm = TRUE)) / first(open),
      max_ret_from_open = max(ret_from_open, na.rm = TRUE),
      min_ret_from_open = min(ret_from_open, na.rm = TRUE),
      close_location_intraday = ifelse(
        max(high, na.rm = TRUE) == min(low, na.rm = TRUE),
        NA_real_,
        (last(close) - min(low, na.rm = TRUE)) /
          (max(high, na.rm = TRUE) - min(low, na.rm = TRUE))
      ),
      final_direction = last(final_direction),
      .groups = "drop"
    )
}


###h) 종목별 path dataset 저장 -----------------------------------------

build_intraday_path_dataset <- function(symbol,
                                        path_value = "ret_centered",
                                        max_bar = 300,
                                        min_n_bar = 300,
                                        derived_dir = DERIVED_DIR) {
  
  raw_path <- load_intraday_path(
    symbol = symbol,
    derived_dir = derived_dir
  )
  
  path_features <- add_intraday_path_features(raw_path)
  
  filtered <- filter_valid_intraday_days(
    path_tbl = path_features,
    min_n_bar = min_n_bar
  )
  
  path_clean <- filtered$path_tbl
  quality_tbl <- filtered$quality_tbl
  
  path_wide <- make_path_wide(
    path_tbl = path_clean,
    path_value = path_value,
    max_bar = max_bar
  )
  
  day_summary <- make_intraday_day_summary(path_clean)
  
  saveRDS(
    path_clean,
    file.path(derived_dir, paste0("intraday_path_features_", symbol, ".rds"))
  )
  
  saveRDS(
    path_wide,
    file.path(derived_dir, paste0("intraday_path_wide_", symbol, "_", path_value, ".rds"))
  )
  
  saveRDS(
    day_summary,
    file.path(derived_dir, paste0("intraday_day_summary_", symbol, ".rds"))
  )
  
  saveRDS(
    quality_tbl,
    file.path(derived_dir, paste0("intraday_path_quality_", symbol, ".rds"))
  )
  
  readr::write_csv(
    quality_tbl,
    file.path(derived_dir, paste0("intraday_path_quality_", symbol, ".csv"))
  )
  
  list(
    path_clean = path_clean,
    path_wide = path_wide,
    day_summary = day_summary,
    quality_tbl = quality_tbl
  )
}


###i) 실행부 ------------------------------------------------------------
### 필요할 때만 실행

res_path_122630 <- build_intraday_path_dataset(
  symbol = "122630",
  path_value = "ret_centered",
  max_bar = 300,
  min_n_bar = 300
)

res_path_252670 <- build_intraday_path_dataset(
  symbol = "252670",
  path_value = "ret_centered",
  max_bar = 300,
  min_n_bar = 300
)





