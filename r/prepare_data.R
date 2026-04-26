############################################
### prepare_data.R
### 목적:
### 1) 일자별 minute csv 병합
### 2) 컬럼 타입 표준화
### 3) date + time 중복 제거
### 4) intraday return / bar index 생성
### 5) derived rds 저장
############################################

###a) 패키지 ------------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)


###b) 경로 설정 ---------------------------------------------------------

BASE_DIR <- "C:/app/hts/minute_analysis"

RAW_DIR <- file.path(BASE_DIR, "data", "raw")
DERIVED_DIR <- file.path(BASE_DIR, "data", "derived")

if (!dir.exists(DERIVED_DIR)) {
  dir.create(DERIVED_DIR, recursive = TRUE)
}


###c) 단일 csv 안전 읽기 ------------------------------------------------

read_minute_file_safe <- function(path) {
  
  out <- readr::read_csv(
    file = path,
    col_types = cols(.default = col_guess()),
    show_col_types = FALSE
  )
  
  need_cols <- c(
    "symbol", "name", "date", "time",
    "open", "high", "low", "close",
    "volume", "amount"
  )
  
  for (nm in need_cols) {
    if (!nm %in% names(out)) {
      out[[nm]] <- NA
    }
  }
  
  out |>
    mutate(
      symbol = as.character(symbol),
      name = as.character(name),
      date = as.character(date),
      time = sprintf("%06d", suppressWarnings(as.integer(time))),
      open = suppressWarnings(as.numeric(open)),
      high = suppressWarnings(as.numeric(high)),
      low = suppressWarnings(as.numeric(low)),
      close = suppressWarnings(as.numeric(close)),
      volume = suppressWarnings(as.numeric(volume)),
      amount = suppressWarnings(as.numeric(amount))
    ) |>
    select(
      symbol, name, date, time,
      open, high, low, close,
      volume, amount
    )
}


###d) 종목별 minute csv 병합 --------------------------------------------

merge_minute_csv <- function(symbol,
                             raw_dir = RAW_DIR,
                             pattern_prefix = "minute") {
  
  pattern_i <- paste0("^", pattern_prefix, "_", symbol, "_\\d{8}\\.csv$")
  
  files <- list.files(
    path = raw_dir,
    pattern = pattern_i,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("읽을 파일이 없어: ", raw_dir, " / pattern = ", pattern_i)
  }
  
  message("n_files = ", length(files), " / symbol = ", symbol)
  
  out <- purrr::map_dfr(files, read_minute_file_safe) |>
    mutate(
      symbol = ifelse(is.na(symbol) | symbol == "", as.character(!!symbol), symbol),
      date = as.character(date),
      time = sprintf("%06d", suppressWarnings(as.integer(time)))
    ) |>
    filter(
      !is.na(date),
      !is.na(time),
      !is.na(close)
    ) |>
    arrange(symbol, date, time) |>
    distinct(symbol, date, time, .keep_all = TRUE)
  
  out
}


###e) 일자별 intraday path 생성 -----------------------------------------

make_intraday_path <- function(minute_tbl,
                               start_time = "090000",
                               end_time = "153000") {
  
  minute_tbl |>
    mutate(
      date = as.character(date),
      time = sprintf("%06d", suppressWarnings(as.integer(time)))
    ) |>
    filter(
      time >= start_time,
      time <= end_time
    ) |>
    arrange(symbol, date, time) |>
    group_by(symbol, date) |>
    mutate(
      day_open = first(open),
      ret_from_open = close / day_open - 1,
      bar_ret = close / lag(close) - 1,
      bar_index = row_number(),
      n_bar = n()
    ) |>
    ungroup()
}


###f) 일자별 품질 점검 테이블 -------------------------------------------

make_minute_quality_tbl <- function(path_tbl) {
  
  path_tbl |>
    group_by(symbol, date) |>
    summarise(
      n_bar = n(),
      start_time = min(time, na.rm = TRUE),
      end_time = max(time, na.rm = TRUE),
      first_open = first(open),
      last_close = last(close),
      day_ret = last(close) / first(open) - 1,
      n_na_close = sum(is.na(close)),
      n_dup_time = n() - n_distinct(time),
      .groups = "drop"
    ) |>
    arrange(symbol, date)
}


###g) 저장 함수 ---------------------------------------------------------

save_prepared_minute_data <- function(symbol,
                                      raw_dir = RAW_DIR,
                                      derived_dir = DERIVED_DIR) {
  
  minute_all <- merge_minute_csv(
    symbol = symbol,
    raw_dir = raw_dir
  )
  
  intraday_path <- make_intraday_path(minute_all)
  
  quality_tbl <- make_minute_quality_tbl(intraday_path)
  
  saveRDS(
    minute_all,
    file.path(derived_dir, paste0("minute_", symbol, "_all.rds"))
  )
  
  saveRDS(
    intraday_path,
    file.path(derived_dir, paste0("intraday_path_", symbol, ".rds"))
  )
  
  saveRDS(
    quality_tbl,
    file.path(derived_dir, paste0("minute_quality_", symbol, ".rds"))
  )
  
  readr::write_csv(
    quality_tbl,
    file.path(derived_dir, paste0("minute_quality_", symbol, ".csv"))
  )
  
  list(
    minute_all = minute_all,
    intraday_path = intraday_path,
    quality_tbl = quality_tbl
  )
}


###h) 실행부 ------------------------------------------------------------
### 필요할 때만 실행

res_122630 <- save_prepared_minute_data("122630")
res_252670 <- save_prepared_minute_data("252670")
