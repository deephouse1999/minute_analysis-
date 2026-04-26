# KOSPI200 ETF Minute Analysis

Daily OHLC state and intraday path clustering analysis for KOSPI200 ETF market structure.

## Article

- [HTML article for GitHub Pages](docs/index.html)
- [Rendered HTML article backup](analysis/01_daily_intraday_cluster_blog.html)
- [R Markdown source](analysis/01_daily_intraday_cluster_blog.Rmd)

## Repository Structure

- `analysis/`: R Markdown article and rendered HTML output
- `docs/`: GitHub Pages entry point for the rendered HTML article
- `r/`: data preparation and clustering scripts
- `output/figures/`: generated analysis figures
- `output/tables/`: generated summary tables

## Method

The analysis combines two layers:

1. Daily OHLC features are clustered into daily market states.
2. Intraday minute paths are centered and clustered into 4 simplified path types.

The final tables compare how next-day returns vary by the combination of daily state and intraday path cluster.

## Data Notes

Raw minute data and derived RDS objects are intentionally excluded from git via `.gitignore`.

To reproduce the full pipeline, place the raw minute CSV files under `data/raw/`, then run the scripts in `r/` in this order:

1. `r/prepare_data.R`
2. `r/make_intraday_path.R`
3. `r/clustering_daily_price.R`
4. `r/clustering_intraday_path.R`

The article currently uses 4 intraday clusters (`INTRADAY_K <- 4`) for a simpler and more stable interpretation.
