# ESG Analysis of Corporate 10-K Filings and Stock Performance

This repository contains the code and materials for a research project submitted for the **Quantitative Text Analysis for Social Science (PUBL0099)** module at UCL. The project investigates ESG (Environmental, Social, Governance) disclosures in 10-K filings and their relationship with stock performance.

## Overview

The study explores whether the frequency of ESG-related terms in annual 10-K filings is associated with stock market performance. Using dictionary-based text analysis, the research identifies sector-specific ESG disclosure patterns and evaluates their financial relevance.

## Methodology

- **Filings**: 10-Ks collected from the SEC EDGAR database for S&P 500 firms in four high-emission sectors (Utilities, Materials, Energy, Industrials), from 2016–2024.
- **Text Analysis**: Preprocessing and tokenization done with `quanteda`. ESG content identified using a dictionary adapted from Baier et al. (2020).
- **Modeling**: Fixed-effects panel regressions used to test the relationship between ESG disclosure intensity and:
  - Cumulative Abnormal Returns (CAR)
  - Adjusted Stock Prices

## Repository Structure

```
├── [Analysis](./Analysis)       # Main R script and derived data objects
├── [docs](./docs)               # Research paper and documentation
├── [Writeup](./Writeup)         # Generated figures and tables
└── README.md                    # Project overview
```

## Dependencies

Main packages used in R:

- `quanteda`
- `tidyverse`
- `edgar`
- `rvest`
- `plm`
- `simfinapi`

Install with:

```r
install.packages(c("quanteda", "tidyverse", "edgar", "rvest", "plm", "simfinapi"))
```

## Reproduction

1. Clone the repo:
   ```bash
   git clone https://github.com/acser00/10K-ESG-Insights.git
   ```

2. Run the analysis:
   ```r
   source("Analysis/main.R")
   ```

## Results Summary

- Governance-related disclosures dominate ESG mentions across sectors.
- Positive association between ESG word count and adjusted stock price.
- Relationship with CAR is weaker and varies by model specification.

## More Info

The full research paper is in [`docs/`](./docs/). All code is in [`Analysis/`](./Analysis/).

## Interactive Visualizations

View key ESG visualizations here:  
👉 [GitHub Pages Gallery](https://<acser00>.github.io/<10K-ESG-Insights>/gallery/)

