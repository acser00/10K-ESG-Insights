# ESG Analysis of Corporate 10-K Filings and Stock Performance

This repository contains the coding implementation for a research project conducted as part of the **Quantitative Text Analysis for Social Science (PUBL0099)** module at UCL. The research quantitatively investigates ESG (Environmental, Social, Governance) disclosures in corporate 10-K filings and their impact on stock performance.

## Project Overview

The primary goal of this analysis was to examine whether the frequency and content of ESG-related terms within corporate annual 10-K filings correlate with stock market performance indicators. Using quantitative text analysis methods, particularly dictionary-based approaches, the study explores sector-specific disclosure patterns and their implications on financial performance.

## Methodology

- **Data Collection:**
  - 10-K filings retrieved from the SEC's EDGAR database for companies listed in the S&P 500 across four high-pollution sectors (Utilities, Materials, Energy, Industrials) from 2016 to 2024.

- **Text Processing:**
  - Tokenization, stop-word removal, and stemming using R’s Quanteda package.

- **Analytical Approach:**
  - Application of a domain-specific ESG dictionary (Baier et al., 2020).
  - Frequency analysis of ESG terms by category.
  - Fixed-effects regression models to assess relationships between ESG term frequency and stock performance indicators such as cumulative abnormal returns (CAR) and adjusted stock prices.

## Repository Structure

```
├── data/               # Data files (raw and processed)
├── scripts/            # R scripts for data scraping, processing, and analysis
├── results/            # Generated visualizations and tables
├── docs/               # Research paper PDF and supplementary documents
└── README.md           # Project documentation
```

## Dependencies

The analysis was conducted using R, leveraging libraries such as:
- Quanteda
- Tidyverse
- Edgar
- rvest
- plm
- simfinapi

Install required libraries via:
```R
install.packages(c("quanteda", "tidyverse", "edgar", "rvest", "plm", "simfinapi"))
```

## Usage

1. Clone the repository:
   ```bash
   git clone <repository_url>
   ```

2. Run scripts sequentially in the `/scripts` folder to replicate analyses:
   ```bash
   Rscript scripts/<script_name>.R
   ```

## Key Findings

- Increased frequency of ESG disclosures observed, primarily driven by governance terms.
- Significant positive correlation between ESG disclosures and adjusted stock prices, yet ambiguous results concerning cumulative abnormal returns.

## Further Information

Detailed research findings and methodology discussions are available in the full research paper located under the `/docs` directory.
