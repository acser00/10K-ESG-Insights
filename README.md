# 10K-ESG-Insights

This repository contains a research project that quantitatively analyzes ESG disclosures within corporate 10-K filings and examines their relationship to stock performance.

## Abstract

The project employs quantitative text analysis to extract and evaluate ESG-related language from publicly filed 10-K reports. By leveraging a domain-specific ESG dictionary and methods such as Wordscores, the study investigates how variations in ESG disclosures—particularly in governance—correlate with market performance indicators.

## Methodology

- **Data Collection:** Corporate 10-K filings obtained from the SEC’s EDGAR database.
- **Text Processing:** Preprocessing includes stop-word removal, case folding, stemming, and tokenization.
- **Analysis:** 
  - Application of the Wordscores method to generate an ESG disclosure index.
  - Frequency analysis and proportional weighting to assess term distribution.
  - Correlation of ESG term frequencies with stock performance metrics.
- **Tools:** R for data processing and statistical analysis; LaTeX for document preparation.

## Key Findings

- ESG term frequencies, especially governance-related terms, have increased over time.
- A significant relationship exists between the volume of ESG disclosures and stock performance, though results indicate sensitivity to reference text selection.
- The study highlights the evolving nature of corporate risk disclosure through text-based analysis.

## Repository Structure

- **/data**: Raw and processed data files (10-K filings, ESG dictionaries).
- **/scripts**: R scripts for data extraction, text processing, and statistical analysis.
- **/results**: Generated plots, tables, and supplementary visualizations.
- **/docs**: Research paper (PDF) and supporting documentation.
- **README.md**: This overview.

## Setup & Usage

1. **Dependencies:** R (with packages such as `quanteda`, `tidyverse`, `rvest`, etc.) and LaTeX.
2. **Installation:** Clone the repository and install the required R packages.
3. **Execution:** Run the R scripts in the `/scripts` folder sequentially to reproduce the analysis and generate figures.
4. **Reproducibility:** The complete analysis pipeline is provided to ensure full reproducibility.
