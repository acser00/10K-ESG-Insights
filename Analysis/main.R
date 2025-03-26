#--------------------------------------------------------#
# Quantative Text Analysis (PUBL0099) Research Project   #
# HHPM8                                                  #
#--------------------------------------------------------#

#---------------------#
# Environment Setup   #
#---------------------#
rm(list=ls())
setwd("~/University/MSc/QTA/Research_Project/Analysis")

#---------------------------#
# Load Required Libraries   #
#---------------------------#
library(tidyverse)
library(rvest)
library(edgar)
library(quanteda)
library(foreach)
library(doParallel)
library(progress)
library(lubridate)
library(ggplot2)
library(quanteda.textstats)
library(quanteda.textplots)
library(wordcloud)
library(RColorBrewer)
library(viridis)
library(scales)
library(cowplot)
library(seededlda)
library(plm)
library(simfinapi)
library(stringr)
library(janitor)
library(car)
library(RPostgres)
library(texreg)
#--------------------------------#
# Extract S&P 500 Company CIKs   #
#--------------------------------#
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
page <- read_html(url)
tables <- html_table(page, fill = TRUE)
sp500_table <- tables[[1]] 
filtered_cik_table <- sp500_table %>%
  filter(`GICS Sector` %in% c("Industrials", "Utilities", "Materials", "Energy")) %>% 
  mutate(CIK = as.character(CIK))
cik_numbers <- filtered_cik_table$CIK
cik_vector <- as.numeric(cik_numbers)

#-------------------------------------#
# Parallel Download of 10-K Filings   #
#-------------------------------------#
num_Cores <- detectCores() - 1 
registerDoParallel(cores = num_Cores)
output_list <- foreach(year = c(2016:2024), .combine = 'c', .packages = 'edgar') %dopar% {
  getFilingsHTML(filing.year = year, form.type = "10-K", cik.no = cik_vector, quarter = c(1,4))
}

#----------------------------------------------------------#
# Process HTML Files of Annual Reports into a data frame   #
#----------------------------------------------------------#
base_path <- "~/University/MSc/QTA/Research_Project/Analysis/edgar_FilingsHTML/Form 10-K"
cik_dirs <- list.dirs(path = base_path, full.names = TRUE, recursive = FALSE)

# Function to read the text of an HTML file
read_html_content <- function(file_path) {
  read_html(file_path) %>%
    html_text(trim = TRUE)
}

# Function to extract and format the date from the HTML file name
extract_date <- function(file_name) {
  date_str <- str_extract(basename(file_name), "\\d{4}-\\d{2}-\\d{2}")
  if (is.na(date_str)) {
    return(NA)
  }
  date <- ymd(date_str)
  formatted_date <- format(date, "%d-%m-%Y")
  return(formatted_date)
}

# Parallel processing of the HTML files into a tibble
annual_filings <- foreach(cik_dir = cik_dirs, .combine = 'rbind', .packages = c("rvest", "dplyr", "stringr", "lubridate", "purrr")) %dopar% {
  cik_number <- basename(cik_dir)

  html_files <- list.files(cik_dir, pattern = "\\.html$", full.names = TRUE)

  filing_info <- map_dfr(html_files, function(file_path) {
    content <- read_html_content(file_path)
    filing_date <- extract_date(file_path)
    tibble(CIK = cik_number, Filing_Date = filing_date, Report_Text = content)
  })
  return(filing_info)
}

save(annual_filings, file = "annual_filings.Rda")
stopImplicitCluster()

#------------------------------------------#
# Data Cleaning and Exploratory Analysis   #
#------------------------------------------#
load("annual_filings.Rda")
str(annual_filings)

annual_filings <- annual_filings %>%
  mutate(Filing_Date = dmy(Filing_Date)) %>% 
  mutate(Year = year(Filing_Date))


cik_counts_per_year <- annual_filings %>%
  group_by(CIK, Year) %>%
  summarise(Filings_Count = n(), .groups = 'drop')

total_filings_per_cik <- cik_counts_per_year %>%
  group_by(CIK) %>%
  summarise(Total_Filings = sum(Filings_Count), .groups = 'drop') %>%
  arrange(desc(Total_Filings))

cik_dist_plot <- ggplot(total_filings_per_cik, aes(x = Total_Filings)) +
  geom_histogram(binwidth = 1, fill = '#008cff', color = 'black') +
  labs(title = "Distribution of Total Filings per CIK", x = "Total Filings", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = min(total_filings_per_cik$Total_Filings, na.rm = TRUE), 
                                  to = max(total_filings_per_cik$Total_Filings, na.rm = TRUE), 
                                  by = 1)) 

ggsave("Total_Filings_Distribution.png", plot = cik_dist_plot, path = "~/University/MSc/QTA/Research_Project/Writeup", width = 10, height = 6, dpi = 300)

#---------------------------------------------------------#
# Create dataframe with one filing per company per year   #
#---------------------------------------------------------#
years_present <- range(annual_filings$Year)
expected_filings <- length(years_present[1]:years_present[2])
annual_filings_2 <- annual_filings %>%
  group_by(CIK) %>%
  filter(length(unique(Year)) == expected_filings) %>%
  filter(n() == expected_filings) %>%
  ungroup() %>% 
  mutate(Document_ID = paste(CIK, Year, sep = "_")) %>% 
  left_join(filtered_cik_table, by = c("CIK" = "CIK"))

#---------------------------#
# ESG Dictionary Creation  #
#--------------------------#
esg_data <- read_csv("ESG-Wordlist.csv")
esg_data <- esg_data %>%
  mutate(across(everything(), ~na_if(.x, "-"))) %>% 
  mutate(Topic = ifelse(Topic == "Environmental (Additon LMO 2022)", "Environmental", Topic)) %>% 
  group_by(Word) %>%
  filter(!(duplicated(Word) & Topic == "Governance")) %>%
  ungroup()

dict_list <- list()

for (i in 1:nrow(esg_data)) {
  word <- as.character(esg_data[i, "Word"])
  topic <- as.character(esg_data[i, "Topic"])
  # Use NA directly instead of a string to represent missing values
  category <- ifelse(esg_data[i, "Category"] == "-", NA, as.character(esg_data[i, "Category"]))
  subcategory <- ifelse(esg_data[i, "Subcategory"] == "-", NA, as.character(esg_data[i, "Subcategory"]))

  # Ensure topic, category, and subcategory are handled as character strings
  topic <- toString(topic)
  category <- toString(category)
  subcategory <- toString(subcategory)

  # Initialize topic if not already present
  if (is.null(dict_list[[topic]])) {
    dict_list[[topic]] <- list()
  }

  # Initialize category within topic if not already present
  if (is.na(category) || is.null(dict_list[[topic]][[category]])) {
    if (!is.na(category)) {
      dict_list[[topic]][[category]] <- list()
    } else {
      dict_list[[topic]][["NA_category"]] <- list()
      category <- "NA_category"
    }
  }

  # Initialize subcategory within category if not already present.
  if (is.na(subcategory) || is.null(dict_list[[topic]][[category]][[subcategory]])) {
    if (!is.na(subcategory)) {
      dict_list[[topic]][[category]][[subcategory]] <- c()
    } else {
      dict_list[[topic]][[category]][["NA_subcategory"]] <- c()
      subcategory <- "NA_subcategory"  
    }
  }
  # Append word to the subcategory
  dict_list[[topic]][[category]][[subcategory]] <- c(dict_list[[topic]][[category]][[subcategory]], word)
}

esg_dict <- dictionary(dict_list)

#----------------------#
# Text Preprocessing   #
#----------------------#
corpus <- annual_filings_2 %>% corpus(text_field = "Report_Text")

tokens <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
tokens <- tokens_tolower(tokens)
tokens <- tokens_remove(tokens, stopwords("en"))

filings_dfm <- dfm(tokens)

saveRDS(filings_dfm, file = "filings_dfm.Rds")

#-----------------------------------#
# ESG Category Frequency Analysis   #
#-----------------------------------#
filings_dfm <- readRDS("filings_dfm.Rds")

docvars(filings_dfm, "Year") <- as.factor(docvars(filings_dfm, "Year"))

# Apply proportional weighting to adjust for document length
filings_dfm_wgt <- dfm_weight(filings_dfm, scheme = "prop")

# Lookup the ESG categories in the filings
filings_dfm_lookup <- dfm_lookup(filings_dfm, dictionary = esg_dict, )
filings_dfm_lookup_wgt <- dfm_lookup(filings_dfm_wgt, dictionary = esg_dict)

# Calculate the frequency of ESG categories
categegory_counts_wgt <- textstat_frequency(filings_dfm_lookup_wgt)
category_counts <- textstat_frequency(filings_dfm_lookup)

category_counts <- category_counts %>%
  mutate(Category = sapply(str_split(feature, "\\."), function(x) {
    if (length(x) >= 2 && x[2] != "NA") {
    return(x[2])}  # Return the second level with "Miscellaneous" if the third level is "NA".
    else {
      # Return the topic if the second level is "NA".
    return(paste(x[1], "Miscellaneous"))
    }
  }))

# Plot the frequency of categories for all years
category_counts_plt <- ggplot(category_counts, aes(x = Category, y = frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  coord_flip() +  
  labs(x = "Category", y = "Count", title = "Frequency of Categories in the Corpus") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("Category_Frequency.png", plot = category_counts_plt, path = "~/University/MSc/QTA/Research_Project/Writeup", width = 16, height = 9, dpi = 300)


### Yearly ESG Category Frequency Analysis ###

# Raw counts of ESG words by year
dfm_grouped_by_year <- dfm_group(filings_dfm_lookup, groups = docvars(filings_dfm_lookup, "Year"))

category_counts_by_year <- textstat_frequency(dfm_grouped_by_year, groups = docvars(dfm_grouped_by_year, "Year"))

# Proportional weighting of ESG words by year 
dfm_grouped_by_year_prop <- dfm_grouped_by_year %>%
	dfm_weight(scheme = "prop")

category_counts_by_year_prop <- textstat_frequency(dfm_grouped_by_year_prop, groups = docvars(dfm_grouped_by_year_prop, "Year"))

# Calculate the total frequency of ESG terms per year by Category
yearly_category_frequencies <- category_counts_by_year %>% 
	mutate(Category = sapply(str_split(feature, "\\."), function(x) {
		if (length(x) >= 2 && x[2] != "NA") {
		return(x[2])}  # Return the category with "Miscellaneous" if the subcategory is "NA".
		else {
			# Return the topic with "Msciellaneous" if the category is "NA".
		return(paste(x[1], "Miscellaneous"))
	}
		})) %>% 
	rename(Year = group) %>%
	mutate(Year = as.numeric(Year)) %>%
	group_by(Category, Year) %>%
	summarise(cat_frequency = sum(frequency), .groups = 'drop') %>% 
	arrange(Year, Category)

# Calculate the total frequency of ESG terms weighted proportionally per year by Category
yearly_category_frequencies_prop <- category_counts_by_year_prop %>%
	mutate(Category = sapply(str_split(feature, "\\."), function(x) {
		if (length(x) >= 2 && x[2] != "NA") {
		return(x[2])}  # Return the category with "Miscellaneous" if the subcategory is "NA".
		else {
			# Return the topic with "Msciellaneous" if the category is "NA".
		return(paste(x[1], "Miscellaneous"))
	}
		})) %>% 
	rename(Year = group) %>%
	mutate(Year = as.numeric(Year)) %>%
	group_by(Category, Year) %>%
	summarise(cat_frequency = sum(frequency), .groups = 'drop') %>% 
	arrange(Year, Category) %>%
	group_by(Category) %>%
	mutate(
		prev_freq = lag(cat_frequency, order_by = Year),
		yoy_change = ((cat_frequency - prev_freq) / prev_freq) * 100) %>% 
	ungroup() %>% 
	arrange(Category, Year)

# Create a plot of the raw counts of ESG categories over time
esg_cat_raw_count_plt <- ggplot(yearly_category_frequencies, aes(x = Year, y = cat_frequency, color = Category)) +
  geom_line() +
  facet_wrap(~ Category, scales = "free_y") +
  labs(
    title = "ESG Category Counts Over Time", 
    x = "Year",
    y = "Frequency",
    subtitle = "Each panel corresponds to a specific category of ESG terms") +
  theme_minimal() +
  theme(legend.position = "none",  
        strip.text.x = element_text(size = 7))

ggsave("Category_Frequency_Over_Time.png", plot = esg_cat_raw_count_plt, path = "~/University/MSc/QTA/Research_Project/Writeup", width = 16, height = 9, dpi = 300)


# Create a plot of the proportions of ESG categories over time
cat_pct_change_plt <- ggplot(yearly_category_frequencies_prop, aes(x = Year, y = yoy_change, color = Category)) +
  geom_line() +
  facet_wrap(~ Category, scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(min(yearly_category_frequencies_prop$Year), max(yearly_category_frequencies_prop$Year), by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.x = element_text(size = 7)
  ) +
  labs(
    title = "Year-Over-Year Percent Change of ESG Terms within Categories",
    subtitle = "Each panel corresponds to a specific category of ESG terms",
    x = "Year",
    y = "Percent Change")

ggsave("Subcategory_Percent_Change_Yr_to_Yr.png", plot = cat_pct_change_plt, path = "~/University/MSc/QTA/Research_Project/Writeup", width = 16, height = 9, dpi = 300)

# Create a plot of the proportional composition of ESG categories over time
custom_palette <- c("#8dd3c7", "#ffe680", "#bebada", "#fb8072", "#80b1d3", "#fdb462", 
                    "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#f0e442", "#1f78b4")

proportional_comp_area_plt <- ggplot(yearly_category_frequencies_prop, aes(x = Year, y = cat_frequency, fill = Category)) +
  geom_area(position = "stack", alpha = 0.8) + 
  scale_y_continuous(labels = percent_format()) +
	scale_x_continuous(breaks = seq(min(yearly_category_frequencies_prop$Year), max(yearly_category_frequencies_prop$Year), by = 1)) +
  scale_fill_manual(values = custom_palette) +  
  labs(
    x = "Year",
    y = "Proportion of Categories",
    fill = "Category",  
    title = "Proportional Composition of ESG Categories in Annual Filings Over Time"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "italic", vjust = 0.5),
		legend.title.align = 0.5,
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 0.5),
		panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("Proportional_Composition_of_ESG_Categories_Area_Chart.png", plot = proportional_comp_area_plt, path = "~/University/MSc/QTA/Research_Project/Writeup", width = 16, height = 9, dpi = 300)

proportional_comp_bar_plt <- ggplot(yearly_category_frequencies_prop, aes(x = Year, y = cat_frequency, fill = Category)) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels = percent_format()) +
	scale_x_continuous(breaks = seq(min(yearly_category_frequencies_prop$Year), max(yearly_category_frequencies_prop$Year), by = 1)) + 
	scale_fill_manual(values = custom_palette) +  
	theme_light() +
  labs(
    x = "Year",
    y = "Proportion of Categories",
    fill = "Category",
    title = "Proportional Composition of ESG Categories in Annual Filings Over Time"
  ) +
  theme(
    legend.title = element_text(face = "italic"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
		axis.text.x = element_text(hjust = 0.5),
		legend.box = "vertical",
		legend.background = element_rect(color = "gray", fill = "white", linewidth = 0.3),
		legend.box.margin = margin(1, 1, 1, 1,),
		legend.spacing = unit(0.4, "cm"),
		legend.position = "right",
		legend.justification = c("right", "center"),
		plot.background = element_rect(fill = "white", color = "gray"),
		panel.border = element_rect(color = "gray",),
		panel.spacing = unit(2, "lines"),
		plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  	guides(fill = guide_legend(reverse = FALSE, title.position = "top", title.hjust = 0.5), 
				 title = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave("Proportional_Composition_of_ESG_Categories_Bar_Chart.png", plot = proportional_comp_bar_plt, path = "~/University/MSc/QTA/Research_Project/Writeup", width = 16, height = 9, dpi = 300)

#---------------------------------------------#
# ESG dictionary frequency analysis by word   #
#---------------------------------------------#
# Total ESG Terms Over Time
total_esg_terms <- yearly_category_frequencies %>%
	group_by(Year) %>%
	summarise(total_count = sum(cat_frequency), .groups = 'drop')

all_esg_plt <- ggplot(total_esg_terms, aes(x = Year, y = total_count)) +
  geom_line() +
	scale_y_continuous(labels = comma, 
		breaks = seq(floor(min(total_esg_terms$total_count)/5000)*5000, 
			ceiling(max(total_esg_terms$total_count)/5000)*5000, by = 5000),
    limits = c(floor(min(total_esg_terms$total_count)/5000)*5000, 
               ceiling(max(total_esg_terms$total_count)/5000)*5000)) +
  labs(
    title = "Total Count of All ESG Terms Over Time", 
    x = "Year", 
    y = "Total Count") +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(min(yearly_category_frequencies$Year), max(yearly_category_frequencies$Year), by = 1))
print(all_esg_plt)
ggsave("Total_ESG_Terms_Over_Time.png", plot = all_esg_plt, path = "~/University/MSc/QTA/Research_Project/Writeup", width = 16, height = 9, dpi = 300)

## Create flattened ESG dictionary for term lookup ##
terms_list <- setNames(as.list(esg_data$Word), esg_data$Word)
esg_terms_dict <- dictionary(lapply(terms_list, function(term) list(term)))

# Lookup the ESG terms in the filings
filings_dfm_lookup_terms <- dfm_lookup(filings_dfm, dictionary = esg_terms_dict)

# Calculate the raw counts of ESG terms
term_counts <- textstat_frequency(filings_dfm_lookup_terms)

# Plot the top 50 words
top_terms <- term_counts %>%
  arrange(desc(frequency)) %>%
  top_n(50, frequency)

top_50_words_plt <- ggplot(top_terms, aes(x = reorder(feature, frequency), y = frequency)) +
  geom_col(fill = "#4a4ae9", alpha = 0.8) +
	coord_flip() +
	scale_y_continuous(labels = comma, 
		breaks = seq(0, ceiling(max(top_terms$frequency)*1.5), by = 10000),
		limits = c(0, max(top_terms$frequency)*1.1)) +
  labs(x = "Term", y = "Count", title = "Frequency Counts of Top 50 Words") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11),
    panel.grid.minor.x = element_line(color = "grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
		axis.text.x = element_text(size = 11),
		axis.title.x = element_text(size = 13),
		axis.title.y = element_text(size = 13, vjust = 0.5),
		title = element_text(size = 15))

ggsave("Top_50_ESG_Terms.png", plot = top_50_words_plt, path = "~/University/MSc/QTA/Research_Project/Writeup", width = 16, height = 9, dpi = 300)

# Create a word cloud of the ESG terms
term_counts$feature <- as.character(term_counts$feature)

category_colors <- c("Environmental" = "#006400", "Governance" = "#8B0000", "Social" = "#00008B")

# Map each term to its corresponding topic for color coding
topics <- sapply(term_counts$feature, function(term) {
  topic <- esg_data[, c("Word", "Topic")]$Topic[match(term, esg_data[, c("Word", "Topic")]$Word)]
  return(topic)
})

term_colors <- category_colors[topics]
names(term_colors) <- term_counts$feature

png("~/University/MSc/QTA/Research_Project/Writeup/wordcloud.png", width = 2750, height = 2100, res = 195)
par(mar=c(4,4,4,12))
wordcloud(
  words = term_counts$feature,
  freq = term_counts$frequency,
  scale = c(6, 0.75),
  min.freq = 1,
  max.words = Inf,
  random.order = FALSE,
  rot.per = 0.25,
  colors = term_colors,
  ordered.colors = TRUE
)
legend("right", legend = names(category_colors), fill = category_colors, bty = "n", cex = 1.2)
title(main = "Wordcloud of ESG Terms", outer = TRUE, cex.main = 1.75, line = -2)
dev.off()

#------------------------------------------------#
# ESG dictionary frequency analysis by company   #
#------------------------------------------------#

# Lookup the ESG terms in the filings
filings_dfm_esg_term_lookup <- dfm_lookup(filings_dfm_wgt, dictionary = esg_terms_dict)
# Calculate the proportions of ESG terms
esg_word_prop_cik_yr <- rowSums(filings_dfm_esg_term_lookup)

# Add these counts to a data frame for analysis or visualization
esg_word_summary_firm_yr <- data.frame(
	CIK = docvars(filings_dfm_esg_term_lookup, "CIK"),
	Year = docvars(filings_dfm_esg_term_lookup, "Year"),
  Filing_Date = docvars(filings_dfm_esg_term_lookup, "Filing_Date"),
	ESG_Word_Count = esg_word_prop_cik_yr) %>% 
  left_join(filtered_cik_table, by = c("CIK" = "CIK"))

# Plot ESG word counts by company and year
company_counts_per_yr <- esg_word_summary_firm_yr %>%
  ggplot(aes(x = Year, y = ESG_Word_Count, color = `GICS Sector`)) +
  geom_point(size = 2, alpha = 0.6) +  # Adjust size and opacity of points
  geom_smooth(se = FALSE, method = "loess", aes(group = `GICS Sector`)) +  # Add smooth trend line
  labs(title = "ESG Word Counts by Company and Year", x = "Year", y = "ESG Word Count") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  facet_wrap(~`GICS Sector`, scales = "free_y") +  # Create a facet for each sector
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("ESG_Word_Counts_Company_Year.png", plot = last_plot(), path = "~/University/MSc/QTA/Research_Project/Writeup", width = 16, height = 9, dpi = 300)
#-------------------------------------------------------#
# Financial data retrieval and processing and merging   #
# with ESG word counts by firm                          #
#-------------------------------------------------------#
# Set the API key and cache directory
sfa_set_api_key("db7f8e8c-b822-4c39-a408-fa17de4af62c")
sfa_set_cache_dir("~/University/MSc/QTA/Research_Project/Analysis/simfin_cache", create = TRUE)

# Create vector of tickers from sp500_table
tickers <- filtered_cik_table$Symbol
# Remove tickers DAY, GEV, VLTO as they are not available in the SimFin database
tickers <- tickers[!tickers %in% c("DAY", "GEV", "VLTO")]
# Split tickers into 2 groups to avoid overloading the API
tickers1 <- tickers[1:ceiling(length(tickers) / 2)]
tickers2 <- tickers[(ceiling(length(tickers) / 2)+1):length(tickers)]

# Obtain financial data for the tickers and combine into a single data frame
results_tickers1 <- lapply(tickers1, function(ticker) {
  sfa_load_shareprices(ticker = ticker, start = "2016-01-01", end = "2024-04-10", ratios = TRUE)
})

results_tickers2 <- lapply(tickers2, function(ticker) {
  tryCatch({
    sfa_load_shareprices(ticker = ticker, start = "2016-01-01", end = "2024-04-01", ratios = TRUE)
  }, error = function(e) {
    cat("Error with ticker:", ticker, "\n")
    NULL
  })
})

fin_df <- bind_rows(results_tickers1, results_tickers2)

save(fin_df, file = "financial_data.Rda")
load("financial_data.Rda")
# Merge the financial and abnormal returns data with the ESG word counts by firm and year
wrds_df <- read_csv("abret_wrds.csv")
wrds_car <- read_csv("wrds_edate.csv")
merged_fin_esg_df <- fin_df %>%
  left_join(filtered_cik_table %>%
              select(Symbol, CIK, `GICS Sector`, `GICS Sub-Industry`),
            by = c("ticker" = "Symbol")) %>% 
  mutate(CIK = as.character(CIK), Year = as.factor(year(Date))) %>% 
  left_join(esg_word_summary_firm_yr, by = c("CIK", "Year")) %>% 
  mutate(Date = as.Date(Date),
         Filing_Date = as.Date(Filing_Date)) %>%
  arrange(CIK, Filing_Date, Date) %>%
  group_by(CIK, Filing_Date) %>%
  filter(Date >= Filing_Date & Filing_Date >= min(Date)) %>%
  mutate(Row_Num = row_number()) %>%
  filter(Row_Num <= 4) %>%
  ungroup() %>%
  left_join(wrds_df, by = c("ticker" = "ticker", "Date" = "DATE")) %>%
  left_join(wrds_car, by = c("ticker" = "ticker", "Date" = "evtdate")) %>%
  clean_names()

# Fixed effects model for CAR
fe_model_car <- plm(car ~ esg_word_count + price_to_book_value_ttm + altman_z_score_ttm + ev_ebitda_ttm + book_to_market_value_ttm + price_to_earnings_ratio_ttm  + gics_sector_x + dividend_yield_ttm + log(market_cap) + price_to_free_cash_flow_quarterly + operating_income_ev_ttm, data = merged_fin_esg_df, index = c("cik", "date"), model = "within", effect = "twoways")
summary(fe_model_car)

# Fixed effects model for adjusted closing price
fe_model_adj_p <- plm(log(adjusted_closing_price) ~ esg_word_count + price_to_book_value_ttm + altman_z_score_ttm + ev_ebitda_ttm + book_to_market_value_ttm + price_to_earnings_ratio_ttm  + gics_sector_x + dividend_yield_ttm + log(market_cap) + price_to_free_cash_flow_quarterly + operating_income_ev_ttm, data = merged_fin_esg_df, index = c("cik", "date"), model = "within", effect = "twoways")
summary(fe_model_adj_p)

# Create a latex table for the fixed effects models
stargazer(fe_model_car, fe_model_adj_p, 
  type = "latex", # Change to "text" for plain text output or "html" for HTML output
  title = "Fixed Effects Model Results for CAR and Adjusted Closing Price",
  align = TRUE,
          style = "default",
          summary = TRUE,
          out = "regression_table.tex", # Change the file extension based on the output type
          covariate.labels = c("ESG Word Count", "Price to Book Value TTM", 
                               "Altman Z-Score TTM", "EV/EBITDA TTM", "Book to Market Value TTM",
                               "Price to Earnings Ratio TTM", "GICS Sector", "Dividend Yield TTM",
                               "Log(Market Cap)", "Price to Free Cash Flow Quarterly", 
                               "Operating Income/EV TTM"),
          dep.var.labels = NULL,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes = c("Standard errors in parentheses", 
                    "Significance levels: *p<0.1; **p<0.05; ***p<0.01"),
          notes.append = TRUE,
          model.names = FALSE,
          column.labels = c("CAR Model", "Adjusted Closing Price Model"))
