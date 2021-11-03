# Install & Load Packages -------------------------------------------------

# Install Packages
install.packages(c("tidyverse", 
                   "textclean", 
                   "vader"))

# Load Packages
library(tidyverse)
library(lubridate)
library(textclean)
library(vader)

# Load Data ---------------------------------------------------------------

# Load Data
df <- read_rds("data/tweet-clean-energy.rds")

# Melihat 5 baris pertama data
head(df, 5)

# Data Preprocessing ------------------------------------------------------

# Text Cleaning
df_clean <- df %>%
  mutate(created_at = as_date(created_at)) %>%
  filter(created_at == dmy("08-10-2021")) %>%
  select(text) %>%
  mutate(text = strip(text)) %>%
  mutate(text = replace_emoji(text)) %>%
  mutate(text = replace_html(text)) %>%
  mutate(text = replace_hash(text, pattern = "#([A-Za-z0-9_]+)", replacement = "")) %>%
  mutate(text = replace_tag(text, pattern = "@([A-Za-z0-9_]+)", replacement = ""))

# Sentiment Analysis ------------------------------------------------------

# Analisis sentiment
sentiment <- vader_df(df_clean$text)

# Visualize ---------------------------------------------------------------

# Visualisasi Sentiment
visualisasi_sentiment <- sentiment %>%
  mutate(sentiment = case_when(
    compound >= 0 ~ "positive",
    compound < 0 ~ "negative"
  )) %>%
  group_by(sentiment) %>%
  count() %>%
  drop_na() %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  labs(
    title = "Sentiment Analysis: Clean Energy",
    y = "Jumlah Tweet",
    x = "",
    fill = "Sentiment"
  ) +
  theme_minimal()
visualisasi_sentiment

# Menyimpan hasil visualisasi sentiment
ggsave(visualisasi_sentiment,
       filename = "plot/sentiment.png",
       width = 15,
       height = 15,
       dpi = 300,
       type = "cairo",
       units = "cm",
       limitsize = FALSE
)