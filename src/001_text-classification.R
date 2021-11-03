# Set Parameter -----------------------------------------------------------

# Set Seed
set.seed(1234)

# Install & Load Packages -------------------------------------------------

# Install Packages
install.packages(c("tidymodels", 
                   "textrecipes", 
                   "discrim", 
                   "naivebayes"))


# Load Packages
library(tidyverse)
library(tidymodels)
library(textrecipes)
library(textclean)
library(discrim)
library(naivebayes)

# Load Data ---------------------------------------------------------------

# Load Data
df <- read_csv2("data/tweet-grab.csv")

# Menampilkan keseluruhan data
print(df)

# Menampilkan Rangkuman Data
glimpse(df)

# Fix Data: Mengubah tipe data sentiment sebagai faktor
df_fix  <- df %>%
  mutate(sentiment = as.factor(sentiment))

# Menampilkan Rangkuman Data
glimpse(df_fix)


# Split Data --------------------------------------------------------------

# Split Data
df_split <- df_fix %>%
  initial_split(test = 0.70)

# Menampikan Data Training
df_training <- df_split %>%
  training()
df_training

# Menampilkan Data Testing
df_testing <- df_split %>%
  testing
df_testing


# Membuat Alur Pemrosesan Data --------------------------------------------

# Membuat Alur Pemrosesan data
df_recipe <- df_training %>%
  recipe(sentiment ~.) %>%
  themis::step_downsample(sentiment) %>%
  step_tokenize(text) %>%
  step_tfidf(text)


# Modeling ----------------------------------------------------------------

# Menentukan Model (Naive Bayes)
nb <-  naive_Bayes() %>% 
  set_engine("naivebayes") %>% 
  translate()

# Menjadikan Workflow
workflow <- workflow() %>%
  add_recipe(df_recipe) %>%
  add_model(nb)

# Training Model
model <- fit(workflow, df_training)


# Testing & Evaluation ----------------------------------------------------

# Melihat hasil Prediksi 
model %>%
  predict(df_testing) %>%
  bind_cols(df_testing)

# Menentukan metrik evaluasi untuk mengukur performa model
multi_metrics <- metric_set(accuracy, precision, recall, specificity)

# Melihat performa model
model %>%
  predict(df_testing) %>%
  bind_cols(df_testing) %>%
  multi_metrics(truth = sentiment, estimate = .pred_class)


# Prediksi ke Tweet Baru --------------------------------------------------

# Membuat Data Sentiment Baru
tweet_baru <- tibble(text = "Abang Drivernya Ganteng")

# Deteksi Sentiment
model %>%
  predict(tweet_baru)
