# Install Pacakges
install.packages(c("tm", 
                   "lda", 
                   "LDAvis",
                   "servr"))

# Load Library
library(tidyverse)
library(lubridate)
library(tm)
library(lda)
library(LDAvis)
library(servr)

# Load Data
df <- read_rds("data/tweet-clean-energy.rds")

# Filter Data
df_filter <- df %>%
  mutate(created_at = as_date(created_at)) %>%
  filter(created_at == dmy("08-10-2021"))

# Select Text
tweets <- df_filter$text

# Read Stopwords
stop_words <- stopwords("SMART")

# Pre-processing
tweets <- gsub("'", "", tweets)
tweets <- gsub("[[:punct:]]", " ", tweets) 
tweets <- gsub("[[:cntrl:]]", " ", tweets)
tweets <- gsub("^[[:space:]]+", "", tweets)
tweets <- gsub("[[:space:]]+$", "", tweets) 
tweets <- tolower(tweets)  

# Tokenize
doc.list <- strsplit(tweets, "[[:space:]]+")

# Compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# Remove terms that are stop words or occur fewer than 5 times
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# Now put the documents into the format required by the lda package
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute statistics
D <- length(documents) 
W <- length(vocab) 
doc.length <- sapply(documents, function(x) sum(x[2, ]))  
N <- sum(doc.length) 
term.frequency <- as.integer(term.table)

# Set Parameter
K <- 3
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

# Get topic information
topic <- list(phi = phi,
              theta = theta,
              doc.length = doc.length,
              vocab = vocab,
              term.frequency = term.frequency)

# Create the JSON object to feed the visualization
json <- createJSON(phi = topic$phi, 
                   theta = topic$theta, 
                   doc.length = topic$doc.length, 
                   vocab = topic$vocab, 
                   term.frequency = topic$term.frequency)

# Visualize
serVis(json, out.dir = 'vis', open.browser = TRUE)
