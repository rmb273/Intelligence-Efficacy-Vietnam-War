# TFIDF-DTF-Analysis-Script-R-
TFIDF/DTF Analysis Script (R)

# R Script for DTF and TFIDF Analyses with Words of Estimative Probability
# Adapted from script written by Sean Nolan, Georgetown University

# Specify working directory 
setwd("~/Desktop/TextMining")
# Clear variables
rm(list=ls())
# Clear plots
graphics.off()
# Clear command line screen
cat("\014")

# INSTALL AND CHECK REQUIRED PACKAGES

# install.packages("tm") # tm = text mining
require(tm)
#install.packages("lsa") # lsa = latent semantic analysis
require (lsa)
#install.packages("cluster")
require(cluster)

# ESTABLISH AND CLEAN CORPUS OF TEXT FILES

# Specify directory where .txt file(s) for analysis exist and establish corpus
corpus  <-Corpus(DirSource("~/Desktop/TextMining/Data"), readerControl = list(language="lat"))
corpus<- sapply(corpus, function(row) iconv(row, "latin1", "ASCII", sub=""))
corpus <- Corpus(VectorSource(corpus))

# Clean corpus
remove_dash <- function(x) gsub("-", " ",x)
corpus <- tm_map(corpus, content_transformer(remove_dash))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeNumbers)

# Specify exact search terms to examine if only looking for specific terms
keepOnlyWords <- content_transformer(function(x, words) {
  regmatches(x, 
             gregexpr(paste0("\\b(",  paste(words, collapse = "|"), "\\b)"), x)
             , invert = T) <- " "
  x
})
keep <- c("certain", "certainly", "undoubtedly", "doubtless", "doubtlessly", "probable", "probably", "likely", "perhaps", "uncertain", "maybe", "could", "reportedly", "possible", "possibly", "conceivable", "unlikely", "improbable", "doubtful", "impossible", "inconceivable", "unimaginable", "unbelievable", "unthinkable")
corpus <- tm_map(corpus, keepOnlyWords, keep)
corpus <- tm_map(corpus, stripWhitespace)

# WRITE FILES

# Write files: Document Term Frequency (DTM)
dtm <- DocumentTermMatrix(corpus)
write.csv(as.matrix(dtm),"Analysis_DocumentTermFrequency.csv")

# Write files: Term Frequency/Inverse Document Frequency (TFIDF)
tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
write.csv(as.matrix(tfidf),"Analysis_TFIDF.csv")
