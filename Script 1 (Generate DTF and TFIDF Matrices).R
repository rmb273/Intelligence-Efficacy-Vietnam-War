# R Script for DTF and TFIDF Analyses with Words of Estimative Probability

# CLEAN SLATE
# Specify working directory 
setwd("~/Desktop/SEST704Paper/Results")
# Clear variables
rm(list=ls())
# Clear plots
graphics.off()
# Clear command line screen
cat("\014")


# INSTALL AND CHECK REQUIRED PACKAGES
# install.packages("cluster")
require(cluster)
# install.packages("tm") # tm = text mining
require(tm)
# install.packages("NLP") # nlp = natural language processing infrastructure
# require(NLP)
# install.packages("lsa") # lsa = latent semantic analysis
# require (lsa)
# install.packages("tokenizers")
# require(tokenizers)
# install.packages("tau") # tau = text analysis utilities
# require(tau)


# ESTABLISH AND CLEAN CORPUS OF TEXT FILES
# Specify directory where .txt file(s) for analysis exist and establish corpus
corpus <- Corpus(DirSource("~/Desktop/SEST704Paper/Data/"), readerControl = list(language="lat"))
corpus <- sapply(corpus, function(row) iconv(row, "latin1", "ASCII", sub=""))
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
keep <- c("certain", "certainly", "undoubtedly", "doubtless", "doubtlessly", "probable", "probably", "likely", "perhaps", "uncertain", "maybe", "could", "reportedly", "possible", "possibly", "conceivable", "unlikely", "improbable", "doubtful", "impossible", "inconceivable", "unimaginable", "unbelievable", "unthinkable", "vietnam", "vietnamese", "casualty", "casualties")
corpus <- tm_map(corpus, keepOnlyWords, keep)
corpus <- tm_map(corpus, stripWhitespace)

# CREATE DTM AND INSPECT
myDTM <- DocumentTermMatrix(corpus)
# inspect(DocumentTermMatrix(corpus, list(dictionary=wepList)))
myTFIDF <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
# inspect(myTFIDF, list(dictionary=wepList))

# WRITE PLAIN DTM/TFIDF
# Write files: Document Term Frequency (DTM)
write.csv(as.matrix(myDTM),"Analysis_DTM.csv")
# Write files: Term Frequency/Inverse Document Frequency (TFIDF)
write.csv(as.matrix(myTFIDF),"Analysis_TFIDF.csv")
