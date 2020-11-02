rm(list=ls())
############################################################################
########### ########### Writen by Myles Durkin ################# ###########
############################################################################

# Credit for STidy Text Mining with R
# https://www.tidytextmining.com/tidytext.html

# If required, install the following packages:

#install.packages("tm")
#install.packages(c("SnowballC","wordcloud","RColorBrewer","RCurl","XML"))

#setwd("") # If required

################## Load Libraries #########################
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(factoextra)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(slam)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)
library(stringr)
library(pdftools)

####### Load Data & Data Cleanup ################
# file.choose let's you go find the file not needed but could be handy later
# File needs to be in a plain .txt format. Refer to previous classes to ingest a PDF document
  # Example PDF
  # adrp1_03 <- pdf_text(pdf = 'adrp1_03.pdf') %>% read_lines()
tp525.3.1 <- pdf_text(pdf = 'TP525-3-1.pdf') %>% read_lines() # MDO
tp525.3.8 <- pdf_text(pdf = 'TP525-3-8.pdf') %>% read_lines() # M&M FC

# You can Define your own stopwords with a string
userspecified.stopwords <- c("september","unified", "unclassified", "operations",
                             "jaed","army", "enemy","arcic","also", "get","like", 
                             "made", "can", "issue", "discussion", "just", "i",
                             "recommendation", 'tradoc', 'pamphlet')

toSpace <- content_transformer(function(x,pattern) gsub(pattern, " ", x))

cleandocument <- function(document,wordstotakeout,output){
  doc.x <- Corpus(VectorSource(document))
  doc.x  <- tm_map(doc.x, toSpace,"/")
  doc.x  <- tm_map(doc.x, toSpace, "@")
  doc.x  <- tm_map(doc.x, toSpace, "\\|")
  # Convert the text to lower case
  doc.x <- tm_map(doc.x, content_transformer(tolower))
  # Remove numbers
  doc.x <- tm_map(doc.x, removeNumbers)
  # Remove english common stopwords
  doc.x <- tm_map(doc.x, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  doc.x <- tm_map(doc.x, removeWords, wordstotakeout)
  # Stem document - ie. started, start
  doc.x <- tm_map(doc.x,stemDocument)
  # Remove punctuations
  doc.x <- tm_map(doc.x, removePunctuation)
  # Eliminate extra white spaces
  doc.x <- tm_map(doc.x, stripWhitespace)
  ##################### Build a Term Matrix ##########################
  dtm <- TermDocumentMatrix(doc.x)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 100)
  #Build a Document Term Matrix
  output <- DocumentTermMatrix(doc.x)
  return(output)
}
dtm <- cleandocument(tp525.3.1 , userspecified.stopwords, mdoraw.dtm)
mM_td <- tidy(cleandocument(tp525.3.8, userspecified.stopwords, mdoraw.dtm))
tp525.3.1_td <- tidy(cleandocument(tp525.3.1, userspecified.stopwords, tp525.3.dtm))


###### Bind Documents Together ######################
mdo_td <- bind_rows(mM_td %>% mutate(book="M and M FC"),
                    tp525.3.1_td %>% mutate(book = "MDO"))

# To Make list of dtms - Used for Topic Modeling
# list_of_dtms <- list(mdo.DocumentTermMatrix,uc18.1.DocumentTermMatrix ) # Add all lists
# convert list of dtms into one big dtm
# dtms_combined_into_one <- do.call(tm:::c.DocumentTermMatrix, list_of_dtms)

# Term Frequencies
(mdo_words <- mdo_td %>%
    count(book, term, sort = TRUE) %>% # Counts the words in the document
    ungroup())

mdo_dtm <- mdo_words %>% 
  cast_dtm(book, term, n) # Required for later analysis

total_words <- mdo_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

(mdo_words <- left_join(mdo_words, total_words))

mdo_words

#Term Frequency Distribution
ggplot(mdo_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Zipf's law - states the frequency that a word appears is inversely proportional to its rank
# Application of Zipf's Law
freq_by_rank <- mdo_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
# Insert the incerpets and rank
mdo.intercept = -1.0911
  mdo.slope =  -0.8329 
  
  freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = mdo.intercept, slope = mdo.slope, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#These words are, as measured by tf-idf, the most important to each book and most readers would likely agree. 
#This is the point of tf-idf; 
#it identifies words that are important to one document within a collection of documents.
mdo_words <- mdo_words %>% 
  bind_tf_idf(term, book, n)

mdo_words

mdo_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(term, levels = rev(unique(term)))) %>% 
  group_by(book) %>% 
  top_n(15, tf_idf) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Tokenizing by n-gram
mdo_bigrams <- mdo_td %>% unnest_tokens(bigram, term, token ="ngrams", n=2)

mdo_bigrams

mdo_bigrams %>% 
  count(bigram, sort = T)

bigrams_separated <- mdo_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# Recombined
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# View Trigrams

mdo_td %>%
  unnest_tokens(trigram, term, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Analyzing Bigrams

bigrams_filtered %>%
  filter(word2 == "rapid") %>%
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# 

# Visualize Bigram/N-Gram Combination
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

set.seed(1234)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

###### Pair Wise Correlation #####
#  Counting and correlating among sections
mdo_section_words <- mdo_td %>%
  filter(book == "MDO") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, term) %>%
  filter(!word %in% stop_words$word)

mdo_section_words

# count words co-occuring within sections
word_pairs <- mdo_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

# Find Common Word Pairs 
word_pairs %>%
  filter(item1 == "rapid")

# Pairwise Correlation
# we need to filter for at least relatively common words first
word_cors <- mdo_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# Find the Words most correlated with a word:

# word_cors %>%
#   filter(item1 == "Enter a Word")

# Visualize Words and Correlations

word_cors %>%
  filter(item1 %in% c("antiaccess", "rapid")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# Visualize Bigram
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, width = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


################### Topic Modeling #####################
# Don't run - may take a while
# library(ldatuning)
# topics <- FindTopicsNumber(mdo_dtm, topics = seq(from =2, to = 50, by = 2,
#                                                  method = 'VEM', control = list(seed=1234),
#                                                  verbose = T))
# 
# FindTopicsNumber_plot(topics) # 8 by Griffiths

# Need To Ensure you are Using a DTM 
mdo_lda <-  LDA(mdo_dtm, k=8, control = list(seed=1234))
mdo_lda

mdo_topics <- tidy(mdo_lda, matrix = "beta")
mdo_topics

mdo_top_terms <- mdo_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

mdo_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  arrange(desc(beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# beta_spread <- mdo_topics %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   spread(topic, beta) %>%
#   filter(topic1 > .001 | topic2 > .001) %>%
#   mutate(log_ratio = log2(topic2 / topic1))

# beta_spread

mdo_top_terms_forwordcloud <- mdo_topics %>%
  group_by(topic) %>%
  top_n(200, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

library(reshape2)
png("wordcloud_packages.png", width = 1280, height = 800)
mdo_top_terms_forwordcloud %>% 
  mutate(topic = paste("topic", topic)) %>%
  acast(term ~ topic, value.var = "beta", fill = 0) %>%
  comparison.cloud(max.words = 300)
dev.off()

# LDA Vis -- Bonus
library(LDAvis)
topicmodels_json_ldavis <- function(fitted, doc_term){
  
  # Find required quantities
  phi <- as.matrix(posterior(fitted)$terms)
  theta <- as.matrix(posterior(fitted)$topics)
  vocab <- colnames(phi)
  term_freq <- slam::col_sums(doc_term)
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = as.vector(table(doc_term$i)),
                                 term.frequency = term_freq)
  return(json_lda)
}

# Save the required function that extracts variables from the fitted model to required format 

# Run the function
json <- topicmodels_json_ldavis(fitted = mdo_lda, doc_term = mdo_dtm)
LDAvis::serVis(json)

#Tell Topic Per Document - Examines the per-document-per-topic probability "gamma"
mdo_documents_topic <- tidy(mdo_lda, matrix = "gamma")
mdo_documents_topic 

t <- mdo_documents_topic %>%
  pivot_wider(names_from = topic, names_prefix = "topic ", values_from = gamma)

# Extract to analyze the results
# write.csv(t, 'Comparison of the Two Concept Documents.csv')

