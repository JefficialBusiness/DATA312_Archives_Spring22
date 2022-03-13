# HW2B
# Jeffrey Williams
# Dr. Robinson
# February 22, 2022


# Cleaning workspace / loading libraries
rm(list=ls())
gc()
library(tidyverse)
library(tidytext)
library(gutenbergr)
library(textdata)
library(wordcloud)
library(reshape2)
library(wordcloud)

# Retrieving list of data on Shakesphere work
shakespeare_corpus <- read_csv('shakespeare_gutenberg.csv')
View(shakespeare_corpus)

# Establishing a list of early modern english (EME) stop words for later removal
eme_stop_words <- tibble(word=c("thee", "thou", "hath", "thy", "thine", "ye",
                                "tis"))

# Iterates each book in shakespeare_corpus to add contents into one large data-
# frame
book <- gutenberg_download(shakespeare_corpus$`Gutenberg ID`[1])

for(i in 2:nrow(shakespeare_corpus)) {
  book <- book %>% 
    add_row(gutenberg_download(shakespeare_corpus$'Gutenberg ID'[i]))
}

# Tidying / removal of stop words
tidy_shakespeare <- book %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% anti_join(eme_stop_words)

tidy_shakespeare <- tidy_shakespeare %>%
  inner_join(shakespeare_corpus, by=c('gutenberg_id'="Gutenberg ID"))

View(tidy_shakespeare)
 

# ___ WORD FREQUENCY ANALYSIS ___

# Finding most common words in all (or most) of Shakespeare's work
tidy_shakespeare %>% count(word, sort=TRUE)

# Most common words appear to be lord, king, sir, enter, love, time, speak, mine
# duke, world

# Sorting words and their respective counts
word_counts <- tidy_shakespeare %>% count(word)

# Creating a word cloud using sorted data on word frequencies
tidy_shakespeare %>% count(word) %>% with(wordcloud(word, n, max.words=100))


# ___ SENTIMENT ANALYSIS ___

# Retrieving / applying associated sentiments of words using "bing" lexicon
bing_sentiments <- get_sentiments("bing")

tidy_shakespeare %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
  count(sentiment)

# Graphical analysis of gathered sentiment information
tidy_shakespeare %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
  count(sentiment) %>% ggplot(aes(Type, n, fill=sentiment)) + 
  geom_col(position='dodge')

# Contingency table on sentiment information - widening for multi. relationships
ct <- tidy_shakespeare %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
  count(sentiment) %>% pivot_wider(names_from = sentiment, values_from = n) %>%
  column_to_rownames(var = "Type")

# Chi-sq test 
chSq <- chisq.test(ct)
chSq

# Analysis of chi-square test - expected values and implications regarding
# relationships
chSq$observed
chSq$expected
chSq$stdres

# Pivoting to the use of "afinn" lexicon to determine sentiment by word weight
afinn_sentiments <- get_sentiments("afinn")

tidy_shakespeare_sentimental <- tidy_shakespeare %>%
  inner_join(afinn_sentiments) %>% inner_join(bing_sentiments)

tidy_shakespeare_sentimental %>% group_by(Type) %>% 
  summarize(value = mean(value))

tidy_shakespeare_sentimental %>% ggplot(aes(Type, value)) + geom_violin()

data_for_acast <- tidy_shakespeare_sentimental %>% 
  count(word, sentiment, sort = TRUE)

acasted <- data_for_acast %>% acast(word ~ sentiment, value.var = "n",
                                    fill = 0)

acasted %>% comparison.cloud(colors = c("firebrick4", "darkolivegreen4"),
                             max.words = 100)


# ANALYSIS OF SELECT BOOKS: HAMLET AND LOVE'S LABOUR COST
# _______ BY WILLIAM SHAKESPEARE _______

# Creating new book table and adding both books
hamlet <- gutenberg_download(shakespeare_corpus$'Gutenberg ID'[6]) %>% 
  slice(231:n())
loves_labour <- gutenberg_download(shakespeare_corpus$'Gutenberg ID'[21]) %>% 
  slice(256:n())

tidy_hamlet <- hamlet %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% anti_join(eme_stop_words)

tidy_loves_labour <- loves_labour %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% anti_join(eme_stop_words)

tidy_book <- tidy_hamlet
tidy_book <- tidy_book %>% add_row(tidy_loves_labour)


# Sorting words
tidy_book %>% count(word, sort=TRUE)
word_counts <- tidy_book %>% count(word)

# Creation of wordclouds for individual plays and both combined
tidy_hamlet %>% count(word) %>% with(wordcloud(word, n, max.words=100))
tidy_loves_labour %>% count(word) %>% with(wordcloud(word, n, max.words=100))
tidy_book %>% count(word) %>% with(wordcloud(word, n, max.words=100))

# OBSERVATIONS
# Some key words in Hamlet: hamlet, lord, queen, king, ham. The importance of 
# these words seems to be rooted in the presence of certain main or vital 
# characters in the play for these names to appear as frequent as they do.

# Some key words in Love's Labour's Lost: boyet, berowne, princess, king,
# france, costard. The frequencies of these words imply the vitality of various
# characters whose names are indicated to appear frequently, such as Boyet
# Berowne. Other words listed appear to imply the climate of the government and
# of the time this play was written.

# Some key words in the cumulative list of words from both plays: king, queen
# love, queen, sir. It is implied that a royal government is at play in
# both plays, and that the element of love plays a vital role in both cases.


# Analyzing trends in sentiment
tidy_hamlet <- tidy_hamlet %>%
  inner_join(shakespeare_corpus, by=c('gutenberg_id'="Gutenberg ID"))

tidy_loves_labour <- tidy_loves_labour %>%
  inner_join(shakespeare_corpus, by=c('gutenberg_id'="Gutenberg ID"))

# Graphical analysis
tidy_hamlet %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
  count(sentiment) %>% ggplot(aes(Type, n, fill=sentiment)) + 
  geom_col(position='dodge')

tidy_loves_labour %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
  count(sentiment) %>% ggplot(aes(Type, n, fill=sentiment)) + 
  geom_col(position='dodge')

# Contingency table on sentiment information - widening for multi. relationships
new_ct <- tidy_hamlet %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
  count(sentiment) %>% pivot_wider(names_from = sentiment, values_from = n) %>%
  column_to_rownames(var = "Type")

# Chi-sq test 
chSq <- chisq.test(new_ct)
chSq

# Analysis of chi-square test - expected values and implications regarding
# relationships
chSq$observed
chSq$expected
chSq$stdres

# Pivoting to the use of "afinn" lexicon to determine sentiment by word weight
afinn_sentiments <- get_sentiments("afinn")

tidy_book_sentimental <- tidy_book %>%
  inner_join(afinn_sentiments) %>% inner_join(bing_sentiments)

tidy_book_sentimental %>% group_by(Type) %>% 
  summarize(value = mean(value))

tidy_book_sentimental %>% ggplot(aes(Type, value)) + geom_violin()

new_acast_data <- tidy_book_sentimental %>% 
  count(word, sentiment, sort = TRUE)

new_acasted <- new_acast_data %>% acast(word ~ sentiment, value.var = "n",
                                    fill = 0)

new_acasted %>% comparison.cloud(colors = c("firebrick4", "darkolivegreen4"),
                             max.words = 100)
