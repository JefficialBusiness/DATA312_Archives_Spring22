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


shakespeare_corpus <- read_csv('shakespeare_gutenberg.csv')
View(shakespeare_corpus)

eme_stop_words <- tibble(word=c("thee", "thou", "hath", "thy", "thine", "ye",
                                "tis"))

book <- gutenberg_download(shakespeare_corpus$'Gutenberg ID'[1])

book <- book %>% 
  add_row(gutenberg_download(shakespeare_corpus$'Gutenberg ID'[2]))

for(i in 3:nrow(shakespeare_corpus)) {
  book <- book %>% 
    add_row(gutenberg_download(shakespeare_corpus$'Gutenberg ID'[i]))
}

tidy_shakespeare <- book %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% anti_join(eme_stop_words)

tidy_shakespeare <- tidy_shakespeare %>%
  inner_join(shakespeare_corpus, by=c('gutenberg_id'="Gutenberg ID"))

View(tidy_shakespeare)

tidy_shakespeare %>% count(word, sort=TRUE)

# Most common words appear to be lord, king, sir, enter, love, time, speak, mine
# duke, world


word_counts <- tidy_shakespeare %>% count(word)

wordcloud(word_counts$word, word_counts$n, max.words=15)

tidy_shakespeare %>% count(word) %>% with(wordcloud(word, n, max.words=100))

tidy_shakespeare %>% count(word) %>% with(wordcloud(word, n, max.words=100))

tidy_shakespeare %>% filter(gutenberg_id==1118) %>% count(word) %>% 
  with(wordcloud(word, n, max.words=100)) 

bing_sentiments <- get_sentiments("bing")

tidy_shakespeare %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
  count(sentiment)

tidy_shakespeare %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
  count(sentiment) %>% ggplot(aes(x=n, y=Type, fill=sentiment)) + 
  geom_col(position='dodge')

ct <- tidy_shakespeare %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
  count(sentiment) %>% pivot_wider(names_from=c(sentiment), values_from=n)

M <- as.table(rbind(c(10, 20), c(30, 40)))
dimnames(M) <- list(type=c('Tragedy', 'Comedy'), sentiment=c('positive',
                                                             'negative'))

chSq <- chisq.test(M)
chSq

chSq$observed
chSq$expected


