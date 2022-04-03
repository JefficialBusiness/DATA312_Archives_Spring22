
# Load Libraries
library(tidyverse)
library(gutenbergr)
library(tidytext)

dickens_list <- read_csv('dickens_gutenberg.csv')
mirror <- 'http://gutenberg.readingroo.ms/'
book <- gutenberg_download(dickens_list$'Gutenberg ID'[1], mirror = mirror)

for(i in 2:nrow(dickens_list)) {
  book <- book %>% add_row(gutenberg_download(dickens_list$`Gutenberg ID`[i],
                                              mirror = mirror))
}

tidy_dickens <- book %>% unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE) %>%
  inner_join(dickens_list, by = c(gutenberg_id = "Gutenberg ID"))

total_words <- tidy_dickens %>% group_by(Name) %>% summarize(total=sum(n))

# Retrieving / applying associated sentiments of words using "bing" lexicon
bing_sentiments <- get_sentiments("bing")

tidy_dickens <- tidy_dickens %>% inner_join(bing_sentiments)

# Graphical analysis of gathered sentiment information
tidy_dickens %>% inner_join(bing_sentiments) %>% group_by(Name) %>%
  count(sentiment) %>% ggplot(aes(Name, n, fill=sentiment)) + 
  geom_col(position='dodge')

# Contingency table on sentiment information - widening for multi. relationships
ct <- tidy_dickens %>% inner_join(bing_sentiments) %>% group_by(Type) %>%
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

tidy_dickens_sent <- tidy_dickens %>%
  inner_join(afinn_sentiments) %>% inner_join(bing_sentiments)

tidy_dickens_sent %>% group_by(Type) %>% 
  summarize(value = mean(value))

tidy_dickens_sent %>% ggplot(aes(Type, value)) + geom_violin()

data_for_acast <- tidy_dickens_sent %>% 
  count(word, sentiment, sort = TRUE)

acasted <- data_for_acast %>% acast(word ~ sentiment, value.var = "n",
                                    fill = 0)

acasted %>% comparison.cloud(colors = c("firebrick4", "darkolivegreen4"),
                             max.words = 100)