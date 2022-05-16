
# Load Libraries
library(tidyverse)
library(gutenbergr)
library(tidytext)
library(reshape2)
library(wordcloud)

dickens_list <- read_csv('dickens_gutenberg.csv')
mirror <- 'http://gutenberg.readingroo.ms/'
book <- gutenberg_download(dickens_list$`Gutenberg ID`[1],
                           mirror = mirror) %>% slice(dickens_list$start[1]:n())

for(i in 2:nrow(dickens_list)) {
  book <- book %>% add_row(gutenberg_download(dickens_list$`Gutenberg ID`[i],
                                              mirror = mirror) %>% slice(dickens_list$start[i]:n()))
}

tidy_dickens <- book %>% unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE) %>%
  inner_join(dickens_list, by = c(gutenberg_id = "Gutenberg ID")) 

total_words <- tidy_dickens %>% group_by(Name) %>% summarize(total=sum(n))

# Sentiment Analysis - Bing
bing_sentiments <- get_sentiments("bing")

tidy_dickens <- tidy_dickens %>% inner_join(bing_sentiments) %>% group_by(Name)

# Graphs
tidy_dickens %>%
  count(sentiment) %>% ggplot(aes(Name, n, fill=sentiment)) + 
  geom_col(position='dodge')  + 
  labs(title="Dickens Sentiment Distributions - Bing Lexicon",
       x = "Work",
       y = "Word Count")

# Contingency table
ct <- tidy_dickens %>% inner_join(bing_sentiments) %>% group_by(Name) %>%
  count(sentiment) %>% pivot_wider(names_from = sentiment, values_from = n) %>%
  column_to_rownames(var = "Name")

# Chi-sq test 
chSq <- chisq.test(ct)
chSq

chSq$observed
chSq$expected
chSq$stdres

# Sentiment Analysis - afinn
afinn_sentiments <- get_sentiments("afinn")

tidy_dickens_afinn <- tidy_dickens %>%
  inner_join(afinn_sentiments) %>% inner_join(bing_sentiments)

tidy_dickens_afinn %>% group_by(Name) %>% 
  summarize(value = mean(value))

tidy_dickens_afinn %>% ggplot(aes(Name, value)) + geom_violin()

new_ct <- tidy_dickens_afinn %>% group_by(Name) %>%
  count(sentiment) %>% pivot_wider(names_from = sentiment, values_from = n) %>%
  column_to_rownames(var = "Name")


chSq2 <- chisq.test(new_ct)
chSq2

chSq2$observed
chSq2$expected
chSq2$stdres

data_for_acast <- tidy_dickens_afinn %>% 
  count(word, sentiment, sort = TRUE)

acasted <- data_for_acast %>% acast(word ~ sentiment, value.var = "n",
                                    fill = 0)

acasted %>% comparison.cloud(colors = c("firebrick4", "darkolivegreen4"),
                             max.words = 100)

