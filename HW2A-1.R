# HW2A
# Jeffrey Williams
# Dr. Robinson
# February 15, 2022

# Project Gutenberg books I have analyzed:
# "The Great Gatsby" - F. Scott Fitzgerald (ID: 64317). HEADER ENDS LINE 32.
# "Don Quixote" - Miguel de Cervantes Saavedra (ID: 996). HEADER ENDS LINE 581.


# Clearing workspace and loading libraries
rm(list=ls())
gc()
library(tidyverse)
library(tidytext)
library(gutenbergr)

# Initializing text files as variables
sonnet27 <- read_lines('sonnet27.txt')

View(sonnet27)

# Converting text files into tables and tidying them
sonnet27_df <- tibble(text=sonnet27)
sonnet27_tidied <- unnest_tokens(sonnet27_df, word, text)

# Observations: It seems as though ?unnest_tokens converted all characters in
# the file to lowercase, as well as eliminated all punctuation.

# Counting words in sonnet27
sonnet27_tidied %>% count(word, sort=TRUE)

# If you use "sort=TRUE", it will

# Stripping stop words from table
sonnet27_tidied %>% anti_join(stop_words)
sonnet27_tidied %>% anti_join(stop_words) %>% count(word, sort=TRUE)

sonnet27_tidied %>% anti_join(stop_words) %>% count(word) %>% 
  mutate(word=reorder(word, n)) %>% filter(n>1) %>% ggplot(aes(n, word)) +
  geom_col()

# Mutate switches the position of the columns, filter excludes all words that
# appear only once, and ggplot + geom_col creates a bar chart of word data.

# REPEATING ABOVE PROCESS WITH sonnet38.txt
sonnet38 <- read_lines('sonnet38.txt')
View(sonnet38)

sonnet38_df <- tibble(text=sonnet38)
sonnet38_tidied <- unnest_tokens(sonnet38_df, word, text)

sonnet38_tidied %>% count(word, sort=TRUE)

sonnet38_tidied %>% anti_join(stop_words)
sonnet38_tidied %>% anti_join(stop_words) %>% count(word, sort=TRUE)

sonnet38_tidied %>% anti_join(stop_words) %>% count(word) %>% 
  mutate(word=reorder(word, n)) %>% filter(n>1) %>% ggplot(aes(n, word)) +
  geom_col()


# PIVOTING TO PROJECT GUTENBERG

# Initializing book variables; removing stop words
book <- gutenberg_download(1511)
temp_book <- book %>% unnest_tokens(word, text) %>% anti_join(stop_words)

# Removing rows
book %>% slice(45:n())

# Tidying the book by removing stop words
tidy_book <- temp_book %>% anti_join(stop_words)

earlystopwords <- tibble(word=c("thee", "thy", "thou", "mine", "hath",
                              "ah", "thyself"))

tidier_book <- tidy_book %>% anti_join(earlystopwords)

tidy_john <- tidier_book

# Pulling Maunder's Book for Analysis
book <- gutenberg_download(35937)
tidy_astronauts <- book %>% slice(75:n()) %>% unnest_tokens(word, text) %>% 
  count(word, sort=TRUE) %>% anti_join(stop_words)

# ANALYSIS: Personally, no relationship has been noticed between this book and
# the previously observed book by Shakesphere.

# Trying another Shakesphere book for Analysis and possible comparison
book <- gutenberg_download(1998)
tidy_other_book <- book %>% slice(216:n()) %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% anti_join(earlystopwords)

# Comparing word frequencies between both documents
tidy_john2 <- mutate(tidy_john, book = "John")
tidy_other2 <- mutate(tidy_other_book, book = "Other")

stacked <- bind_rows(tidy_john2, tidy_other2)

counts <- count(stacked, book, word)

group_by(counts, book)

frequencies <- counts %>% group_by(book) %>% mutate(proportion =
                                                      -log10(n/sum(n)))

frequencies
final_table <- pivot_wider(frequencies, word, names_from = book, values_from =
                             proportion)

final_table
final_table %>% ggplot(aes(x=John, y=Other)) + 
  geom_text(aes(label=word))

# Analysis on book of choice 1: "The Great Gatsby" - F. Scott Fitzgerald
book <- gutenberg_download(64317)

tidy_gatsby <- book %>% slice(32:n()) %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# Analysis on book of choice 2: "Don Quixote" - Miguel de Cervantes Saavedra
book <- gutenberg_download(996)

# In the case of this particular work, which was released in parts throughout
# the early 15th century, early modern English is in use, thus a separate table 
# is required in this case for removal of stop words. The existing table created
# for Shakesphere is sufficient, thus I have simply reused it.

tidy_don_quixote <- book %>% slice(581:n()) %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% anti_join(earlystopwords)

# Comparing two books
tidy_gatsby2 <- mutate(tidy_gatsby, book = "Gatsby")
tidy_don_quixote2 <- mutate(tidy_don_quixote, book = "DonQuixote")

stacked <- bind_rows(tidy_gatsby2, tidy_don_quixote2)

counts <- count(stacked, book, word)

group_by(counts, book)

frequencies <- counts %>% group_by(book) %>% mutate(proportion =
                                                      -log10(n/sum(n)))

# Histogram of top 10 most frequent words from each book
frequencies %>% slice_max(n=10, n) %>% ggplot(aes(y=word, x=n)) + 
  facet_wrap(~book) + geom_col()

# Word Scatterplot
final_table <- pivot_wider(frequencies, word, names_from = book, values_from =
                             proportion)

final_table %>% ggplot(aes(x=Gatsby, y=DonQuixote)) + 
  geom_text(aes(label=word))

# 5 words: didn't, myrtle, don, car, duke
# 1. "didn't" - seems much more prominent in "The Great Gatsby" and not so much
# in "Don Quixote", presumably because contractions were not very popular, if 
# at all during that time for the English lexicon.
# 2. "myrtle" is also seemingly exclusive to "The Great Gatsby", which makes 
# sense because there exists a character by the name of Myrtle Wilson only in 
# that book.
# 3. "don" appears to be quite exclusive to "Don Quixote". There is no such
# name for any characters in "The Great Gatsby".
# 4. "car" also has exclusivity in "The Great Gatsby", which makes sense, as
# it is extremely unlikely that cars existed in the era of "Don Quixote".
# 5. "duke" is exclusive to "Don Quixote", which makes sense, because there was
# a duke and dutchess in "Don Quixote". The "Great Gatsby" takes place in 1920's
# New York, where there does not exist such a government that has either.
