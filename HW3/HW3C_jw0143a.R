# HW3C
# Jeffrey Williams
# Dr. Robinson
# April 04, 2022

# Cleaning workspace and loading libraries
rm(list=ls())
gc()

library(tidyverse)
library(tuneR)
library(tabr)
library(audio)
library(GENEAread)

# Loading MIDI files
bach <- read_midi('bach_846_format0.midi')
edwin_improv <- read_midi('improv.midi')     
edwin_improv2 <- read_midi('improv2.midi')
duodecaphonic <- read_midi('justin_rubin_lyric.midi')

# Tidying/consolidating files & gathering specific data
songs <- bind_rows(mutate(bach, name = 'bach'),
                   mutate(edwin_improv, name = 'edwin_improv'),
                   mutate(edwin_improv2, name = 'edwin_improv2'),
                   mutate(duodecaphonic, name = 'duodecaphonic'))

songs_notes <- songs %>% filter(!is.na(pitch)) %>%
  mutate(as_music_df(pitch),
         end_time = time + length)

song_lengths <- songs_notes %>% group_by(name) %>%
  summarize(total_time = max(end_time))

# Experimentation
songs_notes %>% group_by(name) %>% summarize(max(freq))
songs_notes %>% group_by(name) %>% summarize(max(velocity))

# Add note lengths to table
songs_notes <- songs_notes %>% inner_join(song_lengths)

# Dividing notes by time for various analysis
time_blocks <- 30

songs_notes_blocks <- songs_notes %>% 
  mutate(time_block = floor(time_blocks * time/total_time))

songs_notes_blocks %>% group_by(name) %>% count(time_block) %>%
  pivot_wider(names_from = name, values_from = n)

songs_notes_blocks %>% filter(name == 'bach') %>% 
  ggplot(aes(xmin = time, xmax = end_time, y = freq, color = channel)) + 
  geom_linerange()

songs_notes_blocks %>% count(name, note, time_block) %>%
  ggplot(aes(time_block, n, group = note, color =  name)) + geom_line() +
  facet_wrap(~note)

songs_notes_blocks %>% filter(name == 'bach') %>% 
  count(name, note, time_block) %>%
  ggplot(aes(time_block, n, group = note, color =  name)) + geom_line() +
  facet_wrap(~note)

# Normalizing 
songs_notes_blocks %>% filter(name == 'bach') %>% 
  add_count(name, time_block, name = 'notes_per_block') %>% 
  add_count(note, time_block) %>%
  ggplot(aes(time_block, n / notes_per_block, group = note)) +
  geom_line() + facet_wrap(~note)

songs_notes_blocks %>% count(name, length, time_block) %>%
  ggplot(aes(time_block, n, color = name)) + geom_point()

# Pivoting to analysis of WAV files
wavfile <- load.wave("lightly_row_violin.wav")

wav <- tibble(sample = wavfile) %>% mutate(time = row_number() / wavfile$rate) 

wav %>% slice(seq(1, n(), by = 100)) %>% ggplot(aes(time, sample)) +
  geom_line() 

wav_fft <- tibble(sample_fft = abs(fft(wav$sample))) %>% 
  mutate(freq = row_number() / n() * wavfile$rate)

wav_fft %>% slice(seq(1, n(), by = 1000)) %>% ggplot(aes(freq, sample_fft)) +
  geom_line()

notes <- c("a", "b", "c", "d", "e" , "f", "g", "a'", "b'", "c'", "d'", "e'", 
           "f'", "g'", "a''", "b''", "c''", "d''", "e''", "f''", "g''")

freqs <- as_music_df(notes)

wav_fft %>% filter(freq < 1000) %>% ggplot(aes(freq, sample_fft)) +
  geom_line() +
  scale_x_continuous(name = 'Note', breaks = freqs$freq, labels = notes)

wav_stft <- stft(wav$sample, freq = wavfile$rate, win = 0.5, inc = 0.1)

frequency_table <- tibble(frequency = wav_stft$frequency) %>% 
  mutate(index = paste("V", row_number(), sep = ''))

wav_stft_tidy <- as_tibble(wav_stft$values, name_repair = 'minimal') %>%
  mutate(time = wav_stft$times) %>% pivot_longer(cols = starts_with('V')) %>%
  left_join(frequency_table, by = c('name' = 'index')) %>%
  mutate(name = NULL)

wav_stft_tidy %>% filter(frequency < 1000) %>% ggplot(aes(time, frequency)) +
  geom_raster(aes(fill = value)) + 
  scale_fill_gradient(low = 'blue', high = 'red') +
  scale_y_continuous(name = 'Note', breaks = freqs$freq, labels = notes)

wav_stft_tidy %>% filter(frequency > 256, frequency < 500) %>% 
  group_by(time) %>% filter(value == max(value)) %>% filter(value > 10)


# -- ANALYSIS OF MIDI FILES OF CHOICE --

# The MIDI files I have selected are as follows:

# "Unravel": Opening from another anime I watched, Tokyo Ghoul
# 

unravel <- read_midi("unravel_tokyo_ghoul.mid")
traitor_requiem <- read_midi("Traitors Requiem (Theishter).midi")

my_songs <- bind_rows(mutate(unravel, name = 'unravel'),
                   mutate(traitor_requiem, name = 'traitor_requiem'))

my_songs_notes <- my_songs %>% filter(!is.na(pitch)) %>%
  mutate(as_music_df(pitch), end_time = time + length)

my_songs_lengths <- my_songs_notes %>% group_by(name) %>%
  summarize(total_time = max(end_time))

my_songs_notes %>% group_by(name) %>% summarize(max(freq))

my_songs_notes <- my_songs_notes %>% inner_join(my_songs_lengths)

new_time_blocks <- 40

my_songs_notes_blocks <- my_songs_notes %>% inner_join(my_songs_lengths) %>%
  mutate(time_block = floor(new_time_blocks * time / total_time))

my_songs_notes_blocks %>% group_by(name) %>% count(time_block) %>%
  pivot_wider(names_from = name, values_from = n)

my_songs_notes_blocks %>% filter(name == 'unravel') %>% 
  ggplot(aes(xmin = time, xmax = end_time, y = freq, color = channel)) + 
  geom_linerange()

my_songs_notes_blocks %>% count(name, note, time_block) %>%
  ggplot(aes(time_block, n, group = note, color =  name)) + geom_line() +
  facet_wrap(~note)

my_songs_notes_blocks %>% filter(name == 'unravel') %>% 
  count(name, note, time_block) %>%
  ggplot(aes(time_block, n, group = note, color =  name)) + geom_line() +
  facet_wrap(~note)

# Normalizing 
my_songs_notes_blocks %>% filter(name == 'unravel') %>% 
  add_count(name, time_block, name = 'notes_per_block') %>% 
  add_count(note, time_block) %>%
  ggplot(aes(time_block, n / notes_per_block, group = note)) +
  geom_line() + facet_wrap(~note)

my_songs_notes_blocks %>% count(name, length, time_block) %>%
  ggplot(aes(time_block, n, color = name)) + geom_point()

