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
time_blocks <- 20

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


# -- ANALYSIS OF MUSIC OF MY CHOICE (MIDI OPTION) --

# The MIDI files I have selected are as follows:

# "Unravel": Opening from anime, Tokyo Ghoul
# "Traitor's Requiem": Second opening from anime, JoJo's Bizarre Adventure:
# Vento Aureo

unravel <- read_midi("unravel_tokyo_ghoul.mid")
traitor_requiem <- read_midi("Traitors Requiem (Theishter).midi")
gurenge <- read_midi("Gurenge_full.mid")

my_songs <- bind_rows(mutate(unravel, name = 'unravel'),
                   mutate(traitor_requiem, name = 'traitor_requiem'),
                   mutate(gurenge, name = 'gurenge'))

my_songs_notes <- my_songs %>% filter(!is.na(pitch)) %>%
  mutate(as_music_df(pitch), end_time = time + length)

my_songs_lengths <- my_songs_notes %>% group_by(name) %>%
  summarize(total_time = max(end_time))

my_total_notes <- my_songs %>% group_by(name) %>% 
  summarize(total_notes = sum(!is.na(pitch)))

my_total_notes
my_songs_lengths

# From above table: "Unravel" contains 3630 notes and has length of 251645; 
# "Traitor's Requiem" contains 1772 notes and has length of 221760.

my_songs_notes <- my_songs_notes %>% inner_join(my_songs_lengths) %>%
  inner_join(my_total_notes)

new_time_blocks <- 20

my_songs_notes_blocks <- my_songs_notes %>% inner_join(my_songs_lengths) %>%
  mutate(time_block = floor(new_time_blocks * time / total_time))

my_songs_notes_blocks %>% group_by(name) %>% count(time_block) %>%
  pivot_wider(names_from = name, values_from = n)

# Observations:
# Using 10 time blocks:

# In "Unravel", the table demonstrates the changes in beat and tempo,
# particularly by the major jumps in note counts from blocks 0 to 1, 2 to 3,
# 5 to 6, and 8 to 9.

# Using 20 time blocks:

# "Unravel" shows several major changes, particularly from 0 to 1, 1 to 2, 9
# to 10, 17 to 18.

# With this number of time blocks, "Traitor's Requiem" shows more subtle changes
# in note counts.

# Using 50 time blocks:

# Using 100 time blocks:


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

my_songs_notes_blocks %>% filter(name == 'gurenge') %>% 
  count(name, note, time_block) %>%
  ggplot(aes(time_block, n, group = note, color =  name)) + geom_line() +
  facet_wrap(~note)


# Normalizing / Analysis of note use over time
my_songs_notes_blocks %>% filter(name == 'unravel') %>% 
  add_count(name, time_block, name = 'notes_per_block') %>% 
  add_count(note, time_block) %>%
  ggplot(aes(time_block, n / notes_per_block, group = note)) +
  geom_line() + facet_wrap(~note)

my_songs_notes_blocks %>% filter(name == 'traitor_requiem') %>% 
  add_count(name, time_block, name = 'notes_per_block') %>% 
  add_count(note, time_block) %>%
  ggplot(aes(time_block, n / notes_per_block, group = note)) +
  geom_line() + facet_wrap(~note)



# Observations: It appears in "Unravel" that note length is roughly even with
# a few spikes after trying with several note block counts, including 10, 20,
# and 50.

# In "Traitor's Requiem", it appears at 10 and 20 note block counts that a_
# regresses in usage throughout the progression of the piece while e 
# increases in use significantly. The latter notes remain the same.

# However, when increasing the note block count to 50, a_'s usage appears more
# consitent throughout the piece with one dip in the middle (circa block 25).
# e's usage is still portrayed as increasing as the piece progresses.

# Such a case remains when switching the note blocks frequency to 100, however,
# it is evident that e's increase in frequency occurs closer to the end of the
# piece, while usage remains generally consistent throughout most of it. It can
# also be seen rather subtle decreases in frequency for notes g, g_, and perhaps
# e_.

# Checking for tempo change
my_songs_notes_blocks %>% count(name, length, time_block) %>%
  ggplot(aes(time_block, n, color = name)) + geom_point()

# When playing the files in GarageBand, it is indicated that there is a tempo
# change right after what would be the "opening" verse in the original song.
# Particularly it rises from 137 to 140, and fluctuates throughout. There is a 
# rather sporadic 
