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
# Source: https://musescore.com/kevintran99/scores/5490570
# "Gurenge": Opening from anime, Kimetsu No Yaiba (Demon Slayer)
# Source: https://sheet.host/sheet/V8Wzoq

unravel <- read_midi("unravel_tokyo_ghoul.mid")
gurenge <- read_midi("Gurenge_full.mid")

my_songs <- bind_rows(mutate(unravel, name = 'unravel'),
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
# "Gurenge" contains 3008 notes and has length of 249599.

my_songs_notes <- my_songs_notes %>% inner_join(my_songs_lengths) %>%
  inner_join(my_total_notes)

new_time_blocks <- 100

my_songs_notes_blocks <- my_songs_notes %>% inner_join(my_songs_lengths) %>%
  mutate(time_block = floor(new_time_blocks * time / total_time))

my_songs_notes_blocks %>% group_by(name) %>% count(time_block) %>%
  pivot_wider(names_from = name, values_from = n)

# Observations:
# Using 10 time blocks:

# In "Unravel", the table demonstrates the changes in beat and tempo,
# particularly by the major jumps in note counts from blocks 0 to 1, 2 to 3,
# 5 to 6, and 8 to 9.

# In "Gurenge", the table implies several major differences in note appearance
# frequencies, particularly between blocks 2 and 3, 5 and 6, 6 and 7, 7 and 8.

# Using 20 time blocks:

# "Unravel" shows several major changes, particularly from 0 to 1, 1 to 2, 9
# to 10, 17 to 18.

# With this number of time blocks, Gurenge continues to demonstrate significant
# difference, particularly between 0 to 1, 1 to 2, 4 to 5, 8 to 9, 11 to 12, 12
# to 13, etc.

# Using 50 time blocks:

# Relatively significant differences in note occurrence remain evident for both
# pieces. There are instances where note counts per time block roughly double
# similarly to the previous experiment (20), albeit by smaller margins due to
# less notes per time block.

# Using 100 time blocks:

# With even smaller note counts per time block, the differences become even
# smaller, however there remain few significant jumps (for example, between
# blocks 5 and 6 for "Gurenge" and 3 and 4 for "Unravel".)


my_songs_notes_blocks %>% filter(name == 'unravel') %>% 
  ggplot(aes(xmin = time, xmax = end_time, y = freq, color = channel)) + 
  geom_linerange()

# Attempt at visualizing potential structural changes

my_songs_notes_blocks %>% filter(name == 'gurenge') %>% 
  ggplot(aes(xmin = time, xmax = end_time, y = freq, color = channel)) + 
  geom_linerange()

my_songs_notes_blocks %>% filter(name == 'unravel') %>% 
  ggplot(aes(xmin = time, xmax = end_time, y = freq, color = channel)) + 
  geom_linerange()

my_songs_notes_blocks %>% filter(name == 'gurenge') %>% 
  count(name, note, time_block) %>%
  ggplot(aes(time_block, n, group = note, color =  name)) + geom_line()

my_songs_notes_blocks %>% filter(name == 'unravel') %>% 
  count(name, note, time_block) %>%
  ggplot(aes(time_block, n, group = note, color =  name)) + geom_line()

# Checking note usage

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

my_songs_notes_blocks %>% filter(name == 'gurenge') %>% 
  add_count(name, time_block, name = 'notes_per_block') %>% 
  add_count(note, time_block) %>%
  ggplot(aes(time_block, n / notes_per_block, group = note)) +
  geom_line() + facet_wrap(~note)


# Observations: It appears in "Unravel" that note length is roughly even with
# a few spikes after trying with 10 note block counts. b appears to be a very
# commonly used note, alongside d, f, and g. d becomes more frequently used 
# with f throughout the progression of the song, according to the first test.

# At 50 time blocks, all notes appear to be even more even in usage, with the
# most apparent dips and rises belonging to notes b_, d, f, and g.

# At 100 time blocks, all notes generally appear even in usage, however b_
# appears to experience a dip midway and gradually increase in usage from there
# to the end of the piece. As with f, it appears that there is one point where
# it is used the most frequently.

# In "Gurenge", note usage appears quite con consistent at 10 time blocks, with
# e showing the most interesting behavior, starting at at a high point and
# ending at a high point.

# At 10 time blocks, note usage appears even more evened out. d and e
# appear to be used the most commonly, and perhaps b as well. It appears
# that e_ is not used until roughly the middle of the song, interestingly.

# At 50 time blocks, with the exception of a few spikes, most notably for
# e, note usage appears rather even.

# At 100 time blocks, note usage appears even for most notes. d appears to
# be more frequently used at around roughly the third quarter of the piece,as
# does e. Interestingly, there is little "action" in the line representing e_,
# even at this number of time blocks. There are several long and straight
# sections in the line.

# Checking for tempo change
my_songs_notes_blocks %>% count(name, length, time_block) %>%
  ggplot(aes(time_block, n, color = name)) + geom_point()

# Note: I am not confident that I know how to read this graph, so I'd like to
# discuss it briefly at some point. My best guess by reading this graph is that
# there is a considerable amount of variance in note length for both songs, so
# there are tempo changes on occasion. I strongly suspect this as someone
# familiar with both songs; there are several "slow" and "fast" moments.
