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

song_notes <- songs %>% filter(!is.na(pitch)) %>%
  mutate(as_music_df(pitch),
         end_time = time + length)

song_lengths <- song_notes %>% group_by(name) %>%
  summarize(total_time = max(end_time))

# Experimentation
song_notes %>% group_by(name) %>% summarize(max(freq))
song_notes %>% group_by(name) %>% summarize(max(velocity))

# Add note lengths to table
song_notes <- song_notes %>% inner_join(song_lengths)

# Dividing notes by time for various analysis
time_blocks <- 10

songs_notes_blocks <- song_notes %>% 
  mutate(time_block = floor(time_blocks * time/total_time))

songs_notes_blocks %>% group_by(name) %>% count(time_block) %>%
  pivot_wider(names_from = name, values_from = n)

songs_notes_blocks %>% filter(name == 'bach') %>% 
  ggplot(aes(xmin = time, xmax = end_time, y = freq)) + geom_linerange() +
  facet_wrap(~channel)

songs_notes_blocks %>% count(name, note, time_block) %>%
  ggplot(aes(time_block, n, group = note, color =  name)) + geom_line() +
  facet_wrap(~note)

# Pivoting to analysis of WAV files
wavfile <- load.wave("")