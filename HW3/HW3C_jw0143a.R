# HW3C
# Jeffrey Williams
# Dr. Robinson
# April 04, 2022

# Cleaning workspace and loading libraries
rm(list=ls())
gc()

library(tidyverse)
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

