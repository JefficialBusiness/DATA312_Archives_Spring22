# HW3A
# Jeffrey Williams
# Dr. Robinson
# Friday, March 18, 2022

# Cleaning workspace, invoking libraries
rm(list=ls())
gc()

library(tidyverse)
library(tabr)

# Retrieving each MIDI file, initializing reference variables
bach <- read_midi('bach_846_format0.midi')
edwin_improv <- read_midi('improv.midi')
edwin_improv2 <- read_midi('improv2.midi')
duodecatonic <- read_midi('justin_rubin_lyric.midi')

# Retrieving summaries of types of data frames in MIDI files
count(bach, event, name = 'bach') %>% 
  full_join(count(edwin_improv, event, name  = 'edwin_improv')) %>%
  full_join(count(edwin_improv2, event, name  = 'edwin_improv2')) %>%
  full_join(count(duodecatonic, event, name = 'duodecatonic'))


songs <- bind_rows(mutate(bach, name = 'bach'),
                   mutate(edwin_improv, name = 'edwin_improv'),
                   mutate(edwin_improv2, name = 'edwin_improv2'),
                   mutate(duodecatonic, name = 'duodecatonic'))

# Building a comparison table with event data across all four files
songs %>% group_by(name) %>% count(event) %>% 
  pivot_wider(names_from = name, values_from = n)

songs %>% group_by(name) %>% count(channel) %>% 
  pivot_wider(names_from = name, values_from = n)

# gplot(aes(x=name, y=length)) + geom_boxplot()
  

