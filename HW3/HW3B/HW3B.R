# HW3A
# Jeffrey Williams
# Dr. Robinson
# Friday, March 18, 2022

# Cleaning workspace, invoking libraries
rm(list=ls())
gc()

library(tidyverse)
library(tabr)
library(tuneR)

# Retrieving each MIDI file, initializing reference variables
bach <- read_midi('bach_846_format0.midi')
edwin_improv <- read_midi('improv.midi')
edwin_improv2 <- read_midi('improv2.midi')
duodecatonic <- read_midi('justin_rubin_lyric.midi')

# Retrieving summaries of types of data frames in all 4 files
count(bach, event, name = 'bach') %>% 
  full_join(count(edwin_improv, event, name  = 'edwin_improv')) %>%
  full_join(count(edwin_improv2, event, name  = 'edwin_improv2')) %>%
  full_join(count(duodecatonic, event, name = 'duodecatonic'))

# Restructuring data for increased versatility in analysis
songs <- bind_rows(mutate(bach, name = 'bach'),
                   mutate(edwin_improv, name = 'edwin_improv'),
                   mutate(edwin_improv2, name = 'edwin_improv2'),
                   mutate(duodecatonic, name = 'duodecatonic'))

# Explorative Analysis using more versatile tidyverse table
songs %>% group_by(name) %>% count(event) %>% 
  pivot_wider(names_from = name, values_from = n)

songs %>% group_by(name) %>% count(channel) %>% 
  pivot_wider(names_from = name, values_from = n)

songs %>% group_by(name) %>% count(track) %>% 
  pivot_wider(names_from = name, values_from = n)

songs %>% group_by(name) %>% count(velocity) %>% 
  pivot_wider(names_from = name, values_from = n)

songs %>% group_by(name) %>% count(type) %>% 
  pivot_wider(names_from = name, values_from = n)

# Determining connotations of param1 & param2 through graphical analysis
songs %>% group_by(name) %>% ggplot(aes(x = name, y = parameter1)) + 
  geom_violin()


songs %>% group_by(name) %>% ggplot(aes(x = pitch, y = parameter2)) + 
  geom_point()

songs %>% group_by(name) %>% ggplot(aes(x = name, y = parameter2)) + 
  geom_violin()

# Cont. connotation investigation: Note frequency search, excluding all N/A 
# instances
songs %>% filter(!is.na(pitch)) %>% mutate(as_music_df(pitch)) %>%
  ggplot(aes(freq, parameter1)) + geom_point()

songs %>% filter(!is.na(pitch)) %>% mutate(as_music_df(pitch)) %>%
  ggplot(aes(freq, parameter1)) + geom_point() + scale_x_log10()

# Understanding parameter2
songs %>% filter(!is.na(pitch)) %>% ggplot(aes(x = velocity, 
                                               y = parameter2)) + geom_point()

# Analyzing velocity / volume distributions in each piece
songs %>% group_by(name) %>% ggplot(aes(x = name, y = velocity)) +
  geom_boxplot()

# Analyzing note length distributions
songs %>% group_by(name) %>% ggplot(aes(x = name, y = length)) +
  geom_boxplot()

songs %>% group_by(name) %>% count(length) %>% 
  ggplot(aes(x = length, y = n, color = name)) + geom_point() + 
  scale_x_log10() +
  scale_y_log10()

# Analysis of note off/on statuses
note_off_times <- edwin_improv2 %>% filter(event == 'Note Off') %>% select(time)

note_on_times_plus_length <- edwin_improv2 %>% filter(event == 'Note On') %>%
  mutate(note_end = time + length) %>%
  select(note_end) %>%
  arrange(note_end)

all(note_off_times == note_on_times_plus_length) # Are these the same? Yes.

# Analysis of note velocities and frequencies in all four pieces
songs_notes <- songs %>% filter(!is.na(pitch)) %>%
  mutate(as_music_df(pitch),
         end_time = time + length)

songs_notes %>% group_by(name) %>% 
  ggplot(aes(x = freq, y = velocity, color = length)) + 
  geom_point() + facet_wrap(~name)

# Plotting individual songs
songs_notes %>% filter(name == 'bach') %>%
  ggplot(aes(xmin = time, xmax = end_time, y = freq, color = velocity)) +
  geom_linerange() + scale_y_log10()

songs_notes %>% filter(name == 'edwin_improv') %>%
  ggplot(aes(xmin = time, xmax = end_time, y = freq, color = velocity)) +
  geom_linerange() + scale_y_log10()

songs_notes %>% filter(name == 'edwin_improv2') %>%
  ggplot(aes(xmin = time, xmax = end_time, y = freq, color = velocity)) +
  geom_linerange() + scale_y_log10()

songs_notes%>%filter(name=='duodecatonic') %>%
  ggplot(aes(xmin=time,xmax=end_time,y=freq,color=velocity)) +
  geom_linerange() + scale_y_log10()

# Determining keys for improvised pieces
edwin_improv2 %>% filter(event == 'Note On') %>% select(pitch) %>%
  map(~is_diatonic(.x, key = 'c'))

edwin_improv %>% filter(event == 'Note On') %>% select(pitch) %>%
  map(~is_diatonic(.x, key = 'c'))

# Splitting notes and harmony in edwin_improv
edwin_improv_notes <- songs_notes %>% filter(name == 'edwin_improv')
edwin_improv_melody <- edwin_improv_notes %>% filter(freq > 250)
edwin_improv_harmony <- edwin_improv_notes %>% filter(freq < 250)

edwin_improv_harmony %>%
  ggplot(aes(xmin = time, xmax = end_time, y = freq, fill = velocity)) + 
  geom_linerange()

# Checking harmony and melody
edwin_improv_melody %>% select(pitch) %>% 
  mutate(keycheck = map(pitch, ~is_diatonic(.x, key = 'c'))) %>%
  count(keycheck == TRUE)

edwin_improv_harmony %>% select(pitch) %>% 
  mutate(keycheck = map(pitch, ~is_diatonic(.x, key = 'c'))) %>%
  count(keycheck == TRUE)

# Attempting 
ei <- edwin_improv_melody

for (k in keys()) {
  ei <- ei %>% 
    mutate(!!k:=map(pitch, ~is_diatonic(.x, key = k)))
  
}

ei <- ei %>% pivot_longer(cols = keys(), names_to = 'key')

ei %>% group_by(key) %>%
  filter(value == TRUE) %>%
  count(value) %>%
  ggplot(aes(reorder(key, desc(n)), n)) + geom_col()

# SHIFTING TO MIDI FILES OF CHOICE
# File 1: https://musescore.com/user/22574601/scores/6544221
# File 2: https://bitmidi.com/queen-bohemian-rhapsody-mid

# The first file is an opening from the first season of one of my favorite 
# anime, The Quintessential Quintuplets.
# The second file is a version of Queen's "Bohemian Rhapsody"

bohemian <- read_midi('Queen - Bohemian Rhapsody.midi')
gotoubun <- read_midi('gotoubun_no_kimochi.midi')

# Observations: Both "Bohemian Rhapsody" and "Gotoubun no Kimochi" contain
# a wide variety of events, several of them being indicative that both are
# recordings, rather than composures of sheet music. Specifically, in addition
# to a presence of events such as changes in time and key signatures, programs, 

my_songs <- bind_rows(mutate(bach, name = 'bohemian'),
                   mutate(edwin_improv, name = 'gotoubun'))



my_songs %>% group_by(name) %>% ggplot(aes(x = name, y = length)) +
  geom_boxplot()

my_songs %>% group_by(name) %>% count(length) %>% 
  ggplot(aes(x = length, y = n, color = name)) + geom_point() + 
  scale_x_log10() +
  scale_y_log10()


my_songs_notes <- my_songs %>% filter(!is.na(pitch)) %>%
  mutate(as_music_df(pitch),
         end_time = time + length)

my_songs_notes %>% group_by(name) %>% 
  ggplot(aes(x = freq, y = velocity, color = length)) + 
  geom_point() + facet_wrap(~name)

my_songs_notes %>% filter(name=='gotoubun') %>% 
  ggplot(aes(xmin=time,xmax=end_time,y=freq,color=velocity)) +
  geom_linerange() +
  scale_y_log10()

# Checking Keys
gotoubun %>% filter(event == 'Note On') %>% select(pitch) %>%
  map(~is_diatonic(.x, key = 'c'))

