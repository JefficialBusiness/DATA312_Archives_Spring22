rm(list=ls())
gc()
library(tidyverse)
library(tabr) 
library(audio) 
library(GENEAread)

bach<-read_midi('bach_846_format0.midi')
edwin_improv<-read_midi('improv.midi')
edwin_improv2<-read_midi('improv2.midi')


songs<-bind_rows(mutate(bach,name='bach'), 
                 mutate(edwin_improv,name='edwin_improv'), 
                 mutate(edwin_improv2,name='edwin_improv2'))

songs_notes <- songs %>% filter(!is.na(pitch)) %>% mutate(as_music_df(pitch),
                                                          end_time=time+length)

song_lengths <- songs_notes %>% group_by(name) %>% summarize(total_time=max(end_time))

num_time_blocks<-25 
songs_notes <- songs_notes %>%
  inner_join(song_lengths) %>% mutate(time_block=floor(num_time_blocks*time/total_time))


note_starts1 <- songs_notes %>% group_by(name,time_block) %>% arrange(time,.by_group=TRUE)

note_starts2 <- note_starts1 %>% mutate(num_within_block=row_number()) %>% mutate(time=time-min(time))

note_starts3 <- note_starts2 %>% select(name,time,time_block,num_within_block)

note_starts4 <- note_starts3 %>% pivot_wider(names_from=num_within_block,values_from=time) %>%
  ungroup() # Since we are now done with the time_block groups... they're now rows!

note_starts <- note_starts4 %>% select(name,`2`:`5`)

pca_note_starts_bach <- note_starts %>% filter(name == 'bach') %>%
  select(-name ) %>%
  prcomp()

pca_note_starts_bach$x %>%
  as_tibble() %>% ggplot(aes(x=PC1,y=PC2)) + geom_count()

pca_note_starts_edwin <- note_starts %>% filter(name == 'edwin_improv') %>%
  select(-name ) %>%
  prcomp()

pca_note_starts_edwin$x %>%
  as_tibble() %>% ggplot(aes(x=PC1,y=PC2)) + geom_count()

