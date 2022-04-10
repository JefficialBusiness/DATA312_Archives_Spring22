# HW3B
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
duodecaphonic <- read_midi('justin_rubin_lyric.midi')

# Retrieving summaries of types of data frames in all 4 files
count(bach, event, name = 'bach') %>% 
  full_join(count(edwin_improv, event, name  = 'edwin_improv')) %>%
  full_join(count(edwin_improv2, event, name  = 'edwin_improv2')) %>%
  full_join(count(duodecaphonic, event, name = 'duodecaphonic'))

# Restructuring data for increased versatility in analysis
songs <- bind_rows(mutate(bach, name = 'bach'),
                   mutate(edwin_improv, name = 'edwin_improv'),
                   mutate(edwin_improv2, name = 'edwin_improv2'),
                   mutate(duodecaphonic, name = 'duodecaphonic'))

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

songs_notes%>%filter(name=='duodecaphonic') %>%
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


# SHIFTING TO MIDI FILES OF CHOICE
# File 1: https://musescore.com/user/22574601/scores/6544221
# File 2: https://bitmidi.com/queen-bohemian-rhapsody-mid

# The first file, "Gotoubun no Kimochi", is an opening from the first season of 
# one of my favorite anime, The Quintessential Quintuplets.
# The second file is a version of Queen's "Bohemian Rhapsody"

bohemian <- read_midi('Queen - "Bohemian Rhapsody".midi')
gotoubun <- read_midi('gotoubun_no_kimochi.midi')

# Observations: Both ""Bohemian Rhapsody"" and "Gotoubun no Kimochi" contain
# a wide variety of events, several of them being indicative that both are
# recordings, rather than composures of sheet music. Specifically, in addition
# to a presence of events such as changes in time and key signatures, programs, 
# tempo, and notes, there are several controller-related events as well.

# "Gotoubun no Kimochi" has several "MIDI port" events that further imply that
# it was a recording from a device that communicated data through a MIDI port.
# "Bohemian Rhapsody" does not have this. "Gotoubun no Kimochi" also has "Key
# Signature" events that permit my understanding that this piece is intended
# to be in E major. No such event is present in "Bohemian Rhapsody", perhaps
# because it is not specified.

my_songs <- bind_rows(mutate(bohemian, name = 'bohemian'),
                   mutate(gotoubun, name = 'gotoubun'))

# Note length distribution analysis
my_songs %>% group_by(name) %>% ggplot(aes(x = name, y = length)) +
  geom_boxplot()

# Observation: Gotoubun no Kimochi's note length distribution seems rather even
# with exactly 6 outliers. The IQR indicates that notes are generally of a
# length beneath 500. The minimum and maximum lengths are generally of equal
# distance from the 25th and 75th percentile on the IQR, respectively. The
# highest outlier note is above a note length of 2000. The reason for this may
# be because it is a rather fast-paced song, relatively. Upon listening to it
# there are few evident long notes.

# On the other hand, "Bohemian Rhapsody"'s note distribution is not as even. 
# There is a wide variety of outliers, including a note with a length of nearly 
# 6000. The IQR of note lengths is quite low, however. The positions of the 
# outlier notes are not equal in distance, and are rather sporadic. The
# distribution might be consistent with the inconsistent pace of the song. There
# are "slower" moments and there are "faster" ones.


my_songs_notes <- my_songs %>% filter(!is.na(pitch)) %>%
  mutate(as_music_df(pitch),
         end_time = time + length)

# Pitch vs. velocity analysis
my_songs_notes %>% group_by(name) %>% 
  ggplot(aes(x = freq, y = velocity, color = length)) + 
  geom_point() + facet_wrap(~name)

# Observation: It is not clear whether either piece has a melody, though some
# distinction can be seen for "Bohemian Rhapsody". This is not the case at all 
# for "Gotoubun no Kimochi", which has no velocity variation whatsoever. 
# Observed also is the singular outlier note in this plot, with respect to
# length. This can be seen as a result of coloring the plots to reflect note 
# length.


# Plotting note frequency as a function of time
my_songs_notes %>% filter(name=='gotoubun') %>% 
  ggplot(aes(xmin=time,xmax=end_time,y=freq,color=velocity)) +
  geom_linerange() +
  scale_y_log10()

my_songs_notes %>% filter(name=='bohemian') %>% 
  ggplot(aes(xmin=time,xmax=end_time,y=freq,color=velocity)) +
  geom_linerange() +
  scale_y_log10()

# Observation: With some effort, it is possible to separate harmony from
# melody in Gotoubun no Kimochi, as it can now be seen a more definitive
# distribution of frequency. The harmony and melody are much more difficult to
# make out in "Bohemian Rhapsody" with many of the notes clustered together.

# Upon listening to "Bohemian Rhapsody", there is no foundation that can be
# made out. The range of frequency in this piece is not as vast.

# "Gotoubun no Kimochi" has a foundation/harmony, or a series of "deeper" notes
# that can be heard in the recording.

# Checking Keys
my_songs_notes %>% filter(name=='gotoubun') %>% 
  mutate(keycheck = map(pitch, ~is_diatonic(.x, key = 'e_m'))) %>%
  count(keycheck == TRUE)

# Percentage True: 1653 / (763 + 1653) = 0.68418874172 = roughly 68%

my_songs_notes %>% filter(name=='bohemian') %>% 
  mutate(keycheck = map(pitch, ~is_diatonic(.x, key = 'c_'))) %>%
  count(keycheck == TRUE)

# Percentage True: 2770 / (3155 + 2770) = 0.46751054852 = roughly 47%
