# part 1 - acoustics ####

# load packages ####

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(warbleR)

# check recordings ####

house_sparrow_song <- query_xc(qword = 'Passer domesticus cnt: "Germany" type: song len:5-200', download = FALSE)

house_sparrow_alarm <- query_xc(qword = 'Passer domesticus cnt: "Germany" type: alarm len:5-200', download = FALSE)

blackbird_songs <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:song len:5-25', download = FALSE)

blackbird_alarm <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:alarm len:5-25', download = FALSE)

# map the recordings ####

map_xc(house_sparrow_song, leaflet.map = TRUE)

map_xc(blackbird_songs, leaflet.map = TRUE)

# create subfolder to store song calls and alarm calls ####

dir.create(file.path("blackbird_songs"))
dir.create(file.path("blackbird_alarm"))

dir.create(file.path("housesparrow_songs"))
dir.create(file.path("housesparrow_alarm"))

# download the files into the folders ####

query_xc(X = blackbird_songs, path="blackbird_songs")
query_xc(X = blackbird_alarm, path="blackbird_alarm")

query_xc(X = house_sparrow_song, path="housesparrow_songs")
query_xc(X = house_sparrow_alarm, path="housesparrow_alarm")

# rename the files for future ease in analysis ####

# blackbird song ####

library(stringr)

old_files <- list.files("blackbird_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# house sparrow song ####


old_files <- list.files("housesparrow_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# minor change for alarm calls ####

# blackbird alarm ####

old_files <- list.files("blackbird_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# house sparrow alarm ####

old_files <- list.files("housesparrow_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# now files are nenamed, they need to be copied into a new subfolder called blackbird_audio and house housesparrow_audio ####

dir.create(file.path("blackbird_audio"))
file.copy(from=paste0("blackbird_songs/",list.files("blackbird_songs")),
          to="blackbird_audio")
file.copy(from=paste0("blackbird_alarm/",list.files("blackbird_alarm")),
          to="blackbird_audio")

dir.create(file.path("housesparrow_audio"))
file.copy(from=paste0("housesparrow_songs/",list.files("housesparrow_songs")),
          to="housesparrow_audio")
file.copy(from=paste0("housesparrow_alarm/",list.files("housesparrow_alarm")),
          to="housesparrow_audio")

# convert the files to .wav from .mp3 and simultaneously delete the old files ####

mp32wav(path="blackbird_audio", dest.path="blackbird_audio")
unwanted_mp3 <- dir(path="blackbird_audio", pattern="*.mp3")
file.remove(paste0("blackbird_audio/", unwanted_mp3))

mp32wav(path="housesparrow_audio", dest.path="housesparrow_audio")
unwanted_mp3 <- dir(path="housesparrow_audio", pattern="*.mp3")
file.remove(paste0("housesparrow_audio/", unwanted_mp3))

# create oscillogram and spectrogram ####

blackbird_wav <- readWave("blackbird_audio/Turdusmerula-song_243908.wav")

blackbird_wav

housesparrow_wav <- readWave("housesparrow_audio/Passerdomesticus-song_145522.wav")

housesparrow_wav

# plot oscillo ####

oscillo(blackbird_wav)

oscillo(blackbird_wav, from = 0.59, to = 0.60)

oscillo(housesparrow_wav)

oscillo(housesparrow_wav, from = 0.59, to = 0.60)

# spectrogram ####

SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-song_243908.wav",
                  Colors = "Colors")

SpectrogramSingle(sound.file = "housesparrow_audio/Passerdomesticus-song_145522.wav",
                  Colors = "Colors")

# simplifying the data through feature extraction using mel-frequency ceptral coefficient ####

blackbird_mfcc <- MFCCFunction(input.dir = "blackbird_audio",
                               max.freq=7000)

dim(blackbird_mfcc)

housesparrow_mfcc <- MFCCFunction(input.dir = "housesparrow_audio",
                               max.freq=7000)

dim(housesparrow_mfcc)

# maximum frequency changed from 2000hz to 7000hz as the range of calls are very high ####

# further the analysis by doing a PCA of the data set ####

# source r script ####

source("NES.8010.R")

library(vegan)

# plot blackbird PCA ####

blackbird_pca <- ordi_pca(blackbird_mfcc[, -1], scale=TRUE)

summary(blackbird_pca)

blackbird_sco <- ordi_scores(blackbird_pca, display="sites")
blackbird_sco <- mutate(blackbird_sco, group_code = blackbird_mfcc$Class)

ggplot(blackbird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 

# plot housesparrow PCA ####

housesparrow_pca <- ordi_pca(housesparrow_mfcc[, -1], scale=TRUE)

summary(housesparrow_mfcc)

housesparrow_sco <- ordi_scores(housesparrow_pca, display="sites")
housesparrow_sco <- mutate(housesparrow_sco, group_code = housesparrow_mfcc$Class)

ggplot(housesparrow_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()





