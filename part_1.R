# part 1 - acoustics ####

# load packages ####

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(warbleR)

# check recordings ####

blackbird_songs <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:song len:5-25', download = FALSE)

blackbird_alarm <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:alarm len:5-25', download = FALSE)

# map the recordings ####

map_xc(blackbird_songs, leaflet.map = TRUE)

# create subfolder to store song calls and alarm calls ####

dir.create(file.path("blackbird_songs"))
dir.create(file.path("blackbird_alarm"))

# download the files into the folders ####

query_xc(X = blackbird_songs, path="blackbird_songs")
query_xc(X = blackbird_alarm, path="blackbird_alarm")

# rename the files for future ease in analysis ####

library(stringr)

old_files <- list.files("blackbird_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# minor change for alarm calls ####

old_files <- list.files("blackbird_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# now files are nenamed, they need to be copied into a new subfolder called blackbird_audio ####

dir.create(file.path("blackbird_audio"))
file.copy(from=paste0("blackbird_songs/",list.files("blackbird_songs")),
          to="blackbird_audio")
file.copy(from=paste0("blackbird_alarm/",list.files("blackbird_alarm")),
          to="blackbird_audio")

# convert the files to .wav from .mp3 and simultaneously delete the old files ####

mp32wav(path="blackbird_audio", dest.path="blackbird_audio")
unwanted_mp3 <- dir(path="blackbird_audio", pattern="*.mp3")
file.remove(paste0("blackbird_audio/", unwanted_mp3))

# create oscillogram and spectrogram ####

blackbird_wav <- readWave("blackbird_audio/Turdusmerula-song_243908.wav")

blackbird_wav

# plot oscillo ####

oscillo(blackbird_wav)

oscillo(blackbird_wav, from = 0.59, to = 0.60)

# spectrogram ####

SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-song_243908.wav",
                  Colors = "Colors")

# simplifying the data through feature extraction using mel-frequency ceptral coefficient ####

blackbird_mfcc <- MFCCFunction(input.dir = "blackbird_audio",
                               max.freq=7000)

dim(blackbird_mfcc)

# maximum frequency changed from 2000hz to 7000hz as the range of calls are very high ####

# further the analysis by doing a PCA of the data set ####

# source r script ####

source("NES.8010.R")

library(vegan)

blackbird_pca <- ordi_pca(blackbird_mfcc[, -1], scale=TRUE)

summary(blackbird_pca)

blackbird_sco <- ordi_scores(blackbird_pca, display="sites")
blackbird_sco <- mutate(blackbird_sco, group_code = blackbird_mfcc$Class)

ggplot(blackbird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 

# its evident that song calls being situated in the lowe right whilst the alarm calls are on the upper left ####


