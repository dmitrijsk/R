rm(list = ls())
install.packages("ggplot2")
install.packages("plyr")
install.packages("stringr")
library(ggplot2)
library(plyr)
library(stringr)
setwd("/home/yes_no/Downloads/bakalaurs")
getwd()

emo <- read.csv("emo_2d_coded.csv", header = T, na.strings=c("", "NA"))
emo_words <- read.csv("emo_words.csv")

q1_map <- ggplot(emo, aes(x=av1, y=ai1, col = q1, label = q1)) +
  geom_point() +
  geom_text(
    check_overlap = T
  ) +
  labs(x = "Aktivitāte", y = "Valence") +
  scale_x_continuous(breaks=-4:4,
                     labels=c("-4","-3","-2","-1","0","1","2","3","4")) +
  scale_y_continuous(breaks=-4:4,
                     labels = c("-4","-3","-2","-1","0","1","2","3","4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) 

q1_map

head(emo_words)
emo_words$q1 <- tolower(emo_words$q1)
wordfreq <- count(emo_words, "q1")

x <- "."
wordfreq$q1 %>%
  str_replace_all(".", " ")

barplot(wordfreq)
desc_freq <- sort(wordfreq$freq, decreasing=TRUE)
desc_freq
