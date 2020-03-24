rm(list = ls())
install.packages("ggplot2")
install.packages("plyr")
install.packages("stringr")
install.packages("dplyr")
install.packages('devtools')
library(ggplot2)
library(plyr)
library(stringr)
library(dplyr)
setwd("/home/yes_no/Downloads/bakalaurs")
getwd()

emo <- read.csv("emo_2d_coded.csv", header = T, na.strings=c("", "NA"))
emo_words <- read.csv("emo_words.csv")
names(emo_words)[names(emo_words)=="q1"] <- "word"
names(emo_words)[names(emo_words)=="av1"] <- "valence"
names(emo_words)[names(emo_words)=="ai1"] <- "activity"

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
# darbības, lai radītu pilno sarakstu ar emociju vārdiem
emo_words$word <- tolower(emo_words$word) #eliminate uppercase
no_punct_full <- as.data.frame(gsub("[[:punct:]]", "", emo_words$word))
no_punct_valence <- data.frame(no_punct_full, emo_words$valence) 
no_punct_dim <- data.frame(no_punct_valence, emo_words$activity)
names(no_punct_dim)[names(no_punct_dim)=="gsub.....punct..........emo_words.word."] <- "word"
names(no_punct_dim)[names(no_punct_dim)=="emo_words.valence"] <- "valence"
names(no_punct_dim)[names(no_punct_dim)=="emo_words.activity"] <- "activity"


wordfreq <- count(emo_words, "word") #vārdu biežumu kolonna
no_punctuation <- as.data.frame(gsub("[[:punct:]]", "", wordfreq$word)) #biežumu kolonna bez special symbols


#darbības ar mazajiem datiem
tidied <- data.frame(no_punctuation, wordfreq$freq)
names(tidied)[names(tidied)=="gsub.....punct..........wordfreq.q1."] <- "word"
names(tidied)[names(tidied)=="wordfreq.freq"] <- "frequency"
head(tidied)

small_units <- subset(tidied, (nchar(as.character(word)) <= 20) & frequency > 4)

devtools::install_github('cttobin/ggthemr')
library(ggthemr)
  
ggthemr('flat dark')
ggplot(data = small_units, mapping = aes(x = reorder(word, frequency), frequency)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label=frequency), check_overlap = T) +
  coord_flip() + 
  scale_x_discrete("Visbiežāk izmantotie emociju vārdi") +
  scale_y_continuous("Biežums")

