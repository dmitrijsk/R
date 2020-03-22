rm(list = ls())
install.packages("ggplot2")
library(ggplot2)
setwd("/home/yes_no/Downloads/bakalaurs")
getwd()

emo <- read.csv("emo2d.csv")
head(emo)
q1_map <- ggplot(emo, aes(x=av1, y=ai2, col = q1)) +
  geom_point() +
  labs(
    x = "Aktivitāte",
    y = "Valence"
       ) + 
  scale_x_continuous(breaks=1:9,
                     labels=c("-4","-3","-2","-1","0","1","2","3","4")) +
  scale_y_continuous(breaks=1:9,
                     labels = c("-4","-3","-2","-1","0","1","2","3","4")) +
  geom_hline(yintercept = 5) + geom_vline(xintercept = 5) +
  geom_text(
    data=(emo$q1), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )

q1_map
