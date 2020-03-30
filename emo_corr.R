rm(list = ls())
install.packages("dplyr")
install.packages("tibble")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(tibble)

setwd("/home/yes_no/Downloads/bakalaurs")
getwd()

emo_start <- read.csv("emo_2d_coded.csv", header = T)

emo_start <- emo_start %>% 
  dplyr::rename(
    ai22 = ai23,
    ai23 = ai23.1
  )

colnames(emo_start)


#atlasa vajadzīgās kolonnas Zviedru kodola afekta skalas aprēķiniem
emo_full <- emo_start %>% 
  dplyr::select(starts_with("id"),starts_with("vecums"), starts_with("dzimums"),starts_with("scas"),starts_with("av"), starts_with("ai"))
emo_full$observation <- 1:nrow(emo_full)   

nrow(emo_full)
nrow(na.omit(emo_full))

#izveido apakšskalas
scas_val <- emo_full %>% 
  dplyr::select(scas_apmierinats, scas_priecigs, scas_laimigs)

scas_activ <- emo_full %>% 
  dplyr::select(scas_modrs, scas_energisks, scas_aktivs)

scas_paud <- emo_full %>% 
  dplyr::select(scas_ieinteresets, scas_iesaistits, scas_optimisms)
  
scas_uapd <- emo_full %>% 
  dplyr::select(scas_rams, scas_mierigs, scas_atvieglots)


#izrēķina vidējos apakšskalas rādītājus katram respondentam
scas_val$mean_val = rowMeans(scas_val)
scas_activ$mean_activ = rowMeans(scas_activ)
scas_paud$mean_paud = rowMeans(scas_paud)
scas_uapd$mean_uapd = rowMeans(scas_uapd)


emo_full[, 'scas_val_mean'] <- scas_val$mean_val
emo_full[, 'scas_activ_mean'] <- scas_activ$mean_activ
emo_full[, 'scas_paud_mean'] <- scas_paud$mean_paud
emo_full[, 'scas_uapd_mean'] <- scas_uapd$mean_uapd

#sagatavo tabulu scas plot
scas_full <- cbind(scas_val$mean_val, scas_activ$mean_activ, scas_paud$mean_paud, scas_uapd$mean_uapd)

#valences aktivitātes ggplot
val_activ <- ggplot(emo_full, aes(x=scas_val_mean, y=scas_activ_mean, col = observation)) +
  geom_point(size=3, alpha=0.7, position=position_jitter(w=0.1, h=0)) +
  geom_text(aes(label=observation), check_overlap = FALSE) +
  geom_hline(yintercept = 4) + geom_vline(xintercept = 4)+
  theme_bw()

val_activ

#patīkamas/nepatīkamas aktivitātes/deaktivitātes ggplot
paud_uapd <- ggplot(emo_full, aes(x=scas_paud_mean, y=scas_uapd_mean)) +
  geom_point(size=3, alpha=0.7, position=position_jitter(w=0.1, h=0)) +
  geom_text(aes(label=observation), check_overlap = T) + 
  geom_hline(yintercept = 4) + geom_vline(xintercept = 4)+
  theme_bw() 

paud_uapd

stimulus_values <- emo_start %>% 
  dplyr::select(starts_with("av"), starts_with("ai"))

stimulus_values <- emo_start %>% 
  dplyr::select(starts_with("av"), starts_with("ai"))



