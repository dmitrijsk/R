# rm(list = ls()) # Dmitrijs: This is a bad and dangerous style, we discussed it during the lectures. Use "Restart R" instead.

# Dmitrijs: Comment out the `install.packages()` because is needs to be executed only once.
# install.packages("dplyr")
# install.packages("tibble")
# install.packages("ggplot2")

library(ggplot2)
library(dplyr)
library(tibble)

# Please read this: https://community.rstudio.com/t/project-oriented-workflow-setwd-rm-list-ls-and-computer-fires/3549
# and replace `setwd()`.
# setwd("/home/yes_no/Downloads/bakalaurs")
# getwd()

emo_start <- read.csv("emo_2d_coded.csv", header = TRUE, encoding = "UTF-8")

emo_start <- emo_start %>% 
  rename(ai22 = ai23, # `dplyr::` is not needed becusebecause you use `library()` above.
         ai23 = ai23.1)

colnames(emo_start)


# Atlasa vajadzīgās kolonnas Zviedru kodola afekta skalas aprēķiniem.
emo_full <- emo_start %>% 
  select(starts_with("id"), 
         starts_with("vecums"), 
         starts_with("dzimums"),
         starts_with("scas"),
         starts_with("av"), 
         starts_with("ai")) %>% 
  mutate(observation = row_number())

nrow(emo_full)
nrow(na.omit(emo_full))

# Izveido apakšskalas.
scas_val <- emo_full %>% 
  select(scas_apmierinats, scas_priecigs, scas_laimigs)

scas_activ <- emo_full %>% 
  select(scas_modrs, scas_energisks, scas_aktivs)

scas_paud <- emo_full %>% 
  select(scas_ieinteresets, scas_iesaistits, scas_optimisms)
  
scas_uapd <- emo_full %>% 
  select(scas_rams, scas_mierigs, scas_atvieglots)


# Izrēķina vidējos apakšskalas rādītājus katram respondentam.
scas_val$mean_val <- rowMeans(scas_val)
scas_activ$mean_activ = rowMeans(scas_activ) # Replace `=` with `<-`.
scas_paud$mean_paud = rowMeans(scas_paud)
scas_uapd$mean_uapd = rowMeans(scas_uapd)


emo_full[, "scas_val_mean"] <- scas_val$mean_val
emo_full[, 'scas_activ_mean'] <- scas_activ$mean_activ # Use double quotation marks.
emo_full[, 'scas_paud_mean'] <- scas_paud$mean_paud
emo_full[, 'scas_uapd_mean'] <- scas_uapd$mean_uapd

# Sagatavo tabulu scas plot.
scas_full <- cbind(scas_val$mean_val, scas_activ$mean_activ, scas_paud$mean_paud, scas_uapd$mean_uapd)

# Valences aktivitātes ggplot
library(ggrepel)

data1 <- emo_full %>%
  select(observation, scas_val_mean, scas_activ_mean) %>% 
  na.omit()

val_activ <- data1 %>% # `data1` and `data2` are very bad names - please change.
  # Also change the names of variables, they are hard to understand.
  ggplot(aes(x = scas_val_mean, y = scas_activ_mean, label = observation)) +
  # I see no reason for using colour here.
  geom_jitter(size = 3, alpha = 0.7, width = 0.05, height = 0.05) + # Why only in 1 direction?
  geom_text_repel(size = 3) +
  geom_hline(yintercept = 4) + 
  geom_vline(xintercept = 4) +
  coord_cartesian(xlim = c(0,8), ylim = c(0,8)) + # Controls the limits of x and y.
  theme_bw()

val_activ



# Patīkamas/nepatīkamas aktivitātes/deaktivitātes ggplot.

data2 <- emo_full %>%
  select(observation, scas_paud_mean, scas_uapd_mean) %>%
  na.omit()

paud_uapd <- data2 %>% 
  ggplot(aes(x = scas_paud_mean, y = scas_uapd_mean, label = observation)) +
  geom_jitter(size = 3, alpha = 0.7, width = 0.05, height = 0.05) + # Why only in 1 direction?
  geom_text_repel(size = 3) +
  geom_hline(yintercept = 4) + 
  geom_vline(xintercept = 4) +
  coord_cartesian(xlim = c(0,8), ylim = c(0,8)) + # Controls the limits of x and y.
  theme_bw()

paud_uapd




stimulus_values <- emo_start %>% 
  select(starts_with("av"), starts_with("ai"))

stimulus_values <- emo_start %>% 
  select(starts_with("av"), starts_with("ai"))
