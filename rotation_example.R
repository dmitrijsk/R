# Example with rotation.

library(tidyverse)
ex_df <- tibble(data_point = "original",
                x = c(1, 0.5), 
                y = c(0, 0.5))

# Define the rotation matrix with an angle pi/4 (45 degrees).
rot_m <- matrix(c(cos(pi/4), -sin(pi/4), sin(pi/4), cos(pi/4)), byrow = TRUE, nrow = 2)

# Rotate.
ex_rot_m <- t(rot_m %*% t(as.matrix(ex_df[-1])))

# Change col- and row-names before binding with original data.
colnames(ex_rot_m) <- c("x", "y")
ex_rot_m

ex_rot_df <- as.data.frame(ex_rot_m) %>% 
  mutate(data_point = "rotated")

# Plot.
bind_rows(ex_df, ex_rot_df) %>% 
  ggplot(aes(x = x, y = y, colour = data_point)) + 
  geom_point() + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(-1, 1), y = c(-1, 1)) +
  theme(legend.position = "top")
