##### Visualization
# Load package
library(tidyverse)
library(RColorBrewer)

# Load data 
presVoteEV <- read_csv("http://staff.washington.edu/kpleung/vis/data/presVoteEV.csv")

# Load theme
source("http://staff.washington.edu/kpleung/vis/theme/theme_cavis.R")

# Get nice color
brewer <- brewer.pal(9, "Set1")
blue <- brewer[2]
orange <- brewer[5]

# Factorize variables for proper order
presVoteEV <- 
  presVoteEV %>%
  mutate(
    nonwhite = factor(nonwhite),
    vote92 = factor(vote92, levels = c("Clinton", "Perot", "Bush"))
  )

# Visualize
p <- ggplot(presVoteEV) +
  
  # Plot the estimation lines for both white/non-white
  geom_line(aes(y = pe, x = rlibcon, color = nonwhite)) +
  scale_color_manual(
    values = c(blue, orange),
    labels = c("White", "Non-white")
  ) +
  
  # Small multiples by presidential choice
  facet_grid( ~ vote92) +
  
  # For white, use geom_ribbon to shade CIs
  geom_ribbon(
    aes(ymax = upper, ymin = lower, x = rlibcon, fill = nonwhite), 
    linetype = 0, # No border lines; just the pale polygon
    alpha = 0.5,
    show.legend = FALSE
  ) + 
  
  # For white, fill the geom_ribbon with blue; otherwise get rid of it
  scale_fill_manual(values = c(blue, NA)) +
  
  # For non-white, use two dashed lines to plot CIs
  geom_line(
    aes(x = rlibcon, y = upper, color = nonwhite, linetype = nonwhite),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = rlibcon, y = lower, color = nonwhite, linetype = nonwhite),
    show.legend = FALSE
  ) +
  
  # For non-white, use dashed lines as linetypes; otherwise get rid of it
  scale_linetype_manual(values = c(0, 2)) +
  
  # Scale x-axis
  scale_x_continuous(breaks = 1:7) +
  
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  # Themes
  theme_cavis_hgrid +
  theme(
    legend.position = c(0.06, 0.13),
    legend.key.size = unit(0.2, "cm"),
  ) +
  
  # Axis titles 
  labs(
    y = "Predicted probability of voting", 
    x = "Ideological self-placement \n(from very liberal to very conservative)"
  ) 

width <- 8
ggsave("presVoteEV.pdf", width = width, height = width/1.618, units = "in")

# Bonus: Facet-specific labels
candidates <- c("Clinton", "Perot", "Bush")

facet_labels <- 
  tibble(
    vote92 = rep(candidates, each = 2),
    nonwhite = rep(c(0, 1), 3),
    label = rep(c("White", "Non-white"), 3),
    x_coord = c(1.5, 6.3, 1.4, 6.6, 1.4, 6.3), 
    y_coord = c(0.5, 0.75, 0.32, 0.105, 0.32, 0.13)
  ) %>%
  mutate(vote92 = factor(vote92, levels = candidates),
         nonwhite = factor(nonwhite))

print(facet_labels)

p + 
  geom_text(data = facet_labels,
            aes(x = x_coord, y = y_coord,
                label = label, color = nonwhite),
            size = 2.5) +
  theme(legend.position = "none")

width <- 8
ggsave("presVoteEV2.pdf", width = width, height = width/1.618, units = "in")

## Another way to facet 
presVoteEV %>%
  mutate(nonwhite = case_when(nonwhite == 0 ~ "White", 
                              nonwhite == 1 ~ "Non-white")) %>%
  ggplot() +
  geom_line(aes(y = pe, x = rlibcon)) +
  # Small multiples
  facet_grid(nonwhite ~ vote92) +
  # For white, use geom_ribbon to shade CIs
  geom_ribbon(
    aes(ymax = upper, ymin = lower, x = rlibcon), 
    linetype = 0, # No border lines; just the pale polygon
    alpha = 0.3
  ) + 
  # Scale x and y-axis
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 1, 0.2),
    limits = c(0, 0.99)
  ) +
  # Themes
  theme_cavis_hgrid +
  # Axis titles 
  labs(
    y = "Predicted prob. of voting", 
    x = "Ideological self-placement \n(from very liberal to very conservative)"
  )

width <- 8
ggsave("presVoteEV3.pdf", width = width, height = width/1.618, units = "in")
