# Load required libraries
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(grid) 

## Figure 1.b. Total cases vs Cases in first 100 days
# Read the data
ebola <- read_csv("ebola.csv", show_col_types = FALSE)
View(ebola)

# Define custom colors for each country
country_colors <- c("#045a8d", "#2b8cbe", "grey", "#bdc9e1")

# Create the scatter plot
ggplot(ebola, aes(log(Cases1st100Days), 
                  log(TotalCase), 
                  color = Country, 
                  size = Size, 
                  order = Order)) +
  geom_point(alpha = 1) +
  scale_size(range = c(1, 4), guide = "none") +
  scale_color_manual(values = country_colors) +
  guides(color = guide_legend(title = "", override.aes = list(size = c(1.5, 2.7, 3.5, 4), shape = c(16, 16, 16, 16), color = country_colors))) +
  xlab("Log(Cases in first 100 days)") +
  ylab("Log(Total cases)") +
  xlim(0, 11) +
  ylim(0, 11) +
  geom_abline(size = 0.25) +
  theme(text = element_text(size = 8), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

ggsave(
  "Figure_1b.png",
  plot = last_plot(),
  width = 3.5,
  height = 2.4,
)


## Figure 2. Total road length vs. total river length 
# Create a large matrix for a smooth background shading (values range between 0.4-0.8)
mat <- matrix(NA, 1000,1000)
mat[row(mat)+col(mat) ==1001] <- 0.8

i <- 1
for (x in 1:1000) {
  mat[(row(mat)+col(mat) ==1000 - i + 1)|(row(mat)+col(mat) ==1000 + i + 1)] <- (1-(i/1000))*(0.8-0.4)+0.4
  i <- i + 1
}

# Create the shading by interpolating colors
g <- rasterGrob(
  image = mat,
  width = unit(1, "npc"),
  height = unit(1, "npc"), 
  interpolate = TRUE
)

# Total road length vs. total river length 
p <- ggplot(ebola, aes(x = Road, y = River, label = Spillover)) +
  annotation_custom(
    grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  geom_point(color = "black", size = 2.5) + 
  geom_point(colour="white",pch=21, size=2.5, show.legend = NA) + 
  theme_bw() + 
  scale_x_continuous(limits = c(0,1200)) +
  scale_y_continuous(limits = c(0,1200)) +
  xlab("Total road length (km)") +
  ylab("Total river length (km)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size=8,color="black"), axis.title = element_text(size = 8)) +
  theme(plot.title = element_text(size = 8))

p + geom_text(x=250, y=1200, label="More river connections", size= 3, color='black') + 
  geom_text(x=950, y=0, label="More road connections", size= 3, color='black')

ggsave(
  "Figure2.png",
  plot = last_plot(),
  width = 3.5,
  height = 2.4,
)
