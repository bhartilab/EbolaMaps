# Load libraries 
library(readr)
library(ggplot2)
library(patchwork)

## Main text - Figure 3

# Read data 
ebola_sa <- read_csv("ebola_sa.csv", col_types = c("Radius"="integer"))
View(ebola_sa)

## Figure 3 a. Total comb road and river vs. Cases in first 100 days b. Total road length vs. Cases in first 100 days 

# Isolate 150 km x 150 km Study area (Radius = 75)
ebola_sa150x150 <-ebola_sa[-c(1:36, 55:108), ]
View(ebola_sa150x150 )

# Figure 3a Summary 
Figure3a_lm =lm(First100dayCases~TotalRiverRoad, data=ebola_sa150x150)
coef=coef(Figure3a_lm)
summary(Figure3a_lm)

# Create Figure 3a scatter plot
Figure3a_plot <- ggplot(data = ebola_sa150x150, aes(x = TotalRiverRoad, y = First100dayCases))+
  geom_point(shape = 21, colour = "white", fill = "black", size=3.0, stroke = 1) +
  geom_abline(col="black",intercept = coef[1], slope = coef[2], size=1) +
  labs(title="(a)", x="Total combined road and river length (km)", y="Cases in first 100 days") +
  scale_x_continuous(limits = c(0,2000)) +
  scale_y_continuous(limits = c(0,300)) +
  theme_classic() + theme(text = element_text(family = "Arial", size = 8, hjust = 0.5))
Figure3a_plot

# Figure 3b Summary 
Figure3b_lm =lm(First100dayCases~Road, data=ebola_sa150x150)
coef=coef(Figure3b_lm)
summary(Figure3b_lm)

# Create Figure 3b scatter plot
Figure3b_plot <- ggplot(data = ebola_sa150x150, aes(x = Road, y = First100dayCases))+
  geom_point(shape = 21, colour = "white", fill = "black", size=3.0, stroke = 1) +
  geom_abline(col="black",intercept = coef[1], slope = coef[2], size=1) +
  labs(title="(b)", x="Total road length (km)", y="Cases in first 100 days") +
  scale_x_continuous(limits = c(0,1500)) +
  scale_y_continuous(limits = c(0,300)) +
  theme_classic() + theme(text = element_text(family = "Arial", size = 8, hjust = 0.5))
Figure3b_plot

# Combine Figure 3a and 3b
Figure3 <- (Figure3a_plot|Figure3b_plot)
Figure3

# Save Figure 3a-b 
ggsave("Figure3.png", plot = Figure3, width = 7, height = 2.4, units = "in" )
ggsave("Figure3.eps", plot = Figure3, width = 7, height = 2.4, units = "in" )

## Supplement - Figure S2, S3, S4 

# Read data 
ebola_sa <- read_csv("ebola_sa.csv", col_types = c("Radius"="integer"))
View(ebola_sa)

## Figure S2.Total comb road and river vs.Total duration

# Isolate 100 km x 100 km Study area (Radius = 50)
ebola_sa100x100 <-ebola_sa[-c(1:18, 37:108), ]
View(ebola_sa100x100 )

# Figure S2 Summary 
FigureS2_lm =lm(OD~TotalRiverRoad, data=ebola_sa100x100)
coef=coef(FigureS2_lm)
summary(FigureS2_lm)

# Create Figure S2 scatter plot
FigureS2_plot <- ggplot(data = ebola_sa100x100, aes(x = TotalRiverRoad, y = OD))+
  geom_point(shape = 21, colour = "white", fill = "black", size=3.0, stroke = 1) +
  geom_abline(col="black",intercept = coef[1], slope = coef[2], size=1) +
  labs(title="", x="Total combined road and river length (km)", y="Total duration (days)") +
  scale_x_continuous(limits = c(0,800)) +
  scale_y_continuous(limits = c(0,900)) +
  theme_classic() + theme(text = element_text(family = "Arial", size = 8, hjust = 0.5))
FigureS2_plot

# Save Figure S2 
ggsave("FigureS2.png", plot = FigureS2_plot, width = 3.5, height = 2.4, units = "in" )
ggsave("FigureS2.eps", plot = FigureS2_plot, width = 3.5, height = 2.4, units = "in" )


## Figure S3.Total intersections vs. Cases in first 100 days

# Isolate 150 km x 150 km Study area (Radius = 75)
ebola_sa150x150 <-ebola_sa[-c(1:36, 55:108), ]
View(ebola_sa150x150 )

# Figure S3 Summary
FigureS3_lm =lm(First100dayCases~TotInter, data=ebola_sa150x150)
coef=coef(FigureS3_lm)
summary(FigureS3_lm)

# Create Figure S3 scatter plot
FigureS3_plot <- ggplot(data = ebola_sa150x150, aes(x = TotInter, y = First100dayCases))+
  geom_point(shape = 21, colour = "white", fill = "black", size=3.0, stroke = 1) +
  geom_abline(col="black",intercept = coef[1], slope = coef[2], size=1) +
  labs(title="", x="Total intersections", y="Cases in first 100 days") +
  scale_x_continuous(limits = c(0,40)) +
  scale_y_continuous(limits = c(0,300)) +
  theme_classic() + theme(text = element_text(family = "Arial", size = 8, hjust = 0.5))
FigureS3_plot

# Save Figure S3
ggsave("FigureS3.png", plot = FigureS3_plot, width = 3.5, height = 2.4, units = "in" )
ggsave("FigureS3.eps", plot = FigureS3_plot, width = 3.5, height = 2.4, units = "in" )


### Figure S4.Total comb road and river vs. Cases in first 100 days 
## a: 50 x 50 km, b: 100 x 100 km, c: 200 x 200 km d: 300 x 300 km

## Figure S4a

# Isolate 50 km x 50 km Study area (Radius = 25)
ebola_sa50x50 <-ebola_sa[-c(19:108), ]
View(ebola_sa50x50 )

# Figure S4a Summary 
FigureS4a_lm =lm(First100dayCases~TotalRiverRoad, data=ebola_sa50x50)
coef=coef(FigureS4a_lm)
summary(FigureS4a_lm)

# Create Figure S4a scatter plot
FigureS4a_plot <- ggplot(data = ebola_sa50x50, aes(x = TotalRiverRoad, y = First100dayCases))+
  geom_point(shape = 21, colour = "white", fill = "black", size=3.0, stroke = 1) +
  geom_abline(col="black",intercept = coef[1], slope = coef[2], size=1) +
  labs(title="(a) 50 km x 50 km, R- squared = 0.5580", x="Total combined road and river length (km)", y="Cases in first 100 days")+
  scale_x_continuous(limits = c(0,300)) +
  scale_y_continuous(limits = c(0,300)) +
  theme_classic() + theme(text = element_text(family = "Arial", size = 8, hjust = 0.5))
FigureS4a_plot

## Figure S4b

# Isolate 100 km x 100 km Study area (Radius = 50)
ebola_sa100x100 <-ebola_sa[-c(1:18, 37:108), ]
View(ebola_sa100x100)

# Figure S4b Summary
FigureS4b_lm =lm(First100dayCases~TotalRiverRoad, data=ebola_sa100x100)
coef=coef(FigureS4b_lm)
summary(FigureS4b_lm)

# Create Figure S4b scatter plot
FigureS4b_plot <- ggplot(data = ebola_sa100x100, aes(x = TotalRiverRoad, y = First100dayCases))+
  geom_point(shape = 21, colour = "white", fill = "black", size=3.0, stroke = 1) +
  geom_abline(col="black",intercept = coef[1], slope = coef[2], size=1)+
  labs(title="(b) 100 km x 100 km, R- squared = 0.3541", x="Total combined road and river length (km)", y="Cases in first 100 days")+
  scale_x_continuous(limits = c(0,800)) +
  scale_y_continuous(limits = c(0,300)) +
  theme_classic() + theme(text = element_text(family = "Arial", size = 8, hjust = 0.5))
FigureS4b_plot

## Figure S4c

# Isolate 200 km x 200 km Study area (Radius = 100)
ebola_sa200x200 <-ebola_sa[-c(1:54, 73:108), ]
View(ebola_sa200x200 )

# Figure S4c Summary
FigureS4c_lm =lm(First100dayCases~TotalRiverRoad, data=ebola_sa200x200)
coef=coef(FigureS4c_lm)
summary(FigureS4c_lm)

# Create Figure S4c scatter plot
FigureS4c_plot <- ggplot(data = ebola_sa200x200, aes(x = TotalRiverRoad, y = First100dayCases))+
  geom_point(shape = 21, colour = "white", fill = "black", size=3.0, stroke = 1) +
  geom_abline(col="black",intercept = coef[1], slope = coef[2], size=1)+
  labs(title="(c) 200 km x 200 km, R- squared = 0.5250", x="Total combined road and river length (km)", y="Cases in first 100 days")+
  scale_x_continuous(limits = c(0,3500)) +
  scale_y_continuous(limits = c(0,300)) +
  theme_classic() + theme(text = element_text(family = "Arial", size = 8, hjust = 0.5))
FigureS4c_plot

## Figure S4d

# Isolate 300 km x 300 km Study area (Radius = 150)
ebola_sa300x300 <-ebola_sa[-c(1:72, 91:108), ]
View(ebola_sa300x300 )

# Figure S4d Summary
FigureS4d_lm =lm(First100dayCases~TotalRiverRoad, data=ebola_sa300x300)
coef=coef(FigureS4d_lm)
summary(FigureS4d_lm)

# Create Figure S4d scatter plot
FigureS4d_plot <- ggplot(data = ebola_sa300x300, aes(x = TotalRiverRoad, y = First100dayCases))+
  geom_point(shape = 21, colour = "white", fill = "black", size=3.0, stroke = 1) +
  geom_abline(col="black",intercept = coef[1], slope = coef[2], size=1)+
  labs(title="(d) 300 km x 300 km, R- squared = 0.3893", x="Total combined road and river length (km)", y="Cases in first 100 days")+
  scale_x_continuous(limits = c(0,6500)) +
  scale_y_continuous(limits = c(0,300)) +
  theme_classic() + theme(text = element_text(family = "Arial", size = 8, hjust = 0.5))
FigureS4d_plot

# Combine Figure S4a, S4b, S4c, S4d 
FigureS4 <- wrap_plots(FigureS4a_plot, FigureS4b_plot, FigureS4c_plot, FigureS4d_plot)

# Save Figure S4 
ggsave("FigureS4.png", plot = FigureS4, width = 7, height = 4.8, units = "in" )
ggsave("FigureS4.eps", plot = FigureS4, width = 7, height = 4.8, units = "in" )