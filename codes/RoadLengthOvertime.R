### Updated Figure 2 - Total Road Length (km) overtime 

# Load libraries

library(readr)
library(ggplot2)
library(dplyr) # data manipulation

# Read data

setwd("/Users/alexgonzalez/Desktop/EVD_2024/75kmRadius")
RL_Year_75RadiusCorrected <- read.csv("RL_Year_75RadiusCorrected.csv")  

# Create dataframe for manual red dot positions
## Provide correct x (Spillover_Year) and y (estimated Total_km) values for each location

manual_red_dots <- data.frame(
  Location = c("Bikoro", "Booue", "Etoumbi", "Inkanamango", "Kikwit", 
               "Leubo07", "Leubo08", "Likati", "Mayibout", "Mbomo", 
               "Mbomo2", "Mbandaka", "MekamboMbomo", "Mekouka", 
               "NorthKivu", "Tandala", "Meliandou", "Yambuku"),
  Spillover_Year = c(2018, 1996, 2005, 2014, 1995, 2007, 2008, 
                     2017, 1996, 2003, 2003, 2020, 2001, 1994, 
                     2018, 1977, 2013, 1976),
  Total_km = c(684.5199, 457.14405, 448, 625, 1183.5, 
               690.8954, 673.6631, 520, 194.5861, 424.9933, 
               465.6939, 349.5222, 400.9593, 67, 
               530, 728.5, 838, 371.5627)  # Estimated y-axis values (Total_km)
)

# Create plot

RLOvertime <- ggplot(RL_Year_75RadiusCorrected, aes(x = Year, y = Total_km, color = Location, shape = Location)) +
  geom_line(size = 1) +                              # Add line plot
  geom_point(size = 2) +                            # Add points to distinguish shapes
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +  # Set x-axis breaks
  labs(x = "Year", y = "Total Road Length (km)", color = "Location", shape = "Location") +
  theme_minimal(base_size = 15) +  # Minimal theme with adjusted base font size
  theme(
    panel.background = element_rect(fill = "white", color = NA), # White background with no border
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    legend.position = "right",             # Legend on the right
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_color_viridis_d() +  # Use viridis color palette
  scale_shape_manual(values = 1:length(unique(RL_Year_75RadiusCorrected$Location))) # Assign unique shapes


# Manually add red dots at the spillover years with estimated Total_km

Figure2_RLOvertime <- RLOvertime + geom_point(data = manual_red_dots, 
                    aes(x = Spillover_Year, y = Total_km), 
                    color = "red", size = 2, shape = 19)  # Red dots with size 2 and filled shape


# Display plot

print(Figure2_RLOvertime)
