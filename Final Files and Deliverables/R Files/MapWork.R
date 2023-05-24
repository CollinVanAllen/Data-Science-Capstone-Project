library(tidyverse)
library(lubridate)
library(stringr)
library(raster)
library(rgdal)
library(sp)
library(sf)

dataset <- read_csv("Combined Files/municipFinal.csv")
dataset <- dataset[,2:ncol(dataset)]
dataset <- dataset %>% mutate(Label = paste(Municipality, "-", `Muni Type Code`))

shape <- st_read("GIS/CapstoneFiles/Wi5Counties.shp")

shape$LABEL <- toupper(shape$LABEL)

shape$color_var <- ifelse(shape$LABEL == "PLATTEVILLE - T", "grey", "red")

ggplot() +
  geom_sf(data = shape, aes(fill = color_var)) +
  scale_fill_manual(values = c("red", "grey")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


