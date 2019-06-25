#### ENVIROMENTAL SETUP ####

# Setting the folder for this session and all its materials and data
setwd("~/Desktop/Kymo intensity")

# Installing and loading the required libraries
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(forcats)
library(artyfarty)
library(EBImage)

kymograph_pre <- readImage("preBDNF 1 Kymo.tif", type = "tiff")
kymograph_post1 <- readImage("postBDNF 1 Kymo.tif", type = "tiff")
kymograph_post2 <- readImage("postBDNF 3 Kymo.tif", type = "tiff")

display(kymograph, method = "raster")

#' Turn the kymograph tif into a tibble
kymopre_data <- as_tibble(kymograph_pre)
kymopost1_data <- as_tibble(kymograph_post1)
kymopost2_data <- as_tibble(kymograph_post2)

#' Add a column describing the pixel number as the first colum in the table
kymopre_data <- add_column(kymopre_data, Pixel_position = 1:nrow(kymopre_data), .before = 1)
kymopost1_data <- add_column(kymopost1_data, Pixel_position = 1:nrow(kymopost1_data), .before = 1)
kymopost2_data <- add_column(kymopost2_data, Pixel_position = 1:nrow(kymopost2_data), .before = 1)

#' Reshape the table so intensity values are in one column (this is why I put the `Pixel` column first)
kymopre_data <- gather(kymopre_data, 2:ncol(kymopre_data), key = "Time_point", value = "Intensity")
kymopost1_data <- gather(kymopost1_data, 2:ncol(kymopost1_data), key = "Time_point", value = "Intensity")
kymopost2_data <- gather(kymopost2_data, 2:ncol(kymopost2_data), key = "Time_point", value = "Intensity")

#' Now we tell the characters to become factors with the as_factor function
kymopre_data$Time_point <- as_factor(kymopre_data$Time_point)
kymopost1_data$Time_point <- as_factor(kymopost1_data$Time_point)
kymopost2_data$Time_point <- as_factor(kymopost2_data$Time_point)

#' Just check they are factors
# is.factor(kymo_data$Time_point)

#' Now turn the factors into numbers
kymopre_data$Time_point <- as.numeric(kymopre_data$Time_point)
kymopost1_data$Time_point <- as.numeric(kymopost1_data$Time_point)
kymopost2_data$Time_point <- as.numeric(kymopost2_data$Time_point)

# is.numeric(kymo_data$Time_point)


kymopre_data.averages <- ddply(kymopre_data, c("Pixel_position"), summarise,
                           N    = length(Intensity),
                           average = mean(Intensity),
                           sd   = sd(Intensity),
                           se   = sd / sqrt(N-1),
                           av.lo = average - se,
                           av.up = average + se,
                           "label" = "preBDNF")

kymopost1_data.averages <- ddply(kymopost1_data, c("Pixel_position"), summarise,
                               N    = length(Intensity),
                               average = mean(Intensity),
                               sd   = sd(Intensity),
                               se   = sd / sqrt(N-1),
                               av.lo = average - se,
                               av.up = average + se,
                               "label" = "postBDNF immediate")

kymopost2_data.averages <- ddply(kymopost2_data, c("Pixel_position"), summarise,
                               N    = length(Intensity),
                               average = mean(Intensity),
                               sd   = sd(Intensity),
                               se   = sd / sqrt(N-1),
                               av.lo = average - se,
                               av.up = average + se,
                               "label" = "postBDNF 20 min later")

kymo_data <- rbind(kymopre_data.averages,
                   kymopost1_data.averages,
                   kymopost2_data.averages)

View(kymo_data)




ggplot(data = kymo_data, aes(x= Pixel_position, y= average, ymin= av.lo, ymax= av.up, fill = label)) + 
  geom_line() + 
  geom_ribbon(alpha=0.5) +
  labs (x= "Distance(px)", y= "Intensity (AU)") +
  ylim(0,40) +
  xlim(0,800) +
  scale_fill_manual(values=c("red", "blue", "green")) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13)) +
  ggtitle("Mean intensity along the axon (distal)")


library(ggridges)


plotpre <- kymopre_data %>% ggplot(., aes(x = Pixel_position, y = max(Time_point)-Time_point, group = Time_point, height = Intensity))
plotpre + 
  geom_ridgeline(scale = 5, fill = "#000000", colour = "white", size = 0.25) + 
  coord_cartesian(xlim = c(33, 747)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "#000000"),
        panel.border = element_blank(),
        panel.spacing = unit(c(0,0,0,0),"cm")
  )
plotpost1 <- kymopost1_data %>% ggplot(., aes(x = Pixel_position, y = max(Time_point)-Time_point, group = Time_point, height = Intensity))
plotpost1 + 
  geom_ridgeline(scale = 5, fill = "#000000", colour = "white", size = 0.25) + 
  coord_cartesian(xlim = c(33,747)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "#000000"),
        panel.border = element_blank(),
        panel.spacing = unit(c(0,0,0,0),"cm")
  )
plotpost2 <- kymopost2_data %>% ggplot(., aes(x = Pixel_position, y = max(Time_point)-Time_point, group = Time_point, height = Intensity))
plotpost2 + 
  geom_ridgeline(scale = 5, fill = "#000000", colour = "white", size = 0.25) + 
  coord_cartesian(xlim = c(33,747)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "#000000"),
        panel.border = element_blank(),
        panel.spacing = unit(c(0,0,0,0),"cm")
  )
