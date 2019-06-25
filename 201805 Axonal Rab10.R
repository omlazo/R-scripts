#### ENVIROMENTAL SETUP ####

# Setting the folder for this session and all its materials and data
setwd("~/Desktop/Axonal transport Rab10/")

# Installing and loading the required libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("forcats")
library(forcats)
devtools::install_github('bart6114/artyfarty')
library(artyfarty)
library(plyr)

####INSTANTANEOUS VELOCITIES####

#### DATA PREPARATION ####

# Reading the file with my tidy data and making it a variable to easily work with it.
# I'm calling the variable "intensitydb"
df.preBDNF <- read.csv(file = "Links preBDNF.csv", header = TRUE)
df.preBDNF2 <- read.csv(file = "Links preBDNF2.csv", header = TRUE)
df.postBDNF <- read.csv(file = "Links postBDNF.csv", header = TRUE)
df.postBDNF2 <- read.csv(file = "Links postBDNF2.csv", header = TRUE)

# View to explore if it has header and it is displayed correctly

df.preBDNF$Treatment <- "preBDNF"
df.preBDNF2$Treatment <- "preBDNF"
df.postBDNF$Treatment <- "postBDNF"
df.postBDNF2$Treatment <- "postBDNF"

velocities <- rbind(df.preBDNF,
                    df.preBDNF2,
                    df.postBDNF,
                    df.postBDNF2)

mu <- ddply(velocities, "Treatment", summarise,  grp.mean=mean(VELOCITY))
me <- ddply(velocities, "Treatment", summarise,  grp.median=median(VELOCITY))

View(velocities)

####PLOT####

## Histogram only:

ggplot(velocities,  aes(x=VELOCITY, fill=Treatment, color=Treatment)) +
  theme_scientific() +
  geom_histogram(binwidth=0.04, position="dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Treatment), linetype="dashed") +
  ylim(0, 40) + xlim(-1, 2) +
  labs (x= "Retrograde velocities (µm/sec)", 
        y= "Count") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Rab10 axonal carriers")

## Densities and medians only:

ggplot(velocities,  aes(x=VELOCITY, fill=Treatment, color=Treatment)) +
  theme_scientific() +
  geom_density(alpha=.3,  aes(fill=Treatment)) +
  geom_vline(data=me, aes(xintercept=grp.median, color=Treatment), linetype="dashed") +
  scale_x_continuous(limits = c(-1,2), breaks=seq(-1, 2, 0.25)) +
  ylim(0, 20) +
  labs (x= "Velocities (µm/sec)", 
        y= "Distribution") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13, angle=30, hjust=1, vjust=1),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Rab10 axonal carriers")
  
## Everything together: Histogram with densities and averages...

ggplot(velocities,  aes(x=VELOCITY, fill=Treatment, color=Treatment)) +
  theme_scientific() +
  geom_histogram(binwidth=0.04, position="dodge") +
  geom_density(alpha=.2,  aes(fill=Treatment)) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Treatment), linetype="dashed") +
  scale_x_continuous(limits = c(-1,2), breaks=seq(-1, 2, 0.25)) +
  ylim(0, 40) +
  labs (x= "Retrograde velocities (µm/sec)", 
        y= "Distribution") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13, angle=30, hjust=1, vjust=1),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Rab10 axonal carriers")







ggplot(velocities, aes(x= Treatment, y= VELOCITY, fill= Treatment)) + 
  geom_violin(aes(x= Treatment, y = VELOCITY, fill= Treatment), size=.4) +
  stat_summary(fun.data = "mean_se", colour = "black", geom = "errorbar", size=.4, width=.3,position=position_dodge(.9))+
  stat_summary(fun.y = "mean", colour = "black", size = 4, geom = "point") +
  scale_x_discrete(limits=c("preBDNF","postBDNF")) +
  ylim(-0.2, 2) +
  labs (x= "Treatment", 
        y= "Velocity (µm/sec)") +
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 16),
        legend.text= element_text (size = 13),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16)) +
  ggtitle("Rab10 axonal carriers")
