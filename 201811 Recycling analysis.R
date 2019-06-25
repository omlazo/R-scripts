#### ENVIROMENTAL SETUP ####

# Setting the folder for this session and all its materials and data
setwd("~/Desktop/Recycling/Recycling in mass culture Rab10 and Metformin/DataTables")

# Installing and loading the required libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("forcats")
library(forcats)
devtools::install_github('bart6114/artyfarty')
library(artyfarty)


#### DATA PREPARATION ####

#### Importing the files ####
# Put the files in the same directory of the R script and use this to import them all.
# It takes the files as they are and give variables the name of the file without extension

recycling.files <- list.files(pattern = "*.csv")
recycling.data <- lapply(recycling.files, function(i){
  read.csv(i, header=TRUE)
})
names(recycling.data) <- gsub("\\.csv$", "", recycling.files)
list2env(recycling.data,envir=.GlobalEnv)

#### Labelling and binding ####


EGFP60minMet1$neuron <- 1
EGFP60minMet2$neuron <- 2
EGFP60minMet3$neuron <- 3
EGFP60minMet4$neuron <- 4
EGFP60minMet5$neuron <- 5
EGFP60minMet6$neuron <- 6
EGFP60minMet7$neuron <- 7
EGFP60minMet8$neuron <- 8
EGFP60minMet9$neuron <- 9
EGFP60minMet10$neuron <- 10
EGFP60minMet11$neuron <- 11
EGFP60minMet12$neuron <- 12
EGFP60minMet13$neuron <- 13
EGFP60minMet14$neuron <- 14
EGFP60minMet15$neuron <- 15


EGFP60minMet <- rbind(EGFP60minMet1,
EGFP60minMet2,
EGFP60minMet3,
EGFP60minMet4,
EGFP60minMet5,
EGFP60minMet6,
EGFP60minMet7,
EGFP60minMet8,
EGFP60minMet9,
EGFP60minMet10,
EGFP60minMet11,
EGFP60minMet12,
EGFP60minMet13,
EGFP60minMet14,
EGFP60minMet15)

EGFP60minMet$treatment <- as.character(c("EGFP + Met"))

EGFP60minNomet1$neuron <- 1
EGFP60minNomet2$neuron <- 2
EGFP60minNomet3$neuron <- 3
EGFP60minNomet4$neuron <- 4
EGFP60minNomet5$neuron <- 5
EGFP60minNomet6$neuron <- 6
EGFP60minNomet7$neuron <- 7
EGFP60minNomet8$neuron <- 8
EGFP60minNomet9$neuron <- 9

EGFP60minNomet <- rbind(EGFP60minNomet1,
                        EGFP60minNomet2,
                        EGFP60minNomet3,
                        EGFP60minNomet4,
                        EGFP60minNomet5,
                        EGFP60minNomet6,
                        EGFP60minNomet7,
                        EGFP60minNomet8,
                        EGFP60minNomet9)

EGFP60minNomet$treatment <- as.character(c("EGFP"))



Rab10CA60minNomet1$neuron <- 1
Rab10CA60minNomet2$neuron <- 2
Rab10CA60minNomet3$neuron <- 3
Rab10CA60minNomet4$neuron <- 4
Rab10CA60minNomet5$neuron <- 5
Rab10CA60minNomet6$neuron <- 6
Rab10CA60minNomet7$neuron <- 7


Rab10CA60minNomet <- rbind(Rab10CA60minNomet1,
                           Rab10CA60minNomet2,
                           Rab10CA60minNomet3,
                           Rab10CA60minNomet4,
                           Rab10CA60minNomet5,
                           Rab10CA60minNomet6,
                           Rab10CA60minNomet7)

Rab10CA60minNomet$treatment <- as.character(c("Rab10CA"))

Rab10CA60minMet1$neuron <- 1
Rab10CA60minMet2$neuron <- 2
Rab10CA60minMet3$neuron <- 3
Rab10CA60minMet4$neuron <- 4
Rab10CA60minMet5$neuron <- 5
Rab10CA60minMet6$neuron <- 6
Rab10CA60minMet9$neuron <- 9
Rab10CA60minMet10$neuron <- 10
Rab10CA60minMet11$neuron <- 11
Rab10CA60minMet12$neuron <- 12
Rab10CA60minMet13$neuron <- 13

Rab10CA60minMet <- rbind(Rab10CA60minMet1,
                           Rab10CA60minMet2,
                           Rab10CA60minMet3,
                           Rab10CA60minMet4,
                           Rab10CA60minMet5,
                           Rab10CA60minMet6,
                           Rab10CA60minMet9,
                           Rab10CA60minMet10,
                           Rab10CA60minMet11,
                           Rab10CA60minMet12,
                           Rab10CA60minMet13)

Rab10CA60minMet$treatment <- as.character(c("Rab10CA + Met"))

Rab10DN60minNomet1$neuron <- 1
Rab10DN60minNomet2$neuron <- 2
Rab10DN60minNomet3$neuron <- 3
Rab10DN60minNomet4$neuron <- 4
Rab10DN60minNomet5$neuron <- 5
Rab10DN60minNomet6$neuron <- 6
Rab10DN60minNomet7$neuron <- 7
Rab10DN60minNomet8$neuron <- 8
Rab10DN60minNomet9$neuron <- 9

Rab10DN60minNomet <- rbind(Rab10DN60minNomet1,
                           Rab10DN60minNomet2,
                           Rab10DN60minNomet3,
                           Rab10DN60minNomet4,
                           Rab10DN60minNomet5,
                           Rab10DN60minNomet6,
                           Rab10DN60minNomet7,
                           Rab10DN60minNomet8,
                           Rab10DN60minNomet9)

Rab10DN60minNomet$treatment <- as.character(c("Rab10DN"))

Rab10DN60minMet1$neuron <- 1
Rab10DN60minMet2$neuron <- 2
Rab10DN60minMet3$neuron <- 3
Rab10DN60minMet4$neuron <- 4
Rab10DN60minMet5$neuron <- 5
Rab10DN60minMet6$neuron <- 6


Rab10DN60minMet <- rbind(Rab10DN60minMet1,
                           Rab10DN60minMet2,
                           Rab10DN60minMet3,
                           Rab10DN60minMet4,
                           Rab10DN60minMet5,
                           Rab10DN60minMet6)

Rab10DN60minMet$treatment <- as.character(c("Rab10DN + Met"))

bound.recycling.data <- rbind(EGFP60minMet,
                              EGFP60minNomet,
                              Rab10CA60minNomet,
                              Rab10CA60minMet,
                              Rab10DN60minNomet,
                              Rab10DN60minMet)

bound.recycling.data$domain <- as.character(c("soma", "soma", 
                                              "dendrite","dendrite", "dendrite", "dendrite", 
                                              "axon", "axon", "axon", "axon"))
bound.recycling.data$marker <- as.character(c("recycled", "internalised"))

tidy.recycling.data <- as.data.frame(cbind(treatment= c(bound.recycling.data$treatment), 
                         neuron = c(bound.recycling.data$neuron), 
                         domain = c(bound.recycling.data$domain), 
                         marker = c(bound.recycling.data$marker),
                         intensity = c(bound.recycling.data$Mean),
                         area = c(bound.recycling.data$Area)))

tidy.recycling.data$intensity <- as.numeric(as.character(tidy.recycling.data$intensity))

View(tidy.recycling.data)

#### Filtering ####

## SOMATA ##

somata <- tidy.recycling.data[ which(tidy.recycling.data$domain == 'soma'), ]
somata.rec <- tidy.recycling.data[ which(tidy.recycling.data$domain == 'soma' & tidy.recycling.data$marker == 'recycled'), ]
somata.int <- tidy.recycling.data[ which(tidy.recycling.data$domain == 'soma' & tidy.recycling.data$marker == 'internalised'), ]
somata.rec$relative <- somata.rec$intensity / somata.int$intensity


## AXONS ##

axons <- tidy.recycling.data[ which(tidy.recycling.data$domain == 'axon'), ]
axons.rec <- tidy.recycling.data[ which(tidy.recycling.data$domain == 'axon' & tidy.recycling.data$marker == 'recycled'), ]
axons.int <- tidy.recycling.data[ which(tidy.recycling.data$domain == 'axon' & tidy.recycling.data$marker == 'internalised'), ]
axons.rec$relative <- axons.rec$intensity / axons.int$intensity


## DENDRITES ##

dendrites <- tidy.recycling.data[ which(tidy.recycling.data$domain == 'dendrite'), ]
dendrites.rec <- tidy.recycling.data[ which(tidy.recycling.data$domain == 'dendrite' & tidy.recycling.data$marker == 'recycled'), ]
dendrites.int <- tidy.recycling.data[ which(tidy.recycling.data$domain == 'dendrite' & tidy.recycling.data$marker == 'internalised'), ]
dendrites.rec$relative <- dendrites.rec$intensity / dendrites.int$intensity

#### THE SUMMARY TABLES #### 

axons.tab <- data.frame(axons.int$treatment, axons.int$neuron, axons.int$intensity, axons.rec$intensity)
write.csv(axons.tab, 'recycling in axons.csv')
dendrites.tab <- data.frame(dendrites.int$treatment, dendrites.int$neuron, dendrites.int$intensity, dendrites.rec$intensity)
write.csv(dendrites.tab, 'recycling in dendrites.csv')
somata.tab <- data.frame(somata.int$treatment, somata.int$neuron, somata.int$intensity, somata.rec$intensity)
write.csv(somata.tab, 'recycling in somata.csv')



#### PLOTTING ####

ggplot(somata.rec, aes(x= treatment, y= relative)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =.5, binwidth = .03, fill= 'black') + 
  ylim(0,1) +
  labs (x= "Treatment", 
        y= "Recycled fraction (intensity ratio recycled/internalised)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Normalised recycling of TrkB in somata")

ggplot(somata.rec, aes(x= treatment, y= intensity)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1, dotsize =5, binwidth = .3, fill= 'black') + 
  ylim(0,100) +
  labs (x= "Treatment", 
        y= "Recycled (intensity)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Recycling of TrkB in somata")

ggplot(somata.int, aes(x= treatment, y= intensity)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1, dotsize =5, binwidth = .3, fill= 'black') + 
  ylim(0,100) +
  labs (x= "Treatment", 
        y= "Intensity (a.u.)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Internalisation of TrkB in somata")



ggplot(axons.rec, aes(x= treatment, y= relative)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =.5, binwidth = .03, fill= 'black') + 
  ylim(0,1) +
  labs (x= "Treatment", 
        y= "Recycled fraction (intensity ratio recycled/internalised)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Normalised recycling of TrkB in axons")

ggplot(axons.rec, aes(x= treatment, y= intensity)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1, dotsize =2, binwidth = .3, fill= 'black') + 
  ylim(0,40) +
  labs (x= "Treatment", 
        y= "Recycled (intensity)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Recycling of TrkB in axons")

ggplot(axons.int, aes(x= treatment, y= intensity)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1, dotsize =2, binwidth = .3, fill= 'black') + 
  ylim(0,40) +
  labs (x= "Treatment", 
        y= "Intensity (a.u.)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Internalisation of TrkB in axons")



ggplot(dendrites.rec, aes(x= treatment, y= relative)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =.5, binwidth = .03, fill= 'black') + 
  ylim(0,1) +
  labs (x= "Treatment", 
        y= "Recycled fraction (intensity ratio recycled/internalised)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Normalised recycling of TrkB in dendrites")

ggplot(dendrites.rec, aes(x= treatment, y= intensity)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1, dotsize =2, binwidth = .3, fill= 'black') + 
  ylim(0,40) +
  labs (x= "Treatment", 
        y= "Recycled (intensity)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Recycling of TrkB in dendrites")

ggplot(dendrites.int, aes(x= treatment, y= intensity)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1, dotsize =2, binwidth = .3, fill= 'black') + 
  ylim(0,40) +
  labs (x= "Treatment", 
        y= "Intensity (a.u.)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Internalisation of TrkB in dendrites")
































## CORRELATION between TrkB retrograde transport and Rab10 immunoreactivity

# With trendlines

ggplot(shRab10only5, aes(x= shRab10only5$intensity.Rab10 , y= shRab10only5$intensity.TrkB)) + 
  theme_scientific() +
  scale_color_discrete(limits=c("shRab10NODOX", "shRab10DOX"), name= "Treatment") +
  geom_smooth(data= DOXonly5,
              aes(x=DOXonly5$intensity.Rab10, y=DOXonly5$intensity.TrkB), 
              method=lm , 
              color="#088da5", 
              se=TRUE)+
  geom_smooth(data= NODOXonly5,
              aes(x=NODOXonly5$intensity.Rab10, y=NODOXonly5$intensity.TrkB), 
              method=lm , 
              color="#F6546A", 
              se=TRUE)+
  geom_point(aes(color= shRab10only5$treatment.Rab10))+
  ylim(0, 50) + xlim(0, 50)+
  labs (x= "Rab10 intensity (A.U.)", 
        y= "TrkB intensity (A.U)",
        colour= "Treatment") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Retrograde accumulation of TrkB as a function of Rab10 levels", "Hippocampal neurons transduced with lentivirus 48h, DOX 18 h. Accumulation assay 120 mins.")

# With no trendlines

ggplot(shRab10only5, aes(x= shRab10only5$intensity.Rab10 , y= shRab10only5$intensity.TrkB)) + 
  theme_scientific() +
  scale_color_discrete(limits=c("shRab10NODOX", "shRab10DOX"), name= "Treatment") +
  geom_point(aes(color= shRab10only5$treatment.Rab10))+
  ylim(0, 50) + xlim(0, 50)+
  labs (x= "Rab10 intensity (A.U.)", 
        y= "TrkB intensity (A.U)",
        colour= "Treatment") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Retrograde accumulation of TrkB as a function of Rab10 levels", "Hippocampal neurons transduced with lentivirus 48h, DOX 18 h. Accumulation assay 120 mins.")

## BOXPLOTS

# Effect Rab10

ggplot(shRab10only5, aes(x= treatment.TrkB, y= intensity.TrkB, fill= treatment.TrkB)) +
  theme_scientific() +
  scale_x_discrete(limits=c("shRab10NODOX", "shRab10DOX")) +
  scale_fill_discrete(limits=c("shRab10NODOX", "shRab10DOX"), name= "Treatment") +
  geom_boxplot(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 40) +
  labs (x= "Treatment", 
        y= "TrkB intensity",
        colour= "Treatment") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Effect of shRNA Rab10 on TrkB retrograde transport", "TrkB intensity in cell bodies, 120 mins accumulation")

# Treatment 3-MA

ggplot(ThreeMA, aes(x= treatment.TrkB, y= intensity.TrkB, fill= treatment.TrkB)) +
  theme_scientific() +
  scale_x_discrete(labels=c("control", "3-MA"), limits=c("shRab10NODOX", "3-MA")) +
  scale_fill_discrete(labels=c("control", "3-MA"), limits=c("shRab10NODOX", "3-MA"), name= "Treatment") +
  geom_boxplot(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 40) +
  labs (x= "Treatment", 
        y= "TrkB intensity",
        colour= "Treatment") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Effect of 3-methyladenin on TrkB retrograde transport", "TrkB intensity in cell bodies, 120 mins accumulation")

# All

ggplot(intensitydbwide5, aes(x= treatment.TrkB, y= intensity.TrkB, fill= treatment.TrkB)) +
  theme_scientific() +
  scale_x_discrete(labels=c("shRab10 NO Dox", "shRab10 + Dox", "3-MA"), limits=c("shRab10NODOX", "shRab10DOX", "3-MA")) +
  scale_fill_discrete(labels=c("shRab10 NO Dox", "shRab10 + Dox", "3-MA"), limits=c("shRab10NODOX", "shRab10DOX", "3-MA"), name= "Treatment") +
  geom_boxplot(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 40) +
  labs (x= "Treatment", 
        y= "TrkB intensity",
        colour= "Treatment") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Effect of shRNA Rab10 and 3-methyladenin on TrkB retrograde transport", "TrkB intensity in cell bodies, 120 mins accumulation")


## VIOLINPLOTS

# All

ggplot(intensitydbwide5, aes(x= treatment.TrkB, y= intensity.TrkB, fill= treatment.TrkB)) +
  theme_scientific() +
  scale_x_discrete(labels=c("shRab10 NO Dox", "shRab10 + Dox", "3-MA"), limits=c("shRab10NODOX", "shRab10DOX", "3-MA")) +
  scale_fill_discrete(labels=c("shRab10 NO Dox", "shRab10 + Dox", "3-MA"), limits=c("shRab10NODOX", "shRab10DOX", "3-MA"), name= "Treatment") +
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 40) +
  labs (x= "Treatment", 
        y= "TrkB intensity",
        colour= "Treatment") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Effect of shRNA Rab10 and 3-methyladenin on TrkB retrograde transport", "TrkB intensity in cell bodies, 120 mins accumulation")




ggplot(shRab10only5, aes(x= treatment.TrkB, y= intensity.TrkB, fill= treatment.TrkB)) +
  theme_scientific() +
  scale_x_discrete(limits=c("shRab10NODOX", "shRab10DOX")) +
  scale_fill_discrete(limits=c("shRab10NODOX", "shRab10DOX"), name= "Treatment") +
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 40) +
  labs (x= "Treatment", 
                     y= "TrkB intensity",
                     colour= "Treatment") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Effect of shRNA Rab10 on TrkB retrograde transport", "TrkB intensity in cell bodies, 120 mins accumulation")


## PLOTS efficiency of knockdown

ggplot(shRab10only5, aes(x= treatment.TrkB, y= intensity.Rab10, fill= treatment.TrkB)) + 
  theme_scientific() +
  scale_x_discrete(limits=c("shRab10NODOX", "shRab10DOX")) +
  scale_fill_discrete(limits=c("shRab10NODOX", "shRab10DOX"), name= "Treatment") +
  geom_boxplot(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 40) +
  labs (x= "Treatment", 
        y= "Rab10 intensity",
        colour= "Treatment") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Efficiency of Rab10 knock down", "Rab10 intensity in cell bodies, 48h lentivirus, 18 h DOX")



ggplot(shRab10only5, aes(x= treatment.TrkB, y= intensity.Rab10, fill= treatment.TrkB)) + 
  theme_scientific() +
  scale_x_discrete(limits=c("shRab10NODOX", "shRab10DOX")) +
  scale_fill_discrete(limits=c("shRab10NODOX", "shRab10DOX"), name= "Treatment") +
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =3, binwidth = .5, fill= 'black') + 
  ylim(0, 50) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Efficiency of Rab10 knock down", "Rab10 intensity in cell bodies, 48h lentivirus, 18 h DOX")

####PRINT IT####

pdf("colocalisation_plots.pdf", width=8, height=7)

## if you want to print the plots directly to a pdf file, put a appropriate name and place the code here

dev.off()



#### STATISTICAL ANALYSIS ####


