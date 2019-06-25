#### ENVIROMENTAL SETUP ####

# Setting the folder for this session and all its materials and data
setwd("~/Desktop/shRab10 accumulation assay/correlation Rab10 TrkB")

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

# Reading the file with my tidy data and making it a variable to easily work with it.
# I'm calling the variable "intensitydb"
intensitydb <- read.csv(file = "TrkB retrograde accumulation — Rab10 and TrkB intensities.csv", header = TRUE)
intensitydb

# View to explore if it has header and it is displayed correctly
View(intensitydb)

#### WIDENING the dataset ####
unicell <- paste(intensitydb$exp, intensitydb$treatment, intensitydb$cell, sep = "")
intensitydb$unicell <- unicell
intensitydbwide <- reshape(intensitydb, idvar = "unicell", timevar = "staining", direction = "wide")

View(intensitydbwide)

#### SUBSETTING what I want to show ####

shRab10only5 <- intensitydbwide %>% filter(., exp.Rab10 == "5", treatment.Rab10 != "3-MA")
intensitydbwide5 <- intensitydbwide %>% filter(., exp.Rab10 == "5") 
DOXonly5 <- intensitydbwide %>% filter(., exp.Rab10 == "5", treatment.Rab10 == "shRab10DOX")
NODOXonly5 <- intensitydbwide %>% filter(., exp.Rab10 == "5", treatment.Rab10 == "shRab10NODOX")
ThreeMA <- intensitydbwide %>% filter(., exp.Rab10 == "5", treatment.Rab10 != "shRab10DOX")

shRab10only <- intensitydbwide %>% filter(., treatment.Rab10 != "naive", treatment.Rab10 != "mChRab10", traetment.Rab10 != "3-MA")
DOXonly <- intensitydbwide %>% filter(., treatment.Rab10 == "shRab10DOX")
NODOXonly <- intensitydbwide %>% filter(., treatment.Rab10 == "shRab10NODOX")
ThreeMAonly <- intensitydbwide %>% filter(., treatment.Rab10 == "3-MA")
NOnaive <- intensitydbwide %>% filter(., treatment.Rab10 != "naive")

#### PLOTTING ####

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


