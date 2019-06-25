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
intensitydb <- read.csv(file = "shRNA Rab10 in HN MFC.csv", header = TRUE)
controldb <- read.csv(file = "control in HN MFC.csv", header = TRUE)

retrotrkb <- read.csv(file = "Normalised internalisation TrkB.csv", header = TRUE)
rab10exp <- read.csv(file = "Normalised expression Rab10.csv", header = TRUE)

intensitydbnorm <- read.csv(file = "shRNA Rab10 in HN MFC norm.csv", header = TRUE)
controldbnorm <- read.csv(file = "control in HN MFC norm.csv", header = TRUE)
intensitydb

# View to explore if it has header and it is displayed correctly
View(intensitydb)
View(intensitydbnorm)


#### WIDENING the dataset ####
intensitydbwide <- reshape(intensitydb, idvar = "cell", timevar = "label", direction = "wide")
controldbwide <- reshape(controldb, idvar = "cell", timevar = "label", direction = "wide")

iwnorm <- reshape(intensitydbnorm, idvar = "cell", timevar = "label", direction = "wide")
cwnorm <- reshape(controldbnorm, idvar = "cell", timevar = "label", direction = "wide")


View(controldbwide)

#### SUBSETTING the not-background ####

iw_nobackground <- intensitydbwide %>% filter(., X.TrkB != "background")
cw_nobackground <- controldbwide %>% filter(., X.TrkB != "background")

nobackiwnorm <- iwnorm %>% filter(., X.TrkB != "background")
nobackcwnorm <- cwnorm %>% filter(., X.TrkB != "background")



#### PLOTTING ####

## Correlation between Rab10 expression and TrkB in shRNA Rab10 expressing neurons. 
## Notice that correlation of Rab10 and WIPI2 is quite low in the same cells.

ggplot(intensitydbwide, aes(x= intensitydbwide$mean.Rab10 , y= intensitydbwide$mean.TrkB)) + 
  geom_point(aes(color= intensitydbwide$X.TrkB))+
  scale_fill_manual(values= pal ("economist"))+
  ylim(0, 30) + xlim(0, 8)+
  geom_smooth(data= iw_nobackground,
              aes(x=iw_nobackground$mean.Rab10, y=iw_nobackground$mean.TrkB), 
              method=lm , 
              color="red", 
              se=TRUE)+
  labs (x= "Rab10 intensity (A.U.)", 
        y= "TrkB intensity (A.U)",
        colour= "Fields") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13)) +
  ggtitle("Retrograde accumulation of TrkB as a function of Rab10 levels", "Hippocampal neurons treated with shRNA Rab10. Accumulation assay 120 mins.")

ggplot(intensitydbwide, aes(x= intensitydbwide$mean.Rab10 , y= intensitydbwide$mean.WIPI2b)) + 
  geom_point(aes(color= intensitydbwide$X.TrkB))+
  scale_fill_manual(values= pal ("economist"))+
  ylim(0, 30) + xlim(0, 8)+
  labs (x= "Rab10 intensity (A.U.)", 
        y= "WIPI2b intensity (A.U)",
        colour= "Fields") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13)) +
  ggtitle("Expression of WIPI2b as a function of Rab10 levels", "Hippocampal neurons treated with shRNA Rab10 (same cells accumulation assay)")

## Comparing to WIPI2b to see if I lost the correlation between internalisation of TrkB and expression of WIPI2b

ggplot(iwnorm, aes(x= mean.WIPI2b , y= mean.TrkB)) + 
  geom_point(aes(color= X.TrkB))+
  scale_fill_manual(values= pal ("economist"))+
  ylim(0, 25) + xlim(0, 75)+
  geom_smooth(data= nobackiwnorm,
              aes(x= nobackiwnorm$mean.WIPI2b, y= nobackiwnorm$mean.TrkB), 
              method=lm , 
              color="red", 
              se=TRUE)+
  labs (x= "WIPI2b intensity (A.U.)", 
        y= "TrkB intensity (A.U)",
        colour= "Fields")+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13)) +
  ggtitle("Retrograde accumulation of TrkB as a function of WIPI2b levels", "Hippocampal neurons treated with shRNA Rab10. Accumulation assay 120 mins.")

ggplot(cwnorm, aes(x= mean.WIPI2b , y= mean.TrkB)) + 
  geom_point(aes(color= X.TrkB))+
  scale_fill_manual(values= pal ("economist"))+
  ylim(0, 25) + xlim(0, 75)+
  geom_smooth(data= nobackcwnorm,
              aes(x= nobackcwnorm$mean.WIPI2b, y=nobackcwnorm$mean.TrkB), 
              method=lm , 
              color="red", 
              se=TRUE)+
  labs (x= "WIPI2b intensity (A.U.)", 
        y= "TrkB intensity (A.U)",
        colour= "Fields")+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13)) +
  ggtitle("Retrograde accumulation of TrkB as a function of WIPI2b levels", "Intact hippocampal neurons. Accumulation assay 120 mins.")



## Boxplot and violin of the TrkB intensity in cell bodies

ggplot(retrotrkb, aes(x= treatment, y= normintensity, fill= treatment)) + 
  geom_boxplot(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 25) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13)) +
  ggtitle("Effect of shRNA Rab10 on TrkB retrograde transport", "Normalised TrkB intensity in cell bodies, 120 mins accumulation")

ggplot(retrotrkb, aes(x= treatment, y= normintensity, fill= treatment)) + 
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 25) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13)) +
  ggtitle("Effect of shRNA Rab10 on TrkB retrograde transport", "Normalised TrkB intensity in cell bodies, 120 mins accumulation")


## Boxplot and violin of the Rab10 intensity in cell bodies

ggplot(rab10exp, aes(x= treatment, y= normintensity, fill= treatment)) + 
  geom_boxplot(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 25) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13)) +
  ggtitle("Effect of shRNA Rab10 on Rab10 levels", "Normalised Rab10 intensity in cell bodies")

ggplot(rab10exp, aes(x= treatment, y= normintensity, fill= treatment)) + 
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize =1, binwidth = .5, fill= 'black') + 
  ylim(0, 25) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13)) +
  ggtitle("Effect of shRNA Rab10 on Rab10 levels", "Normalised Rab10 intensity in cell bodies")




####PRINT IT####

pdf("colocalisation_plots.pdf", width=8, height=7)

## if you want to print the plots directly to a pdf file, put a appropriate name and place the code here

dev.off()



#### STATISTICAL ANALYSIS ####


