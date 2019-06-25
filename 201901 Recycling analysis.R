#### ENVIROMENTAL SETUP ####

# Setting the folder for this session and all its materials and data
setwd("~/Desktop/Recycling/Datatables")

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

#### Labelling and binding EGFP ####

EGFP60minNomet1$neuron <- as.numeric(1)
EGFP60minNomet2$neuron <- as.numeric(2)
EGFP60minNomet3$neuron <- as.numeric(3)
EGFP60minNomet4$neuron <- as.numeric(4)
EGFP60minNomet5$neuron <- as.numeric(5)
EGFP60minNomet6$neuron <- as.numeric(6)
EGFP60minNomet7$neuron <- as.numeric(7)
EGFP60minNomet8$neuron <- as.numeric(8)
EGFP60minNomet9$neuron <- as.numeric(9)

EGFP60minRECYCLING2$neuron <- as.numeric(10)
EGFP60minRECYCLING3$neuron <- as.numeric(11)
EGFP60minRECYCLING4$neuron <- as.numeric(12)
EGFP60minRECYCLING5$neuron <- as.numeric(13)
EGFP60minRECYCLING6$neuron <- as.numeric(14)
EGFP60minRECYCLING7$neuron <- as.numeric(15)
EGFP60minRECYCLING8$neuron <- as.numeric(16)

EGFP60min.exp1 <- rbind(EGFP60minNomet1,
                   EGFP60minNomet2,
                   EGFP60minNomet3,
                   EGFP60minNomet4,
                   EGFP60minNomet5,
                   EGFP60minNomet6,
                   EGFP60minNomet7,
                   EGFP60minNomet8,
                   EGFP60minNomet9)

EGFP60min.exp2 <- rbind(EGFP60minRECYCLING2,
                   EGFP60minRECYCLING3,
                   EGFP60minRECYCLING4,
                   EGFP60minRECYCLING5,
                   EGFP60minRECYCLING6,
                   EGFP60minRECYCLING7,
                   EGFP60minRECYCLING8)

EGFP60min.exp1$treatment <- as.character(c("EGFP"))
EGFP60min.exp2$treatment <- as.character(c("EGFP"))

#### Labelling and binding Rab10CA ####

Rab10CA60minNomet1$neuron <- as.numeric(1)
Rab10CA60minNomet2$neuron <- as.numeric(2)
Rab10CA60minNomet3$neuron <- as.numeric(3)
Rab10CA60minNomet4$neuron <- as.numeric(4)
Rab10CA60minNomet5$neuron <- as.numeric(5)
Rab10CA60minNomet6$neuron <- as.numeric(6)
Rab10CA60minNomet7$neuron <- as.numeric(7)

Rab10CA60minRECYCLING1$neuron <- as.numeric(8)
Rab10CA60minRECYCLING2$neuron <- as.numeric(9)
Rab10CA60minRECYCLING3$neuron <- as.numeric(10)
Rab10CA60minRECYCLING4$neuron <- as.numeric(11)
Rab10CA60minRECYCLING5$neuron <- as.numeric(12)
Rab10CA60minRECYCLING6$neuron <- as.numeric(13)
Rab10CA60minRECYCLING7$neuron <- as.numeric(14)
Rab10CA60minRECYCLING8$neuron <- as.numeric(15)
Rab10CA60minRECYCLING9$neuron <- as.numeric(16)
Rab10CA60minRECYCLING10$neuron <- as.numeric(17)

Rab10CA60min.exp1 <- rbind(Rab10CA60minNomet1,
                           Rab10CA60minNomet2,
                           Rab10CA60minNomet3,
                           Rab10CA60minNomet4,
                           Rab10CA60minNomet5,
                           Rab10CA60minNomet6,
                           Rab10CA60minNomet7)

Rab10CA60min.exp2 <- rbind(Rab10CA60minRECYCLING1,
                           Rab10CA60minRECYCLING2,
                           Rab10CA60minRECYCLING3,
                           Rab10CA60minRECYCLING4,
                           Rab10CA60minRECYCLING5,
                           Rab10CA60minRECYCLING6,
                           Rab10CA60minRECYCLING7,
                           Rab10CA60minRECYCLING8,
                           Rab10CA60minRECYCLING9,
                           Rab10CA60minRECYCLING10)

Rab10CA60min.exp1$treatment <- as.character(c("Rab10CA"))
Rab10CA60min.exp2$treatment <- as.character(c("Rab10CA"))

#### Labelling and binding Rab10DN ####

Rab10DN60minNomet1$neuron <- as.numeric(1)
Rab10DN60minNomet2$neuron <- as.numeric(2)
Rab10DN60minNomet3$neuron <- as.numeric(3)
Rab10DN60minNomet4$neuron <- as.numeric(4)
Rab10DN60minNomet5$neuron <- as.numeric(5)
Rab10DN60minNomet6$neuron <- as.numeric(6)
Rab10DN60minNomet7$neuron <- as.numeric(7)
Rab10DN60minNomet8$neuron <- as.numeric(8)
Rab10DN60minNomet9$neuron <- as.numeric(9)

Rab10DN60minRECYCLING1$neuron <- as.numeric(10)
Rab10DN60minRECYCLING2$neuron <- as.numeric(11)
Rab10DN60minRECYCLING3$neuron <- as.numeric(12)
Rab10DN60minRECYCLING4$neuron <- as.numeric(13)
Rab10DN60minRECYCLING5$neuron <- as.numeric(14)
Rab10DN60minRECYCLING6$neuron <- as.numeric(15)
Rab10DN60minRECYCLING7$neuron <- as.numeric(16)
Rab10DN60minRECYCLING8$neuron <- as.numeric(17)
Rab10DN60minRECYCLING9$neuron <- as.numeric(18)


Rab10DN60min.exp1 <- rbind(Rab10DN60minNomet1,
                      Rab10DN60minNomet2,
                      Rab10DN60minNomet3,
                      Rab10DN60minNomet4,
                      Rab10DN60minNomet5,
                      Rab10DN60minNomet6,
                      Rab10DN60minNomet7,
                      Rab10DN60minNomet8,
                      Rab10DN60minNomet9)

Rab10DN60min.exp2 <- rbind(Rab10DN60minRECYCLING1,
                      Rab10DN60minRECYCLING2,
                      Rab10DN60minRECYCLING3,
                      Rab10DN60minRECYCLING4,
                      Rab10DN60minRECYCLING5,
                      Rab10DN60minRECYCLING6,
                      Rab10DN60minRECYCLING7,
                      Rab10DN60minRECYCLING8,
                      Rab10DN60minRECYCLING9)

Rab10DN60min.exp1$treatment <- as.character(c("Rab10DN"))
Rab10DN60min.exp2$treatment <- as.character(c("Rab10DN"))

#### Sorting, filtering and normalising EXPERIMENT 1 ####

bound.exp1 <- rbind(EGFP60min.exp1,
                              Rab10CA60min.exp1,
                              Rab10DN60min.exp1)

bound.exp1$domain <- as.character(c("soma", "soma", 
                                              "dendrite","dendrite", "dendrite", "dendrite", 
                                              "axon", "axon", "axon", "axon"))
bound.exp1$marker <- as.character(c("recycled", "internalised"))

tidy.exp1 <- as.data.frame(cbind(treatment= c(bound.exp1$treatment), 
                         neuron = c(bound.exp1$neuron), 
                         domain = c(bound.exp1$domain), 
                         marker = c(bound.exp1$marker),
                         intensity = c(bound.exp1$Mean),
                         area = c(bound.exp1$Area)))

tidy.exp1$intensity <- as.numeric(as.character(tidy.exp1$intensity))

View(tidy.exp1)

## somata ##

soma.rec.1 <- tidy.exp1[ which(tidy.exp1$domain == 'soma' & tidy.exp1$marker == 'recycled'), ]
soma.int.1 <- tidy.exp1[ which(tidy.exp1$domain == 'soma' & tidy.exp1$marker == 'internalised'), ]

soma.rec.EGFP.1 <- soma.rec.1[ which(soma.rec.1$treatment == 'EGFP'), ]
av.soma.rec.EGFP.1 <- mean(soma.rec.EGFP.1$intensity)
soma.rec.1$normalised <- soma.rec.1$intensity / av.soma.rec.EGFP.1

soma.int.EGFP.1 <- soma.int.1[ which(soma.int.1$treatment == 'EGFP'), ]
av.soma.int.EGFP.1 <- mean(soma.int.EGFP.1$intensity)
soma.int.1$normalised <- soma.int.1$intensity / av.soma.int.EGFP.1

## axons ##

axon.rec.1 <- tidy.exp1[ which(tidy.exp1$domain == 'axon' & tidy.exp1$marker == 'recycled'), ]
axon.int.1 <- tidy.exp1[ which(tidy.exp1$domain == 'axon' & tidy.exp1$marker == 'internalised'), ]

axon.rec.EGFP.1 <- axon.rec.1[ which(axon.rec.1$treatment == 'EGFP'), ]
av.axon.rec.EGFP.1 <- mean(axon.rec.EGFP.1$intensity)
axon.rec.1$normalised <- axon.rec.1$intensity / av.axon.rec.EGFP.1

axon.int.EGFP.1 <- axon.int.1[ which(axon.int.1$treatment == 'EGFP'), ]
av.axon.int.EGFP.1 <- mean(axon.int.EGFP.1$intensity)
axon.int.1$normalised <- axon.int.1$intensity / av.axon.int.EGFP.1

## dendrites ##

dendrite.rec.1 <- tidy.exp1[ which(tidy.exp1$domain == 'dendrite' & tidy.exp1$marker == 'recycled'), ]
dendrite.int.1 <- tidy.exp1[ which(tidy.exp1$domain == 'dendrite' & tidy.exp1$marker == 'internalised'), ]

dendrite.rec.EGFP.1 <- dendrite.rec.1[ which(dendrite.rec.1$treatment == 'EGFP'), ]
av.dendrite.rec.EGFP.1 <- mean(dendrite.rec.EGFP.1$intensity)
dendrite.rec.1$normalised <- dendrite.rec.1$intensity / av.dendrite.rec.EGFP.1

dendrite.int.EGFP.1 <- dendrite.int.1[ which(dendrite.int.1$treatment == 'EGFP'), ]
av.dendrite.int.EGFP.1 <- mean(dendrite.int.EGFP.1$intensity)
dendrite.int.1$normalised <- dendrite.int.1$intensity / av.dendrite.int.EGFP.1


#### Sorting, filtering and normalising EXPERIMENT 2 ####

bound.exp2 <- rbind(EGFP60min.exp2,
                    Rab10CA60min.exp2,
                    Rab10DN60min.exp2)

bound.exp2$domain <- as.character(c("soma", "soma", 
                                    "dendrite","dendrite", "dendrite", "dendrite", 
                                    "axon", "axon", "axon", "axon"))
bound.exp2$marker <- as.character(c("recycled", "internalised"))

tidy.exp2 <- as.data.frame(cbind(treatment= c(bound.exp2$treatment), 
                                 neuron = c(bound.exp2$neuron), 
                                 domain = c(bound.exp2$domain), 
                                 marker = c(bound.exp2$marker),
                                 intensity = c(bound.exp2$Mean),
                                 area = c(bound.exp2$Area)))

tidy.exp2$intensity <- as.numeric(as.character(tidy.exp2$intensity))

View(tidy.exp2)

## somata ##

soma.rec.2 <- tidy.exp2[ which(tidy.exp2$domain == 'soma' & tidy.exp2$marker == 'recycled'), ]
soma.int.2 <- tidy.exp2[ which(tidy.exp2$domain == 'soma' & tidy.exp2$marker == 'internalised'), ]

soma.rec.EGFP.2 <- soma.rec.2[ which(soma.rec.2$treatment == 'EGFP'), ]
av.soma.rec.EGFP.2 <- mean(soma.rec.EGFP.2$intensity)
soma.rec.2$normalised <- soma.rec.2$intensity / av.soma.rec.EGFP.2

soma.int.EGFP.2 <- soma.int.2[ which(soma.int.1$treatment == 'EGFP'), ]
av.soma.int.EGFP.2 <- mean(soma.int.EGFP.2$intensity)
soma.int.2$normalised <- soma.int.2$intensity / av.soma.int.EGFP.2

## axons ##

axon.rec.2 <- tidy.exp2[ which(tidy.exp2$domain == 'axon' & tidy.exp2$marker == 'recycled'), ]
axon.int.2 <- tidy.exp2[ which(tidy.exp2$domain == 'axon' & tidy.exp2$marker == 'internalised'), ]

axon.rec.EGFP.2 <- axon.rec.2[ which(axon.rec.2$treatment == 'EGFP'), ]
av.axon.rec.EGFP.2 <- mean(axon.rec.EGFP.2$intensity)
axon.rec.2$normalised <- axon.rec.2$intensity / av.axon.rec.EGFP.2

axon.int.EGFP.2 <- axon.int.2[ which(axon.int.2$treatment == 'EGFP'), ]
av.axon.int.EGFP.2 <- mean(axon.int.EGFP.2$intensity)
axon.int.2$normalised <- axon.int.2$intensity / av.axon.int.EGFP.2

## dendrites ##

dendrite.rec.2 <- tidy.exp2[ which(tidy.exp2$domain == 'dendrite' & tidy.exp2$marker == 'recycled'), ]
dendrite.int.2 <- tidy.exp2[ which(tidy.exp2$domain == 'dendrite' & tidy.exp2$marker == 'internalised'), ]

dendrite.rec.EGFP.2 <- dendrite.rec.2[ which(dendrite.rec.2$treatment == 'EGFP'), ]
av.dendrite.rec.EGFP.2 <- mean(dendrite.rec.EGFP.2$intensity)
dendrite.rec.2$normalised <- dendrite.rec.2$intensity / av.dendrite.rec.EGFP.2

dendrite.int.EGFP.2 <- dendrite.int.2[ which(dendrite.int.2$treatment == 'EGFP'), ]
av.dendrite.int.EGFP.2 <- mean(dendrite.int.EGFP.2$intensity)
dendrite.int.2$normalised <- dendrite.int.2$intensity / av.dendrite.int.EGFP.2


#### BINDING all the experiments ####

## SOMA ##

soma.rec <- rbind(soma.rec.1,
                  soma.rec.2)
soma.int <- rbind(soma.int.1,
                  soma.int.2)
soma.rec$relative <- soma.rec$normalised / soma.int$normalised

## AXON ##

axon.rec <- rbind(axon.rec.1,
                  axon.rec.2)
axon.int <- rbind(axon.int.1,
                  axon.int.2)
axon.rec$relative <- axon.rec$normalised / axon.int$normalised

## DENDRITE ##

dendrite.rec <- rbind(dendrite.rec.1,
                  dendrite.rec.2)
dendrite.int <- rbind(dendrite.int.1,
                  dendrite.int.2)
dendrite.rec$relative <- dendrite.rec$normalised / dendrite.int$normalised



#### THE SUMMARY TABLES #### 


axon.tab <- as.data.frame(cbind('treatment'= as.character(axon.int$treatment), 
                  'neuron' = (axon.int$neuron), 
                  'raw internalisation' = (axon.int$intensity), 
                  'raw recycling' = c(axon.rec$intensity),
                  'normalised internalisation' = c(axon.int$normalised),
                  'normalised recycling' = c(axon.rec$normalised),
                  'corrected recycling' = c(axon.rec$relative)))
write.csv(axon.tab, 'recycling in axons.csv')

soma.tab <- as.data.frame(cbind('treatment'= as.character(soma.int$treatment), 
                                'neuron' = (soma.int$neuron), 
                                'raw internalisation' = (soma.int$intensity), 
                                'raw recycling' = c(soma.rec$intensity),
                                'normalised internalisation' = c(soma.int$normalised),
                                'normalised recycling' = c(soma.rec$normalised),
                                'corrected recycling' = c(soma.rec$relative)))
write.csv(soma.tab, 'recycling in somata.csv')

dendrite.tab <- as.data.frame(cbind('treatment'= as.character(dendrite.int$treatment), 
                                'neuron' = (dendrite.int$neuron), 
                                'raw internalisation' = (dendrite.int$intensity), 
                                'raw recycling' = c(dendrite.rec$intensity),
                                'normalised internalisation' = c(dendrite.int$normalised),
                                'normalised recycling' = c(dendrite.rec$normalised),
                                'corrected recycling' = c(dendrite.rec$relative)))
write.csv(dendrite.tab, 'recycling in dendrites.csv')



#### PLOTTING ####

ggplot(soma.rec, aes(x= treatment, y= relative)) +
  theme_scientific() +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =2, binwidth = .03, fill= 'black') + 
  ylim(0,3) +
  labs (x= "Treatment", 
        y= "Recycling (ratio recycled/internalised)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Normalised recycling of TrkB-flag in somata")


ggplot(soma.rec, aes(x= treatment, y= relative)) +
  theme_scientific() +
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =1, binwidth = .03, fill= 'black') + 
  ylim(0,2.5) +
  labs (x= "Treatment", 
        y= "Recycling (ratio recycled/internalised)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Normalised recycling of TrkB-flag in somata")


ggplot(soma.int, aes(x= treatment, y= normalised)) +
  theme_scientific() +
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =2, binwidth = .03, fill= 'black') + 
  ylim(0,5) +
  labs (x= "Treatment", 
        y= "Raw internalisation (normalised to EGFP)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Internalisation of TrkB-flag in somata")





ggplot(axon.rec, aes(x= treatment, y= relative)) +
  theme_scientific() +
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =1, binwidth = .03, fill= 'black') + 
  ylim(0,2.5) +
  labs (x= "Treatment", 
        y= "Recycling (ratio recycled/internalised)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Normalised recycling of TrkB-flag in axons")


ggplot(axon.int, aes(x= treatment, y= normalised)) +
  theme_scientific() +
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =2, binwidth = .03, fill= 'black') + 
  ylim(0,5) +
  labs (x= "Treatment", 
        y= "Raw internalisation (normalised to EGFP)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Internalisation of TrkB-flag in axons")








ggplot(dendrite.rec, aes(x= treatment, y= relative)) +
  theme_scientific() +
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =1, binwidth = .03, fill= 'black') + 
  ylim(0,2.5) +
  labs (x= "Treatment", 
        y= "Recycling (ratio recycled/internalised)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Normalised recycling of TrkB-flag in dendrites")


ggplot(dendrite.int, aes(x= treatment, y= normalised)) +
  theme_scientific() +
  geom_violin(size=.3) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.1, dotsize =2, binwidth = .03, fill= 'black') + 
  ylim(0,5) +
  labs (x= "Treatment", 
        y= "Raw internalisation (normalised to EGFP)") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x =element_text (size = 13),
        legend.text= element_text (size = 13),
        plot.title = element_text(hjust=0)) +
  ggtitle("Internalisation of TrkB-flag in dendrites")


















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


