#### 2019 REU Project - Alitzel Villanueva - BioCON 2017 MPN Experiment ####
#Coders: Aliztel Villanueva and Kathryn Bloodworth
#Date Started: 07/17/2019
#Purpose: Analyze MPN data and make graphs for Alitzel's intern presentation

#### Set Working Directory ####

setwd("/Users/bloodworthk/Dropbox (Smithsonian)/SERC Ecosystem Conservation/Projects/Interns/2019/2019_REU_Villanueva/MPN2018/MPN_data_analysis/Test_Data")

#### Load and install libraries ####
#install.packages("vegan")
library(vegan)
#install.packages("devtools")
library(devtools)
#install.packages("tidyverse")
library(tidyverse)

#### Set ggplot2 theme ####
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=40, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=40), axis.title.y=element_text(size=40, angle=90, vjust=0.5,
                                                                          margin=margin(r=15)), axis.text.y=element_text(size=40), plot.title =
               element_text(size=40, vjust=2), panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),
             legend.text=element_text(size=30))

#### Read in Data and create usable data sheet ####

#Read in the data sheet to new data frame
LECA_Nen_Monoculture_1<-read.csv("MPN_Analysis_LECA_Nen_Monoculture_TEST.csv")
LECA_Nam_Monoculture_1<-read.csv("MPN_Analysis_LECA_Nam_Monoculture_TEST.csv")
LUPE_Nen_Monoculture_1<-read.csv("MPN_Analysis_LUPE_Nen_Monoculture_TEST.csv")
LUPE_Nam_Monoculture_1<-read.csv("MPN_Analysis_LUPE_Nam_Monoculture_TEST.csv")
LECA_Nen_Monoculture_2<-read.csv("MPN_Analysis_LECA_Nen_Monoculture_TEST.csv")
LECA_Nam_Monoculture_2<-read.csv("MPN_Analysis_LECA_Nam_Monoculture_TEST.csv")
LUPE_Nen_Monoculture_2<-read.csv("MPN_Analysis_LUPE_Nen_Monoculture_TEST.csv")
LUPE_Nam_Monoculture_2<-read.csv("MPN_Analysis_LUPE_Nam_Monoculture_TEST.csv")


####
#Create a new data frame with vector that have all treatment combinations, all MPN data (must be in the same order as treatment data), and all LogMPN data (must be in the same order as treatment data) 
MPN_Data<-data.frame("Treatment"=c("LECA_Nen_Monoculture_1","LECA_Nam_Monoculture_1","LUPE_Nen_Monoculture_1","LUPE_Nam_Monoculture_1","LECA_Nen_Monoculture_2","LECA_Nam_Monoculture_2","LUPE_Nen_Monoculture_2","LUPE_Nam_Monoculture_2"),
                     "MPN"=c(as.numeric(as.vector(LECA_Nen_Monoculture_1[1,"X.1"])),as.numeric(as.vector(LECA_Nam_Monoculture_1[1,"X.1"])),as.numeric(as.vector(LUPE_Nen_Monoculture_1[1,"X.1"])),as.numeric(as.vector(LUPE_Nam_Monoculture_1[1,"X.1"])),as.numeric(as.vector(LECA_Nen_Monoculture_2[1,"X.1"])),as.numeric(as.vector(LECA_Nam_Monoculture_2[1,"X.1"])),as.numeric(as.vector(LUPE_Nen_Monoculture_2[1,"X.1"])),as.numeric(as.vector(LUPE_Nam_Monoculture_2[1,"X.1"]))),
                     "LogMPN"=c(as.numeric(as.vector(LECA_Nen_Monoculture_1[2,"X.1"])),as.numeric(as.vector(LECA_Nam_Monoculture_1[2,"X.1"])),as.numeric(as.vector(LUPE_Nen_Monoculture_1[2,"X.1"])),as.numeric(as.vector(LUPE_Nam_Monoculture_1[2,"X.1"])),as.numeric(as.vector(LECA_Nen_Monoculture_2[2,"X.1"])),as.numeric(as.vector(LECA_Nam_Monoculture_2[2,"X.1"])),as.numeric(as.vector(LUPE_Nen_Monoculture_2[2,"X.1"])),as.numeric(as.vector(LUPE_Nam_Monoculture_2[2,"X.1"]))))%>%
  separate(Treatment,c("Species","N_Treatment","Diversity","Rep_Number"),sep="_")


# Easier way to create above data frame? Ben is working on this
#obvec<-c(LECA_Nen_Monoculture,LECA_Nam_Monoculture,LUPE_Nen_Monoculture,LUPE_Nam_Monoculture)
#output<-NULL
#for(i in 1:length(obvec)){
#  temp<-as.data.frame(obvec[i])
#  df<-data.frame("MPN"=temp[1,"X.1"],
#                 "LogMPN"=temp[2,"X.1"])
# output<-cbind(output,df) 
#}


#### Statistical Analyses ####



















