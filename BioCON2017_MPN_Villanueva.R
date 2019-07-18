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
LECA_Nen_Monoculture_2<-read.csv("MPN_Analysis_LECA_Nen_Monoculture_2_TEST.csv")
LECA_Nam_Monoculture_2<-read.csv("MPN_Analysis_LECA_Nam_Monoculture_2_TEST.csv")
LUPE_Nen_Monoculture_2<-read.csv("MPN_Analysis_LUPE_Nen_Monoculture_2_TEST.csv")
LUPE_Nam_Monoculture_2<-read.csv("MPN_Analysis_LUPE_Nam_Monoculture_2_TEST.csv")


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
MPN_Averages<-MPN_Data%>%
  group_by(Species,N_Treatment,Diversity)%>%
  summarise(MPN_Avg=mean(MPN),LogMPN_Avg=mean(LogMPN))%>%
  ungroup

MPN_Summary<-MPN_Data%>%
  #Group data by the columns "Watershed" and "exclosure"
  group_by(Species,N_Treatment,Diversity)%>%
  #In this data frame, summarize the data.  Make a new column named "Richness_Std" and calculate the standard deviation from the column "Richness".  Also calculate the mean and length from the column "Richness" and place them into their own columns.
  summarize(MPN_Std=sd(MPN),MPN_Mean=mean(MPN),MPN_n=length(MPN),LogMPN_Std=sd(LogMPN),LogMPN_Mean=mean(LogMPN),LogMPN_n=length(LogMPN))%>%
  #Make a new column called "Richness_St_Error" and divide "Richness_Std" by the square root of "Richness_n"
  mutate(MPN_St_Error=MPN_Std/sqrt(MPN_n),LogMPN_St_Error=LogMPN_Std/sqrt(LogMPN_n))%>%
  ungroup

t.test(MPN_Summary$MPN_Mean~MPN_Summary$Species)
t.test(MPN_Summary$MPN_Mean~MPN_Summary$N_Treatment)


#### Graphs ####

#### MPN by N treatment Bar Graph
#png(filename="MPN by N Treatment", width =6 ,height =6,units = 'in' ,res =300 )
#Use data from "MPN_Summary".  Change the aesthetics x is equal to the data from "exlosure", and y is equal to the "Richness_Mean"
ggplot(MPN_Summary,aes(x=N_Treatment,y=MPN_Mean, fill=Species))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.   -BW
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("lightcyan3","cadetblue4"))+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=MPN_Mean-MPN_St_Error,ymax=MPN_Mean+MPN_St_Error),position=position_dodge(0.9),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Nitrogen Treatment")+
  #Label the y-axis "Species Richness"
  ylab(bquote("Density of Soil Rhizobia (per g)"))+
  scale_x_discrete(labels=c("LECA"="Lespedeza Capitata","LUPE"="Lupinus Perennis"))+
  #Make the y-axis extend to 50
  expand_limits(y=100)+
  annotate("text",x=1,y=95,label="A",size=15)+
  #Add "B" to the graph in size 6 at position 2,13.5
  annotate("text",x=2,y=90,label="B",size=15)
#dev.off()
#Save at the graph at 1400x1500

#### LogMPN by N treatment Bar Graph
#png(filename="MPN by N Treatment", width =6 ,height =6,units = 'in' ,res =300 )
#Use data from "MPN_Summary".  Change the aesthetics x is equal to the data from "exlosure", and y is equal to the "Richness_Mean"
ggplot(MPN_Summary,aes(x=N_Treatment,y=LogMPN_Mean, fill=Species))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.   -BW
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("lightcyan3","cadetblue4"))+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=LogMPN_Mean-LogMPN_St_Error,ymax=LogMPN_Mean+LogMPN_St_Error),position=position_dodge(0.9),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Nitrogen Treatment")+
  #Label the y-axis "Species Richness"
  ylab(bquote("Log Density of Soil Rhizobia (per g)"))+
  scale_x_discrete(labels=c("LECA"="Lespedeza Capitata","LUPE"="Lupinus Perennis"))+
  #Make the y-axis extend to 50
  expand_limits(y=3)+
  annotate("text",x=1,y=2.5,label="A",size=15)+
  #Add "B" to the graph in size 6 at position 2,13.5
  annotate("text",x=2,y=2.5,label="B",size=15)
#dev.off()
#Save at the graph at 1400x1500


#### MPN by N treatment Box Plot
MPN_Data$col.nm<-ifelse(MPN_Data$Species=="LECA", yes="blue", no="red")
ggplot(MPN_Data, aes(x=N_Treatment , y=LogMPN, fill=Species))+ #feed in data, x, y, and variable you want to color boxes by (if desired)
  #annotate("rect",xmin=1-.5,xmax=3+.5,ymin=1,ymax=5,alpha=.2,fill="blue")+ #These color the background region of the plotting area different colors
  #annotate("rect",xmin=3-.5,xmax=7+.5,ymin=1,ymax=5,alpha=.2,fill="red")+
  geom_boxplot(notch=F)+ #Can delete the second argument if not coloring the boxes. I have color names (DATA$col.nm) as a column in the dataframe here
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #removes background gridlines
        panel.background = element_blank(), axis.line = element_line(colour = "black"), #makes white background with black axes
        axis.title.x=element_text(size=20), axis.text.x=element_text(size=20, colour="black"), #Increases axis text size
        axis.title.y=element_text(size=20), axis.text.y=element_text(colour="Black", size=16), #increases axis text size
        axis.line.y=element_line(color="black", size=1),axis.line.x=element_line(color="black", size=1), #Changes axis thickness etc.
        plot.title=element_text(size=20), legend.position="none")+ #Sets text size for title (not necessary) and removes legend
  labs(title="",x="Nitrogen Treatment",y="MPN")+ #Sets axis titles
  scale_x_discrete(labels=c("Ambient Nitrogen","Enriched Nitrogen"))+ #Sets labels for groups along X axis
  annotate("text", x=c(.8,1.2,1.8,2.2), y=c(2.2), label=c("a","a", "a","b"), size=c(5), colour="black") #Puts in letters of significance from ANOVA
#ggtitle("b)")+ #puts in the panel label. can remove if not for a multipanel figure
#theme(plot.title=element_text(size=30,hjust=-.18)) #sets size and position (top left) of panel label














