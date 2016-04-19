#script to give figures showing the percentage cover 
#of different species > 10years in age for each year, for each landis scenario

#load packages
library(raster)
library(rgdal)
library(dplyr)
library(plyr)
library(ggplot2)
library(scales)

#clear objects
rm(list=ls())

#get file names for all .img files stored in this directory
File_names<-list.files(pattern="*.img",recursive=T)
#remove any files from the the list that are not useful
#e.g. biomass, species richness, minimum age, biomass, or others
File_names<-File_names[!grepl("AGE-MAX",File_names)]
File_names<-File_names[!grepl("SPP-RICH",File_names)]
File_names<-File_names[!grepl("MIN",File_names)]
File_names<-File_names[!grepl("Biomass",File_names)]
File_names<-File_names[!grepl("reclass",File_names)]
File_names<-File_names[!grepl("initial",File_names)]
File_names<-File_names[!grepl("ecoregions",File_names)]


#loop to get statistics for each species at each time point for each scenario
Cell_stats<-NULL
n<-0
for (j in 1:length(File_names)){
  File<-raster(File_names[j])#load a raster
  Cell_freq<-data.frame(freq(File))#produce table with a count of the numberof pixels with different maximum ages
  Cell_freq$scenario<-sub("\\/.*","",sub("LandisOutputs/*","",File_names[j]))#get scenario name
  Cell_freq$age<-as.numeric(sub("^(.*)[.].*", "\\1",gsub("^.*?MAX-","", File_names[j])))#get time step
  Cell_freq$species<-sub(".*?/(.*?)-MAX.*", "\\1", File_names[j])#get species name
  Cell_stats<-rbind(Cell_freq,Cell_stats)#bind all together into one table
  n<-n+1#output the percentage of the task that is done
  print((n/length(File_names))*100)
}

#now create summary of these results so they can be plotted
bins<-c(10,3000)
head(Cell_stats)
Cell_stats$bin_cut<-cut(Cell_stats$value,bins,include.lowest=T,labels =c(3000))
Cell_stats<-subset(Cell_stats,!is.na(bin_cut))
Cell_stats2<-ddply(Cell_stats2,.(species,age,scenario),summarise,Av=mean(Total),std.dev=sd(Total))
write.csv(Cell_stats3, file="summary_species.csv")

#now plot results
theme_set(theme_bw(base_size=12))
P1<-ggplot(Cell_stats2,aes(x=age,y=((Av)/56656)*100,ymax=((Av)/56656)*100,ymin=((Av)/56656)*100,T,colour=scenario))+geom_line(alpha=0.8)+facet_wrap(~species)+scale_colour_brewer("Scenarios",palette="Set1")
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Percentage cover")+xlab("Year")+scale_y_continuous(labels = comma)
ggsave("Figures/Species_cover_10yrs.pdf",height=6,width=10,units="in",dpi=400)

#plot results to a single pdf with a different page for each scenario
pdf("Figures/Species_cover_10_2.pdf")
un_scen<-unique(Cell_stats2$scenario)
for (i in 1:length(un_scen)){
  cell_sub<-subset(Cell_stats2,scenario==un_scen[i])
  P1<-ggplot(cell_sub,aes(x=age,y=((Av)/56656)*100,ymax=((Av)/56656)*100,ymin=((Av)/56656)*100,T))+geom_line(alpha=0.8)+facet_wrap(~species)
  P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
  P3<-P2+ylab("Percentage cover")+xlab("Year")+scale_y_continuous(labels = comma)+ggtitle(un_scen[i])
  print(P3)
}
dev.off()