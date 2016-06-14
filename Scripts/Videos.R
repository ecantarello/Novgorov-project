#script to produce animations of maps from landis outputs


library(raster)
library(animation)
library(gtools)
library(ggplot2)
library(ggmap)
library(gganimate)


#find rasters for species richness
File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[grepl("SPP-RICH",File_names)]
File_names<-mixedsort(File_names,decreasing = T)


#produce a list of the unique scenarios
Sce_num<-as.numeric(unlist(regmatches(File_names, gregexpr("[[:digit:]]+", File_names))))
scenarios<-paste("Test",unique(Sce_num[c(TRUE,FALSE)]),sep="")

df_all<-NULL
for (i in 1:length(scenarios)){
  scen_sub<-File_names[grepl(scenarios[i],File_names)]
  for(j in 1:length(scen_sub))
  {
    map.p <- rasterToPoints((raster(scen_sub[j])))
    df <- data.frame(map.p)
    colnames(df) <- c("x", "y", "SpR")
    year<-unique(na.omit(as.numeric(unlist(strsplit(unlist(scen_sub[j]), "[^0-9]+")))))[2]
    P1<-ggplot(data=df, aes(y=y, x=x)) +
      geom_raster(aes(fill=SpR))+
      scale_fill_gradient("Tree species richness",low="white",high="dark blue",limits=c(0, 11),breaks=c(0,2,4,6,8,10))+
      coord_equal()+
      theme_bw()+ 
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())+
      ggtitle(label = paste(year,"years"))
print(P1)
ggsave(paste("Videos/pngs/",scenarios[i],"_species_richness",
             ifelse(j<10,"_00","_0")
             ,j,".png",sep=""),width = 4.5,height =2.77)
  }
}


######################################################
#this part of the script produces maps for videos of##
#individual species but not species richness##########
######################################################


#find rasters for species richness
File_names<-list.files(pattern="*.img",recursive=T)
File_names<-File_names[!grepl("SPP-RICH",File_names)]
File_names<-File_names[grepl("Test16",File_names)]


plot(raster(File_names[671]))

#produce a list of the unique scenarios
Sce_num<-as.numeric(unlist(regmatches(File_names, gregexpr("[[:digit:]]+", File_names))))
scenarios<-paste("Test",unique(Sce_num[c(TRUE,FALSE)]),sep="")

#produce a list of the unique species
species<-unique(sub(".*?/(.*?)-MAX.*", "\\1", File_names))

df_all<-NULL
for (i in 1:length(species)){
  scen_sub<-File_names[grepl(species[i],File_names)]
  for(j in 1:length(scen_sub))
  {
    map.p <- rasterToPoints((raster(scen_sub[j])))
    df <- data.frame(map.p)
    colnames(df) <- c("x", "y", "Abun")
    year<-unique(na.omit(as.numeric(unlist(strsplit(unlist(scen_sub[j]), "[^0-9]+")))))[2]
    P1<-ggplot(data=df, aes(y=y, x=x)) +
      geom_raster(aes(fill=Abun))+
      scale_fill_gradient("Number of trees",low="white",high="dark blue",limits=c(0,400))+
      coord_equal()+
      theme_bw()+ 
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())+
      ggtitle(label = paste("Species=",species[i],",",year," years",sep=""))
    print(P1)
    ggsave(paste("Videos/pngs/",species[i],
                 ifelse(j<10,"_00","_0")
                 ,j,".png",sep=""),width = 4.5,height =2.77 )
  }
}


