#script for performing exploratory analysis of Kakum bird diversity data

library(tidyverse)

#load data
#pointcounts
pcts<-read_csv("Kakum_point_counts.2014.2017.csv")
pcts$Observation.species<-gsub("  ",".",pcts$Observation.species)
pcts$Observation.species<-gsub(" ",".",pcts$Observation.species)

pcts<-pcts %>% mutate(Observation.species=replace(Observation.species,Observation.species=="Trachyphonus/Trachylaemus.purpuratus",
                                                  "Trachyphonus.purpuratus"),
                      Observation.species=replace(Observation.species,Observation.species=="Trachyphonus/Trachylaemus.margaritatus",
                                                  "Trachyphonus.margaritatus"),
                      Observation.species=replace(Observation.species,Observation.species=="Platysteira/Dyaphorophyia.castanea",
                                                  "Platysteira.cyanea"),
                      Observation.species=replace(Observation.species,Observation.species=="Lamprotornis/Hylopsar.cupreocauda",
                                                  "Hylopsar.cupreocauda"),
                      Observation.species=replace(Observation.species,Observation.species=="Prionops/Sigmodus.caniceps",
                                                  "Prionops.caniceps"),
                      Observation.species=replace(Observation.species,Observation.species=="Lonchura/Spermestes.cucullata",
                                                  "Lonchura.cucullata"),
                      Observation.species=replace(Observation.species,Observation.species=="Bleda.?",
                                                  "Bleda.sp"),
                      Observation.species=replace(Observation.species,Observation.species=="Treron.calva",
                                                 "Treron.calvus"))
sort(unique(pcts$Observation.species))

#trait dataset
traits<-read_csv("All bird traits 2014-2017.csv")
temp<-traits %>% filter(grepl("Bleda",Unique_Scientific_Name)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% mutate(Unique_Scientific_Name="Bleda.sp")
traits<-bind_rows(traits,temp)
temp<-traits %>% filter(grepl("Pogoniulus",Unique_Scientific_Name)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% mutate(Unique_Scientific_Name="Pogoniulus.sp")
traits<-bind_rows(traits,temp)
pcts_traits<-left_join(pcts,traits %>% rename(Observation.species=Unique_Scientific_Name) %>% 
                       select(Observation.species,Body_mass,Bill_TotalCulmen,Bill_Nares,Bill_Width,
                              Bill_Depth,Tarsus_Length,`Kipp's_Distance`,Secondary1,Wing_Chord,`Hand-Wing Index (Claramunt 2011)`,
                              Tail_Length),by="Observation.species")
#check for gaps
tmp<-pcts_traits %>% filter(is.na(Tail_Length))

#separate cocoa and forest data points
pcts_forest<-pcts_traits %>% filter(Region.Stratum=="Forest")
pcts_cocoa<-pcts_traits %>% filter(Region.Stratum=="Cocoa")

#plot locations
latlon<-read_csv("kakumplots_latlon.csv")
latlon<-latlon %>% mutate(Plot=gsub(" ","",name2))

#combine into separate databases for cocoa
pcts_cocoa.ll<-left_join(pcts_cocoa,latlon %>% select(Plot,X,Y),by="Plot")

#transect locations
trans<-read_csv("biodiversity_transects.csv")
trans<-trans %>% mutate(Plot=gsub(" ","",name))

pcts_forest$Plot<-paste0(gsub("Forest","",pcts_forest$Plot),gsub("-","",pcts_forest$`Point transect.Distance`))

#combine into separate databases for forest
pcts_forest.ll<-left_join(pcts_forest %>% select(-`Point transect.Distance`,-`Point transect.Distance.cont`),trans %>% select(Plot,X,Y),by="Plot")

