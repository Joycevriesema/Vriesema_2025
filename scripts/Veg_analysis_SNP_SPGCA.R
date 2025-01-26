# script by Joyce Vriesema,  started January 10, 2025
#### CLEAR SCRIPT AND READ LIBRARIES ####
# clear and read libraries
rm(list= ls())
Sys.setenv(PROJ_LIB="/usr/local/Cellar/proj/9.3.0/share/proj") ### put this above every R script when you work with spatial data
setwd("/Users/joycevriesema/Downloads/Studie/MSc project 2025")

####Libraries###
library(tidyverse)
library(psych)
library(vegan)


#### MULTIVARIATE ANALYSIS DOMINANT SPECIES ####
# Question to answer: how does species composition differ between SNP and SPGCA?
# take df DimTransect to exctract Transect_ID, Region and CatenaPos
# take df FactPointsVeg to extract Transect_ID and GrassDom
 
DimTransect<- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSDn2hqZTiiM7gN9UN_ZMjbxDjHLTjq35vtdIFYRYolS4Q_u_p1RREyiNVAzs3zZmuGl6izk7UNRVMu/pub?gid=1790967677&single=true&output=csv") %>%
  dplyr::select(Transect_ID,Region, CatenaPos)%>%
  mutate(across(everything(), ~ na_if(., ""))) %>% 
  drop_na() 

FactPointsVeg <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSDn2hqZTiiM7gN9UN_ZMjbxDjHLTjq35vtdIFYRYolS4Q_u_p1RREyiNVAzs3zZmuGl6izk7UNRVMu/pub?gid=0&single=true&output=csv") %>%
  dplyr::select(Transect_ID, GrassDom) %>%
  drop_na() %>%
  dplyr::filter(GrassDom != "bare") %>%
  dplyr::mutate(Value = 1) %>%
  dplyr::group_by(Transect_ID, GrassDom) %>%
  dplyr::summarise(Value = sum(Value), .groups = "drop") %>%  # Summarize duplicates
  tidyr::pivot_wider(names_from = GrassDom, values_from = Value, values_fill = 0)%>%
  tibble::column_to_rownames(var="Transect_ID")

#### ordination: 
dca <- vegan::decorana(FactPointsVeg)



# merge df's
df_merged <- FactPointsVeg %>%
  cbind(Region = DimTransect$Region, CatenaPos = DimTransect$CatenaPos)
# and show the ordination with the most abundance species with priority
SpecTotCov<-colSums(FactPointsVeg)
region_colors <- setNames(c("blue", "green"), c("Serengeti_NP", "SpekeGulf_GCA"))
catena_shapes <- setNames(c(16, 17, 15), c("high", "mid", "low"))
vegan::ordiplot(dca,display="none",cex=0.7,type="text",xlim=c(-7,7))



vegan::orditorp(dca, display = "species", priority = SpecTotCov,
                col = region_colors[df_merged$Region],        
                pcol = "red",                                
                pch = catena_shapes[df_merged$CatenaPos],    
                cex = 0.8,                                    
                xlim = c(-5, 5))                              

# Add legend for Region
legend("topright",                              
       legend = names(region_colors),             
       fill = region_colors,                     
       title = "Region",                         
       box.lty = 0,                              
       border = "white",                       
       bty = "n",                               
       cex = 0.8) 

legend("bottomright",                           
       legend = names(catena_shapes),            
       pch = catena_shapes,                      
       title = "CatenaPos",                      
       box.lty = 0,                              
       border = "white",                         
       bty = "n",                               
       cex = 0.8) 



