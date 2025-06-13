# script by Joyce Vriesema,  started January 10, 2025
#### CLEAR SCRIPT AND READ LIBRARIES ####
# clear and read libraries
rm(list= ls())
Sys.setenv(PROJ_LIB="/usr/local/Cellar/proj/9.3.0/share/proj") ### put this above every R script when you work with spatial data
setwd("/Users/joycevriesema/Downloads/Studie/MSc project 2025")

library(tidyverse)
library(psych)
library(vegan)
 

#### MULTIVARIATE ANALYSIS DOMINANT SPECIES ####
# Question to answer: how does species composition differ between SNP and SPGCA?
# take df DimTransect to exctract Transect_ID, Region and CatenaPos
# take df FactPointsVeg to extract Transect_ID and GrassDom
 
DimTransect<- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSDn2hqZTiiM7gN9UN_ZMjbxDjHLTjq35vtdIFYRYolS4Q_u_p1RREyiNVAzs3zZmuGl6izk7UNRVMu/pub?gid=1790967677&single=true&output=csv")|>
  dplyr::select(Transect_ID,Region, CatenaPos)|>
  mutate(across(everything(), ~ na_if(., "")))|> 
  drop_na()|>
  tibble::column_to_rownames(var="Transect_ID")

FactPointsVeg <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSDn2hqZTiiM7gN9UN_ZMjbxDjHLTjq35vtdIFYRYolS4Q_u_p1RREyiNVAzs3zZmuGl6izk7UNRVMu/pub?gid=0&single=true&output=csv") |>
  dplyr::select(Transect_ID, GrassDom) |>
  drop_na() |>
  dplyr::filter(GrassDom != "bare") |>
  dplyr::mutate(Value = 1)|>
  dplyr::group_by(Transect_ID, GrassDom)|>
  dplyr::summarise(Value = sum(Value), .groups = "drop") |>  # Summarize duplicates
  tidyr::pivot_wider(names_from = GrassDom, values_from = Value, values_fill = 0) |>
  tibble::column_to_rownames(var="Transect_ID")

#for ordination with functions from the vegan library you need an separate environmental factors dataset
FactPointsVeg
DimTransect

#DCA
dca <- vegan::decorana(FactPointsVeg)
dca 

#make biplot, only display species
vegan::ordiplot(dca, display = "species", cex = 0.7, type = "text", xlim = c(-4, 4), col="red")
vegan::ordipointlabel(dca, display = "species", choices = c(1, 2),
               col = c(1, 2), pch="+",font = c(1, 1),
               cex = c(0.8, 0.8), add = FALSE)

#fit the environmental factors to the dca ordination surface
names(DimTransect)
ef_dca<-vegan::envfit(dca~CatenaPos,data=DimTransect,na.rm=T)

#add the result to the ordination plot as vectors for each variable
plot(ef_dca,add=T)

# give each region a shape
site_regions <- factor(DimTransect$Region)
pch_values <- ifelse(site_regions == "Serengeti_NP", 1, 2)
points(dca, display = "sites", pch = pch_values, col = "black", cex = 1.2)
pch_values <- ifelse(site_regions == "Serengeti_NP", 21, 24)  # 21 = filled circle, 24 = filled triangle
fill_colors <- ifelse(site_regions == "Serengeti_NP", 
                      rgb(0, 0, 1, alpha = 0.5),  # Blue with 50% transparency
                      rgb(0, 1, 0, alpha = 0.5)) # Green with 50% transparency
points(dca, display = "sites", pch = pch_values, col = "black", bg = fill_colors, cex = 1.2)
legend("topright", 
       legend = levels(site_regions),  # Region names
       pch = c(21, 24),                # Shapes matching the regions
       col = "black",                  # Outline color
       pt.bg = c(rgb(0, 0, 1, alpha = 0.5), rgb(0, 1, 0, alpha = 0.5)), # Fill colors
       pt.cex = 1.2,                   # Point size in the legend
       title = "Regions")


contour_colors <- c("blue", "green")  # Blue for Serengeti_NP, Green for SGCGA

# Loop through each region and draw ellipses with the specified color
for (i in levels(site_regions)) {
  vegan::ordiellipse(dca, 
                     groups = site_regions, 
                     kind = "se", 
                     conf = 0.99, 
                     col = contour_colors[which(levels(site_regions) == i)],  # Set contour color
                     lty = 1, 
                     lwd = 1, 
                     add = TRUE, 
                     show.groups = i)
}


# add catena contour for the different classes of CatenaPos
DimTransect$CatenaPos <- as.numeric(factor(DimTransect$CatenaPos))
names(DimTransect$CatenaPos)
vegan::ordisurf(dca,DimTransect$CatenaPos,add=T,col="darkgreen")

# to do:
# names not overlapping --> function in vegan
# make surface fitting for catena pos