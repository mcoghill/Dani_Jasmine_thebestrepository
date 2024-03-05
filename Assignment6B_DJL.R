#Assignment 6
#Dani Lafleur 
#part B

install.packages("tidyverse")
install.packages("readxl")
install.packages("sf")
install.packages("terra")
install.packages("mapview")
install.packages("bcdata")
install.packages("bcmaps")

library("sf")
library("terra")
library("readxl")
library("bcmaps")
library("bcdata")
library("mapview")
View(available_layers())
BEC<-bec()
mapview(BEC) 

print(colnames(BEC))

park_search_results <- bcdc_search("BC Parks Ecological Reserves Protected Areas")
View(park_search_results)
protected_areas <- bcdc_get_data(record = "1130248f-f1a3-4956-8b2e-38d29d3e4af7")
View(protected_areas)
LacDuBois <- protected_areas %>%
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA")
mapview(LacDuBois)

BEC_Inside_LacDuBois<- st_intersection(BEC, LacDuBois)

mapview(BEC_Inside_LacDuBois)
View(BEC_Inside_LacDuBois)
head(BEC_Inside_LacDuBois)
BEC_AREA<-st_area(BEC_Inside_LacDuBois)
BEC_Inside_LacDuBois$area_sqm <- st_area(BEC_Inside_LacDuBois)

BEC_Inside_LacDuBois$area_ha <- BEC_Inside_LacDuBois$area_sqm / 10000
View(BEC_Inside_LacDuBois)

BEC_Zone_Area_Summary <- BEC_Inside_LacDuBois %>%
  summarise(Total_Area_Ha = sum(area_ha))

View(BEC_AREA)

BEC_Inside_LacDuBois$area_ha <- as.numeric(st_area(BEC_Inside_LacDuBois) / 10000)
BEC_Inside_LacDuBois$area_ha <- st_area(BEC_Inside_LacDuBois) / 10000
total_area_ha <- sum(BEC_Inside_LacDuBois$area_ha, na.rm = TRUE)
print(paste("Total area in hectares:", total_area_ha))
#"Total area in hectares: 15773.3785374985"



ggplot(BEC_Inside_LacDuBois, aes(x = MAP_LABEL, y = area_ha, fill = MAP_LABEL)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "MAP_LABEL", y = "Area (ha)", title = "Area by MAP_LABEL in Lac Du Bois") +
  scale_fill_viridis_d()

library(terra)
dem_data <- cded_terra("BEC_Inside_LacDuBois")
BEC_Inside_LacDuBois$mean_elevation <- exact_extract(dem_data, BEC_Inside_LacDuBois, fun = mean)
library(mapview)
library(viridis) # for a nice color palette
map <- mapview(BEC_Inside_LacDuBois, zcol = 'ZONE', legend = TRUE)
map@map$colors <- viridis::viridis(length(unique(BEC_Inside_LacDuBois$ZONE)))
View(map)
map










