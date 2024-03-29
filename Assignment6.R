## Assignment 6: Part 1a
## Jasmine Ehlert

library(sf)
library(tidyverse)
library(mapview)
library(bcdata)


ldb <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7", crs = 3005) %>%  
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA") %>% 
  collect()
mapview(ldb)

bcdc_query_geodata("https://catalogue.data.gov.bc.ca/dataset/e2dadc60-292f-4d98-b42b-56ca9e4fe694")
bcdc_describe_feature("https://catalogue.data.gov.bc.ca/dataset/e2dadc60-292f-4d98-b42b-56ca9e4fe694")

ldb_fire<- bcdc_query_geodata("e2dadc60-292f-4d98-b42b-56ca9e4fe694") %>% 
  filter(INTERSECTS(ldb)) %>% 
  collect()

#########
## 4/4 ##
#########

fire.per.year<-ldb_fire %>% group_by(FIRE_YEAR) %>% 
  summarise(Fire.Count=n())

#########
## 2/2 ##
#########

fire.cause <- ldb_fire %>%  group_by(FIRE_CAUSE) %>% 
  summarise(Cause.Count=n())

#########
## 2/2 ##
#########

mapview(ldb_fire, zcol="FIRE_CAUSE")

#########
## 2/2 ##
#########

data<-ldb_fire %>% 
  group_by(FIRE_YEAR, FIRE_CAUSE) %>% 
  summarise(rate=n())

ggplot(data, aes(x=FIRE_CAUSE, y=rate)) + geom_boxplot() +
  labs(y="Mean Fires/Year", x="Fire Cause")

#########
## 4/5 ##
#########
  
## Part of this question asked you to use different colors for each box!


## Total: 

###########
## 14/15 ##
###########

## PS: I really appreciate the conciseness of your code, well done!
