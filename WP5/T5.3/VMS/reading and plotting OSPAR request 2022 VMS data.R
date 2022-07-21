library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")

setwd("C://Users/poos001/OneDrive - WageningenUR/projects/Seawise/WP5.3/Shapefiles by year updated")

testme <- NULL
flnms <- list.files(pattern=".shp",recursive=T)
for (ii in flnms){
   if(!exists("testme") | is.null("testme")){
    testme <- list( st_read(ii))
   }else{
     testme  <- append(testme,  list(st_read(ii)))
   }
}
names(testme) <- flnms

ggplot(testme[[1]]) +
  geom_sf( mapping = aes(fill = log(surface)), colour=NA) +
  scale_fill_viridis(option = "D") +
  geom_sf(data=world)+
  coord_sf(xlim = c(-12, 13), ylim = c(48, 63), expand = FALSE)
  #ggtitle(ii)

