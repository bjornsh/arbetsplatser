#---------------------------------------------------------------------------------------------------
# Syfte
#---------------------------------------------------------------------------------------------------

# Identifiera kluster av arbetsplatsområden från SCB dagbefolkning rutnätsdata genom att:  
  
# identifiera SCB rutor med mer än X dagbefolkning  
# skapa en buffer runtom rutornas centerkoordinater
# slå ihop rutor med överlappande buffer, dvs angränsande rutor, till ett arbetsområde 
# summera dagbefolkning för alla rutor som ingår i samma arbetsområdet



#---------------------------------------------------------------------------------------------------
# clean
#---------------------------------------------------------------------------------------------------
rm(list = ls())
invisible(gc())



#---------------------------------------------------------------------------------------------------
# set up
#---------------------------------------------------------------------------------------------------

# libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, sp, mapview, leaflet, htmlwidgets)


# avoid scientific notation
options(scipen=999)


# create output directory
dir.create(paste0(getwd(), "/output"))


# get paths
api_fil <- read_file(paste0("Z:/api"))
scb_data = gsub('^.*scb_data: \\s*|\\s*\r.*$', "", api_fil)

output = paste0(getwd(),"/output/")




#---------------------------------------------------------------------------------------------------
# Fetch data
#---------------------------------------------------------------------------------------------------

# dagbefolkning per ruta
scb = read.csv2(paste0(scb_data, "/aa_befolkning/Ruta500_Dagbefolkning20191231.csv"), sep = ",")




#---------------------------------------------------------------------------------------------------
# Define variables
#---------------------------------------------------------------------------------------------------

# !!!!!!!!! ATT GÖRA: min dagbefolkning per ruta för att identifiera viktiga arbetsplatsrutor 
dagbef_min = 100


# !!!!!!!!! ATT GÖRA:  namn av kolumn med dagbefolkningsstorlek
dagbef_antal = "Personer"


# Identify grid size from RutID  
grid_size = scb %>%
  rename(ruta = 1) %>% 
  select(1) %>%
  mutate(cell = substr(ruta, 10, 13)) %>%
  filter(cell != "0000") %>% 
  summarise(min = min(cell)) %>%
  mutate(min = as.numeric(min)) %>%
  pull()




#---------------------------------------------------------------------------------------------------
# Manipulate SCB grid data
#---------------------------------------------------------------------------------------------------

### Create center points for SCB grid center coordinates


# calculate 50% of grid diameter: to be added to corner coordinates to create center coordinates
dist_to_center = grid_size/2


# create grid center coordinates: add 50% of grid diameter to bottom left coordinate in x and y direction
scb1 = scb %>%
  as.data.frame() %>%
  mutate(Personer = ifelse(Personer == "..C", 2,
                           ifelse(Personer == "-", 0, Personer))) %>%
  dplyr::select(starts_with("Rut"), Personer) %>% 
  rename(ruta = 1) %>% 
  mutate(x_center = as.numeric(substr(ruta, 1, 6)) + dist_to_center,   
         y_center = as.numeric(substr(ruta, 7, 13)) + dist_to_center)


scb1 = scb %>%
  as.data.frame() %>%
  mutate(!!sym(dagbef_antal) := ifelse(!!sym(dagbef_antal) == "..C", 2, 
                           ifelse(!!sym(dagbef_antal) == "-", 0, !!sym(dagbef_antal))),
         !!sym(dagbef_antal) := as.numeric(!!sym(dagbef_antal))) %>%
  dplyr::select(starts_with("Rut"), !!sym(dagbef_antal)) %>% 
  rename(ruta = 1) %>% 
  mutate(x_center = as.numeric(substr(ruta, 1, 6)) + dist_to_center,   
         y_center = as.numeric(substr(ruta, 7, 13)) + dist_to_center)





### remove rutor with less than X dagbefolkning
urval = filter(scb1, !!sym(dagbef_antal) > dagbef_min)


### create spatial points from center coordinates
xy = urval[,c("x_center", "y_center")]

spdf <- SpatialPointsDataFrame(coords = xy, data = urval) # create spatial points

spdf1 = st_as_sf(spdf) %>% # convert to sf object
  st_set_crs(3006) # set projection, SCB data is in Sweref 99TM



### Cluster adjacent rutor where each has more than X dagbefolkning 
spdf1_buffer = st_buffer(spdf1, grid_size * 1.5) # radius buffer = grid size * 1.5  to ensure all adjacent cells are joined

# dissolve boundaries between overlapping polygons 
parts = st_cast(st_union(spdf1_buffer),"POLYGON")

# create index for polygons within same merged polygon
clust <- unlist(st_intersects(spdf1_buffer, parts))

# use index to add sum dagbefolkning for all cells that are included in a dissolved polygon
arbetsplatser <- cbind(spdf1_buffer, clust) %>%
  group_by(clust) %>%
  summarize(dagbef = sum(!!sym(dagbef_antal)))




#---------------------------------------------------------------------------------------------------
# View results
#---------------------------------------------------------------------------------------------------

# View center coordinates for all rutor med X dagbefolkning
mapview(spdf1)

# View cluster of arbetsplatsrutor
mapview(arbetsplatser, zcol = "dagbef")


### Leaflet map
bins <- c(100, 500, 1500, 3000, 100000)
pal <- colorBin("YlOrRd", domain = arbetsplatser$dagbef, bins = bins)


karta = arbetsplatser %>% 
  st_transform(4326) %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~ pal(dagbef),
              fillOpacity = 1,
              popup = ~paste(dagbef, "arbetsplatser finns i området")) %>%   
  addLegend(pal = pal, 
            values = ~dagbef, 
            # labFormat = labelFormat(suffix = "%",
            #                         transform = function(x) 100 * x),
            title = "Arbetsplatser", position = "bottomright")

karta


#---------------------------------------------------------------------------------------------------
# Save results
#---------------------------------------------------------------------------------------------------

st_write(arbetsplatser, paste0(output, "arbetsplatser2019.shp"))


saveWidget(karta, file=paste0(output, "karta.html"), selfcontained = FALSE)






