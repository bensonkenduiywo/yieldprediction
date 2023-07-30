rm(list = ls(all=TRUE))
root <- "D:/RCMRD/Data/Yields/"

source('D:/RCMRD/Code/yieldprediction/hybrid/functions.R')
#=======================================================================
## Spatial Visualization
#=======================================================================
library(raster)
ken <- shapefile("D:/Adm data/Kenya_counties_2011/Kenya_county_dd.shp")
names(ken)[4] <- "County"
path <-'D:\\RCMRD\\Data\\Yields\\Results\\KEN\\'
temp <- na.omit(readRDS(paste0(path,"RHEAS_accuracy_Districts.rds")))
ref_path <- paste0(root, "Reference/Kenya/MOA/")
ke_ref <- read.csv(paste0(ref_path, 'Maize production by County MOALD.csv'))
ke_ref$District[ke_ref$District=="Elgeyo Marakwet"] <- "Elgeyo-Marakwet"
ke_ref$District[ke_ref$District=="Garrisa"] <- "Garisa"
ke_ref$District[ke_ref$District=="Homabay"] <- "Homa Bay"
ke_ref$District[ke_ref$District=="Trans-Nzoia"] <- "Trans Nzoia"
ke_ref$District <- toupper(ke_ref$District)
names(ken)[1] <- "District"
ken$District <- toupper(ken$District)
ken$Country <- 'Kenya'
ken <-  merge(ken[,c("District", 'Country')], temp, by = "District", all.x=T) # duplicateGeoms = TRUE
rm(temp)

#Malawi
mwi <- shapefile("D:/Adm data/Malawi/gadm40_MWI_1.shp")
mw_ref <- read.csv(paste0(root, "Reference/Malawi/MOA/MALAWI_MOA_MAIZE_Production data.csv"), stringsAsFactors =  FALSE)
mw_ref$District <- toupper(mw_ref$District)
mw_ref$Division <- toupper(mw_ref$Division)
names(mw_ref)[1] <- "year"
mw_ref$yield_MT_ha <- as.numeric(mw_ref$yield_MT_ha)
mw_ref$yield_MT_ha[mw_ref$yield_MT_ha>6] <- NA
path <-'D:\\RCMRD\\Data\\Yields\\Results\\MWI\\'
temp <- na.omit(readRDS(paste0(path,"RHEAS_accuracy_Districts.rds")))
names(mwi)[13] <- "District"
mwi$District <- toupper(mwi$District)
mwi$Country <- 'Malawi'
mwi <-  merge(mwi[,c("District", 'Country')], temp, by = "District", all.x=T) # duplicateGeoms = TRUE
rm(temp)

#Zambia
zmb <- shapefile("D:/Adm data/Zambia/2010 Districts/district_74_dd.shp")
zm_ref <- read.csv(paste0(root, "Reference/Zambia/MOA/Zambia_District _Maize_Forecasting_2011_2022.csv"), stringsAsFactors =  FALSE)
zm_ref$District <- toupper(zm_ref$District)
zm_ref$Province <- trimws(toupper(zm_ref$Province), which = c("both"))
sort(unique(zm_ref$Province))
zm_ref$District <- trimws(toupper(zm_ref$District), which = c("both"))
names(zm_ref)[7] <- "year"
zm_ref$District[zm_ref$District=="ITEZHI-TEZHI"] <- "ITEZHI TEZHI"
path <-'D:\\RCMRD\\Data\\Yields\\Results\\ZMB\\'
temp <- na.omit(readRDS(paste0(path,"RHEAS_accuracy_Districts.rds")))
names(zmb)[3] <- "District"
zmb$District <- toupper(zmb$District)
zmb$PROVINCE <- toupper(zmb$PROVINCE)
zmb$Country <- 'Zambia'
zmb <-  merge(zmb[,c("District", 'Country')], temp, by = "District", all.x=T) # duplicateGeoms = TRUE

all <- Reduce('rbind', list(ken, mwi, zmb))
shapefile(all, paste0(root, "Results/RHEAS_Errors_Spatial_distribution.shp"),overwrite=TRUE)

# Random Forest RMSE
#=========================================================================
library(tmap)
library(mapview)
tmap_mode("plot")
map1 <- tm_shape(ken, name="RRMSE") +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("RRMSE", title="RRMSE", breaks = seq(10, 90, 10), textNA = "No data") +
  tm_borders(col = 'black')+
  #tm_text("County", size = 0.4, remove.overlap = TRUE)+
  tm_layout(panel.label.size=6, legend.show = FALSE, legend.position = c("left", "bottom"), title= 'Kenya', title.position = c('right', 'top'))#+
#tm_layout(legend.outside = TRUE)# +
#tm_format("World")
map1

map2 <- tm_shape(mwi, name="RRMSE") +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("RRMSE", title="RRMSE", breaks = seq(10, 70, 5), textNA = "No data") +
  tm_borders(col = 'black')+
  tm_layout(panel.label.size=6, legend.show = FALSE, legend.position = c("left", "bottom"), title= 'Malawi', title.position = c('right', 'top'))
map2

map3 <- tm_shape(zmb, name="RRMSE") +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("RRMSE", title="RRMSE", breaks = seq(10, 70, 5), textNA = "No data") +
  tm_borders(col = 'black')+
  tm_layout(panel.label.size=6, legend.show = FALSE, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('center', 'top'))
map3

legend.map <- tm_shape(zmb) + 
  tm_fill("RRMSE", style = 'fixed', breaks = seq(10, 90, 10)) +
  tm_layout(legend.only = TRUE, legend.text.size=1.0, legend.title.size = 1.2)

all_map <- tmap_arrange(map1, map2, map3, legend.map, nrow =2)
all_map
tmap_save(all_map, scale =1.6, dpi= 600, height=8, width=8, units = 'in', filename=paste0(root, "Results/RHEAS_RRMSE_Spatial_Distribution.png"))

#Map in view mode to help check values 
tmap_mode("view")
all_v1 <- tm_shape(all) +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("RRMSE", title="RRMSE", breaks = seq(10, 90, 10), textNA = "No data", 
          legend.show = F) +
  tm_facets(by = "Country", as.layers = T) 

all_v1
tmap_save(all_v1, paste0(root, "Results/RHEAS_RRMSE_Spatial_Distribution.html"))

# Random Forest ubRMSE
#=========================================================================
tmap_mode("plot")
map4 <- tm_shape(ken, name="ubRMSE") +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("ubRMSE", title="ubRMSE", breaks = seq(0.1, 2, 0.2), textNA = "No data") +
  tm_borders(col = 'black')+
  tm_layout(panel.label.size=6, legend.show = FALSE, legend.position = c("left", "bottom"), title= 'Kenya', title.position = c('right', 'top'))#+
map4

map5 <- tm_shape(mwi, name="ubRMSE") +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("ubRMSE", title="ubRMSE", breaks = seq(0.1, 2, 0.2), textNA = "No data") +
  tm_borders(col = 'black')+
  tm_layout(panel.label.size=6, legend.show = FALSE, legend.position = c("left", "bottom"), title= 'Malawi', title.position = c('right', 'top'))
map5

map6 <- tm_shape(zmb, name="ubRMSE") +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("ubRMSE", title="ubRMSE", breaks = seq(0.1, 2, 0.2), textNA = "No data") +
  tm_borders(col = 'black')+
  tm_layout(panel.label.size=6, legend.show = FALSE, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('center', 'top'))
map6

leg.ubRMSE <- tm_shape(zmb) + 
  tm_fill("ubRMSE",style = 'fixed',breaks = seq(0.1, 2, 0.2)) +
  tm_layout(legend.only = TRUE, legend.text.size=1.0, legend.title.size = 1.2)

maps2 <- tmap_arrange(map4, map5, map6, leg.ubRMSE, nrow =2)
maps2

tmap_save(maps2, scale =1.6, dpi= 600, height=8, width=8, units = 'in', filename=paste0(root, "Results/RHEAS_ubRMSE_Spatial_Distribution.png"))

#Map in view mode to help check values 
tmap_mode("view")
all_v3 <- tm_shape(all) +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("ubRMSE", title="ubRMSE", breaks = seq(0.1, 2, 0.2), textNA = "No data", 
          legend.show = F) +
  tm_facets(by = "Country", as.layers = T) 

all_v3
tmap_save(all_v3, paste0(root, "Results/RHEAS_ubRMSE_Spatial_Distribution.html"))


# Random Forest MBE palette = get_brewer_pal("RdYlBu", n = 6),
#=========================================================================
tmap_mode("plot")
map7 <- tm_shape(ken, name="MBE") +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("MBE",  palette = get_brewer_pal("Spectral", n = 6), midpoint =0, title="MBE", breaks = seq(-2.5,3,0.5), textNA = "No data") +
  tm_borders(col = 'black')+
  tm_layout(panel.label.size=6, legend.show = FALSE, legend.position = c("left", "bottom"), title= 'Kenya', title.position = c('right', 'top'))#+
map7

map8 <- tm_shape(mwi, name="MBE") +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("MBE", palette = get_brewer_pal("Spectral", n = 6), midpoint =0, title="MBE", breaks = seq(-2.5,3,0.5), textNA = "No data") +
  tm_borders(col = 'black')+
  tm_layout(panel.label.size=6, legend.show = FALSE, legend.position = c("left", "bottom"), title= 'Malawi', title.position = c('right', 'top'))
map8

map9 <- tm_shape(zmb, name="MBE") +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("MBE", palette = get_brewer_pal("Spectral", n = 6), midpoint =0, title="MBE", breaks = seq(-2.5,3,0.5), textNA = "No data") +
  tm_borders(col = 'black')+
  tm_layout(panel.label.size=6, legend.show = FALSE, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('center', 'top'))
map9

leg.MBE <- tm_shape(zmb) + 
  tm_fill("MBE", palette = get_brewer_pal("Spectral", n = 6), midpoint =0, breaks = seq(-2.5,3,0.5)) +
  tm_layout(legend.only = TRUE, legend.text.size=1.0, legend.title.size = 1.2)

maps3 <- tmap_arrange(map7, map8, map9, leg.MBE, nrow =2)
maps3

tmap_save(maps3, scale =1.6, dpi= 600, height=8, width=8, units = 'in', filename=paste0(root, "Results/RF-H_MBE_Spatial_Distribution.png"))

#Map in view mode to help check values 
tmap_mode("view")
all_v4 <- tm_shape(all) +
  tm_grid(lines = FALSE, labels.size = 1)+
  tm_fill("MBE", palette = get_brewer_pal("Spectral", n = 6), midpoint =0, breaks = seq(-2.5,3,0.5),
          legend.show = F) +
  tm_facets(by = "Country", as.layers = T) 

all_v4
tmap_save(all_v4, paste0(root, "Results/RF-H_MBE_Spatial_Distribution.html"))

