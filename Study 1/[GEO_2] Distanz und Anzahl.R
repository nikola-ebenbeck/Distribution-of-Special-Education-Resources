library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(sf)
library(ggplot2)
library(raster)

Pfad <- ""


#' DatensÃ¤tze Import:
#-------------------------------------------------------------------------------


Schulen <- read.csv2(file = paste0(Pfad, "/Daten_Schulen.csv"))

Kreise_Inklusion_SP <- readOGR(paste0(
  Pfad, "/Geodaten/Kreise/Kreise_Inklusionsanteil.shp"), 
  use_iconv = T, encoding = "UTF-8")


#' Vorprozessierung:
#-------------------------------------------------------------------------------

summary(Schulen)

Schulen$lon <- as.numeric(Schulen$lon)
Schulen$lat <- as.numeric(Schulen$lat)

Schulen$RS_Kreis <- paste0("0", Schulen$RS_Kreis)


#' Filtern:

Grundschulen <- Schulen %>% filter(Jahr == 2020, Schulart == "Grundschulen") 
Mittelschulen <- Schulen %>% filter(Jahr == 2020, Schulart == "Mittelschulen") 
Realschulen <- Schulen %>% filter(Jahr == 2020, Schulart == "Realschulen")
Gymnasien <- Schulen %>% filter(Jahr == 2020, Schulart == "Gymnasien") 
Förderzentren <- Schulen %>% filter(Jahr == 2020, Schulart == "Förderzentren")  

#' Konvertierung zu SPDF (spatial): 

Förderzentren_SP <- SpatialPointsDataFrame(coords = cbind(Förderzentren$lon, 
                                                          Förderzentren$lat), 
                                           data = data.frame(Förderzentren), 
                                           proj4string = CRS("+init=epsg:4326"))

Grundschulen_SP <- SpatialPointsDataFrame(coords = cbind(Grundschulen$lon, 
                                                         Grundschulen$lat), 
                                          data = data.frame(Grundschulen), 
                                          proj4string = CRS("+init=epsg:4326"))

Mittelschulen_SP <- SpatialPointsDataFrame(coords = cbind(Mittelschulen$lon, 
                                                          Mittelschulen$lat), 
                                          data = data.frame(Mittelschulen), 
                                          proj4string = CRS("+init=epsg:4326"))

Realschulen_SP <- SpatialPointsDataFrame(coords = cbind(Realschulen$lon, 
                                                        Realschulen$lat), 
                                          data = data.frame(Realschulen), 
                                          proj4string = CRS("+init=epsg:4326"))

Gymnasien_SP <- SpatialPointsDataFrame(coords = cbind(Gymnasien$lon, 
                                                         Gymnasien$lat), 
                                          data = data.frame(Gymnasien), 
                                          proj4string = CRS("+init=epsg:4326"))

plot(Grundschulen_SP)
plot(Mittelschulen_SP)
plot(Realschulen_SP)
plot(Gymnasien_SP)
plot(Förderzentren_SP)

#' Reprojektion zu UTM Zone 32:

Förderzentren_SP <- spTransform(Förderzentren_SP, CRS("+init=epsg:25832"))
Grundschulen_SP <- spTransform(Grundschulen_SP, CRS("+init=epsg:25832"))
Mittelschulen_SP <- spTransform(Mittelschulen_SP, CRS("+init=epsg:25832"))
Realschulen_SP <- spTransform(Realschulen_SP, CRS("+init=epsg:25832"))
Gymnasien_SP <- spTransform(Gymnasien_SP, CRS("+init=epsg:25832"))

#' Distanz von jeder Grundschule zu Förderzentren:
#-----------------------------------------------------------------------------------


Distanz_fn <- function(Grundschulendatensatz, Förderzentrumsdatensatz, Name_Spalte){
  
  #' Matrix mit Distanz von jeder Grundschule zu jedem FÃ¶rderzentrum:
  Distanz_mat <- gDistance(Grundschulendatensatz, Förderzentrumsdatensatz, byid = T)
  
  #' Sortieren jeder Spalte der Matrix von klein nach groÃŸ
  Distanz_mat_sort <- apply(Distanz_mat, 2, sort)
  
  #' HinzufÃ¼gen zum Datensatz:
  
  #' 1: Distanz zur nÃ¤chsten Förderschule:
  Grundschulendatensatz[[Name_Spalte]] <- apply(Distanz_mat_sort, 2, min)
  
  #' 2: Durchscnittliche Distanz zu den nÃ¤chsten 5 Förderschulen:
  Distanz_mat_nearest_5 <- head(Distanz_mat_sort, 5)
  Grundschulendatensatz[[paste0(Name_Spalte, "_5")]] <- apply(Distanz_mat_nearest_5, 2, mean)
  return(Grundschulendatensatz)
}

Grundschulen_SP_Distanz <- Distanz_fn(Grundschulen_SP, Förderzentren_SP, "Distanz_FZ")
Mittelschulen_SP_Distanz <- Distanz_fn(Mittelschulen_SP, Förderzentren_SP, "Distanz_FZ")
Realschulen_SP_Distanz <- Distanz_fn(Realschulen_SP, Förderzentren_SP, "Distanz_FZ")
Gymnasien_SP_Distanz <- Distanz_fn(Gymnasien_SP, Förderzentren_SP, "Distanz_FZ")

#' Berechnung von 
#' -  Durchschnittlicher Distanz einer jeden Grundschule zum nächsten FZ
#' -  Durchschnittlicher Distanz einer jeden Grundschule zum nächsten Trias
#' für jeden Kreis:

Grundschulen_data <- Grundschulen_SP_Distanz@data
Grundschulen_data <- Grundschulen_data %>% 
  group_by(RS_Kreis) %>%
  mutate(Distanz_Kreis = mean(Distanz_FZ))
Grundschulen_data <- Grundschulen_data %>%
  dplyr::select(RS_Kreis, Distanz_Kreis) %>%
  distinct()

Mittelschulen_data <- Mittelschulen_SP_Distanz@data
Mittelschulen_data <- Mittelschulen_data %>% 
  group_by(RS_Kreis) %>%
  mutate(Distanz_Kreis = mean(Distanz_FZ))
Mittelschulen_data <- Mittelschulen_data %>%
  dplyr::select(RS_Kreis, Distanz_Kreis) %>%
  distinct()

Realschulen_data <- Realschulen_SP_Distanz@data
Realschulen_data <- Realschulen_data %>% 
  group_by(RS_Kreis) %>%
  mutate(Distanz_Kreis = mean(Distanz_FZ))
Realschulen_data <- Realschulen_data %>%
  dplyr::select(RS_Kreis, Distanz_Kreis) %>%
  distinct()

Gymnasien_data <- Gymnasien_SP_Distanz@data
Gymnasien_data <- Gymnasien_data %>% 
  group_by(RS_Kreis) %>%
  mutate(Distanz_Kreis = mean(Distanz_FZ))
Gymnasien_data <- Gymnasien_data %>%
  dplyr::select(RS_Kreis, Distanz_Kreis) %>%
  distinct()

  
Kreise_Distanz <- merge(merge(merge(Grundschulen_data, 
                        Mittelschulen_data,
                        by = "RS_Kreis"), 
                        Realschulen_data, 
                        by = "RS_Kreis"),
                        Gymnasien_data,
                        by = "RS_Kreis")

colnames(Kreise_Distanz) <- c("ARS",
                           "Grundschulen", 
                           "Mittelschulen", 
                           "Realschulen", 
                           "Gymnasien")


#head(Kreise_Distanz)

#' Hinzufügen dieser Informationen zu den Landkreis Geodaten: 

Kreise_Inklusion_Distanz_SP <- merge(Kreise_Inklusion_SP, Kreise_Distanz, 
                                     by = "ARS", all.x = F)

head(Kreise_Inklusion_Distanz_SP@data)


#' Zählen der FZs je Kreis:
#-------------------------------------------------------------------------------

Anzahl <- count(Förderzentren_SP@data, RS_Kreis)

names(Anzahl) <- c("ARS", "n_FZ")

#' Hinzufügen von #FZ und #Trias zum Landkreisdatensatz:
Kreise_Inklusion_Distanz_Anzahl_SP <- merge(Kreise_Inklusion_Distanz_SP, Anzahl, 
                                            by = "ARS")


#' Berechnung der Fläche jeder Kreises:
#-----------------------------------------------------------------------------------

Kreise_Inklusion_Distanz_Anzahl_SP$Fläche_km2 <- 
  area(Kreise_Inklusion_Distanz_Anzahl_SP)/1000000

#' Anzahl FZ und Trias pro km²
Kreise_Inklusion_Distanz_Anzahl_SP$n_FZ_km2 <- 
  Kreise_Inklusion_Distanz_Anzahl_SP$n_FZ/Kreise_Inklusion_Distanz_Anzahl_SP$Fläche_km2


#' Datenexport   
#-----------------------------------------------------------------------------------


#' Export als Shapefile:
writeOGR(Kreise_Inklusion_Distanz_Anzahl_SP, dsn = paste0(Pfad, "/Geodaten/Kreise"), 
         layer = "Kreise_Inklusionsanteil_Distanz", driver="ESRI Shapefile", overwrite_layer = T)

#' Export als CSV:
write.csv2(Kreise_Inklusion_Distanz_Anzahl_SP@data, 
           file = paste0(Pfad, "/Geodaten/Kreise/Kreise_Inklusionsanteil_Distanz.csv"))



#' Visualisierung   
#-----------------------------------------------------------------------------------

Kreise_Inklusion_Distanz_Anzahl_SF <- st_as_sf(Kreise_Inklusion_Distanz_Anzahl_SP)

#' Map Distanz je Kreis:

Distanz_2020 <- ggplot() +
  geom_sf(data = Kreise_Inklusion_Distanz_Anzahl_SF, aes(fill = Distanz_FZ), size = 0.1, color = NA) +
  #geom_sf(data = RBZ_BY_SF, aes(), color="black", alpha=0, size = 1) +
  #scale_fill_gradientn(colours = myPalette(70), 
   #                    limits=c(0, 70), 
    #                   breaks = seq(0, 70, 10)) +
  guides(fill = guide_colourbar(barwidth = 7, 
                                barheight = 1, 
                                frame.colour=c("black"),
                                frame.linewidth = 1, 
                                ticks.colour="black", 
                                direction="horizontal")) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "top")

Distanz_2020


#' Korrelationen: 

cor(Kreise_Inklusion_Distanz_Anzahl_SF$IA_2020, Kreise_Inklusion_Distanz_Anzahl_SF$Distanz_FZ, method = "pearson")

cor(Kreise_Inklusion_Distanz_Anzahl_SF$IA_2020, Kreise_Inklusion_Distanz_Anzahl_SF$n_FZ_km2, method = "spearman")


#' Scatterplots:

Kreise_Inklusion_Distanz_Anzahl_SF


ggplot(data = Kreise_Inklusion_Distanz_Anzahl_SF, aes(x=as.numeric(IA_2020), y = Distanz_FZ, fill = KTYP4)) +
  geom_point(shape = 21) +
#  geom_smooth(method='lm',formula = y~x) + 
  theme(legend.position = "top",
        legend.direction = "vertical")

ggplot(data = Kreise_Inklusion_Distanz_Anzahl_SF, aes(x=as.numeric(IA_2020), y = n_FZ_km2, fill = KTYP4)) +
  geom_point(shape = 21) +
#  geom_smooth(method='lm',formula = y~x) + 
  theme(legend.position = "top",
        legend.direction = "vertical")
