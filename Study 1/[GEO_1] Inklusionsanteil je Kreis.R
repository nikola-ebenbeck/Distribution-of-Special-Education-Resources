library(sp)
library(sf)
library(rgdal)
library(rgeos)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)


Pfad <- ""


#' Datensätze Import:
#-----------------------------------------------------------------------------------


Kreise_DE_SP <- readOGR(paste0(Pfad, "/Geodaten/Verwaltungseinheiten Deutschland/vg250_ebenen_0101/VG250_KRS.shp"), use_iconv = T, encoding = "UTF-8")

Schulen <- read.csv2(file = paste0(Pfad, "/Daten_Schulen.csv"))

RBZ_SF <- read_sf(paste0(Pfad, "/Geodaten/Verwaltungseinheiten Deutschland/vg250_ebenen_0101/VG250_RBZ.shp"), int64_as_string = T)


#' Datensätze vorprozessieren:
#-----------------------------------------------------------------------------------


#' Daten zur Inklusion:

Kreise_Inklusion_2010 <- Schulen %>% 
                        filter(Jahr == 2010) %>% 
                        distinct(RS_Kreis, .keep_all = T) %>%
                        subset(select = c("RS_Kreis","Jahr", "Inklusionsanteil_Kreis"))

Kreise_Inklusion_2020 <- Schulen %>% 
                        filter(Jahr == 2020) %>% 
                        distinct(RS_Kreis, .keep_all = T) %>%
                        subset(select = c("RS_Kreis","Jahr", "Inklusionsanteil_Kreis", "KTYP4"))
                        
Kreise_Inklusion <- merge(Kreise_Inklusion_2010, Kreise_Inklusion_2020, by = "RS_Kreis")

Kreise_Inklusion$RS_Kreis <- paste0("0", Kreise_Inklusion$RS_Kreis)

names(Kreise_Inklusion) <- c("ARS", "Jahr.x", "IA_2010", "Jahr.y", "IA_2020", "KTYP4")

Kreise_Inklusion  <-  subset(Kreise_Inklusion, select = c("ARS","IA_2010", "IA_2020", "KTYP4"))


#' Geodaten:

Kreise_BY_SP <- subset(Kreise_DE_SP, SN_L == "09", select = c("ARS", "GEN", "BEZ")) 
names(Kreise_BY_SP) <- c("ARS", "Name", "Art")

RBZ_BY_SF <- subset(RBZ_SF, SN_L == "09") 

#' Check: 

nrow(Kreise_BY_SP) == nrow(Kreise_Inklusion)


#' Datensätze mergen:
#-----------------------------------------------------------------------------------


Kreise_BY_Inklusion_SP <- merge(Kreise_BY_SP, Kreise_Inklusion, by = "ARS" )

head(Kreise_BY_Inklusion_SP@data)


#' Export:
#-----------------------------------------------------------------------------------


writeOGR(Kreise_BY_Inklusion_SP, dsn = paste0(Pfad, "/Geodaten/Kreise"), layer = "Kreise_Inklusionsanteil", driver="ESRI Shapefile", overwrite_layer = T)


#' Maps:
#-----------------------------------------------------------------------------------


Kreise_BY_Inklusion_SF <- st_as_sf(Kreise_BY_Inklusion_SP)

myPalette <- colorRampPalette(brewer.pal(9, "Blues"))


#' Map 2010:

IA_2010 <- ggplot() +
  geom_sf(data = Kreise_BY_Inklusion_SF, aes(fill = IA_2010), size = 0.1, color = NA) +
  geom_sf(data = RBZ_BY_SF, aes(), color="black", alpha=0, size = 1) +
  scale_fill_gradientn(colours = myPalette(70), 
                       limits=c(0, 70), 
                       breaks = seq(0, 70, 10)) +
  guides(fill = guide_colourbar(barwidth = 7, 
                                barheight = 1, 
                                frame.colour=c("black"),
                                frame.linewidth = 1, 
                                ticks.colour="black", 
                                direction="horizontal")) +
  theme_void() +
  theme(legend.title = element_blank()) 

#' Map 2020:

IA_2020 <- ggplot() +
  geom_sf(data = Kreise_BY_Inklusion_SF, aes(fill = IA_2020), size = 0.1, color = NA) +
  geom_sf(data = RBZ_BY_SF, aes(), color="black", alpha=0, size = 1) +
  scale_fill_gradientn(colours = myPalette(70), 
                       limits=c(0, 70), 
                       breaks = seq(0, 70, 10)) +
  guides(fill = guide_colourbar(barwidth = 7, 
                                barheight = 1, 
                                frame.colour=c("black"),
                                frame.linewidth = 1, 
                                ticks.colour="black", 
                                direction="horizontal")) +
  theme_void() +
  theme(legend.title = element_blank())

#' Maps combined:

IA_2010_2020 <- ggarrange(IA_2010, IA_2020,
                          ncol = 2, nrow = 1,
                          vjust = 2, hjust = -0.5,
                          font.label = list(size=10),
                          common.legend=T)

IA_2010_2020
