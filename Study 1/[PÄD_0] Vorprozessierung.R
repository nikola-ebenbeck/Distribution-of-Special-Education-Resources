################################################################################
## This Syntax belongs to the following article: ###############################
## Ebenbeck, Rieser, Gebhardt (2022): Das sonderpädagogische Fördersystem ######
## und die schulische Inklusion in Bayern zwischen 2010 und 2020. ##############
################################################################################
## contact: nikola.ebenbeck@ur.de ##############################################
################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(ggpubr)

#_______________________________________________________________________________

setwd("C:/Users/Nikola/OneDrive/Sogeffekt")

#_______________________________________________________________________________

#Einladen aller Daten aus dem Pfad / Ordner
##Ergebnis ist eine Liste von Tabellen
Daten_Schueler <- lapply(list.files("Bayern Daten Original/Daten_aufbereitet", 
                                    full.names = T), 
                         read.csv2, 
                         encoding = "UTF-8")
Daten_Adressen <- lapply(list.files("Bayern Geocoding", full.names = T), 
                         read.csv2, 
                         encoding = "UTF-8")

##Umbenennen der Tabellen in der Schüler-Liste
names(Daten_Schueler) <- c("Foerderschueler_2010", 
                           "Foederschueler_2015", 
                           "Foerderschueler_2020",
                           "Grundschueler_2010", 
                           "Grundschueler_2015", 
                           "Grundschueler_2020",
                           "Gymnasialschueler_2010", 
                           "Gymnasialschueler_2015", 
                           "Gymnasialschueler_2020",
                           "Mittelschueler_2010", 
                           "Mittelschueler_2015", 
                           "Mittelschueler_2020", 
                           "Realschueler_2010", 
                           "Realschueler_2015", 
                           "Realschueler_2020")

##Umbenennen der Tabellen in der Adressen-Liste                    
names(Daten_Adressen) <- c("Foerderschulen", 
                           "Grundschulen", 
                           "Gymnasien", 
                           "Mittelschulen", 
                           "Realschulen")

#_______________________________________________________________________________

#Anpassung der Spaltenbenennung

Daten_Schueler<- lapply(Daten_Schueler, setNames, nm=c("Jahr", 
                                                       "Schulart", 
                                                       "Schulnummer", 
                                                       "Kreis", 
                                                       "Gemeindekennzahl",
                                                       "Geschlecht", 
                                                       "Kontinent", 
                                                       "UNSD_Region", 
                                                       "Migrationshintergrund", 
                                                       "SPF", 
                                                       "MSD"))

Daten_Adressen<- lapply(Daten_Adressen, setNames, nm=c("Schulnummer", 
                                                       "Schultyp", 
                                                       "Name", 
                                                       "Strasse", 
                                                       "PLZ", 
                                                       "Ort", 
                                                       "Homepage", 
                                                       "Link"))

#_______________________________________________________________________________

#Erstellen einer Tabelle mit allen Schuelerdaten fÃ¼r alle Jahre und Schulen
Daten_Schueler <- bind_rows(Daten_Schueler)

#Erstellen einer Tabelle mit allen Adressen
Daten_Adressen <- bind_rows(Daten_Adressen)

#Erstellen einer Tabelle mit allen Schulen
Daten_Schulen <- unique(select(Daten_Schueler, 2:5))

#_______________________________________________________________________________

#ErgÃ¤nzen der Tabelle um die FÃ¶rderschwerpunke und Adressen
Daten_Schulen <- merge(Daten_Schulen, Daten_Adressen, by="Schulnummer", all.x=T)
Daten_Schulen <- select(Daten_Schulen, 1:5, 7:9)
Daten_Schulen <- distinct(Daten_Schulen, Schulnummer, .keep_all = T)
rm(Daten_Adressen)

#_______________________________________________________________________________

#Einfügen der Regionalschlüssel Informationen  
Daten_Schulen$RS_Gemeinde <- paste0("09", 
                                    str_sub(Daten_Schulen$Gemeindekennzahl,
                                            -6,
                                            -1))
Daten_Schulen$Gemeindekennzahl <- NULL
Daten_Schulen$RS_Kreis <- str_sub(Daten_Schulen$RS_Gemeinde,
                                  1,
                                  5)
Daten_Schulen$Kreis <- NULL

#_______________________________________________________________________________

#Ergänzen der Tabelle um KTYP4-Kategorien auf Kreisebene
Daten_KTYP4 <- read.csv("./Geodaten/Gebietseinheiten Deutschland/ge1000/ktyp4_1000/KTYP4_1000_Tabelle.csv"
                        , sep = ";",
                        header=T, 
                        quote='\"', 
                        colClasses = c("factor", 
                                       "character", 
                                       "numeric", 
                                       "character"))

Daten_Schulen <- merge(Daten_Schulen, 
                       Daten_KTYP4[c("RS", "SN_KTYP4", "KTYP4")], 
                       by.x = "RS_Kreis", 
                       by.y = "RS", 
                       all.x = T)

#_______________________________________________________________________________

#Herausfinden der Koordinaten aller Schulen
myOSMGeoCode <- function(street, zip, city, country){
  
  url <-paste0(
    "http://nominatim.openstreetmap.org/search.php?q=", 
    street, ",+", 
    zip, "+",
    city, ",+", 
    country, 
    "&limit=1&format=json")
  
  osm.json <- fromJSON(url,simplify=FALSE)
  if(length(osm.json) > 0){
    r1 <- osm.json[[1]]$lon
    r2 <- osm.json[[1]]$lat
    r3 <- osm.json[[1]]$display_name
    r4 <- "OK"
    geocode <- c(r1,r2,r3,r4)
  } else{
    url <-paste0("http://nominatim.openstreetmap.org/search.php?q=", 
                 zip, "+", city, ",+", country, "&limit=1&format=json")
    osm.json <- fromJSON(url,simplify=FALSE)
    if(length(osm.json) > 0){
      r1 <- osm.json[[1]]$lon
      r2 <- osm.json[[1]]$lat
      r3 <- osm.json[[1]]$display_name
      r4 <- "ZIP"
      geocode <- c(r1,r2,r3,r4)
    } else{
      geocode <- c(NA,NA,NA,"NOK")
    }
  }
  return(geocode)
}

Daten_Schulen_Koordinaten <- list()
for(i in 1:nrow(Daten_Schulen)){
  z <- myOSMGeoCode(Daten_Schulen$Strasse[i], 
                    Daten_Schulen$PLZ[i], 
                    Daten_Schulen$Ort[i], 
                    "Germany")
  Daten_Schulen_Koordinaten[[i]] <- z
}

#Umbauen zu Dataframe
Daten_Schulen_Koordinaten <- t(as.data.frame(Daten_Schulen_Koordinaten))
Daten_Schulen_Koordinaten <- as.data.frame(Daten_Schulen_Koordinaten)
rm(Daten_Schulen_Koordinaten)

#Hinzufuegen der Koordinaten zur Tabelle mit den Schulen
Daten_Schulen$lat <- Daten_Schulen_Koordinaten$V2
Daten_Schulen$lon <- Daten_Schulen_Koordinaten$V1

##Abspeichern der Tabelle
write.csv(Daten_Schulen, "Daten_Schulen_mit_Koordinaten.csv")

#_______________________________________________________________________________

##Ergänzen der Schultabelle um die Koordinaten
Daten_Schulen_Koordinaten <- read.csv("Daten_Schulen_mit_Koordinaten.csv")

Daten_Schulen <- merge(Daten_Schulen, 
                       Daten_Schulen_Koordinaten
                       [c("Schulnummer", "lat", "lon")], 
                       by.x = "Schulnummer", 
                       by.y = "Schulnummer", 
                       all.x = T)

rm(Daten_Schulen_Koordinaten)

#_______________________________________________________________________________

##Ergänzen der Schultabelle um die Jahreszahlen
Daten_Schulen <- merge(Daten_Schulen, Daten_Schueler[c("Schulnummer", "Jahr")], 
                       by.x = "Schulnummer", by.y = "Schulnummer", all.x = T)
Daten_Schulen <- distinct(Daten_Schulen)

#_______________________________________________________________________________

##Bereinigen von ungenauen Benennungen der Schulen
Daten_Schulen[Daten_Schulen == "GE; SFZ"] <- "SFZ"
Daten_Schulen[Daten_Schulen == "L; GE"] <- "GE"

#_______________________________________________________________________________

#Filtern von allen Schulen mit Förderschülern
Schulen_mit_Foerderschuelern <- filter(Daten_Schueler, MSD=="Ja")
Schulen_mit_Foerderschuelern <- select(Schulen_mit_Foerderschuelern, 1:5) 
Schulen_mit_Foerderschuelern <- merge(Schulen_mit_Foerderschuelern, 
                                      Daten_Schulen[c("Schulnummer", 
                                                      "SN_KTYP4")], 
                                      by.x = "Schulnummer", 
                                      by.y = "Schulnummer", 
                                      all.x = T)
Schulen_mit_Foerderschuelern <- distinct(Schulen_mit_Foerderschuelern)

##Setzen der Inklusionsinformation
Schulen_mit_Foerderschuelern$Inklusion <- "Ja"

##Hinzufügen der Inklusionsinformation zur Schul-Tabelle
Daten_Schulen <- merge(Daten_Schulen, 
                       Schulen_mit_Foerderschuelern
                       [c("Schulnummer", "Inklusion")], 
                       by.x = "Schulnummer", 
                       by.y = "Schulnummer", 
                       all.x = T)
Daten_Schulen <- distinct(Daten_Schulen)
Daten_Schulen[is.na(Daten_Schulen)] <- "Nein"

#_______________________________________________________________________________

#Berenigen von ungenauen Benennungen der Schulen
Daten_Schulen[Daten_Schulen == "Grundschule"] <- "Grundschulen"
Daten_Schulen[Daten_Schulen == "Gymnasium"] <- "Gymnasien"
Daten_Schulen[Daten_Schulen == "Mittel-/Hauptschule"] <- "Mittelschulen"
Daten_Schulen[Daten_Schulen == "Mittel-/Hauptschulen"] <- "Mittelschulen"
Daten_Schulen[Daten_Schulen == "Realschule"] <- "Realschulen"

SmF <- Schulen_mit_Foerderschuelern
SmF[SmF == "Grundschule"] <- "Grundschulen"
SmF[SmF == "Gymnasium"] <- "Gymnasien"
SmF[SmF == "Mittel-/Hauptschule"] <- "Mittelschulen"
SmF[SmF == "Mittel-/Hauptschulen"] <- "Mittelschulen"
SmF[SmF == "Realschule"] <- "Realschulen"

#Bereinigen von ungenauen Benennungen der Schulen in der Schüler-Tabelle
Daten_Schueler[Daten_Schueler == "Grundschule"] <- "Grundschulen"
Daten_Schueler[Daten_Schueler == "Gymnasium"] <- "Gymnasien"
Daten_Schueler[Daten_Schueler == "Mittel-/Hauptschule"] <- "Mittelschulen"
Daten_Schueler[Daten_Schueler == "Mittel-/Hauptschulen"] <- "Mittelschulen"
Daten_Schueler[Daten_Schueler == "Realschule"] <- "Realschulen"

##Vereinheitlichen des SPF / MSD / FB
Daten_Schueler$SPF[Daten_Schueler$SPF=="Ja"] <- "1"
Daten_Schueler$SPF[Daten_Schueler$SPF=="Nein"] <- "0"
Daten_Schueler$MSD[Daten_Schueler$MSD=="Ja"] <- "1"
Daten_Schueler$MSD[Daten_Schueler$MSD=="01"] <- "1"
Daten_Schueler$MSD[Daten_Schueler$MSD=="02"] <- "1"
Daten_Schueler$MSD[Daten_Schueler$MSD=="03"] <- "1"
Daten_Schueler$MSD[Daten_Schueler$MSD=="04"] <- "1"
Daten_Schueler$MSD[Daten_Schueler$MSD=="05"] <- "1"
Daten_Schueler$MSD[Daten_Schueler$MSD=="06"] <- "1"
Daten_Schueler$MSD[Daten_Schueler$MSD=="07"] <- "1"
Daten_Schueler$MSD[Daten_Schueler$MSD=="Nein"] <- "0"

Daten_Schueler$FB <- as.numeric(
  Daten_Schueler$SPF)+as.numeric(Daten_Schueler$MSD)
Daten_Schueler$FB[Daten_Schueler$FB=="2"] <- "Ja"
Daten_Schueler$FB[Daten_Schueler$FB=="1"] <- "Ja"
Daten_Schueler$FB[Daten_Schueler$FB=="0"] <- "Nein"

#_______________________________________________________________________________

##Values für alle Schüler in den verschiedenen Jahren
Alle_Schueler_2010 <- nrow(filter(Daten_Schueler, Jahr==2010))
Alle_Schueler_2015 <- nrow(filter(Daten_Schueler, Jahr==2015))
Alle_Schueler_2020 <- nrow(filter(Daten_Schueler, Jahr==2020))

##Values alle Schüler mit Förderbedarf in den Jahren

Alle_Schueler_SPF_2010 <- nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja"))
Alle_Schueler_SPF_2015 <- nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja"))
Alle_Schueler_SPF_2020 <- nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja"))

#_______________________________________________________________________________

##Ergänzen einer Schüler-ID
Daten_Schueler$"Schüler-ID" <- c(1:nrow(Daten_Schueler))
Daten_Schueler <- merge(Daten_Schueler, 
                        Daten_Schulen
                        [c("Schulnummer", 
                           "SN_KTYP4", 
                           "KTYP4")], 
                        by = "Schulnummer", 
                        all.x = T)
Daten_Schueler <- distinct(Daten_Schueler)

#_______________________________________________________________________________

##Ergänzen der "09" vor der Gemeindekennzahl im Jahr 2010
Daten_Schueler_test <- filter(Daten_Schueler, nchar(Gemeindekennzahl)==6)
Daten_Schueler_test$Gemeindekennzahl <- paste0(
  "9",Daten_Schueler_test$Gemeindekennzahl)
Daten_Schueler_long <- filter(Daten_Schueler, nchar(Gemeindekennzahl)==7)
Daten_Schueler <- rbind(Daten_Schueler_test, Daten_Schueler_long)
Daten_Schueler$Gemeindekennzahl <- paste0("0", Daten_Schueler$Gemeindekennzahl)

#_______________________________________________________________________________

##Hinzufügen der Kreiskennzahl zur Schülertabelle
Daten_Schueler <- left_join(Daten_Schueler, Daten_Schulen %>% 
                              select("Schulnummer", "RS_Kreis"), 
                            by=c("Schulnummer"="Schulnummer"))
Daten_Schueler <- distinct(Daten_Schueler)

#_______________________________________________________________________________

##Berechnen der Inklusionsquote auf Kreisebene
Daten_Kreise <- Daten_Schueler
Daten_Kreise <- filter(Daten_Kreise, FB=="Ja")

Daten_Kreise_2010 <- filter(Daten_Kreise, Jahr=="2010")
Daten_Kreise_2015 <- filter(Daten_Kreise, Jahr=="2015")
Daten_Kreise_2020 <- filter(Daten_Kreise, Jahr=="2020")

Daten_Kreise_2010_Anzahl_SPF <- count(Daten_Kreise_2010, RS_Kreis)
names(Daten_Kreise_2010_Anzahl_SPF)[2] <- "FB_gesamt"
Daten_Kreise_2010_Anzahl_Inklusion <- 
  Daten_Kreise_2010[!(Daten_Kreise_2010$Schulart=="Förderzentren"),]
Daten_Kreise_2010_Anzahl_Inklusion <- 
  count(Daten_Kreise_2010_Anzahl_Inklusion, RS_Kreis)
names(Daten_Kreise_2010_Anzahl_Inklusion)[2] <- "FB_inklusiv"

Daten_Kreise_2015_Anzahl_SPF <- count(Daten_Kreise_2015, RS_Kreis)
names(Daten_Kreise_2015_Anzahl_SPF)[2] <- "FB_gesamt"
Daten_Kreise_2015_Anzahl_Inklusion <- 
  Daten_Kreise_2015[!(Daten_Kreise_2015$Schulart=="Förderzentren"),]
Daten_Kreise_2015_Anzahl_Inklusion <- 
  count(Daten_Kreise_2015_Anzahl_Inklusion, RS_Kreis)
names(Daten_Kreise_2015_Anzahl_Inklusion)[2] <- "FB_inklusiv"

Daten_Kreise_2020_Anzahl_SPF <- count(Daten_Kreise_2020, RS_Kreis)
names(Daten_Kreise_2020_Anzahl_SPF)[2] <- "FB_gesamt"
Daten_Kreise_2020_Anzahl_Inklusion <- 
  Daten_Kreise_2020[!(Daten_Kreise_2020$Schulart=="Förderzentren"),]
Daten_Kreise_2020_Anzahl_Inklusion <- 
  count(Daten_Kreise_2020_Anzahl_Inklusion, RS_Kreis)
names(Daten_Kreise_2020_Anzahl_Inklusion)[2] <- "FB_inklusiv"

Daten_Kreise_2010 <- merge(Daten_Kreise_2010_Anzahl_SPF, 
                           Daten_Kreise_2010_Anzahl_Inklusion, by="RS_Kreis")
Daten_Kreise_2015 <- merge(Daten_Kreise_2015_Anzahl_SPF, 
                           Daten_Kreise_2015_Anzahl_Inklusion, by="RS_Kreis")
Daten_Kreise_2020 <- merge(Daten_Kreise_2020_Anzahl_SPF, 
                           Daten_Kreise_2020_Anzahl_Inklusion, by="RS_Kreis")

Daten_Kreise_2010 <- cbind(Daten_Kreise_2010, "Jahr"="2010")
Daten_Kreise_2015 <- cbind(Daten_Kreise_2015, "Jahr"="2015")
Daten_Kreise_2020 <- cbind(Daten_Kreise_2020, "Jahr"="2020")

Daten_Kreise <- rbind(Daten_Kreise_2010, Daten_Kreise_2015, Daten_Kreise_2020)
Daten_Kreise$Inklusionsanteil_Kreis <- 
  round(((Daten_Kreise$FB_inklusiv/Daten_Kreise$FB_gesamt)*100), digits=1)

##Änderung: Jahr ist Characterstring
Daten_Schulen$Jahr <- as.character(Daten_Schulen$Jahr)

##Hinzufügen der Inklusionsquote pro Kreis zu der Schulentabelle
Daten_Schulen <- left_join(Daten_Schulen, 
                           Daten_Kreise, 
                           by=c("RS_Kreis"="RS_Kreis", 
                                "Jahr"="Jahr"))

#_______________________________________________________________________________

##Hinzufügen des Kreises zur Schultablle
Daten_Schulen <- left_join(Daten_Schulen, Daten_KTYP4 %>% 
                             select(RS, GEN), by=c("RS_Kreis"="RS"))

##Hinzufügen des Kreises zur Schülertabelle
Daten_Schueler <- left_join(Daten_Schueler, Daten_KTYP4 %>% 
                              select(RS, GEN), by=c("RS_Kreis"="RS"))

#_______________________________________________________________________________

#Abspeichern der fertigen Tabellen
write.csv(Daten_Schulen, 
          file="C:/Users/Nikola/OneDrive/Sogeffekt/Daten_Schulen.csv", 
          row.names=F)
write.csv(Daten_Schueler, 
          file="C:/Users/Nikola/OneDrive/Sogeffekt/Daten_Schueler.csv", 
          row.names=F)

#_______________________________________________________________________________
