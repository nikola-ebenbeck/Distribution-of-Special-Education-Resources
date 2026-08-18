
library(dplyr)
library(tidyr)
library(stringr)
library(ggpubr)
library(ggsignif)

#_______________________________________________________________________________

setwd("")

Daten_Schulen <- read.csv2("Daten_Schulen.csv")
Daten_Schueler <- read.csv("Daten_Schueler.csv")

#_______________________________________________________________________________

#Prozentualer Anteil der Regelschulen mit inklusiver Beschulung

#Art und Anzahl der Förderschulen in Bayern im Vergleich
# 4 = dünn besiedelt,3 = ländlich, 2 = Städtisch, 1 = Kreisfreie Großstädte

SFZ <- data.frame("Jahr"=c("2010", "2015", "2020"),
                  "1"=c((nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2010" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2020" & SN_KTYP4=="1")))),
                  "2"=c((nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2010" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2020" & SN_KTYP4=="2")))),
                  "3"=c((nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2010" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2020" & SN_KTYP4=="3")))),
                  "4"=c((nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2010" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="4")))),
                  "Schultyp"="SFZ")

Sehen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                    "1"=c((nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2010" & SN_KTYP4=="1"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="1"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2020" & SN_KTYP4=="1")))),
                    "2"=c((nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2010" & SN_KTYP4=="2"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="2"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2020" & SN_KTYP4=="2")))),
                    "3"=c((nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2010" & SN_KTYP4=="3"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="3"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2020" & SN_KTYP4=="3")))),
                    "4"=c((nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2010" & SN_KTYP4=="4"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="4"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="4")))),
                    "Schultyp"="Sehen")

Hören <- data.frame("Jahr"=c("2010", "2015", "2020"),
                    "1"=c((nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2010" & SN_KTYP4=="1"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="1"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2020" & SN_KTYP4=="1")))),
                    "2"=c((nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2010" & SN_KTYP4=="2"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="2"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2020" & SN_KTYP4=="2")))),
                    "3"=c((nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2010" & SN_KTYP4=="3"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="3"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2020" & SN_KTYP4=="3")))),
                    "4"=c((nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2010" & SN_KTYP4=="4"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="4"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="4")))),
                    "Schultyp"="Hören")

GE <- data.frame("Jahr"=c("2010", "2015", "2020"),
                 "1"=c((nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2010" & SN_KTYP4=="1"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="1"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2020" & SN_KTYP4=="1")))),
                 "2"=c((nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2010" & SN_KTYP4=="2"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="2"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2020" & SN_KTYP4=="2")))),
                 "3"=c((nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2010" & SN_KTYP4=="3"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="3"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2020" & SN_KTYP4=="3")))),
                 "4"=c((nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2010" & SN_KTYP4=="4"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="4"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="4")))),
                 "Schultyp"="GE")

KME <- data.frame("Jahr"=c("2010", "2015", "2020"),
                  "1"=c((nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2010" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2020" & SN_KTYP4=="1")))),
                  "2"=c((nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2010" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2020" & SN_KTYP4=="2")))),
                  "3"=c((nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2010" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2020" & SN_KTYP4=="3")))),
                  "4"=c((nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2010" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="4")))),
                  "Schultyp"="KME")

Sprache <- data.frame("Jahr"=c("2010", "2015", "2020"),
                      "1"=c((nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2010" & SN_KTYP4=="1"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="1"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2020" & SN_KTYP4=="1")))),
                      "2"=c((nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2010" & SN_KTYP4=="2"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="2"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2020" & SN_KTYP4=="2")))),
                      "3"=c((nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2010" & SN_KTYP4=="3"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="3"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2020" & SN_KTYP4=="3")))),
                      "4"=c((nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2010" & SN_KTYP4=="4"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="4"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="4")))),
                      "Schultyp"="Sprache")

Lernen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                     "1"=c((nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2010" & SN_KTYP4=="1"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="1"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2020" & SN_KTYP4=="1")))),
                     "2"=c((nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2010" & SN_KTYP4=="2"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="2"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2020" & SN_KTYP4=="2")))),
                     "3"=c((nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2010" & SN_KTYP4=="3"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="3"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2020" & SN_KTYP4=="3")))),
                     "4"=c((nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2010" & SN_KTYP4=="4"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="4"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="4")))),
                     "Schultyp"="Lernen")

ESE <- data.frame("Jahr"=c("2010", "2015", "2020"),
                  "1"=c((nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2010" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2020" & SN_KTYP4=="1")))),
                  "2"=c((nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2010" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2020" & SN_KTYP4=="2")))),
                  "3"=c((nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2010" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2020" & SN_KTYP4=="3")))),
                  "4"=c((nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2010" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="4")))),
                  "Schultyp"="ESE")

Anzahl_Foederschulen_Raum <- rbind(SFZ, Sehen, Hören, GE, KME, Sprache, Lernen, ESE)

rm(ESE, GE, Hören, KME, Lernen, Sehen, SFZ, Sprache)

#_______________________________________________________________________________

#Art und Anzahl der Schulen, die Schüler:innen mit Förderbedarf beschulen

Alle_Grundschulen_1 <- nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="1"))
Alle_Grundschulen_2 <- nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="2"))
Alle_Grundschulen_3 <- nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="3"))
Alle_Grundschulen_4 <- nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="4"))

Alle_Gymnasien_1 <- nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="1"))
Alle_Gymnasien_2 <- nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="2"))
Alle_Gymnasien_3 <- nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="3"))
Alle_Gymnasien_4 <- nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="4"))

Alle_Mittelschulen_1 <- nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="1"))
Alle_Mittelschulen_2 <- nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="2"))
Alle_Mittelschulen_3 <- nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="3"))
Alle_Mittelschulen_4 <- nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="4"))

Alle_Realschulen_1 <- nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="1"))
Alle_Realschulen_2 <- nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="2"))
Alle_Realschulen_3 <- nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="3"))
Alle_Realschulen_4 <- nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="4"))


Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "1"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                           "2"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                           "3"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                           "4"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                           "Schulart"="Grundschulen")

Grundschulen <- mutate(Grundschulen, X1=round((X1/Alle_Grundschulen_1)*100))
Grundschulen <- mutate(Grundschulen, X2=round((X2/Alle_Grundschulen_2)*100))
Grundschulen <- mutate(Grundschulen, X3=round((X3/Alle_Grundschulen_3)*100))
Grundschulen <- mutate(Grundschulen, X4=round((X4/Alle_Grundschulen_4)*100))

Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                            "2"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                            "3"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                            "4"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                            "Schulart"="Mittelschulen")

Mittelschulen <- mutate(Mittelschulen, X1=round((X1/Alle_Mittelschulen_1)*100))
Mittelschulen <- mutate(Mittelschulen, X2=round((X2/Alle_Mittelschulen_2)*100))
Mittelschulen <- mutate(Mittelschulen, X3=round((X3/Alle_Mittelschulen_3)*100))
Mittelschulen <- mutate(Mittelschulen, X4=round((X4/Alle_Mittelschulen_4)*100))

Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "1"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                          "2"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                          "3"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                          "4"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                          "Schulart"="Realschulen")

Realschulen <- mutate(Realschulen, X1=round((X1/Alle_Realschulen_1)*100))
Realschulen <- mutate(Realschulen, X2=round((X2/Alle_Realschulen_2)*100))
Realschulen <- mutate(Realschulen, X3=round((X3/Alle_Realschulen_3)*100))
Realschulen <- mutate(Realschulen, X4=round((X4/Alle_Realschulen_4)*100))

Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "1"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="1"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="1"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="1")))),
                        "2"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="2"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="2"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="2")))),
                        "3"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="3"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="3"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="3")))),
                        "4"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="4"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4")))),
                        "Schulart"="Gymnasien")

Gymnasien <- mutate(Gymnasien, X1=round((X1/Alle_Gymnasien_1)*100))
Gymnasien <- mutate(Gymnasien, X2=round((X2/Alle_Gymnasien_2)*100))
Gymnasien <- mutate(Gymnasien, X3=round((X3/Alle_Gymnasien_3)*100))
Gymnasien <- mutate(Gymnasien, X4=round((X4/Alle_Gymnasien_4)*100))

Anzahl_Regelschulen_Inklusion <- rbind(Grundschulen, Mittelschulen, Realschulen, Gymnasien)

Anzahl_Regelschulen_Inklusion[Anzahl_Regelschulen_Inklusion == "Grundschulen"] <- "GS"
Anzahl_Regelschulen_Inklusion[Anzahl_Regelschulen_Inklusion == "Gymnasien"] <- "GY"
Anzahl_Regelschulen_Inklusion[Anzahl_Regelschulen_Inklusion == "Realschulen"] <- "RS"
Anzahl_Regelschulen_Inklusion[Anzahl_Regelschulen_Inklusion == "Mittelschulen"] <- "MS"


#Abbildung
X1 <- ggplot(data=Anzahl_Regelschulen_Inklusion, aes(fill=Jahr, x=Schulart, y=X1)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anteil in %", title="Kreisfreie Großstädte") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

X2 <- ggplot(data=Anzahl_Regelschulen_Inklusion, aes(fill=Jahr, x=Schulart, y=X2)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anteil in %", title="Städtische Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

X3 <- ggplot(data=Anzahl_Regelschulen_Inklusion, aes(fill=Jahr, x=Schulart, y=X3)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anteil in %", title="Ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

X4 <- ggplot(data=Anzahl_Regelschulen_Inklusion, aes(fill=Jahr, x=Schulart, y=X4)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anteil in %", title="Dünn besiedelte ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()


Abbildung_Anzahl_Regelschulen <- ggarrange(X1, X2, X3, X4,
                                           ncol = 4, nrow = 1,
                                           vjust = 2, hjust = -0.5,
                                           font.label = list(size=10),
                                           common.legend=T)

#_______________________________________________________________________________

#Anzahl und Anteil der Schüler:innen in Bayern an den verschiedenen Schularten

Foerderschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                             "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010"))),
                                         (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015"))),
                                         (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020")))),
                             "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                         (round(((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                         (round(((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                             "Schulart"="Förderschulen")

Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010"))),
                                       (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015"))),
                                       (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020")))),
                           "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                       (round(((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                       (round(((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                           "Schulart"="Grundschulen")

Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010"))),
                                        (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015"))),
                                        (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020")))),
                            "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                        (round(((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                        (round(((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                            "Schulart"="Mittelschulen")

Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010"))),
                                      (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015"))),
                                      (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020")))),
                          "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                      (round(((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                      (round(((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                          "Schulart"="Realschulen")

Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010"))),
                                    (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015"))),
                                    (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020")))),
                        "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                    (round(((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                    (round(((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                        "Schulart"="Gymnasien")

Anzahl_Anteil_Alle_Schueler <- rbind(Foerderschulen, Grundschulen, Mittelschulen, Realschulen, Gymnasien)

#_______________________________________________________________________________

#Anzahl an Schüler:innen mit SPF, die die Schularten im jeweiligen Bereich besucht haben
Förderschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                            "2"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                            "3"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                            "4"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                            "Schulart"="Förderschulen")

Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "1"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                           "2"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                           "3"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                           "4"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                           "Schulart"="Grundschulen")

Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                            "2"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                            "3"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                            "4"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                            "Schulart"="Mittelschulen")

Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "1"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                          "2"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                          "3"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                          "4"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                          "Schulart"="Realschulen")

Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "1"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                        "2"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                        "3"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                        "4"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                        "Schulart"="Gymnasien")


Anzahl_Schueler_SPF_Regelschulen <- rbind(Förderschulen, Grundschulen, Mittelschulen, Realschulen, Gymnasien)

#Anzahl diagnostizierter Förderbedarf

Anzahl_Anteil_Foerderbedarf_Schueler <- data.frame("Jahr"=c("2010", "2015", "2020"),
                                                   "Anzahl"=c(Alle_Schueler_SPF_2010,
                                                              Alle_Schueler_SPF_2015,
                                                              Alle_Schueler_SPF_2020),
                                                   "Anteil"=c((Alle_Schueler_SPF_2010/Alle_Schueler_2010)*100),
                                                   ((Alle_Schueler_SPF_2015/Alle_Schueler_2015)*100),
                                                   ((Alle_Schueler_SPF_2020/Alle_Schueler_2020)*100))

#Anteil der Schüler:innen mit FB, die die Schularten im jeweiligen Bereich besucht haben

Förderschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "Schulart"="Förderschulen",
                            "Beschulung"="separativ",
                            "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                            "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                            "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                            "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "Schulart"="Grundschulen",
                           "Beschulung"="inklusiv",
                           "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                           "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                           "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                           "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "Schulart"="Mittelschulen",
                            "Beschulung"="inklusiv",
                            "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                            "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                            "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                            "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "Schulart"="Realschulen",
                          "Beschulung"="inklusiv",
                          "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                          "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                          "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                          "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "Schulart"="Gymnasien",
                        "Beschulung"="inklusiv",
                        "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                        "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                        "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                        "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Anteil_SPF_räumlicher_Vergleich <- rbind(Förderschulen, Grundschulen, Mittelschulen, Realschulen, Gymnasien)

Anteil_SPF_räumlicher_Vergleich$X1 <- round(Anteil_SPF_räumlicher_Vergleich$X1, digits=1)
Anteil_SPF_räumlicher_Vergleich$X2 <- round(Anteil_SPF_räumlicher_Vergleich$X2, digits=1)
Anteil_SPF_räumlicher_Vergleich$X3 <- round(Anteil_SPF_räumlicher_Vergleich$X3, digits=1)
Anteil_SPF_räumlicher_Vergleich$X4 <- round(Anteil_SPF_räumlicher_Vergleich$X4, digits=1)


#Abbildung inklusive Beschulung bei Schüler:innen mit SPF

Y1 <- ggplot(data=Anteil_SPF_räumlicher_Vergleich, aes(fill=Schulart, x=Beschulung, y=X1)) +
  geom_bar(position="stack", stat="identity", color="black") +
  facet_grid(.~Jahr) +
  labs(x="Beschulung", y="Anteil in %", title="Kreisfreie Großstädte") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

Y2 <- ggplot(data=Anteil_SPF_räumlicher_Vergleich, aes(fill=Schulart, x=Beschulung, y=X2)) +
  geom_bar(position="stack", stat="identity", color="black") +
  facet_grid(.~Jahr) +
  labs(x="Beschulung", y="Anteil in %", title="Städtische Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

Y3 <- ggplot(data=Anteil_SPF_räumlicher_Vergleich, aes(fill=Schulart, x=Beschulung, y=X3)) +
  geom_bar(position="stack", stat="identity", color="black") +
  facet_grid(.~Jahr) +
  labs(x="Beschulung", y="Anteil in %", title="Ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

Y4 <- ggplot(data=Anteil_SPF_räumlicher_Vergleich, aes(fill=Schulart, x=Beschulung, y=X4)) +
  geom_bar(position="stack", stat="identity", color="black") +
  facet_grid(.~Jahr) +
  labs(x="Beschulung", y="Anteil in %", title="Dünn besiedelte ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

Abbildung_Anzahl_Regelschulen <- ggarrange(Y1, Y2, Y3, Y4,
                                           ncol = 1, nrow = 4,
                                           vjust = 2, hjust = -0.5,
                                           font.label = list(size=10),
                                           common.legend=T)

Abbildung_Anzahl_Regelschulen


#Voranalyse Verteilung der Inklusionsquote nach Kreisen

Abbildung_Kreise_Inklusion <- select(Daten_Schulen, RS_Kreis, SN_KTYP4, Jahr, FB_gesamt, Inklusionsanteil_Kreis, GEN)
Abbildung_Kreise_Inklusion <- distinct(Abbildung_Kreise_Inklusion)

Landkreise <- count(Abbildung_Kreise_Inklusion, GEN)
Abbildung_Kreise_Inklusion <- merge(Abbildung_Kreise_Inklusion, Landkreise, by.x = "GEN", by.y = "GEN")
Landkreise <- filter(Abbildung_Kreise_Inklusion, n==6)
Landkreise <- select(Landkreise, GEN, RS_Kreis, SN_KTYP4)
Landkreise <- distinct(Landkreise)
Landkreise$Ende <- Landkreise$RS_Kreis
Landkreise$Ende <- as.numeric(Landkreise$Ende)
Landkreise_Land <- Landkreise %>% group_by(GEN) %>% summarise(Ende=max(Ende))
Landkreise_Land$Region <- "Land"
Landkreise_Stadt <- Landkreise %>% group_by(GEN) %>% summarise(Ende=min(Ende))
Landkreise_Stadt$Region <- "Stadt"
Landkreise <- rbind(Landkreise_Stadt, Landkreise_Land)
Landkreise$Ende <- paste0("0", Landkreise$Ende)
Landkreise$Ende <- as.integer(Landkreise$Ende)
Abbildung_Kreise_Inklusion <- left_join(Abbildung_Kreise_Inklusion, Landkreise, 
                                        by=c("GEN"="GEN", "RS_Kreis"="Ende"))
Test <- filter(Abbildung_Kreise_Inklusion, Region=="Land")
Test <- distinct(Test, GEN, Region, RS_Kreis)
Test$GEN_neu <- paste0("Lkr. ", Test$GEN)
Test <- distinct(Test, GEN_neu, RS_Kreis)
Abbildung_Kreise_Inklusion <- distinct(Abbildung_Kreise_Inklusion, GEN, RS_Kreis, SN_KTYP4, Jahr, FB_gesamt, Inklusionsanteil_Kreis)
Abbildung_Kreise_Inklusion <- merge(Abbildung_Kreise_Inklusion, Test, by.x="RS_Kreis", by.y="RS_Kreis", all.x=T)
Abbildung_Kreise_Inklusion$GEN_neu[is.na(Abbildung_Kreise_Inklusion$GEN_neu)] <- Abbildung_Kreise_Inklusion$GEN[is.na(Abbildung_Kreise_Inklusion$GEN_neu)]
Abbildung_Kreise_Inklusion$SN_KTYP4 <- as.factor(Abbildung_Kreise_Inklusion$SN_KTYP4)
Abbildung_Kreise_Inklusion_2015 <- filter(Abbildung_Kreise_Inklusion, Jahr!=2015)

#Abbildung Inklusionsquote nach Kreisen

A <- ggplot(data = filter(Abbildung_Kreise_Inklusion_2015, Jahr=="2010"), 
            aes(fill=SN_KTYP4, x = reorder(GEN_neu,-Inklusionsanteil_Kreis), y = Inklusionsanteil_Kreis)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colorBlindGrey8, name="Kreistyp", 
                    labels=c("Kreisfreie Großstädte", "Städtische Kreise", "Ländliche Kreise", "Dünn besiedelte ländliche Kreise")) +
  scale_y_continuous(name = "Inklusionsanteil (%)",
                     expand = c(0,0),
                     limits = c(0, 70),
                     breaks = seq(0, 100, 10),) +
  scale_x_discrete (expand = c(0,0),) +
  geom_text(x=90, y=60, label="2010", color="black", size=15) +
  theme_bw() +
  theme(axis.text.x=element_text(size=10, angle = 90, vjust = 0,5, hjust=1), 
        legend.position = "top", 
        axis.title.x=element_blank())

B <- ggplot(data = filter(Abbildung_Kreise_Inklusion_2015, Jahr=="2020"), 
            aes(fill=SN_KTYP4, x = reorder(GEN_neu,-Inklusionsanteil_Kreis), y = Inklusionsanteil_Kreis)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colorBlindGrey8, name="Kreistyp", 
                    labels=c("Kreisfreie Großstädte", "Städtische Kreise", "Ländliche Kreise", "Dünn besiedelte ländliche Kreise")) +
  scale_y_continuous(name = "Inklusionsanteil (%)",
                     expand = c(0,0),
                     limits = c(0, 70),
                     breaks = seq(0, 100, 10),) +
  scale_x_discrete (expand = c(0,0),) +
  geom_text(x=90, y=60, label="2020"
            , color="black", size=15) +
  theme_bw() +
  theme(axis.text.x=element_text(size=10, angle = 90, vjust = 0,5, hjust=1), 
        legend.position = "top", 
        axis.title.x=element_blank())


Abbildung_Kreise_Inklusion_Jahr <- ggarrange(A, B,
                                             ncol = 1, nrow = 2,
                                             vjust = 2, hjust = -0.5,
                                             font.label = list(size=10),
                                             common.legend=T)

Abbildung_Kreise_Inklusion_Jahr

# Abbildung Boxplots Veränderung des Inklusionsanteils i. B. a. den Kreistyp

Regionen <- c("1"="Large Cities", 
              "2"="Urban Counties", 
              "3"="Rural Counties", 
              "4"="Sparsely Populated")

summary(Abbildung_Kreise_Inklusion)
Abbildung_Kreise_Inklusion$Jahr <- as.character(Abbildung_Kreise_Inklusion$Jahr)
Abbildung_Kreise_Inklusion$Inklusionsanteil_Kreis <- as.numeric(Abbildung_Kreise_Inklusion$Inklusionsanteil_Kreis)

Abbildung_Boxplot_Inklusion_Zeit <-
  ggplot(data=Abbildung_Kreise_Inklusion,
         aes(x=Jahr, y=Inklusionsanteil_Kreis)) +
  geom_boxplot(fill="lightblue", color="black") +
  coord_flip() +
  scale_y_continuous(name="Inklusion Rate (%)") +
  facet_wrap(.~SN_KTYP4, labeller = as_labeller(Regionen),
             nrow=2) +
  theme_bw() +
  theme(axis.title.x=element_blank())

Abbildung_Boxplot_Inklusion_Zeit

# Varianzanalyse

KTYP4_1 <- filter(Abbildung_Kreise_Inklusion, SN_KTYP4==1)
KTYP4_2 <- filter(Abbildung_Kreise_Inklusion, SN_KTYP4==2)
KTYP4_3 <- filter(Abbildung_Kreise_Inklusion, SN_KTYP4==3)
KTYP4_4 <- filter(Abbildung_Kreise_Inklusion, SN_KTYP4==4)

anova(lm(Inklusionsanteil_Kreis ~ Jahr, KTYP4_1))
TukeyHSD(aov(KTYP4_1$Inklusionsanteil_Kreis ~ KTYP4_1$Jahr))

anova(lm(Inklusionsanteil_Kreis ~ Jahr, KTYP4_2))
TukeyHSD(aov(KTYP4_2$Inklusionsanteil_Kreis ~ KTYP4_2$Jahr))

anova(lm(Inklusionsanteil_Kreis ~ Jahr, KTYP4_3))
TukeyHSD(aov(KTYP4_3$Inklusionsanteil_Kreis ~ KTYP4_3$Jahr))

anova(lm(Inklusionsanteil_Kreis ~ Jahr, KTYP4_4))
TukeyHSD(aov(KTYP4_4$Inklusionsanteil_Kreis ~ KTYP4_4$Jahr))

Jahr_2010 <- filter(Abbildung_Kreise_Inklusion, Jahr=="2010")
Jahr_2015 <- filter(Abbildung_Kreise_Inklusion, Jahr=="2015")
Jahr_2020 <- filter(Abbildung_Kreise_Inklusion, Jahr=="2020")

anova(lm(Inklusionsanteil_Kreis ~ SN_KTYP4, Jahr_2010))
TukeyHSD(aov(Jahr_2010$Inklusionsanteil_Kreis ~ Jahr_2010$SN_KTYP4))

anova(lm(Inklusionsanteil_Kreis ~ SN_KTYP4, Jahr_2015))
TukeyHSD(aov(Jahr_2015$Inklusionsanteil_Kreis ~ Jahr_2015$SN_KTYP4))

anova(lm(Inklusionsanteil_Kreis ~ SN_KTYP4, Jahr_2020))
TukeyHSD(aov(Jahr_2020$Inklusionsanteil_Kreis ~ Jahr_2020$SN_KTYP4))

#_______________________________________________________________________________
#Anzahl an Regelschulen im zeitlichen und räumlichen Vergleich

Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "1"=c((nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                           "2"=c((nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                           "3"=c((nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                           "4"=c((nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                           "Schulart"="GS")

Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                            "2"=c((nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                            "3"=c((nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                            "4"=c((nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                            "Schulart"="MS")

Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "1"=c((nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                          "2"=c((nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                          "3"=c((nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                          "4"=c((nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                          "Schulart"="RS")

Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "1"=c((nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="1"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="1"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="1")))),
                        "2"=c((nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="2"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="2"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="2")))),
                        "3"=c((nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="3"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="3"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="3")))),
                        "4"=c((nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="4"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4")))),
                        "Schulart"="GY")

Förderzentren <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="1"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="1"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="1")))),
                            "2"=c((nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="2"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="2"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="2")))),
                            "3"=c((nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="3"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="3"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="3")))),
                            "4"=c((nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="4"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="4"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="4")))),
                            "Schulart"="FS")

Anzahl_Regelschulen_Raum <- rbind(Grundschulen, Mittelschulen, Realschulen, Gymnasien, Förderzentren)

#Abbildung Anzahl

Z1 <- ggplot(data=Anzahl_Regelschulen_Raum, aes(fill=Jahr, x=Schulart, y=X1)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anzahl", title="Kreisfreie Großstädte") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 800) +
  theme_bw()

Z2 <- ggplot(data=Anzahl_Regelschulen_Raum, aes(fill=Jahr, x=Schulart, y=X2)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anzahl", title="Städtische Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 800) +
  theme_bw()

Z3 <- ggplot(data=Anzahl_Regelschulen_Raum, aes(fill=Jahr, x=Schulart, y=X3)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anzahl", title="Ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 800) +
  theme_bw()

Z4 <- ggplot(data=Anzahl_Regelschulen_Raum, aes(fill=Jahr, x=Schulart, y=X4)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anzahl", title="Dünn besiedelte ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 800) +
  theme_bw()


Abbildung_Anzahl_Regelschulen <- ggarrange(Z1, Z2, Z3, Z4,
                                           ncol = 2, nrow = 2,
                                           vjust = 2, hjust = -0.5,
                                           font.label = list(size=10),
                                           common.legend=T)

Abbildung_Anzahl_Regelschulen

# Anteil Schulen in Bayern

Alle_Schulen_2010 <- nrow(filter(Daten_Schulen, Jahr==2010))
Alle_Schulen_2015 <- nrow(filter(Daten_Schulen, Jahr==2015))
Alle_Schulen_2020 <- nrow(filter(Daten_Schulen, Jahr==2020))

Schulen_2010 <- count((filter(Daten_Schulen, Jahr==2010)), Schulart)
Schulen_2015 <- count((filter(Daten_Schulen, Jahr==2015)), Schulart)
Schulen_2020 <- count((filter(Daten_Schulen, Jahr==2020)), Schulart)

Schulen_2010$n <- round((Schulen_2010$n/Alle_Schulen_2010)*100)
Schulen_2015$n <- round((Schulen_2015$n/Alle_Schulen_2015)*100)
Schulen_2020$n <- round((Schulen_2020$n/Alle_Schulen_2020)*100)

#_______________________________________________________________________________

# Modellregion Kempten

## Ergänzen der Tabelle um die Veränderung der Inklusionsquote zwischen 2010 und 2020

Kreise_Inklusionsquote <- filter(Daten_Schulen, Jahr%in%c("2010", "2020"))
Kreise_Inklusionsquote <- select(Kreise_Inklusionsquote, 
                                 RS_Kreis, 
                                 SN_KTYP4, 
                                 Jahr, 
                                 Inklusionsanteil_Kreis, 
                                 GEN)
Kreise_Inklusionsquote <- distinct(Kreise_Inklusionsquote)
Kreise_Inklusionsquote <- spread(Kreise_Inklusionsquote, 
                                 Jahr, 
                                 Inklusionsanteil_Kreis)

Kreise_Inklusionsquote$`2010` <- as.numeric(Kreise_Inklusionsquote$`2010`)
Kreise_Inklusionsquote$`2020` <- as.numeric(Kreise_Inklusionsquote$`2020`)

Kreise_Inklusionsquote$Differenz <- Kreise_Inklusionsquote$`2020`-Kreise_Inklusionsquote$`2010`

summary(Kreise_Inklusionsquote)

##Abbildung Boxplot

ggplot(Kreise_Inklusionsquote, aes(y=Differenz)) +
  stat_boxplot(geom="errorbar", width = 0.2) +
  geom_boxplot(fill="lightblue") +
  labs(y="Veränderung der Inklusionsquote") +
  geom_hline(yintercept = 23.1, colour="red", size=1) +
  annotate(geom="text", x=0.1, y=26, label="Kempten", colour="red") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 

#_______________________________________________________________________________
#Prozentualer Anteil der Regelschulen, die Schüler:innen mit Förderbedarf beschulen

#Art und Anzahl der Förderschulen in Bayern im zeitlichen und räumlichen Vergleich
# 4 = dünn besiedelt,3 = ländliche Kreise, 2 = Städtische Kreise, 1 = Kreisfreie Großstädte


SFZ <- data.frame("Jahr"=c("2010", "2015", "2020"),
                  "1"=c((nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2010" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2020" & SN_KTYP4=="1")))),
                  "2"=c((nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2010" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2020" & SN_KTYP4=="2")))),
                  "3"=c((nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2010" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2020" & SN_KTYP4=="3")))),
                  "4"=c((nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2010" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="SFZ" & Jahr=="2015" & SN_KTYP4=="4")))),
                  "Schultyp"="SFZ")

Sehen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                    "1"=c((nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2010" & SN_KTYP4=="1"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="1"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2020" & SN_KTYP4=="1")))),
                    "2"=c((nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2010" & SN_KTYP4=="2"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="2"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2020" & SN_KTYP4=="2")))),
                    "3"=c((nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2010" & SN_KTYP4=="3"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="3"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2020" & SN_KTYP4=="3")))),
                    "4"=c((nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2010" & SN_KTYP4=="4"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="4"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Sehen" & Jahr=="2015" & SN_KTYP4=="4")))),
                    "Schultyp"="Sehen")

Hören <- data.frame("Jahr"=c("2010", "2015", "2020"),
                    "1"=c((nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2010" & SN_KTYP4=="1"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="1"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2020" & SN_KTYP4=="1")))),
                    "2"=c((nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2010" & SN_KTYP4=="2"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="2"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2020" & SN_KTYP4=="2")))),
                    "3"=c((nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2010" & SN_KTYP4=="3"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="3"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2020" & SN_KTYP4=="3")))),
                    "4"=c((nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2010" & SN_KTYP4=="4"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="4"))),
                          (nrow(filter(Daten_Schulen, Schultyp=="Hören" & Jahr=="2015" & SN_KTYP4=="4")))),
                    "Schultyp"="Hören")

GE <- data.frame("Jahr"=c("2010", "2015", "2020"),
                 "1"=c((nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2010" & SN_KTYP4=="1"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="1"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2020" & SN_KTYP4=="1")))),
                 "2"=c((nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2010" & SN_KTYP4=="2"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="2"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2020" & SN_KTYP4=="2")))),
                 "3"=c((nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2010" & SN_KTYP4=="3"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="3"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2020" & SN_KTYP4=="3")))),
                 "4"=c((nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2010" & SN_KTYP4=="4"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="4"))),
                       (nrow(filter(Daten_Schulen, Schultyp=="GE" & Jahr=="2015" & SN_KTYP4=="4")))),
                 "Schultyp"="GE")

KME <- data.frame("Jahr"=c("2010", "2015", "2020"),
                  "1"=c((nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2010" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2020" & SN_KTYP4=="1")))),
                  "2"=c((nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2010" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2020" & SN_KTYP4=="2")))),
                  "3"=c((nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2010" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2020" & SN_KTYP4=="3")))),
                  "4"=c((nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2010" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="KME" & Jahr=="2015" & SN_KTYP4=="4")))),
                  "Schultyp"="KME")

Sprache <- data.frame("Jahr"=c("2010", "2015", "2020"),
                      "1"=c((nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2010" & SN_KTYP4=="1"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="1"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2020" & SN_KTYP4=="1")))),
                      "2"=c((nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2010" & SN_KTYP4=="2"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="2"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2020" & SN_KTYP4=="2")))),
                      "3"=c((nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2010" & SN_KTYP4=="3"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="3"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2020" & SN_KTYP4=="3")))),
                      "4"=c((nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2010" & SN_KTYP4=="4"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="4"))),
                            (nrow(filter(Daten_Schulen, Schultyp=="Sprache" & Jahr=="2015" & SN_KTYP4=="4")))),
                      "Schultyp"="Sprache")

Lernen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                     "1"=c((nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2010" & SN_KTYP4=="1"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="1"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2020" & SN_KTYP4=="1")))),
                     "2"=c((nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2010" & SN_KTYP4=="2"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="2"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2020" & SN_KTYP4=="2")))),
                     "3"=c((nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2010" & SN_KTYP4=="3"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="3"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2020" & SN_KTYP4=="3")))),
                     "4"=c((nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2010" & SN_KTYP4=="4"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="4"))),
                           (nrow(filter(Daten_Schulen, Schultyp=="L" & Jahr=="2015" & SN_KTYP4=="4")))),
                     "Schultyp"="Lernen")

ESE <- data.frame("Jahr"=c("2010", "2015", "2020"),
                  "1"=c((nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2010" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="1"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2020" & SN_KTYP4=="1")))),
                  "2"=c((nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2010" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="2"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2020" & SN_KTYP4=="2")))),
                  "3"=c((nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2010" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="3"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2020" & SN_KTYP4=="3")))),
                  "4"=c((nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2010" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="4"))),
                        (nrow(filter(Daten_Schulen, Schultyp=="ESE" & Jahr=="2015" & SN_KTYP4=="4")))),
                  "Schultyp"="ESE")

Anzahl_Foederschulen_Raum <- rbind(SFZ, Sehen, Hören, GE, KME, Sprache, Lernen, ESE)

rm(ESE, GE, Hören, KME, Lernen, Sehen, SFZ, Sprache)

#_______________________________________________________________________________

#Art und Anzahl der Schulen, die Schüler:innen mit Förderbedarf beschulen

Alle_Grundschulen_1 <- nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="1"))
Alle_Grundschulen_2 <- nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="2"))
Alle_Grundschulen_3 <- nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="3"))
Alle_Grundschulen_4 <- nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="4"))

Alle_Gymnasien_1 <- nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="1"))
Alle_Gymnasien_2 <- nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="2"))
Alle_Gymnasien_3 <- nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="3"))
Alle_Gymnasien_4 <- nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="4"))

Alle_Mittelschulen_1 <- nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="1"))
Alle_Mittelschulen_2 <- nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="2"))
Alle_Mittelschulen_3 <- nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="3"))
Alle_Mittelschulen_4 <- nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="4"))

Alle_Realschulen_1 <- nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="1"))
Alle_Realschulen_2 <- nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="2"))
Alle_Realschulen_3 <- nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="3"))
Alle_Realschulen_4 <- nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="4"))


Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "1"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                           "2"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                           "3"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                           "4"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                 (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                           "Schulart"="Grundschulen")

Grundschulen <- mutate(Grundschulen, X1=round((X1/Alle_Grundschulen_1)*100))
Grundschulen <- mutate(Grundschulen, X2=round((X2/Alle_Grundschulen_2)*100))
Grundschulen <- mutate(Grundschulen, X3=round((X3/Alle_Grundschulen_3)*100))
Grundschulen <- mutate(Grundschulen, X4=round((X4/Alle_Grundschulen_4)*100))

Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                            "2"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                            "3"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                            "4"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                  (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                            "Schulart"="Mittelschulen")

Mittelschulen <- mutate(Mittelschulen, X1=round((X1/Alle_Mittelschulen_1)*100))
Mittelschulen <- mutate(Mittelschulen, X2=round((X2/Alle_Mittelschulen_2)*100))
Mittelschulen <- mutate(Mittelschulen, X3=round((X3/Alle_Mittelschulen_3)*100))
Mittelschulen <- mutate(Mittelschulen, X4=round((X4/Alle_Mittelschulen_4)*100))

Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "1"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                          "2"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                          "3"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                          "4"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                          "Schulart"="Realschulen")

Realschulen <- mutate(Realschulen, X1=round((X1/Alle_Realschulen_1)*100))
Realschulen <- mutate(Realschulen, X2=round((X2/Alle_Realschulen_2)*100))
Realschulen <- mutate(Realschulen, X3=round((X3/Alle_Realschulen_3)*100))
Realschulen <- mutate(Realschulen, X4=round((X4/Alle_Realschulen_4)*100))

Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "1"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="1"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="1"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="1")))),
                        "2"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="2"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="2"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="2")))),
                        "3"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="3"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="3"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="3")))),
                        "4"=c((nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="4"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4"))),
                              (nrow(filter(Schulen_mit_Foerderschuelern, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4")))),
                        "Schulart"="Gymnasien")

Gymnasien <- mutate(Gymnasien, X1=round((X1/Alle_Gymnasien_1)*100))
Gymnasien <- mutate(Gymnasien, X2=round((X2/Alle_Gymnasien_2)*100))
Gymnasien <- mutate(Gymnasien, X3=round((X3/Alle_Gymnasien_3)*100))
Gymnasien <- mutate(Gymnasien, X4=round((X4/Alle_Gymnasien_4)*100))

Anzahl_Regelschulen_Inklusion <- rbind(Grundschulen, Mittelschulen, Realschulen, Gymnasien)

Anzahl_Regelschulen_Inklusion[Anzahl_Regelschulen_Inklusion == "Grundschulen"] <- "GS"
Anzahl_Regelschulen_Inklusion[Anzahl_Regelschulen_Inklusion == "Gymnasien"] <- "GY"
Anzahl_Regelschulen_Inklusion[Anzahl_Regelschulen_Inklusion == "Realschulen"] <- "RS"
Anzahl_Regelschulen_Inklusion[Anzahl_Regelschulen_Inklusion == "Mittelschulen"] <- "MS"


#Abbildung
X1 <- ggplot(data=Anzahl_Regelschulen_Inklusion, aes(fill=Jahr, x=Schulart, y=X1)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anteil in %", title="Kreisfreie Großstädte") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

X2 <- ggplot(data=Anzahl_Regelschulen_Inklusion, aes(fill=Jahr, x=Schulart, y=X2)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anteil in %", title="Städtische Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

X3 <- ggplot(data=Anzahl_Regelschulen_Inklusion, aes(fill=Jahr, x=Schulart, y=X3)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anteil in %", title="Ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

X4 <- ggplot(data=Anzahl_Regelschulen_Inklusion, aes(fill=Jahr, x=Schulart, y=X4)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anteil in %", title="Dünn besiedelte ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()


Abbildung_Anzahl_Regelschulen <- ggarrange(X1, X2, X3, X4,
                                           ncol = 4, nrow = 1,
                                           vjust = 2, hjust = -0.5,
                                           font.label = list(size=10),
                                           common.legend=T)

#_______________________________________________________________________________

#Anzahl und Anteil der Schüler:innen in Bayern an den verschiedenen Schularten

Foerderschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                             "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010"))),
                                         (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015"))),
                                         (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020")))),
                             "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                         (round(((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                         (round(((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                             "Schulart"="Förderschulen")

Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010"))),
                                       (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015"))),
                                       (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020")))),
                           "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                       (round(((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                       (round(((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                           "Schulart"="Grundschulen")

Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010"))),
                                        (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015"))),
                                        (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020")))),
                            "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                        (round(((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                        (round(((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                            "Schulart"="Mittelschulen")

Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010"))),
                                      (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015"))),
                                      (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020")))),
                          "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                      (round(((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                      (round(((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                          "Schulart"="Realschulen")

Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "absolut"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010"))),
                                    (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015"))),
                                    (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020")))),
                        "relativ"=c((round(((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010")))/Alle_Schueler_2010)*100)),
                                    (round(((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015")))/Alle_Schueler_2015)*100)),
                                    (round(((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020")))/Alle_Schueler_2020)*100))),
                        "Schulart"="Gymnasien")

Anzahl_Anteil_Alle_Schueler <- rbind(Foerderschulen, Grundschulen, Mittelschulen, Realschulen, Gymnasien)

#_______________________________________________________________________________

#Anzahl an Schüler:innen mit SPF, die die Schularten im jeweiligen Bereich besucht haben
Förderschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                            "2"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                            "3"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                            "4"=c((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                            "Schulart"="Förderschulen")

Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "1"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                           "2"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                           "3"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                           "4"=c((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                                 (nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                           "Schulart"="Grundschulen")

Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                            "2"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                            "3"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                            "4"=c((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                                  (nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                            "Schulart"="Mittelschulen")

Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "1"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                          "2"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                          "3"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                          "4"=c((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                                (nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                          "Schulart"="Realschulen")

Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "1"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="1" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="1" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="1" & FB=="Ja")))),
                        "2"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="2" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="2" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="2" & FB=="Ja")))),
                        "3"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="3" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="3" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="3" & FB=="Ja")))),
                        "4"=c((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="4" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4" & FB=="Ja"))),
                              (nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="4" & FB=="Ja")))),
                        "Schulart"="Gymnasien")


Anzahl_Schueler_SPF_Regelschulen <- rbind(Förderschulen, Grundschulen, Mittelschulen, Realschulen, Gymnasien)

#Anzahl diagnostizierter Förderbedarf

Anzahl_Anteil_Foerderbedarf_Schueler <- data.frame("Jahr"=c("2010", "2015", "2020"),
                                                   "Anzahl"=c(Alle_Schueler_SPF_2010,
                                                              Alle_Schueler_SPF_2015,
                                                              Alle_Schueler_SPF_2020),
                                                   "Anteil"=c((Alle_Schueler_SPF_2010/Alle_Schueler_2010)*100),
                                                   ((Alle_Schueler_SPF_2015/Alle_Schueler_2015)*100),
                                                   ((Alle_Schueler_SPF_2020/Alle_Schueler_2020)*100))

#Anteil der Schüler:innen mit FB, die die Schularten im jeweiligen Bereich besucht haben

Förderschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "Schulart"="Förderschulen",
                            "Beschulung"="exklusiv",
                            "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                            "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                            "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                            "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Förderzentren" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "Schulart"="Grundschulen",
                           "Beschulung"="inklusiv",
                           "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                           "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                           "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                           "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                 (((nrow(filter(Daten_Schueler, Schulart=="Grundschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "Schulart"="Mittelschulen",
                            "Beschulung"="inklusiv",
                            "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                            "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                            "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                            "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                  (((nrow(filter(Daten_Schueler, Schulart=="Mittelschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "Schulart"="Realschulen",
                          "Beschulung"="inklusiv",
                          "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                          "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                          "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                          "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                                (((nrow(filter(Daten_Schueler, Schulart=="Realschulen" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "Schulart"="Gymnasien",
                        "Beschulung"="inklusiv",
                        "1"=c((((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="1"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="1"))))*100)),
                        "2"=c((((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="2"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="2"))))*100)),
                        "3"=c((((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="3"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="3"))))*100)),
                        "4"=c((((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2010" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2015" & FB=="Ja" & SN_KTYP4=="4"))))*100),
                              (((nrow(filter(Daten_Schueler, Schulart=="Gymnasien" & Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4")))/(nrow(filter(Daten_Schueler, Jahr=="2020" & FB=="Ja" & SN_KTYP4=="4"))))*100)))


Anteil_SPF_räumlicher_Vergleich <- rbind(Förderschulen, Grundschulen, Mittelschulen, Realschulen, Gymnasien)

Anteil_SPF_räumlicher_Vergleich$X1 <- round(Anteil_SPF_räumlicher_Vergleich$X1, digits=1)
Anteil_SPF_räumlicher_Vergleich$X2 <- round(Anteil_SPF_räumlicher_Vergleich$X2, digits=1)
Anteil_SPF_räumlicher_Vergleich$X3 <- round(Anteil_SPF_räumlicher_Vergleich$X3, digits=1)
Anteil_SPF_räumlicher_Vergleich$X4 <- round(Anteil_SPF_räumlicher_Vergleich$X4, digits=1)


#Abbildung inklusive Beschulung bei Schüler:innen mit SPF

Y1 <- ggplot(data=Anteil_SPF_räumlicher_Vergleich, aes(fill=Schulart, x=Beschulung, y=X1)) +
  geom_bar(position="stack", stat="identity", color="black") +
  facet_grid(.~Jahr) +
  labs(x="Beschulung", y="Anteil in %", title="Kreisfreie Großstädte") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

Y2 <- ggplot(data=Anteil_SPF_räumlicher_Vergleich, aes(fill=Schulart, x=Beschulung, y=X2)) +
  geom_bar(position="stack", stat="identity", color="black") +
  facet_grid(.~Jahr) +
  labs(x="Beschulung", y="Anteil in %", title="Städtische Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

Y3 <- ggplot(data=Anteil_SPF_räumlicher_Vergleich, aes(fill=Schulart, x=Beschulung, y=X3)) +
  geom_bar(position="stack", stat="identity", color="black") +
  facet_grid(.~Jahr) +
  labs(x="Beschulung", y="Anteil in %", title="Ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

Y4 <- ggplot(data=Anteil_SPF_räumlicher_Vergleich, aes(fill=Schulart, x=Beschulung, y=X4)) +
  geom_bar(position="stack", stat="identity", color="black") +
  facet_grid(.~Jahr) +
  labs(x="Beschulung", y="Anteil in %", title="Dünn besiedelte ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 100) +
  theme_bw()

Abbildung_Anzahl_Regelschulen <- ggarrange(Y1, Y2, Y3, Y4,
                                           ncol = 2, nrow = 2,
                                           vjust = 2, hjust = -0.5,
                                           font.label = list(size=10),
                                           common.legend=T)

Abbildung_Anzahl_Regelschulen


#Voranalyse Verteilung der Inklusionsquote nach Kreisen

Abbildung_Kreise_Inklusion <- select(Daten_Schulen, RS_Kreis, SN_KTYP4, Jahr, FB_gesamt, Inklusionsanteil_Kreis, GEN)
Abbildung_Kreise_Inklusion <- distinct(Abbildung_Kreise_Inklusion)

Landkreise <- count(Abbildung_Kreise_Inklusion, GEN)
Abbildung_Kreise_Inklusion <- merge(Abbildung_Kreise_Inklusion, Landkreise, by.x = "GEN", by.y = "GEN")
Landkreise <- filter(Abbildung_Kreise_Inklusion, n==6)
Landkreise <- select(Landkreise, GEN, RS_Kreis, SN_KTYP4)
Landkreise <- distinct(Landkreise)
Landkreise$Ende <- Landkreise$RS_Kreis
Landkreise$Ende <- as.numeric(Landkreise$Ende)
Landkreise_Land <- Landkreise %>% group_by(GEN) %>% summarise(Ende=max(Ende))
Landkreise_Land$Region <- "Land"
Landkreise_Stadt <- Landkreise %>% group_by(GEN) %>% summarise(Ende=min(Ende))
Landkreise_Stadt$Region <- "Stadt"
Landkreise <- rbind(Landkreise_Stadt, Landkreise_Land)
#Landkreise$Ende <- paste0("0", Landkreise$Ende)

Landkreise$Ende <- as.integer(Landkreise$Ende)
Abbildung_Kreise_Inklusion <- left_join(Abbildung_Kreise_Inklusion, Landkreise, 
                                        by=c("GEN"="GEN", "RS_Kreis"="Ende"))
Test <- filter(Abbildung_Kreise_Inklusion, Region=="Land")
Test <- distinct(Test, GEN, Region, RS_Kreis)
Test$GEN_neu <- paste0("Lkr. ", Test$GEN)
Test <- distinct(Test, GEN_neu, RS_Kreis)
Abbildung_Kreise_Inklusion <- distinct(Abbildung_Kreise_Inklusion, GEN, RS_Kreis, SN_KTYP4, Jahr, FB_gesamt, Inklusionsanteil_Kreis)
Abbildung_Kreise_Inklusion <- merge(Abbildung_Kreise_Inklusion, Test, by.x="RS_Kreis", by.y="RS_Kreis", all.x=T)
Abbildung_Kreise_Inklusion$GEN_neu[is.na(Abbildung_Kreise_Inklusion$GEN_neu)] <- Abbildung_Kreise_Inklusion$GEN[is.na(Abbildung_Kreise_Inklusion$GEN_neu)]
Abbildung_Kreise_Inklusion$SN_KTYP4 <- as.factor(Abbildung_Kreise_Inklusion$SN_KTYP4)
Abbildung_Kreise_Inklusion_2015 <- filter(Abbildung_Kreise_Inklusion, Jahr!=2015)

#Abbildung Inklusionsquote nach Kreisen

A <- ggplot(data = filter(Abbildung_Kreise_Inklusion_2015, Jahr=="2010"), 
            aes(fill=SN_KTYP4, x = reorder(GEN_neu,-Inklusionsanteil_Kreis), y = Inklusionsanteil_Kreis)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colorBlindGrey8, name="Kreistyp", 
                    labels=c("Kreisfreie Großstädte", "Städtische Kreise", "Ländliche Kreise", "Dünn besiedelte ländliche Kreise")) +
  scale_y_continuous(name = "Inklusionsanteil (%)",
                     expand = c(0,0),
                     limits = c(0, 70),
                     breaks = seq(0, 100, 10),) +
  scale_x_discrete (expand = c(0,0),) +
  geom_text(x=90, y=60, label="2010", color="black", size=15) +
  theme_bw() +
  theme(axis.text.x=element_text(size=10, angle = 90, vjust = 0,5, hjust=1), 
        legend.position = "top", 
        axis.title.x=element_blank())

B <- ggplot(data = filter(Abbildung_Kreise_Inklusion_2015, Jahr=="2020"), 
            aes(fill=SN_KTYP4, x = reorder(GEN_neu,-Inklusionsanteil_Kreis), y = Inklusionsanteil_Kreis)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colorBlindGrey8, name="Kreistyp", 
                    labels=c("Kreisfreie Großstädte", "Städtische Kreise", "Ländliche Kreise", "Dünn besiedelte ländliche Kreise")) +
  scale_y_continuous(name = "Inklusionsanteil (%)",
                     expand = c(0,0),
                     limits = c(0, 70),
                     breaks = seq(0, 100, 10),) +
  scale_x_discrete (expand = c(0,0),) +
  geom_text(x=90, y=60, label="2020"
            , color="black", size=15) +
  theme_bw() +
  theme(axis.text.x=element_text(size=10, angle = 90, vjust = 0,5, hjust=1), 
        legend.position = "top", 
        axis.title.x=element_blank())


Abbildung_Kreise_Inklusion_Jahr <- ggarrange(A, B,
                                             ncol = 1, nrow = 2,
                                             vjust = 2, hjust = -0.5,
                                             font.label = list(size=10),
                                             common.legend=T)

# Abbildung Boxplots Veränderung des Inklusionsanteils i. B. a. den Kreistyp

Regionen <- c("1"="Large Cities", 
              "2"="Urban Counties", 
              "3"="Rural Counties", 
              "4"="Sparsely Populated")

Abbildung_Boxplot_Inklusion_Zeit <-
  ggplot(data=Abbildung_Kreise_Inklusion,
             aes(x=Jahr, y=Inklusionsanteil_Kreis)) +
  geom_boxplot(fill="lightblue", color="black") +
  geom_signif(comparisons = list(c("2010", "2020")),
              map_signif_level = T) +
  coord_flip() +
  scale_y_continuous(name="Inklusion Rate (%)") +
  facet_wrap(.~SN_KTYP4, labeller = as_labeller(Regionen),
                nrow=2) +
  theme_bw() +
  theme(axis.title.x=element_blank())
Abbildung_Boxplot_Inklusion_Zeit

# Varianzanalyse

Abbildung_Kreise_Inklusion$Jahr <- as.factor(Abbildung_Kreise_Inklusion$Jahr)

KTYP4_1 <- filter(Abbildung_Kreise_Inklusion, SN_KTYP4==1)
KTYP4_2 <- filter(Abbildung_Kreise_Inklusion, SN_KTYP4==2)
KTYP4_3 <- filter(Abbildung_Kreise_Inklusion, SN_KTYP4==3)
KTYP4_4 <- filter(Abbildung_Kreise_Inklusion, SN_KTYP4==4)

anova(lm(Inklusionsanteil_Kreis ~ Jahr, KTYP4_1))
TukeyHSD(aov(KTYP4_1$Inklusionsanteil_Kreis ~ KTYP4_1$Jahr), conf.level=.95)

anova(lm(Inklusionsanteil_Kreis ~ Jahr, KTYP4_2))
TukeyHSD(aov(KTYP4_2$Inklusionsanteil_Kreis ~ KTYP4_2$Jahr))

anova(lm(Inklusionsanteil_Kreis ~ Jahr, KTYP4_3))
TukeyHSD(aov(KTYP4_3$Inklusionsanteil_Kreis ~ KTYP4_3$Jahr))

anova(lm(Inklusionsanteil_Kreis ~ Jahr, KTYP4_4))
TukeyHSD(aov(KTYP4_4$Inklusionsanteil_Kreis ~ KTYP4_4$Jahr))

#_______________________________________________________________________________

Jahr_2010 <- filter(Abbildung_Kreise_Inklusion, Jahr=="2010")
Jahr_2015 <- filter(Abbildung_Kreise_Inklusion, Jahr=="2015")
Jahr_2020 <- filter(Abbildung_Kreise_Inklusion, Jahr=="2020")

anova(lm(Inklusionsanteil_Kreis ~ SN_KTYP4, Jahr_2010))
TukeyHSD(aov(Jahr_2010$Inklusionsanteil_Kreis ~ Jahr_2010$SN_KTYP4))

anova(lm(Inklusionsanteil_Kreis ~ SN_KTYP4, Jahr_2015))
TukeyHSD(aov(Jahr_2015$Inklusionsanteil_Kreis ~ Jahr_2015$SN_KTYP4))

anova(lm(Inklusionsanteil_Kreis ~ SN_KTYP4, Jahr_2020))
TukeyHSD(aov(Jahr_2020$Inklusionsanteil_Kreis ~ Jahr_2020$SN_KTYP4))

#_______________________________________________________________________________
#Anzahl an Regelschulen im zeitlichen und räumlichen Vergleich

Grundschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                           "1"=c((nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                           "2"=c((nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                           "3"=c((nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                           "4"=c((nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                 (nrow(filter(Daten_Schulen, Schulart=="Grundschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                           "Schulart"="GS")

Mittelschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                            "2"=c((nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                            "3"=c((nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                            "4"=c((nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Mittelschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                            "Schulart"="MS")

Realschulen <- data.frame("Jahr"=c("2010", "2015", "2020"),
                          "1"=c((nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="1"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="1"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="1")))),
                          "2"=c((nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="2"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="2"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="2")))),
                          "3"=c((nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="3"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="3"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2020" & SN_KTYP4=="3")))),
                          "4"=c((nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2010" & SN_KTYP4=="4"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4"))),
                                (nrow(filter(Daten_Schulen, Schulart=="Realschulen" & Jahr=="2015" & SN_KTYP4=="4")))),
                          "Schulart"="RS")

Gymnasien <- data.frame("Jahr"=c("2010", "2015", "2020"),
                        "1"=c((nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="1"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="1"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="1")))),
                        "2"=c((nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="2"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="2"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="2")))),
                        "3"=c((nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="3"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="3"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2020" & SN_KTYP4=="3")))),
                        "4"=c((nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2010" & SN_KTYP4=="4"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4"))),
                              (nrow(filter(Daten_Schulen, Schulart=="Gymnasien" & Jahr=="2015" & SN_KTYP4=="4")))),
                        "Schulart"="GY")

Förderzentren <- data.frame("Jahr"=c("2010", "2015", "2020"),
                            "1"=c((nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="1"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="1"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="1")))),
                            "2"=c((nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="2"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="2"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="2")))),
                            "3"=c((nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="3"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="3"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2020" & SN_KTYP4=="3")))),
                            "4"=c((nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2010" & SN_KTYP4=="4"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="4"))),
                                  (nrow(filter(Daten_Schulen, Schulart=="Förderzentren" & Jahr=="2015" & SN_KTYP4=="4")))),
                            "Schulart"="FS")

Anzahl_Regelschulen_Raum <- rbind(Grundschulen, Mittelschulen, Realschulen, Gymnasien, Förderzentren)

#Abbildung Anzahl


Z1 <- ggplot(data=Anzahl_Regelschulen_Raum, aes(fill=Jahr, x=Schulart, y=X1)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anzahl", title="Kreisfreie Großstädte") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 800) +
  theme_bw()

Z2 <- ggplot(data=Anzahl_Regelschulen_Raum, aes(fill=Jahr, x=Schulart, y=X2)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anzahl", title="Städtische Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 800) +
  theme_bw()

Z3 <- ggplot(data=Anzahl_Regelschulen_Raum, aes(fill=Jahr, x=Schulart, y=X3)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anzahl", title="Ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 800) +
  theme_bw()

Z4 <- ggplot(data=Anzahl_Regelschulen_Raum, aes(fill=Jahr, x=Schulart, y=X4)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  labs(x="Schulart", y="Anzahl", title="Dünn besiedelte ländliche Kreise") +
  scale_fill_brewer(palette="Blues") +
  ylim(0, 800) +
  theme_bw()


Abbildung_Anzahl_Regelschulen <- ggarrange(Z1, Z2, Z3, Z4,
                                           ncol = 2, nrow = 2,
                                           vjust = 2, hjust = -0.5,
                                           font.label = list(size=10),
                                           common.legend=T)

Abbildung_Anzahl_Regelschulen

# Anteil Schulen in Bayern

Alle_Schulen_2010 <- nrow(filter(Daten_Schulen, Jahr==2010))
Alle_Schulen_2015 <- nrow(filter(Daten_Schulen, Jahr==2015))
Alle_Schulen_2020 <- nrow(filter(Daten_Schulen, Jahr==2020))

Schulen_2010 <- count((filter(Daten_Schulen, Jahr==2010)), Schulart)
Schulen_2015 <- count((filter(Daten_Schulen, Jahr==2015)), Schulart)
Schulen_2020 <- count((filter(Daten_Schulen, Jahr==2020)), Schulart)

Schulen_2010$n <- round((Schulen_2010$n/Alle_Schulen_2010)*100)
Schulen_2015$n <- round((Schulen_2015$n/Alle_Schulen_2015)*100)
Schulen_2020$n <- round((Schulen_2020$n/Alle_Schulen_2020)*100)

# Geschlossene Schulen

Daten_Foerderschulen <- filter(Daten_Schulen, Schulart=="Förderzentren")
a <- count(Daten_Foerderschulen, Schulnummer)
filter(a, n<3) #München und Ansbach#

# Anteil inklusive Beschulung in ganz Bayern

Inklusive_Beschulung <- filter(Daten_Schueler, Schulart%in%c("Grundschulen", "Mittelschulen", "Realschulen", "Gymnasien"), FB=="Ja")
(nrow(filter(Inklusive_Beschulung, Jahr=="2010"))/nrow(filter(Daten_Schueler, FB=="Ja", Jahr=="2010")))*100
(nrow(filter(Inklusive_Beschulung, Jahr=="2020"))/nrow(filter(Daten_Schueler, FB=="Ja", Jahr=="2020")))*100

# Inklusionsanteil Schwabach, Landshut, Coburg, Oberallgäu

b <- filter(Daten_Schulen, Jahr%in%c("2010", "2020"), GEN%in%c("Schwabach", "Landshut", "Coburg", "Oberallgäu"))
b <- select(b, GEN, Jahr, Inklusionsanteil_Kreis)
b <- distinct(b)
