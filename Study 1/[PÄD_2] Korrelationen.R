
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(apaTables)

#_______________________________________________________________________________

setwd()

Daten_Schulen <- read.csv("Daten_Schulen.csv")
Daten_Schueler <- read.csv("Daten_Schueler.csv")
Kreise <- read.csv2("Geodaten/Kreise/Kreise_Inklusionsanteil_Distanz.csv")
Wahlen <- read.csv2("Geodaten/Kreise/Kreise_Wahlergebnisse.csv")
Erreichbarkeit <- read.csv2("Geodaten/Kreise/Kreise_Erreichbarkeit.csv")
Einkommen <- read.csv2("Haushaltseinkommen.csv", encoding = "UTF-8")

#_______________________________________________________________________________

# Anpassen der Tabellen

Kreise <- merge(Kreise, Wahlen[c("Name",
                                 "Wahlkreis", 
                                 "CSU", 
                                 "SPD", 
                                 "AFD", 
                                 "GRUENE", 
                                 "LINKE", 
                                 "FDP")], by="Name")

Kreise <- merge(Kreise, Erreichbarkeit[c("Name", 
                                         "Erreichbarkeit")], by="Name")

summary(Kreise)

Kreise$CSU <- as.numeric(Kreise$CSU)
Kreise$SPD <- as.numeric(Kreise$SPD)
Kreise$AFD <- as.numeric(Kreise$AFD)
Kreise$GRUENE <- as.numeric(Kreise$GRUENE)
Kreise$LINKE <- as.numeric(Kreise$LINKE)
Kreise$FDP <- as.numeric(Kreise$FDP)
Kreise$Erreichbarkeit <- as.numeric(Kreise$Erreichbarkeit)
Kreise$IA_2010 <- as.numeric(Kreise$IA_2010)
Kreise$IA_2020 <- as.numeric(Kreise$IA_2020)
Kreise <- mutate(Kreise, IA_Diff = IA_2020-IA_2010)

summary(Kreise)

Kreise$Wahlen_rechts <- Kreise$AFD + Kreise$CSU
Kreise$Wahlen_links <- Kreise$SPD + Kreise$GRUENE + Kreise$LINKE

#_______________________________________________________________________________

# Haushaltseinkommen in den bayerischen Landkreisen

Einkommen <- Einkommen %>%
  select("Regional.schlüssel", 
         "Land", 
         "Gebietseinheit",
         "X2019") %>%
  filter(Land=="BY")

Kreise <- merge(Kreise, Einkommen[c("Regional.schlüssel", "X2019")],
                by.x = "ARS", by.y = "Regional.schlüssel")

Kreise$X2019 <- gsub("[[:space:]]", "", Kreise$X2019)
Kreise$X2019 <- as.numeric(Kreise$X2019)

#_______________________________________________________________________________

# Durchschnittliche Größe eines Förderzentrums im Landkreis

FZ <- filter(Daten_Schueler, 
             Jahr=="2020",
             Schulart=="Förderzentren")

FZ <- FZ %>% 
  group_by(Schulnummer) %>%
  mutate(Anzahl_Schueler = length(Schulnummer)) %>%
  select("Schulnummer", "RS_Kreis","Anzahl_Schueler") %>%
  distinct()

FZ <- FZ %>%
  group_by(RS_Kreis) %>%
  mutate(n_Schueler_mean = mean(Anzahl_Schueler)) %>%
  select("RS_Kreis", "n_Schueler_mean") %>%
  distinct()

Kreise <- merge(Kreise, FZ, by.x = "ARS", by.y = "RS_Kreis")

#_______________________________________________________________________________

#Korrelation Inklusionsanteil 2020 und Distanz

plot(Kreise$IA_2020, Kreise$Distanz_FZ)
cor.test(Kreise$IA_2020, Kreise$Distanz_FZ)

#Korrelation Inklusionsanteil 2020 und Distanz dünne Besiedelung

Kreise_4 <- filter(Kreise, KTYP4 == "Dünn besiedelte ländliche Kreise")
plot(Kreise_4$IA_2020, Kreise_4$Distanz_FZ)
cor.test(Kreise_4$IA_2020, Kreise_4$Distanz_FZ)

#Korrelation Inklusionsanteil 2020 und Distanz ländliche Gebiete

Kreise_3 <- filter(Kreise, KTYP4 == "Ländliche Kreise mit Verdichtungsansätzen")
plot(Kreise_3$IA_2020, Kreise_3$Distanz_FZ)
cor.test(Kreise_3$IA_2020, Kreise_3$Distanz_FZ)

#Korrelation Inklusionsanteil 2020 und Distanz städtische Gebiete

Kreise_2 <- filter(Kreise, KTYP4 == "Städtische Kreise")
plot(Kreise_2$IA_2020, Kreise_2$Distanz_FZ)
cor.test(Kreise_2$IA_2020, Kreise_2$Distanz_FZ)

#Korrelation Inklusionsanteil 2020 und Distanz städtische Gebiete

Kreise_1 <- filter(Kreise, KTYP4 == "Kreisfreie Großstädte")
plot(Kreise_1$IA_2020, Kreise_1$Distanz_FZ)
cor.test(Kreise_1$IA_2020, Kreise_1$Distanz_FZ)

#Korrelation Differenz Inklusionsanteil 2020 und Distanz

plot(Kreise$IA_Diff, Kreise$Distanz_FZ)
cor.test(Kreise$IA_Diff, Kreise$Distanz_FZ)

#Korrelation Anzahl Förderzentren und Inklusionsanteil

plot(Kreise$IA_2020, Kreise$n_FZ_km2)
cor.test(Kreise$IA_2020, Kreise$n_FZ_km2)

#Korrelation Wahlergebnisse und Inklusionsanteil 2020

plot(Kreise$IA_2020, Kreise$CSU)
cor.test(Kreise$IA_2020, Kreise$CSU)

plot(Kreise$IA_2020, Kreise$SPD)
cor.test(Kreise$IA_2020, Kreise$SPD)

plot(Kreise$IA_2020, Kreise$AFD)
cor.test(Kreise$IA_2020, Kreise$AFD)

plot(Kreise$IA_2020, Kreise$FDP)
cor.test(Kreise$IA_2020, Kreise$FDP)

plot(Kreise$IA_2020, Kreise$GRUENE)
cor.test(Kreise$IA_2020, Kreise$GRUENE)

plot(Kreise$IA_2020, Kreise$LINKE)
cor.test(Kreise$IA_2020, Kreise$LINKE)

plot(Kreise$IA_2020, Kreise$LINKE)
cor.test(Kreise$IA_2020, Kreise$LINKE)

plot(Kreise$IA_2020, Kreise$Wahlen_rechts)
cor.test(Kreise$IA_2020, Kreise$Wahlen_rechts)

plot(Kreise$IA_2020, Kreise$Wahlen_links)
cor.test(Kreise$IA_2020, Kreise$Wahlen_links)

#Korrelation Inklusionsanteil 2020 und Erreichbarkeit

plot(Kreise$IA_2020, Kreise$Erreichbarkeit)
cor.test(Kreise$IA_2020, Kreise$Erreichbarkeit)

#Korrelation Größe des FÖrderzentrums und Inklusionsanteil 2020

plot(Kreise$IA_2020, Kreise$n_Schueler_mean)
cor.test(Kreise$IA_2020, Kreise$n_Schueler_mean)

#Korrelation Größe des FÖrderzentrums und Inklusionsanteil 2020

plot(Kreise$Distanz_FZ, Kreise$n_Schueler_mean)
cor.test(Kreise$Distanz_FZ, Kreise$n_Schueler_mean)

#_______________________________________________________________________________

# Korrelationstabelle

Kreise_cor_tabelle <- select(Kreise, 
                             "IA_2020",
                             "Grundschulen",
                             "Mittelschulen",
                             "Realschulen",
                             "Gymnasien", 
                             "n_FZ_km2", 
                             "Erreichbarkeit", 
                             "n_Schueler_mean", 
                             "X2019",
                             "Wahlen_links")

names(Kreise_cor_tabelle) <- c("inclusion rate", 
                               "distance primary",
                               "distance general",
                               "distance intermediate",
                               "distance grammar",
                               "number",
                               "accessibility",
                               "size",
                               "income",
                               "left parties")

apa.cor.table(Kreise_cor_tabelle, filename="Korellation.doc", table.number=2)

#_______________________________________________________________________________

# Distanz GS - FS im räumlichen Vergleich

Regionen <- c("Kreisfreie Großstädte"="Large Cities", 
              "Städtische Kreise"="Urban Counties", 
              "Ländliche Kreise mit Verdichtungsansätzen"="Rural Counties", 
              "Dünn besiedelte ländliche Kreise"="Sparsely Populated")

Kreise$Grundschulen <- (Kreise$Grundschulen)/1000
Kreise$KTYP4 = factor(Kreise$KTYP4, levels=c("Kreisfreie Großstädte",
                                              "Städtische Kreise",
                                              "Ländliche Kreise mit Verdichtungsansätzen",
                                              "Dünn besiedelte ländliche Kreise"))
summary(Kreise$Grundschulen)

ggplot(data=Kreise,
         aes(y=Grundschulen)) +
  geom_boxplot(fill="lightblue", color="black") +
  scale_y_continuous(name="Distance (km)") +
  facet_wrap(.~KTYP4, labeller = as_labeller(Regionen),
             nrow=1) +
  geom_hline(yintercept=5.570045, 
             linetype="dashed", 
             color="red", 
             size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

#_______________________________________________________________________________

# Regressionsanalyse

Kreise$ZIA_2020 <- scale(Kreise$IA_2020)
Kreise$ZGrundschulen <- scale(Kreise$Grundschulen)
Kreise$Zn_FZ_km2 <- scale(Kreise$n_FZ_km2)
Kreise$ZErreichbarkeit <- scale(Kreise$Erreichbarkeit)
Kreise$Zn_Schueler_mean <- scale(Kreise$n_Schueler_mean)
Kreise$ZX2019 <- scale(Kreise$X2019)

# Regression ganz Bayern

Modell_all_1 <- lm(IA_2020 ~ 
                     Grundschulen, 
                   data=Kreise)

Modell_all_2 <- lm(IA_2020 ~ 
                     Grundschulen + 
                     n_FZ_km2, 
                   data=Kreise)

Modell_all_3 <- lm(IA_2020 ~ 
                     Grundschulen + 
                     n_FZ_km2 + 
                     n_Schueler_mean,
                   data=Kreise)

Modell_all_4 <- lm(IA_2020 ~ 
                     Grundschulen +
                     n_FZ_km2 +
                     n_Schueler_mean +
                     Erreichbarkeit,
                   data=Kreise)
                     
Modell_all <- lm(IA_2020 ~ 
               Grundschulen +
               n_FZ_km2 +
               n_Schueler_mean +
               Erreichbarkeit +
               X2019,
               data=Kreise)

apa.reg.table(Modell_all_1,
              Modell_all_2,
              
              
              Modell_all_3,
              Modell_all_4,
              Modell_all,
              filename = "Regression_all.doc", table.number = 3)


# Regression KTYP1

Modell1 <- lm(IA_2020 ~ 
                Grundschulen +
                n_FZ_km2 +
                Erreichbarkeit +
                n_Schueler_mean +
                X2019,
              data=filter(Kreise, KTYP4=="Kreisfreie Großstädte"))

apa.reg.table(Modell1, filename = "Regression_KTYP1.doc", table.number = 4)

Modell2 <- lm(IA_2020 ~ 
                Grundschulen +
                n_FZ_km2 +
                Erreichbarkeit +
                n_Schueler_mean +
                X2019,
              data=filter(Kreise, KTYP4=="Städtische Kreise"))

apa.reg.table(Modell2, filename = "Regression_KTYP2.doc", table.number = 5)

Modell3 <- lm(IA_2020 ~ 
                   Grundschulen +
                   n_FZ_km2 +
                   Erreichbarkeit +
                   n_Schueler_mean +
                   X2019,
              data=filter(Kreise, KTYP4=="Ländliche Kreise mit Verdichtungsansätzen"))

apa.reg.table(Modell3, filename = "Regression_KTYP3.doc", table.number = 6)

Modell4 <- lm(IA_2020 ~ 
                   Grundschulen +
                   n_FZ_km2 +
                   Erreichbarkeit +
                   n_Schueler_mean +
                   X2019,
              data=filter(Kreise, KTYP4=="Dünn besiedelte ländliche Kreise"))

apa.reg.table(Modell4, filename = "Regression_KTYP4.doc", table.number = 7)
