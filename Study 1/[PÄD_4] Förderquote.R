library(dplyr)
library(tidyr)
library(apaTables)

Daten_Schueler <- read.csv("C:/Users/LocalAdmin/OneDrive/Sogeffekt/Daten_Schueler.csv")
Daten_Schulen <- read.csv("C:/Users/LocalAdmin/OneDrive/Sogeffekt/Daten_Schulen.csv")
Kreise <- read.csv2("Geodaten/Kreise/Kreise_Inklusionsanteil_Distanz.csv", encoding="UTF-8")

# Inklusionsanteil an Grundschulen
Kreise <- unique(left_join(Kreise, filter(Daten_Schulen, Jahr=="2020") %>% select(RS_Kreis, Inklusionsanteil_Kreis_GS), by=c("ARS"="RS_Kreis")))

#Größe des Förderzentrums
FZ <- filter(Daten_Schueler, Jahr=="2020", Schulart=="Förderzentren") %>% group_by(Schulnummer) %>% mutate(Anzahl_Schueler = length(Schulnummer)) %>% select("Schulnummer", "RS_Kreis","Anzahl_Schueler") %>% distinct()
FZ <- FZ %>% group_by(RS_Kreis) %>% mutate(n_Schueler_mean = mean(Anzahl_Schueler)) %>% select("RS_Kreis", "n_Schueler_mean") %>% distinct()
Kreise <- merge(Kreise, FZ, by.x = "ARS", by.y = "RS_Kreis")

#Umrechnung Kreise Meter in Kilometer
Kreise$Grundschulen <- (Kreise$Grundschulen)/1000

#Förderquote (Anteil der Schülerinnen und Schüler mit Förderbedarf an allen Schülerinnen und Schülern der Primar- und Sekundarstufe I)

Daten_FQuote <- Daten_Schueler %>% filter(Jahr=="2020") %>% group_by(RS_Kreis, FB) %>% count() %>% spread(FB, n, fill=0) %>% mutate(FQuote=Ja/Nein)
Daten_FQuote$FQuote <- Daten_FQuote$FQuote*100
Kreise <- merge(Kreise, Daten_FQuote, by.x="ARS", by.y="RS_Kreis")

#Regression mit der Förderquote als abhängige Variable


# Regressionsanalyse
# Regression ganz Bayern
Modell_all_1 <- lm(FQuote ~ Grundschulen, data=Kreise)
Modell_all_2 <- lm(FQuote ~ Grundschulen + n_FZ_km2, data=Kreise)
Modell_all_3 <- lm(FQuote ~ Grundschulen + n_FZ_km2 + n_Schueler_mean, data=Kreise)

summary(Modell_all_3)

apa.reg.table(Modell_all_1, Modell_all_2, Modell_all_3, filename = "Regression_all.doc", table.number = 3)

# Regression KTYP1

Modell1 <- lm(FQuote ~ Grundschulen + n_FZ_km2 + n_Schueler_mean, data=filter(Kreise, KTYP4=="Kreisfreie Großstädte"))
apa.reg.table(Modell1, filename = "Regression_KTYP1.doc", table.number = 4)

Modell2 <- lm(FQuote ~ Grundschulen + n_FZ_km2 + n_Schueler_mean, data=filter(Kreise, KTYP4=="Städtische Kreise"))
apa.reg.table(Modell2, filename = "Regression_KTYP2.doc", table.number = 5)

Modell3 <- lm(FQuote ~ Grundschulen + n_FZ_km2 + n_Schueler_mean, data=filter(Kreise, KTYP4=="Ländliche Kreise mit Verdichtungsansätzen"))
apa.reg.table(Modell3, filename = "Regression_KTYP3.doc", table.number = 6)

Modell4 <- lm(FQuote ~ Grundschulen + n_FZ_km2 + n_Schueler_mean, data=filter(Kreise, KTYP4=="Dünn besiedelte ländliche Kreise"))
apa.reg.table(Modell4, filename = "Regression_KTYP4.doc", table.number = 7)
