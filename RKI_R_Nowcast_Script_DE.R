# Reproducible Nowcast R-Script für R-Faktor in Deutschland, berechnet vom RKI

# Überprüfen Sie, ob Sie alle notwendigen R-Pakete haben - diese sind in der u.
# stehenden Zeile aufgeführt. (Wie geht das? 



# Necessary packages
install.packages("xlsx", "stringr", "dplyr", "ggplot2", "scales")

# Wenn Sie eine Fehlermeldung über ein spezifisches Package bekommen
# löschen Sie die "#" Zeichen vorne (Kommentare in R beginnen mit einem #) die 
# Zeilen unterhalb der Überschrift # Notwendige Libraries laden
# If you get an error installing a specific package, uncomment (i.e., delete the '#') 
# the line below that starts with install.packages
# CAREFUL! This will (re)install all the packages listed here! This takes some time
# install.packages(pkgs = c("xlsx", "stringr", "dplyr", "ggplot2", "scales"), dependencies=TRUE)

# Notwendige Libraries laden
library(xlsx)
library(stringr)
library(dplyr)
library(ggplot2)
library(scales)

# Lade neuesten Nowcast von der RKI Webseite
daten_file <- str_c("Nowcasting_Zahlen-",Sys.Date(),".xlsx")
if (!file.exists(daten_file)) { 
  file_url <- 
    "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/Nowcasting_Zahlen.xlsx?__blob=publicationFile" 
  download.file(url=file_url,destfile= daten_file, mode="wb") 
}

# Lese Excel-File 
data <- xlsx::read.xlsx(file = daten_file, sheetName = "Nowcast_R", encoding = "UTF-8") 
data <- data[,1:13] 

# Umbennung der Spalten Namen zu kürzeren Variabelnamen 
names(data) <- c("Datum", "NeuErkr", "lb_NeuErkr", "ub_NeuErkr", "NeuErkr_ma4", "lb_NeuErkr_ma4", 
                 "ub_NeuErkr_ma4", "R", "lb_R", "ub_R", "R_7Tage", "lb_R_7Tage", "ub_R_7Tage")

# R-Wert Berechnung bei einem seriellen Intervall von 4 Tagen 
R_Wert <- rep(NA, nrow(data)) 
for (t in 8:nrow(data)) { 
  R_Wert[t] <- sum(data$NeuErkr[t-0:3]) / sum(data$NeuErkr[t-4:7])
  } 
data <- data %>% dplyr::mutate(R_Wert = round(R_Wert, digits = 2))

# Vergleiche mit den R-Werten in der Excel-Tabelle 
data %>% select(Datum, R, R_Wert) %>% tail()

# Plot 
ggplot(data=data, aes(x=Datum)) +
geom_ribbon(aes(ymin = lb_R, ymax = ub_R), stat="identity", fill="steelblue")+
  geom_line(aes(y = R), stat="identity", fill="steelblue")+
  theme_minimal() + 
  labs(title = "", x = "", y = "Reproduktionszahl R") + 
  scale_x_date(date_breaks = "2 days", labels = 
                 scales::date_format("%d.%m.")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + 
  theme(axis.text.x = element_text(angle=90, vjust=0))



# Berechnung des 7-Tage R-Werts 
R7_Wert <- rep(NA, nrow(data)) 
for (t in 11:nrow(data)) { 
  R7_Wert[t-1] <- sum(data$NeuErkr[t-0:6]) / sum(data$NeuErkr[t-4:10]) 
  }
data <- data %>% dplyr::mutate(R7_Wert = round(R7_Wert, digits = 2)) 

# Vergleiche mit den R-Werten in der Excel-Tabelle 
data %>% select(Datum, R_7Tage, R7_Wert) %>% tail()

# Plot für Berechnung des 7-Tage R-Wertes
ggplot(data=data, aes(x=Datum, y = R, color="R")) + 
  geom_ribbon(aes(ymin = lb_R, ymax = ub_R, color=NULL), fill="steelblue") + 
  geom_ribbon(aes(ymin = lb_R_7Tage, ymax = ub_R_7Tage, color=NULL), fill="orange") + 
  geom_line(aes(y = R, color="R")) + geom_line(aes(y = R_7Tage, color="R_7Tage"), size = 1) +
  theme_minimal() + 
  labs(title = "", 
       x = "", 
       y = "Reproduktionszahl R") + 
  scale_x_date(date_breaks = "2 days", labels = 
                 scales::date_format("%d.%m.")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + 
  scale_color_manual(name="Methode:", values=c("darkblue","orangered")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) + 
  theme(legend.position="bottom")