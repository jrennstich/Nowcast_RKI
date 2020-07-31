###################################################################################################################################
# Reproducible Nowcast R-Script für R-Faktor in Deutschland auf Grundlage des Nowcast Datensatzes des Robert Koch Instituts (RKI) #
###################################################################################################################################

# Die hier präsentierten Scripts sind vom RKI in einem paper publiziert worden. Sie wurden so ergänzt, dass Nutzer_innen, auch mit geringeren 
# R-Kenntnissen diese Skripts ohne Probleme in ihrer eigenen R oder RStudio Installation laufen lassen können.
# Ferner wurden die Achsenbezeichnungen in Teilen sprachlich angepasst.

# Um die aktuellen RKI Nowcast Daten herunterzuladen, die R-Werte zu berechnung und die Plots selbständig erstellen zu können,
# benötigen Sie einige R-Erweiterungen, sog. "Packages". Im ersten Schritt werden Ihnen 3 Optionen präsentiert, um Sicherzustellen,
# dass Sie alle notwendigen Packages installiert und geladen haben. 


########################################
# BENÖTIGTE PACKAGES - BEIM ERSTEN MAL #
########################################

# OPTION 1
# Wenn Sie wissen, dass Sie KEINES der Packages installiert haben:
install.packages("xlsx", "stringr", "dplyr", "ggplot2", "scales")

# OPTION 2
# Wenn Sie nicht genau wissen, welche der Packages fehlen aber Sie nicht erst prüfen wollen, welche, dann können Sie mit diesem
# Befehl alle Packages neu installieren. ACHTUNG! Dies kann eine Weile in Anspruch nehmen. 
install.packages(pkgs = c("xlsx", "stringr", "dplyr", "ggplot2", "scales"), dependencies=TRUE)

# OPTION 3
# Die folgenden Schritt-für-Schritt-Option ist am sichersten. Wenn Sie eine Fehlermeldung bekommen, dann entfernen Sie zunächst das "#"
# am Anfang der folgenden zwei Zeilen (das nennt sich "Ent-Kommentieren" oder "uncomment"), das lässt R erkennen, dass es sich hier um Code handelt.
# Dann wird das fehlende Package installiert und geladen. 

# Load necessary packages
library(xlsx)
#install.packages("xlsx")
#library(xlsx)
library(stringr)
#install.packages(stringr)
#library(stringr)
library(dplyr)
#install.packages(dplyr)
#library(dplyr)
library(ggplot2)
#install.packages(ggplot2)
#library(ggplot2
library(scales)
#install.packages(scales)
#library(scales)

# In der Zukunft müssen Sie diesen Schritt nicht mehr machen. Sie können direk hier starten.


######################
# BENÖTIGTE PACKAGES #
######################

# Load necessary packages
library(xlsx)
library(stringr)
library(dplyr)
library(ggplot2)
library(scales)


###########################
# HERUNTERLADEN DER DATEN #
###########################

# Lade neuesten Nowcast Datensatz von der RKI Webseite
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


###########################
# BERECHNUNG DES R-WERTES #
###########################

# R-Wert Berechnung bei einem seriellen Intervall von 4 Tagen 
R_Wert <- rep(NA, nrow(data)) 
for (t in 8:nrow(data)) { 
  R_Wert[t] <- sum(data$NeuErkr[t-0:3]) / sum(data$NeuErkr[t-4:7])
  } 
data <- data %>% dplyr::mutate(R_Wert = round(R_Wert, digits = 2))

# Vergleiche mit den R-Werten in der Excel-Tabelle 
data %>% select(Datum, R, R_Wert) %>% tail()


####################
#PLOT DES R-WERTES #
####################

# Plot mit deutschen Bezeichnungen
ggplot(data=data, aes(x=Datum)) +
  geom_ribbon(aes(ymin = lb_R, ymax = ub_R), stat="identity", fill="steelblue")+
  geom_line(aes(y = R), stat="identity", fill="steelblue")+
  theme_minimal() + 
  labs(title = "", x = "", y = "Reproduktionszahl R") + 
  scale_x_date(date_breaks = "2 days", labels = 
                 scales::date_format("%d.%m.")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) + 
  theme(axis.text.x = element_text(angle=90, vjust=0))


##################################
# BERECHNUNG DES 7-TAGE R-WERTES #
##################################

# Berechnung des 7-Tage R-Wertes
R7_Wert <- rep(NA, nrow(data)) 
for (t in 11:nrow(data)) { 
  R7_Wert[t-1] <- sum(data$NeuErkr[t-0:6]) / sum(data$NeuErkr[t-4:10]) 
}
data <- data %>% dplyr::mutate(R7_Wert = round(R7_Wert, digits = 2)) 

# Vergleiche mit R-Werten in Excel Tabelle 
data %>% select(Datum, R_7Tage, R7_Wert) %>% tail()


######################
# PLOT 7-Tage R-Wert #
######################

# Plot mit deutschen Bezeichnungen
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
