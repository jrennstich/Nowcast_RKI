##############################################################################################################
# Reproducible Nowcast R-Script for R-Factor in Germany from Nowcast Dataset from Robert Koch Institut (RKI) #
##############################################################################################################

# The script presented here is taken from Robert Koch Institute (2020, May 15), Erläuterung der Schätzung der zeitlich variierenden Reproduktionszahl R, RKI.
# Edited naming of variables and descriptions, also included the necessary packages in the script

# You need to take several steps first to ensure you can actually download the data, calculate the scores and the plot them.


######################################
# REQUIRED PACKAGES - FIRST TIME RUN #
######################################

# OPTION 1
# If you know, that you do are missing ALL of the required packages:
install.packages("xlsx", "stringr", "dplyr", "ggplot2", "scales")

# OPTION 2
# If you are not sure which packages you already have installed, but don't want to be bothered to check use this option.
# CAREFUL! This will (re)install all the packages listed here! This takes some time
install.packages(pkgs = c("xlsx", "stringr", "dplyr", "ggplot2", "scales"), dependencies=TRUE)

# OPTION 3
# The following step-by-step options are safest. Just try to load the package.
# If you get an error installing a specific package, uncomment (i.e., delete the '#') 
# the line below that starts with install.packages and the one below, after you have successfully installed the package

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


# Next time you want to download the latest data and plot them, you can start directly here:


#####################
# REQUIRED PACKAGES #
#####################

# Load necessary packages
library(xlsx)
library(stringr)
library(dplyr)
library(ggplot2)
library(scales)


####################
# GETTING THE DATA #
####################

# Load latest Nowcast data from RKI website
daten_file <- str_c("Nowcasting_Zahlen-",Sys.Date(),".xlsx")
if (!file.exists(daten_file)) { 
  file_url <- 
    "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/Nowcasting_Zahlen.xlsx?__blob=publicationFile" 
  download.file(url=file_url,destfile= daten_file, mode="wb") 
}

# Read Excel file 
data <- xlsx::read.xlsx(file = daten_file, sheetName = "Nowcast_R", encoding = "UTF-8") 
data <- data[,1:13] 

# Renaming colum names to shorter variable names
names(data) <- c("Datum", "NeuErkr", "lb_NeuErkr", "ub_NeuErkr", "NeuErkr_ma4", "lb_NeuErkr_ma4", 
                 "ub_NeuErkr_ma4", "R", "lb_R", "ub_R", "R_7Tage", "lb_R_7Tage", "ub_R_7Tage")


#######################
# CALCULATING R VALUE #
#######################

# Calculating R-value with a serial interval of 4 days
R_Wert <- rep(NA, nrow(data)) 
for (t in 8:nrow(data)) { 
  R_Wert[t] <- sum(data$NeuErkr[t-0:3]) / sum(data$NeuErkr[t-4:7])
} 
data <- data %>% dplyr::mutate(R_Wert = round(R_Wert, digits = 2))

# Compare to R-values in the Excel table
data %>% select(Datum, R, R_Wert) %>% tail()


###################
#PLOTTING R-VALUE #
###################

# Plot with English labels
ggplot(data=data, aes(x=Datum)) +
  geom_ribbon(aes(ymin = lb_R, ymax = ub_R), stat="identity", fill="steelblue")+
  geom_line(aes(y = R), stat="identity", fill="steelblue")+
  theme_minimal() + 
  labs(title = "", x = "", y = "Reproduction Number R") + 
  scale_x_date(date_breaks = "2 days", labels = 
                 scales::date_format("%d.%m.")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) + 
  theme(axis.text.x = element_text(angle=90, vjust=0))


#############################
# CALCULATING 7-DAY R VALUE #
#############################

# Calculation of 7-day R-value
R7_Wert <- rep(NA, nrow(data)) 
for (t in 11:nrow(data)) { 
  R7_Wert[t-1] <- sum(data$NeuErkr[t-0:6]) / sum(data$NeuErkr[t-4:10]) 
}
data <- data %>% dplyr::mutate(R7_Wert = round(R7_Wert, digits = 2)) 

# Compare with R-values in Excel table 
data %>% select(Datum, R_7Tage, R7_Wert) %>% tail()


#########################################
# Plot for calculation of 7-day R-value #
#########################################

# Plot with English labels
ggplot(data=data, aes(x=Datum, y = R, color="R")) + 
  geom_ribbon(aes(ymin = lb_R, ymax = ub_R, color=NULL), fill="steelblue") + 
  geom_ribbon(aes(ymin = lb_R_7Tage, ymax = ub_R_7Tage, color=NULL), fill="orange") + 
  geom_line(aes(y = R, color="R")) + geom_line(aes(y = R_7Tage, color="R_7Days"), size = 1) +
  theme_minimal() + 
  labs(title = "", 
       x = "", 
       y = "Reproduction Number R 7-Day") + 
  scale_x_date(date_breaks = "2 days", labels = 
                 scales::date_format("%d.%m.")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) + 
  scale_color_manual(name="Method:", values=c("darkblue","orangered")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) + 
  theme(legend.position="bottom")
