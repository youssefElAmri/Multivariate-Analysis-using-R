#Youssef El Amri 4 BI 9

#PARTIE 1 Etude d’une variable qualitative

summary(handball)
length(handball)

# List variable names
names(handball)

#3
quantitative_vars <- setdiff(names(handball), "POSTE")

#4
summary(handball$POSTE)

#5
# Calculate relative frequencies
poste_freq <- table(handball$POSTE) / length(handball$POSTE)

# Display relative frequencies
poste_freq


#6

# Plot a pie chart
pie(poste_freq, main = "Relative Frequencies of POSTE")

#7

# Calculate absolute frequencies
poste_abs_freq <- table(handball$POSTE)

# Plot a bar chart
barplot(poste_abs_freq, main = "Absolute Frequencies of POSTE", xlab = "POSTE", ylab = "Frequency")

#8

summary(handball$TAD)

#9

# Plotting an histogram for "TAD"
hist(handball$TAD, breaks = 10, main = "Histogram of TAD", xlab = "TAD")


#10

# Counting the number of handball players with height between 175 and 180 cm
num_players_between_175_and_180 <- sum(which(handball$TAD >= 175 & handball$TAD <= 180))

# Display the count
num_players_between_175_and_180

# PARTIE 2 

# Charger la bibliothèque nécessaire pour la manipulation des données
# Importer les données du fichier ski.txt
db <- read.table("ski.txt", header = TRUE, sep = "\t")

# Calculer la moyenne, les quartiles, l'écart type, le min et le max pour "Age"
age_summary <- summary(db$Age)
age_summary

# Extraire la ligne avec l'âge le plus élevé
highest_age_row <- db[which.max(db$Age), ]
highest_age_row

# Définir les classes d'âge avec un intervalle de 5 ans
age_classes <- cut(db$Age, breaks = seq(min(db$Age), max(db$Age) + 5, by = 5), include.lowest = TRUE)

# Trouver la classe modale
modal_class <- as.data.frame(table(age_classes))
modal_class <- modal_class[which.max(modal_class$Freq), "Var1"]
modal_class

# Tracer un histogramme
hist(db$Age, breaks = seq(min(db$Age), max(db$Age) + 5, by = 5), 
     main = "Histogramme des âges", xlab = "Âge", ylab = "Fréquence")

# Modalités pour Q5_lieu et Q6_niveau
modalites_Q5_lieu <- unique(db$Q5_lieu)
modalites_Q6_niveau <- unique(db$Q6_niveau)

modalites_Q5_lieu
modalites_Q6_niveau

# Fréquence de chaque modalité pour Q5_lieu et Q6_niveau
freq_Q5_lieu <- table(db$Q5_lieu)
freq_Q6_niveau <- table(db$Q6_niveau)

freq_Q5_lieu
freq_Q6_niveau

# Mode pour Q5_lieu et Q6_niveau
mode_Q5_lieu <- modal(db$Q5_lieu)
mode_Q6_niveau <- modal(db$Q6_niveau)

mode_Q5_lieu
mode_Q6_niveau

# Tracer un diagramme en barres pour Q5_lieu
barplot(freq_Q5_lieu, main = "Fréquence de Q5_lieu", xlab = "Q5_lieu", ylab = "Fréquence")

# Tracer un diagramme en barres pour Q6_niveau
barplot(freq_Q6_niveau, main = "Fréquence de Q6_niveau", xlab = "Q6_niveau", ylab = "Fréquence")