# ==============================================================================
#                                MEMO COMMANDES R
# ==============================================================================
#
# --- 1. IMPORT & EXPORT ---
# setwd("chemin")          : Définit le dossier de travail (Working Directory).
# read.csv("fichier.csv")  : Importe un fichier CSV dans un dataframe.
#                            Syntaxe : read.csv("file", header=TRUE, sep=";", dec=",")
# write.table(df, "file")  : Exporte un dataframe vers un fichier.
#
# --- 2. EXPLORATION ---
# nrow(df)                 : Renvoie le nombre de lignes du dataframe.
# summary(df)              : Affiche un résumé statistique (Min, Max, Quartiles, NA...)
# head(df, n)              : Affiche les n premières lignes.
# print(x)                 : Affiche le contenu d'une variable ou d'un résultat.
#
# --- 3. STATISTIQUES DESCRIPTIVES ---
# mean(x, na.rm=TRUE)      : Calcule la moyenne (na.rm=TRUE ignore les valeurs manquantes).
# sum(x, na.rm=TRUE)       : Calcule la somme.
# sd(x, na.rm=TRUE)        : Calcule l'écart-type (Standard Deviation).
# median(x, na.rm=TRUE)    : Calcule la médiane.
# quantile(x, probs)       : Calcule les quartiles ou centiles.
#                            Ex: quantile(x, seq(0, 1, 0.01)) pour les centiles.
# round(x, digits)         : Arrondit x avec un nombre de décimales défini.
#
# --- 4. MANIPULATION DE DONNÉES (TRI & FILTRE) ---
# order(x)                 : Renvoie les indices pour trier.
#                            Syntaxe tri : df[order(df$Col, decreasing=TRUE), ]
# subset(df, condition)    : Filtre le dataframe selon une condition.
#                            Ex: subset(df, Col > 100 & Col2 == "Texte")
#                            Opérateurs : == (égal), != (différent), > (sup), < (inf),
#                                         & (ET), | (OU).
#
# --- 5. CORRÉLATION & GRAPHIQUES ---
# plot(x, y)               : Trace un nuage de points basique (X vs Y).
# cor(x, y)                : Calcule le coefficient de corrélation.
#                            Option : use="complete.obs" (ignore les lignes avec NA).
# library(package)         : Charge une librairie installée (ex: corrplot).
# corrplot(matrice)        : Affiche un graphique de corrélation (nécessite library corrplot).
#
# ==============================================================================



setwd("/Users/bachir/Desktop/Cours/BUT/Semestre 2/R/TP/TP2")
df = read.csv("fao.csv",header = TRUE,sep = ";",dec = ",")
nrow(df)
print(summary(df))

moyenne_dispo = mean(df$Dispo_alim,na.rm=TRUE)
print(moyenne_dispo)

population = sum(df$Population,na.rm=TRUE)
print(population)

ecart_type_export = sd(df$Export_viande,na.rm=TRUE)
print(ecart_type_export)

ecart_type_import = sd(df$Import_viande,na.rm=TRUE)
print(ecart_type_import)

mediane_volume_viande = median(df$Prod_viande,na.rm=TRUE)
print(mediane_volume_viande)

quantile_dispo = quantile(df$Dispo_alim,na.rm=TRUE)
print(quantile_dispo)

centile_import = quantile(df$Import_viande, seq(0,1,0.01))
print(centile_import)

cinq_pays_moins = df[order(df$Population, decreasing = FALSE),]
print(head(cinq_pays_moins,5))

cinq_pays_plus = df[order(df$Population, decreasing = TRUE),]
print(head(cinq_pays_plus,5))

cinq_pays_plus_prod = df[order(df$Prod_viande, decreasing = TRUE),]
print(head(cinq_pays_plus_prod,5))

cinq_pays_plus_import = df[order(df$Import_viande, decreasing = TRUE),]
print(head(cinq_pays_plus_import,5))

print(nrow(subset(df, Dispo_alim >= 2300, select = Nom))) 

sup3500_viande1m = subset(df, Dispo_alim > 3500 & Import_viande >= 1000, select = Nom)
nrow(sup3500_viande1m)

fr_be = subset(df,Nom == "France" | Nom == "Belgique")
print(fr_be)

df$Part_Export = df$Export_viande / df$Prod_viande

df$dispo_alim_pays = df$Dispo_alim * df$Population

write.table(df, "ExportTp2.csv", sep = ";", dec = ",")

dispo_alim_mondiale = sum(df$dispo_alim_pays, na.rm=TRUE)
print(dispo_alim_mondiale)

dispo_alim_mondiale_pers = dispo_alim_mondiale / 2300
print(dispo_alim_mondiale_pers)

# Nuage de points entre Production et Exportation
plot(df$Prod_viande, df$Export_viande, main = "Relation Production / Exportation")

# Coefficient de corrélation entre Production et Exportation
correlation = cor(df$Prod_viande, df$Export_viande, use = "complete.obs")
print(correlation)

# Matrice de corrélation des variables quantitatives
matrice_cor = cor(df[, -1], use = "complete.obs")
matrice_cor = round(matrice_cor, 2)
print(matrice_cor)

#Corrélogramme
library(corrplot)
corrplot(matrice_cor, method = "circle")












