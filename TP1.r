# ==============================================================================
# 🧠 MÉMO RÉCAPITULATIF DES COMMANDES (TP1)
# ==============================================================================
#
# --- 1. EXPLORATION DE DONNÉES ---
# class(objet)      : Donne le type de l'objet (ex: "data.frame", "numeric").
# View(df)          : Ouvre le tableau de données dans une nouvelle fenêtre.
# dim(df)           : Affiche les dimensions (Lignes x Colonnes).
# nrow(df)          : Affiche uniquement le nombre de lignes.
# ncol(df)          : Affiche uniquement le nombre de colonnes.
# colnames(df)      : Affiche les noms des colonnes.
# summary(df)       : Résumé statistique rapide (Min, Max, Moyenne, Quartiles...).
# head(df)          : Affiche les 6 premières lignes (utile pour un aperçu rapide).
#
# --- 2. SÉLECTION ET INDEXATION ---
# df$colonne        : Sélectionne une colonne spécifique (ex: dfManga$Score).
# df[L, C]          : Sélectionne par position [Lignes, Colonnes].
#                     Ex: df[1:5, ]   -> Lignes 1 à 5, toutes les colonnes.
#                     Ex: df[, c("A")] -> Toutes les lignes, colonne "A" uniquement.
#
# --- 3. STATISTIQUES DESCRIPTIVES ---
# mean(vec)         : Calcule la moyenne.
# median(vec)       : Calcule la médiane.
# sd(vec)           : Calcule l'écart-type (standard deviation).
# sum(vec)          : Calcule la somme totale des valeurs.
# quantile(vec, probs=...) : Calcule les déciles/quartiles.
#                     Ex: probs = seq(0.1, 0.9, 0.1) pour les déciles.
# table(vec)        : Compte les effectifs pour chaque catégorie (tableau de contingence).
# prop.table(table) : Transforme les effectifs en fréquences (pourcentages si *100).
# length(obj)       : Donne la longueur d'un vecteur ou d'une liste.
#
# --- 4. MANIPULATION DE DATA FRAMES ---
# subset(df, cond)  : Filtre le tableau selon une condition logique.
#                     Ex: subset(df, Score > 8)
# rbind(df1, df2)   : Fusionne deux tableaux verticalement (les colle l'un sous l'autre).
#                     (Attention : les colonnes doivent avoir les mêmes noms !).
#
# --- 5. IMPORT / EXPORT ---
# read.csv("f.csv") : Importe un fichier CSV.
#                     Arguments clés : header=TRUE (noms colonnes), sep="," ou ";", dec=".".
# write.table(df)   : Exporte un tableau en fichier CSV/Texte.
#                     Arguments clés : row.names=FALSE (pas de numéro de ligne).
#
# --- 6. OPÉRATEURS LOGIQUES (Pour subset) ---
# ==                : Égal à (Attention au double égal !).
# !=                : Différent de.
# > / >=            : Plus grand / Plus grand ou égal.
# < / <=            : Plus petit / Plus petit ou égal.
# &                 : ET (Les deux conditions doivent être vraies).
# |                 : OU (Au moins une des deux conditions doit être vraie).
# %in%              : DANS (Vérifie si la valeur est dans une liste).
#                     Ex: Rating %in% c("A", "B") équivaut à (Rating == "A" | Rating == "B").
# !                 : NON (Inverse la condition).
#                     Ex: !Rating %in% c(...) -> Tout sauf ce qui est dans la liste.
# ==============================================================================


iris # Initie Iris
class(iris) 
View(iris)
#View(iris) # Affiche la base de donnée Iris
nrow(iris) #Nombre de lignes
ncol(iris) #Nb de colonnes
colnames(iris) #Nom des colonnes
summary(iris) # Donne les stats clés : quartiles moyenne médianes max min

iris[,c("Sepal.Length","Species")] # Affiche seulement les colonnes Sepal.Length et Species

iris[c(100,103,105),] # Affiche uniquement la ligne 100, 103 et 105
iris[c(50:100),] # Affiche la ligne 50 à 100
mean(iris$Sepal.Length)# Affiche la moyenne de la variable Sepal.Length

median(iris$Sepal.Width) # Affiche la médiane de la variable Sepal.Width

sd(iris$Petal.Length)  # Affiche l'écart type de la variable Petal.Length

quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by = 0.1)) # Affiche les déciles de la variable Petal.Width






#PARTIE 2
dfManga = read.csv('/Users/bachir/Desktop/Cours/BUT/Semestre 2/R/TP/TP1/manga.csv',header = TRUE, sep =',',dec ='.')
class(dfManga)

dfAnime = read.csv('/Users/bachir/Desktop/Cours/BUT/Semestre 2/R/TP/TP1/anime.csv',header = TRUE, sep =',',dec ='.')
class(dfAnime)

#View(dfManga)
#View(dfAnime)

dim(dfManga)
dim(dfAnime)

mean(dfManga$Score) #Affiche la moyenne du score des mangas. Environ 7.36
mean(dfAnime$Score) #Affiche la moyenne du score des animes. Environ 7.42

sum(dfManga$Vote) # Affiche le total des votes pour le manga. 35 056 671
sum(dfAnime$Vote) # Affiche le total des votes pour les animes. 854 127 650

sd(dfManga$Score) # Affiche l'écart type des scores pour les mangas. 0.3774909
sd(dfAnime$Score) # Affiche l'écart type des scores pour les animes. 0.447107

quantile(dfManga$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1)) #Affiche les déciles des scores pour les mangas. 1e décile à 6.98
quantile(dfAnime$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1)) # Affiche les déciles des scores pour les animes. 1er décile à 6.910

nrow(subset(dfManga, Score > 9)) # On en a 10
nrow(subset(dfManga, Vote >= 200000)) # On en a 12
nrow(subset(dfManga, Vote > 200000 & Score >= 8 )) # On en a 11
nrow(subset(dfManga, Score >= 7 & Score <= 8)) # On en a 8038

effectifrating = table(dfAnime$Rating)
print(effectifrating)  # Ecrit tous les ratings avec leur nombre à côté
length(effectifrating) # Donne le nombre de rating
prop.table(effectifrating) # mm chose en prcentage


nrow(subset(dfAnime, Rating == "R - 17+ (violence & profanity)")) # 1538
nrow(subset(dfAnime, Rating == "R - 17+ (violence & profanity)" & Score > 8)) # 326
nrow(subset(dfAnime, Rating != "R - 17+ (violence & profanity)")) # 8462
nrow(subset(dfAnime, Rating %in% c("PG - Children","G - All Ages"))) # 1510
nrow(subset(dfAnime, Rating != "PG - Children" & Rating != "G - All Ages")) # 9050
nrow(subset(dfAnime, Score >= 9 | Vote >= 400000)) # 496

dfMangaTest = dfManga[,c("Title","Score","Vote","Ranked")]
dfAnimeTest = dfAnime[,c("Title","Score","Vote","Ranked")]

dfAnimeTest$Type = "Anime"
dfMangaTest$Type = "Manga"

dfConcat = rbind(dfAnimeTest,dfMangaTest)

write.table(dfConcat, file = "ExportTP1.csv", sep = ";", row.names = FALSE)










