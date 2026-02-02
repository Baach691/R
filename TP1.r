iris # Initie Iris
class(iris) 
View(Iris)
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
dfManga = read.csv('/Users/bachir/Desktop/Cours/BUT/Semestre 2/R/TP/manga.csv',header = TRUE, sep =',',dec ='.')
class(dfManga)

dfAnime = read.csv('/Users/bachir/Desktop/Cours/BUT/Semestre 2/R/TP/anime.csv',header = TRUE, sep =',',dec ='.')
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
nrow(subset(dfManga, Vote >= 20000)) # On en a 294
nrow(subset(dfManga, Vote > 20000 & Score > 8 )) # On en a 186
nrow(subset(dfManga, Score >= 7 & Score <= 8)) # On en a 8038

effectifrating = table(dfAnime$Rating)
print(effectifrating)  # Ecrit tous les ratings avec leur nombre à côté
length(effectifrating) # Donne le nombre de rating
prop.table(effectifrating) # mm chose en prcentage


nrow(subset(dfAnime, Rating == "R - 17+ (violence & profanity)")) # 1538
nrow(subset(dfAnime, Rating == "R - 17+ (violence & profanity)" & Score > 8)) # 326
nrow(subset(dfAnime, Rating != "R - 17+ (violence & profanity)")) # 8462
nrow(subset(dfAnime, Rating == "PG - Children  " & Rating == "G - All Ages")) #0
nrow(subset(dfAnime, Score >= 9 & Vote >= 40000)) # 24

dfMangaTest = dfManga[,c("Title","Score","Vote","Ranked")]
dfAnimeTest = dfAnime[,c("Title","Score","Vote","Ranked")]

dfAnimeTest$Type = "Anime"
dfMangaTest$Type = "Manga"

dfConcat = rbind(dfAnimeTest,dfMangaTest)

write.table(dfConcat, file = "ExportTP1.csv", sep = ",", row.names = FALSE)










