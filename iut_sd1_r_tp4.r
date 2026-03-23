# Exercice 1 - Creation de fonction

# 1. Fonction de base
salaire_net_cadre = function(salaire_brut) {
  salaire_net_avant_impot = salaire_brut * 0.75
  return(salaire_net_avant_impot) 
}
print(salaire_net_cadre(salaire_brut = 3000))

# 2. Valeur par defaut pour salaire_brut
salaire_net_cadre = function(salaire_brut = 2500) {
  salaire_net_avant_impot = salaire_brut * 0.75
  return(salaire_net_avant_impot) 
}
print(salaire_net_cadre())

# 3. Ajout du temps de travail
salaire_net_cadre = function(salaire_brut = 2500, temps_travail = 1) {
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}
print(salaire_net_cadre(salaire_brut = 3000, temps_travail = 0.8))

# 4. Verification si le salaire est numerique
salaire_net_cadre = function(salaire_brut = 2500, temps_travail = 1) {
  if (!is.numeric(salaire_brut)) {
    return("Erreur : le salaire brut doit etre une valeur numerique")
  }
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}
print(salaire_net_cadre(salaire_brut = "2000€"))
print(salaire_net_cadre(salaire_brut = 2000))

# 5. Verification si le temps de travail est conforme
salaire_net_cadre = function(salaire_brut = 2500, temps_travail = 1) {
  if (!is.numeric(salaire_brut)) {
    return("Erreur : le salaire brut doit etre une valeur numerique")
  }
  if (!is.numeric(temps_travail)) {
    return("Erreur : le temps de travail doit etre une valeur numerique")
  }
  if ((temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur : le temps de travail doit etre compris entre 0 et 1")
  }
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}
print(salaire_net_cadre(salaire_brut = 2000, temps_travail = "100%"))
print(salaire_net_cadre(salaire_brut = 2000, temps_travail = 0.8))
print(salaire_net_cadre(salaire_brut = 2000, temps_travail = 100))

# 6. Ajout du statut
salaire_net = function(salaire_brut = 2500, temps_travail = 1, statut) {
  if (!is.numeric(salaire_brut)) {
    return("Erreur : le salaire brut doit etre une valeur numerique")
  }
  if (!is.numeric(temps_travail)) {
    return("Erreur : le temps de travail doit etre une valeur numerique")
  }
  if ((temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur : le temps de travail doit etre compris entre 0 et 1")
  }
  if (!statut %in% c("cadre", "non cadre")) {
    return("Erreur : le statut doit etre cadre ou non cadre")
  }
  
  if (statut == "cadre") {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
  } else {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
  }
  return(salaire_net_avant_impot) 
}
print(salaire_net(salaire_brut = 2000, statut = "cadre"))
print(salaire_net(salaire_brut = 2000, statut = "non cadre"))
print(salaire_net(salaire_brut = 2000, statut = "technicien"))

# 7. Calcul apres prelevement a la source
salaire_net = function(salaire_brut = 2500, temps_travail = 1, statut) {
  if (!is.numeric(salaire_brut)) {
    return("Erreur : le salaire brut doit etre une valeur numerique")
  }
  if (!is.numeric(temps_travail)) {
    return("Erreur : le temps de travail doit etre une valeur numerique")
  }
  if ((temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur : le temps de travail doit etre compris entre 0 et 1")
  }
  if (!statut %in% c("cadre", "non cadre")) {
    return("Erreur : le statut doit etre cadre ou non cadre")
  }
  
  if (statut == "cadre") {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
  } else {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
  }
  
  if (salaire_net_avant_impot <= 1591) {
    salaire_net_apres_impot = salaire_net_avant_impot
  } else if (salaire_net_avant_impot <= 2006) {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.029)
  } else if (salaire_net_avant_impot <= 3476) {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.099)
  } else if (salaire_net_avant_impot <= 8557) {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.20)
  } else {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.43)
  }
  return(salaire_net_apres_impot) 
}
# Test des impots
print(salaire_net(salaire_brut = 2500, statut = "cadre"))

# 8. Jeu Shifumi
shifumi = function() {
  choix_utilisateur = readline(prompt = "Choisissez entre pierre, papier ou ciseaux : ")
  
  if (choix_utilisateur %in% c("pierre", "papier", "ciseaux")) {
    choix_ordi = sample(c("pierre", "papier", "ciseaux"), 1)
    
    cat("Votre choix :", choix_utilisateur, "\n")
    cat("Choix de l'ordinateur :", choix_ordi, "\n")
    
    if (choix_utilisateur == choix_ordi) {
      return("Egalite !")
    } else if ((choix_utilisateur == "pierre" & choix_ordi == "ciseaux") |
               (choix_utilisateur == "papier" & choix_ordi == "pierre") |
               (choix_utilisateur == "ciseaux" & choix_ordi == "papier")) {
      return("Vous avez gagne !")
    } else {
      return("L'ordinateur a gagne !")
    }
  } else {
    return("Valeur invalide. Veuillez choisir entre pierre, papier ou ciseaux.")
  }
}
# shifumi()


# Exercice 2 - Creation des boucles

# 1. Somme cumulee avec for
resultat = 0
for (element in c(1, 2, 3, 4, 5)) {
  resultat = resultat + element
  print(paste("Le resultat est :", resultat))
}

# 2. Somme cumulee avec while
element = 1
resultat = 0
while (resultat <= 50) {
  resultat = resultat + element
  print(paste("Le resultat est :", resultat))
  print(paste("Le programme s'est arrete a la valeur :", element))
  element = element + 1
}

# 3. Parcourir iris avec for
for (colonne in colnames(iris)) {
  type_colonne = class(iris[, colonne])
  print(paste("La colonne", colonne, "est de type :", type_colonne))
}

# 4. Parcourir iris avec while
indice_colonne = 1
while (indice_colonne <= ncol(iris)) {
  nom_colonne = colnames(iris)[indice_colonne]
  type_colonne = class(iris[, nom_colonne])
  print(paste("La colonne", nom_colonne, "est de type :", type_colonne))
  indice_colonne = indice_colonne + 1
}


# Exercice 3 - Cas pratiques

# 1. Demander 5 fois un nombre et afficher le carre
for (i in 1:5) {
  nombre = readline(prompt = "Entrez le nombre :")
  nombre = as.numeric(nombre)
  carre = nombre^2
  print(paste("Le carre de", nombre, "est", carre))
}

# 2. Taille des fichiers d'un dossier
dossier = getwd() # Utilisation du repertoire de travail actuel
fichiers = list.files(dossier, full.names = TRUE)

for (fichier in fichiers) {
  info = file.info(fichier)
  taille = info$size
  cat("Le fichier", basename(fichier), "a une taille de", taille, "octets.\n")
}

# 3. Graphiques sur iris selon le type de donnees
for (colonne in colnames(iris)) {
  if (is.numeric(iris[, colonne])) {
    boxplot(iris[, colonne], main = paste("Boxplot de", colonne))
  } else {
    barplot(table(iris[, colonne]), main = paste("Barplot de", colonne))
  }
}

# 4. Boucle infinie pour le Shifumi
continuer = TRUE
while (continuer) {
  resultat = shifumi()
  cat("Resultat du jeu :", resultat, "\n")
  
  reponse = readline(prompt = "Voulez-vous continuer a jouer ? (oui/non) : ")
  if (tolower(reponse) == "non") {
    print("Arret du jeu.")
    continuer = FALSE
  }
}

# 5. Jeu du juste prix
juste_prix = function() {
  nombre_a_deviner = sample(1:100, 1)
  reponse = -1
  
  while (reponse != nombre_a_deviner) {
    reponse = as.integer(readline(prompt = "Devinez le nombre : "))
    
    if (reponse < nombre_a_deviner) {
      cat("C'est plus !\n")
    } else if (reponse > nombre_a_deviner) {
      cat("C'est moins !\n")
    } else {
      cat("Bravo, vous avez trouve le juste prix !\n")
    }
  }
}