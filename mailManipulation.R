# Programme de manipulation du mail des étudiants de l'INPHB
# de Yamoussoukro.
# Ecrit par Samuel Michaël Vanié


# Chargement de la library stringr qui contient la fonction str_extract_all
# qui permet de récupérer des groupes qui ont été matchés à partir
# d'un pattern
library("stringr")

#'@title toName
#'@description fonction qui à partir récupère le nom, le prénom
# et l'adresse mail d'un étudiant à partir de son mail
#'@param mail : C'est le mail institutionnel à traiter
#'@return identifiants, dataframe qui contient le nom, le prénom
# et l'adresse mail récupérés.
toName <- function(mail) {
  liste <- str_extract_all(mail, "\\w+[^\\d\\.@]")
  donnee <- as.vector(liste)
  identifiants <- data.frame(nom = donnee[[1]][[1]], prenom = donnee[[1]][[2]], email = mail)
  return(identifiants)
}

#'@title toMail
#'@description fonction qui construit un mail institutionnel INPHB
#'@param prenom : Prenoms de l'étudiant dont on veut construire le mail
#'@param nom : Nom de l'étudiant
#'@return : dataframe contenant le nom, le prenom et l'adresse mail
toMail <- function(nom, prenom){
  premier_prenom <- strsplit(prenom, split = " ")[[1]][[1]]
  mail <- paste(premier_prenom, ".", nom, "@inphb.ci", sep = "")
  return(data.frame(nom = nom, prenom = prenom, email = mail))
}


