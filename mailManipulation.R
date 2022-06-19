# Programme de manipulation du mail des étudiants de l'INPHB
# de Yamoussoukro.
# Ecrit par Samuel Michaël Vanié


#-------- Question 1 ----------#


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
  liste <- str_extract_all(mail, "\\w+-*\\w*[^\\d\\.@]")
  donnee <- as.vector(liste)
  identifiants <- data.frame(nom = donnee[[1]][[1]], prenom = donnee[[1]][[2]], email = mail)
  return(identifiants)
}


#-------- Question 2 ----------#


#'@title toMail
#'@description fonction qui construit un mail institutionnel INPHB
#'@param prenom : Prenoms de l'étudiant dont on veut construire le mail
#'@param nom : Nom de l'étudiant
#'@return : dataframe contenant le nom, le prenom et l'adresse mail
toMail <- function(nom, prenom) {

  premier_prenom <- strsplit(prenom, split = " ")[[1]][[1]]

  mail <- paste(tolower(premier_prenom), ".", tolower(nom),
    "@inphb.ci", sep = "")

  return(data.frame(nom = nom, prenom = prenom, email = mail))
}



#-------- Question 3 ----------#


library("readxl")
library("writexl")

data <- read_excel("./data/mail_ingInfo2_2022.xlsx")

data <- as.data.frame(data)

resultats <- toName(data$mail_inp[1])

for (i in 2:length(data$mail_inp)) {
  resultats <- rbind(resultats, toName(data$mail_inp[i]))
}

# write_xlsx(resultats, "./data/names_ingInfo2.xlsx")



#-------- Question 4 ----------#


data <- read_excel("./data/names.xlsx")

data <- as.data.frame(data)

nom_prenoms <- strsplit(data$nom[1], split = " ")[[1]]

resultats <- toMail(nom_prenoms[1], paste(nom_prenoms[-1], collapse = " "))

for (i in 2:length(data$nom)) {

  nom_prenoms <- strsplit(data$nom[i], split = " ")[[1]]

  resultats <- rbind(resultats,
    toMail(nom_prenoms[1], paste(nom_prenoms[-1], collapse = " ")))

}

write_xlsx(resultats, "./data/mails.xlsx")
