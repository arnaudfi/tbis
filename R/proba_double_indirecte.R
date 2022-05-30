#probabilité d'inclusion double

#' Probabilité d'inclusion double indirecte
#'
#' @description Cette fonction est proche de \code{\link{proba_inclusion_indirecte}}
#' mais permet de calculer une probabilité d'inclusion double entre deux unités finales.
#'
#' @param liste1 vecteur des identifiants des unites de dataPop liées a la premiere unite liée
#' @param liste2 vecteur des identifiants des unites de dataPop liées a la seconde unite liée
#' @param ... le reste des arguments est identique a la fonction \code{\link{proba_inclusion_indirecte}}
#'
#' @return probabilite d'inclusion double indirecte des deux unités liées
#' @export
#'
#' @seealso
#' \href{http://jms-insee.fr/jms2022s30_3/}{JMS 2022 : Utilisation des probabilités d’inclusion exactes pour le sondage indirect en population asymétrique}
#'
#' @examples
#' # calcul de la probabilité d'inclusion double des unités liées c("32","43","111") et c("51","103")
#' proba_double_indirecte(c("32","43","111"),c("51","103"),
#'                       dataPop = tbis_dataPop,
#'                       dataStrate =tbis_dataStrate,
#'                       identifiant = "idA",
#'                       nomStrate = "strate",
#'                       population = "Nh",
#'                       echantillon =  "nh")

proba_double_indirecte <- function(liste1,liste2,...){

  if (setequal(liste1,liste2)){
    return(proba_inclusion_indirecte(liste1,...))
  }else{

  p1ou2 <- proba_inclusion_indirecte(union(liste1,liste2),...)
  p1 <- proba_inclusion_indirecte(liste1,...)
  p2 <- proba_inclusion_indirecte(liste2,...)

  return(p1 + p2 - p1ou2)
  }
}

delta_indirect <- function(liste1,liste2,...){
  if (setequal(liste1,liste2)){
    p <- proba_inclusion_indirecte(liste1,...)
    return(p*(1-p))
  }else{

  p1et2 <- proba_double_indirecte(liste1,liste2,...)

  p1 <- proba_inclusion_indirecte(liste1,...)
  p2 <- proba_inclusion_indirecte(liste2,...)
  return(p1et2 - p1*p2)
  }
}
