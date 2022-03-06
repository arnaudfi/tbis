#probabilité d'inclusion double

#' Probabilité d'inclusion double indirecte
#'
#' @param liste1 vecteur des identifiants des unites de dataPop liées a la premiere unite
#' @param liste2 vecteur des identifiants des unites de dataPop liées a la seconde unite
#' @param ... le reste des arguments est identique a la fonction proba_inclusion_indirecte
#'
#' @return
#' @export
#'
#' @examples
#' # calcul de la probabilité d'inclusion double des unités liées c("id1","id6","id12") et c("id2","id5")
#' proba_double_indirecte(c("id1","id6","id12"),c("id2","id5"),
#' dataPop = population,
#' dataStrate =stTest,
#' identifiant = "id",
#' nomStrate = "strate",
#' population = "N",
#' echantillon =  "n")
#'
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
