#probabilité d'inclusion double

#' Calcul de la probabilité d'inclusion double
#'
#' @param liste1
#' @param liste2
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
