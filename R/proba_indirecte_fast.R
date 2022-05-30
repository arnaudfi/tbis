

# pour tester
# UA <- tbis_dataPop
# dataStrate <- tbis_dataStrate
# l_ji <- tbis_liens
# id_j <- "idA"
# id_i <- "idB"
# nomStrate <- "strate"
# echantillon <- "nh"
# population <- "Nh"



proba_non_select <- function(m,n,N){
  terme <- (N-n)/N
  if(m>1){
  for(l in c(1:(m-1))){
    terme <- terme*(N-n-l)/(N-l)
  }
  }
  return(terme);
}


#' Probabilité d'inclusion simple indirecte (version rapide)
#'
#' @description Cette fonction est une version de \code{\link{proba_inclusion_indirecte}} avec un format proche
#' de \code{\link{poids_mgpp}}. Pour des jeux de données comportant de nombreux liens, l'execution de cette fontion
#' sera plus rapide que l'execution de \code{\link{proba_inclusion_indirecte}} (moins de lectures de  tables).
#'
#'
#' @param UA base de sondage (variables id_j + aux_j)
#' @param dataStrate dataframe decrivant les strates
#' @param l_ji table des liens (variables id_j, id_i)
#' @param id_j identifiant au niveau UA
#' @param id_i identifiant au niveau UB
#' @param nomStrate variable strate dans UA et dans dataStrate
#' @param population variable indiquant le nombre d'unite dans la BDS dans la strate dans dataStrate
#' @param echantillon variable indiquant le nombre d'unite a tirer dans la strate dans dataStrate
#' @param controles effectuer des controles sur les tables (default=T)
#'
#'
#' @return dataframe avec 2 colonnes :
#' \itemize{
#'   \item id_i : identifiant au niveau UB
#'   \item w_ind : poids correspondant à l'inverse de la probabilité d'inclusion
#' }
#'
#' @export
#'
#' @examples
#' poids_ind(UA=tbis_dataPop, dataStrate=tbis_dataStrate, l_ji=tbis_liens,  id_j="idA", id_i="idB", nomStrate="strate", population = "Nh", echantillon = "nh", controles=T)
#'
#' @seealso
#' \href{http://jms-insee.fr/jms2022s30_3/}{JMS 2022 : Utilisation des probabilités d’inclusion exactes pour le sondage indirect en population asymétrique}
#'
poids_ind <- function(UA=NULL, dataStrate=NULL, l_ji=NULL,  id_j="idA", id_i="idB", nomStrate="strate", population = "Nh", echantillon = "nh", controles=T){

  if(controles==T) {
    # controles presence des variables dans les tables
    if(!(id_j %in% names(UA))) stop(paste0(id_j,"n'est pas dans UA"))
    if(!(id_i %in% names(l_ji))) stop(paste0(id_i,"n'est pas dans l_ji"))
    if(!(id_j %in% names(l_ji))) stop(paste0(id_j,"n'est pas dans l_ji"))
    if(!(nomStrate %in% names(dataStrate))) stop(paste0(nomStrate,"n'est pas dans dataStrate"))
    if(!(population %in% names(dataStrate))) stop(paste0(population,"n'est pas dans dataStrate"))
    if(!(echantillon %in% names(dataStrate))) stop(paste0(echantillon,"n'est pas dans dataStrate"))
  }

# definition des tables et on met les noms de variables ok avec Indirectsampling
l_ji <- l_ji[,c(id_j,id_i)]
colnames(l_ji) <- c("id_j","id_i")
UA <- UA[,c(id_j,nomStrate)]
colnames(UA) <- c("id_j","strate")
dataStrate <- dataStrate[,c(nomStrate, echantillon, population)]
colnames(dataStrate) <- c("strate","nh","Nh")


if(controles==T) {

  # controles pas de valeurs manquantes dans les dataframes en input (je me limite aux variables necessaires)

  if(anyNA(UA)) stop("UA a des valeurs manquantes")
  if(anyNA(dataStrate)) stop("dataStrate a des valeurs manquantes")
  if(anyNA(l_ji)) stop("l_ji a des valeurs manquantes")

  # controles coherence l_ji UA
  pb <- setdiff(l_ji$id_j, UA$id_j)
  if(length(pb)>0) stop(print("erreur : des id_j sont presents dans UA mais pas dans l_ji, voir liste dans la table resultat"), return(pb))
  pb <- setdiff(UA$id_j, l_ji$id_j)
  if(length(pb)>0) stop(print("erreur : des id_j sont presents dans l_ji mais pas dans UA, voir liste dans la table resultat"), return(pb))

  # controles coherence UA dataStrate
  pb <- setdiff(dataStrate$strate, UA$strate)
  if(length(pb)>0) stop(print("erreur : des nomStrate sont presents dans dataStrate mais pas dans UA, voir liste dans la table resultat"), return(pb))
  pb <- setdiff(UA$strate, dataStrate$strate)
  if(length(pb)>0) stop(print("erreur : des nomStrate sont presents dans UA mais pas dans dataStrate, voir liste dans la table resultat"), return(pb))


  #controle pas de "doublons dans UA"
  if(anyDuplicated(UA$id_j)>0) stop("id_j en doublons dans UA")
  if(anyDuplicated(l_ji)>0) stop("doublons dans l_ji")
}



# etape 0 : calcul du nombre d'unit?s li?es (si 1 seul on excluera du calcul)
m <- l_ji %>% group_by(id_i) %>% summarise(m=n())
# calcul pour les m=1
id_i1SeulLien <- filter(m,m==1) %>%
  merge(l_ji, by="id_i") %>%
  merge(UA, by="id_j") %>%
  merge(dataStrate, by="strate") %>%
  mutate(w_ind=Nh/nh) %>%
  select(id_i,w_ind)


# etape 1 : pour les autres : table id_ep, strate, mh (nb liens dans la strate h)
#### 1.a : id_ep, id_ep_tir, strate

id_iPlusieursLiens <- filter(m,m>1)

tab_1 <- left_join(id_iPlusieursLiens, l_ji, by="id_i") %>%
  left_join(select(UA,id_j,strate), by="id_j")

tab_2 <- tab_1 %>% group_by(id_i,strate) %>% summarise(mh=n()) %>% left_join(dataStrate, by="strate")

# etape 2 : calcul de la proba

##### 2.a : gestion des id_i satur?s

id_iSature <- tab_2 %>%
  mutate(indSature=(Nh-nh-mh<0)) %>%
  group_by(id_i) %>%
  summarise(indSature=sum(indSature)) %>%
  filter(indSature==T) %>%
  mutate(w_ind=1) %>% select(-indSature)

#### 2.b reste plus que les id_iPlusieursliensnonsatur?s

tab_2NonSature <- filter(tab_2, !(id_i %in% id_iSature$id_i)) %>%
  mutate(termeStrate=mapply(proba_non_select,mh,nh,Nh)) %>%
  group_by(id_i) %>%
  summarise(termeid_i=prod(termeStrate))

id_iPlusieursLiensNonSature <- tab_2NonSature %>%
  mutate(w_ind=1/(1-termeid_i)) %>%
  select(id_i,w_ind)

# etape 3 : on agrege tout et on met le nom a dequate a l'identifiant

w_ind <- rbind(id_i1SeulLien, id_iSature, id_iPlusieursLiensNonSature)
colnames(w_ind) <- c(id_i,"w_ind")

return(w_ind)

}

# test

# a <- poids_ind(UA=tbis_dataPop, dataStrate=tbis_dataStrate, l_ji=tbis_liens,  id_j="idA", id_i="idB", nomStrate="strate", population = "Nh", echantillon = "nh", controles=T)
# #
# #
# #
# #
# #
# # Verif en comparant avec la fonction d'origine
#
# trouve_contour <- function(id_uniteB,id_A=NULL,id_B=NULL){
#   return(unique(id_A[which(id_B == id_uniteB)])) #j'ajoute unique pour le cas o? 2 UL de la meme EP_tir seraient li?es ? la meme EP (ca ne ferait qu'un lien dans ce cas !)
# }
#
#
# #fonction qui calcule la proba d'inclusion de l'unit? li?e ? partir de son identifiant
#
# prb <- function(id){
#   proba_inclusion_indirecte(trouve_contour(id,id_A = tbis_dataPop$idA,id_B = tbis_liens$idB),
#                             dataPop = tbis_dataPop,
#                             dataStrate =tbis_dataStrate,
#                             identifiant = "idA",
#                             nomStrate = "strate",
#                             population = "Nh",
#                             echantillon =  "nh")
# }
#
# liste_idB <- unique(tbis_liens$idB)
#
# prb_ind <- mapply(liste_idB,FUN= prb)
# poids_ind <- 1/prb_ind
#
# poids_ind <- cbind("idB"=liste_idB, poids_ind)
#
#
# comp <- merge(a, poids_ind) %>% mutate(dif=w_ind-poids_ind)
# # ok juste des dif a 10e-15 pres !





