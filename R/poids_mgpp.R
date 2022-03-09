# FONCTION POUR CALCULER DES POIDS MGPP


# cette premiere fonction (Martin Chevallier) est utilisee ensuite



namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)), deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm == "")) nm[nonames] <- snm[nonames]
  setNames(L, nm)
}


# fonction mgpp

#' Methode generalisee de Partage des poids
#'
#' @param UA base de sondage (variables id_j + aux_j)
#' @param sA echantillon tire (variables id_j, w_j)
#' @param l_ji table des liens (variables id_j, id_i) : pour le moment on n'autorise pas que j soit lie a plusieurs i
#' @param id_j identifiant au niveau UA
#' @param w_j poids de sondage des unites de sA
#' @param id_i identifiant au niveau UB
#' @param aux_j variable auxiliaire au niveau UA
#' @param controles effectuer des controles sur les tables (default=T)
#' @param sorties ecrit un petit resume a la fin (a ameliorer)
#'
#' @return 2 tables sont produites dans une liste en sortie
#'
#' La table w_i comporte 5 variables :
#'
#' * w_i_cl : poids MGPP classiques
#'
#' * w_i_aux : poids MGPP avec liens ponderes par la variable aux_j
#'
#' * w_i_cl_exh : vaut w_i_cl, sauf si au moins une unite liee appartient a l'exhaustif (w_i_cl_exh vaut alors 1)
#'
#' * w_i_aux_exh : vaut w_i_aux, sauf si au moins une unite liee appartient a l'exhaustif (w_i_aux_exh vaut alors 1)
#'
#' * exh : indicatrice d'appartenance a l'exhaustif : une unite de UB est dans l'exhaustif si au moins une unite qui lui est lie a w_j=1
#'
#'
#' la table teta servira pour calculer les variables zk (utiles pour des traitements type calcul de precision, calage...)
#'
#' @import dplyr
#' @export
#'
#' @examples
#'
#' # Exemples article JMS (pages 4 a 9)
#'
#' # Exemple 3.1
#'
#' bds_ul_31 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   CA=c(1,5,10)
#' )
#'
#' ech_ul_31 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   w_ul=c(10,10,10)
#' )
#'
#' liens_31 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   id_EP=c("A'","B'","B'")
#' )
#'
#' w_ex31 <- poids_mgpp(UA=bds_ul_31, sA=ech_ul_31, l_ji=liens_31, id_j="id_ul", w_j="w_ul", id_i="id_EP", aux_j="CA")
#'
#' #' on constate que les EP ont un poids final de 10 quelque soit la methode (table w_ex31$w_i), et que les teta sont ceux indiques dans letude (table w_ex31$teta)
#'
#'
#' # Exemple 3.2
#'
#' bds_ul_32 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   CA=c(1,5,10)
#' )
#'
#' ech_ul_32 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   w_ul=c(10,1,1)
#' )
#'
#' liens_32 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   id_EP=c("A'","A'","A'")
#' )
#'
#' w_ex32 <- poids_mgpp(UA=bds_ul_32, sA=ech_ul_32, l_ji=liens_32, id_j="id_ul", w_j="w_ul", id_i="id_EP", aux_j="CA")
#'
#'
#'
#'
#' # Exemple 3.3
#'
#' bds_ul_33 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   CA=c(1,5,10)
#' )
#'
#' ech_ul_33 <- data.frame(
#'   id_ul=c("b","c"),
#'   w_ul=c(1,1)
#' )
#'
#' liens_33 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   id_EP=c("A'","A'","A'")
#' )
#'
#' w_ex33 <- poids_mgpp(UA=bds_ul_33, sA=ech_ul_33, l_ji=liens_33, id_j="id_ul", w_j="w_ul", id_i="id_EP", aux_j="CA")
#'
#'
#'
#' # Exemple 3.4
#'
#' bds_ul_34 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   CA=c(1,5,10)
#' )
#'
#' ech_ul_34 <- data.frame(
#'   id_ul=c("a"),
#'   w_ul=c(10)
#' )
#'
#' liens_34 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   id_EP=c("A'","A'","A'")
#' )
#'
#' w_ex34 <- poids_mgpp(UA=bds_ul_34, sA=ech_ul_34, l_ji=liens_34, id_j="id_ul", w_j="w_ul", id_i="id_EP", aux_j="CA")
#'
#'
#'
#' # Exemple 3.5
#'
#' bds_ul_35 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   CA=c(1,5,10)
#' )
#'
#' ech_ul_35 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   w_ul=c(10,1,1)
#' )
#'
#' liens_35 <- data.frame(
#'   id_ul=c("a","b","c"),
#'   id_EP=c("A'","A'","B'")
#' )
#'
#' w_ex35 <- poids_mgpp(UA=bds_ul_35, sA=ech_ul_35, l_ji=liens_35, id_j="id_ul", w_j="w_ul", id_i="id_EP", aux_j="CA")
#'
#'
#' @seealso http://jms-insee.fr/jms2018s24_4/
#'
#'
poids_mgpp <- function(UA, sA, l_ji, id_j, w_j, id_i, aux_j, controles=T, sorties=T) {

if(controles==T) {
  # controles presence des variables dans les tables
  if(!(id_j %in% names(UA))) stop(paste0(id_j,"n'est pas dans UA"))
  if(!(aux_j %in% names(UA))) stop(paste0(aux_j,"n'est pas dans UA"))
  if(!(id_j %in% names(sA))) stop(paste0(id_j,"n'est pas dans sA"))
  if(!(w_j %in% names(sA))) stop(paste0(w_j,"n'est pas dans sA"))
  if(!(id_i %in% names(l_ji))) stop(paste0(id_i,"n'est pas dans l_ji"))
  if(!(id_j %in% names(l_ji))) stop(paste0(id_j,"n'est pas dans l_ji"))
}

# definition des tables et on met les noms de variables ok avec Indirectsampling
  l_ji <- l_ji[,c(id_j,id_i)]
  colnames(l_ji) <- c("id_j","id_i")
  sA <- sA[,c(id_j,w_j)]
  colnames(sA) <- c("id_j","w_j")
  UA <- UA[,c(id_j,aux_j)]
  colnames(UA) <- c("id_j","aux_j")

if(controles==T) {

  # controles pas de valeurs manquantes dans les dataframes en input (je me limite aux variables necessaires)

  if(anyNA(UA)) stop("UA a des valeurs manquantes")
  if(anyNA(sA)) stop("sA a des valeurs manquantes")
  if(anyNA(l_ji)) stop("l_ji a des valeurs manquantes")

  # controles coherence l_ji UA
  pb <- setdiff(l_ji$id_j, UA$id_j)
  if(length(pb)>0) stop(print("erreur : des id_j sont presents dans UA mais pas dans l_ji, voir liste dans la table resultat"), return(pb))
  pb <- setdiff(UA$id_j, l_ji$id_j)
  if(length(pb)>0) stop(print("erreur : des id_j sont presents dans l_ji mais pas dans UA, voir liste dans la table resultat"), return(pb))

  # controles coherence sA UA
  pb <- setdiff(sA$id_j, UA$id_j)
  if(length(pb)>0) stop(print("erreur : des id_j sont presents dans sA mais pas dans UA, voir liste dans la table resultat"), return(pb))

  #controles valeurs de poids et variable auxiliaire
  if(any(UA$aux_j < 0)) stop("il y a des aux_j < 0 dans UA")
  if(any(sA$w_j < 0)) stop("il y a des w_j < 0 dans sA")

  #controle pas de "doublons"
  if(anyDuplicated(UA$id_j)>0) stop("doublons dans UA")
  if(anyDuplicated(sA$id_j)>0) stop("doublons dans sA")
  #if(anyDuplicated(select(l_ji,id_j,id_i))>0) stop("doublons id_i,id_j dans l_ji")

  #controle pas de "doublons" id_j dans l_ji (pas sur que ca marche...)
  if(anyDuplicated(l_ji$id_j)>0) warning("il y a plusieurs lignes avec le meme id_j dans l_ji, erreurs possibles car la fonction a ?t? con?ue dans un cadre o? il n'y a qu'une ligne par id_j dans l_ji")

}


  # creation de l'indicatrice dappartenance a l'echantillon tj dans la table de liens
  l_ji[,"t_j"] <- l_ji$id_j %in% sA$id_j
  # integration du poids au niveau j
  l_ji <-  base::merge(l_ji,sA,by="id_j",all.x=T,all.y=F,stringsAsFactors = F)
  l_ji[is.na(l_ji$w_j),"w_j"] <- 0  # si j n'est pas dans sA alors wj=0

  # identification des unites de B exhaustives (au moins un j li? avec w_j=1)

  id_ep_exh <- l_ji %>% group_by(id_i) %>% summarise(nb_ul_exh=sum(w_j==1)) %>% filter(nb_ul_exh>0)

# calcul des ponderations de liens

# verifier que toutes les UL de contours sont dans la bds
# verifier qu'il n'y a pas de valeurs manquantes pour la variable auxiliaire permettant le calcul de la ponderation de lien

  aux_j <- merge(l_ji,UA, by="id_j")
  aux_i <- aux_j %>% group_by(id_i) %>% summarise(aux_i=sum(aux_j), L_i=n()) # L_i est le nombre de liens entre i et les unit?s de UA
  teta <- merge(aux_j, aux_i, by="id_i") %>% mutate(teta_cl=1/L_i, teta_aux=ifelse(aux_i != 0, aux_j/aux_i, teta_cl), exh=(id_i %in% id_ep_exh$id_i)) # si somme de aux = 0 sur une EP on prend la mgpp traditionnelle


# comptage du nombre de cas o? on calcule la MGPP classique

  nb_cl <- sum(aux_i==0)

# comptage du nombre d'EP exhaustive

  nb_exh <- nrow(id_ep_exh)

# calcul des poids

w_i <- merge(l_ji, teta) %>% group_by(id_i) %>%
  summarise(
    t_i=(sum(t_j)>0), #au moins un j rattache a i dans l'echantillon sA
    w_i_cl=sum(teta_cl*w_j),
    w_i_aux=sum(teta_aux*w_j)) %>%
  filter(t_i==T) %>% # on se limite aux EP dans lechantillon
mutate( exh=(id_i %in% id_ep_exh$id_i), #oblig? de recalculer...
  w_i_cl_exh=ifelse(exh==T,1,w_i_cl),  # traitement de l'exhaustif
       w_i_aux_exh=ifelse(exh==T,1,w_i_aux)
) %>%
  select(-t_i)


# nettoyage de teta : on met lindicatrice exhaustif
teta <- select(teta,id_j,id_i,teta_cl, teta_aux, exh)


# on remet l'identifiant (galere) dans w_i et dans teta
names(w_i)[match("id_i",names(w_i))] <- id_i
names(teta)[match("id_i",names(teta))] <- id_i
names(teta)[match("id_j",names(teta))] <- id_j


taille_ech_av <- nrow(sA)
taille_ech_ap <- nrow(w_i)

if(sorties==T){

cat("la taille de l'echantillon sA est", taille_ech_av, "la taille de l'echantillon sB est",taille_ech_ap)
cat("\n",nb_cl," w_i ont ete calcules avec la MGPP traditionnelle car la somme de aux_j etait nulle")
cat("\n",nb_exh," w_i ont ete mis a 1 car au moins un j lie avait w_j=1")

}

res <- namedList(w_i,teta)

return(res)

}



