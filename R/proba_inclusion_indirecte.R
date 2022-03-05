
#' Probabilite inclusion indirecte
#'
#' @description Cette fonction prend en entree un vecteur d'identifiants d'unités liées entre elles et fournit en retour la probabilite d'inclusion indirecte pour un SAS stratifie
#'
#' @param liste_liees vecteur avec identifiants des unités liée dans la table datapop
#' @param dataPop base de sondage
#' @param dataStrate dataframe decrivant les strates
#' @param identifiant variable identifiant dans dataPop
#' @param nomStrate variable strate dans dataPop et dans dataStrate
#' @param population variable indiquant le nombre d'unite dans la BDS dans la strate dans dataStrate
#' @param echantillon variable indiquant le nombre d'unite a tirer dans la strate dans dataStrate
#'
#' @return probabilite d'inclusion indirecte de l'unite lie
#' @export
#'
#' @examples
#'
#'### calcul des probas d'inclusion d'une unité liée à id1,id6 et id12
#' proba_inclusion_indirecte(c("id1","id6","id12"),
#' dataPop = population,dataStrate =stTest,
#' identifiant = "id",
#'  nomStrate = "strate",
#'  population = "N",
#'  echantillon =  "n")

proba_inclusion_indirecte <- function(liste_liees,dataPop =NULL,dataStrate =NULL ,
                             identifiant = "id",
                             nomStrate = "idStrate",
                             population = "Nh",
                             echantillon = "nh"
                             ){

  dataPop <- dataPop[,c(identifiant,nomStrate)]


  dts <- base::merge(dataPop,dataStrate,by=c(nomStrate),all.x=T,all.y=F,stringsAsFactors = F)
  strates_indiv <- dts[,c(identifiant,nomStrate,population,echantillon)]

  #normalise le nom des variables
  colnames(strates_indiv) <- c("id_ind","id_str","Nh","nh")
  pour_calculs <- data.frame(id_ind = as.vector(liste_liees),stringsAsFactors = F)
  pour_calculs <- base::merge(pour_calculs,strates_indiv,by="id_ind",all.x=T,all.y = F,stringsAsFactors = F)
  pour_calculs$id_str <- as.vector(pour_calculs$id_str)
  # à ce stade, pour_calcul contient la liste des individus liés et les infos des strates

  # calcul les mh (le nombre d'individus liés dans chaque strate)

  compte <- table(pour_calculs$"id_str")
  m <- data.frame(id_str=as.vector(names(compte)),mh=as.vector(compte),stringsAsFactors = F)

   pour_calculs <- base::merge(m,pour_calculs,by="id_str",all.x=T,all.y = F)

  # return(cbind(m$"id_str",pour_calculs$"id_str"))
  # return(pour_calculs
  #        )
  # return(c(identifiant,nomStrate,population,echantillon))
  # return(c(identifiant,nomStrate))
   pour_calculs <- pour_calculs[order(pour_calculs$id_str),]
   pour_calculs$l <- 0
   nLignes <- nrow(pour_calculs)

   resultat <- NA

   if(nLignes ==1){
      # s'il n'y a qu'une unité  liée
     resultat <- pour_calculs$nh / pour_calculs$Nh
   }else{

     #regarde s'il y a une strate saturée : on ne peut rien tirer d'autre
     if( with(pour_calculs,min(Nh-nh-mh)) <=0){

       resultat <- 1
     }else{
       #cas général : plusieurs unités liées, pas de strate saturée
       for(i in 2:nLignes){
         if(pour_calculs$id_str[i]==pour_calculs$id_str[i-1]){pour_calculs$l[i]=pour_calculs$l[i-1]+1}
       }
       f <- with(pour_calculs, (Nh-nh-l)/(Nh-l))
       resultat <- 1 - prod(f)
     }


   }
   return(resultat)
}


