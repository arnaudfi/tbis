# script qui a servi a generer les donnees en exemple

library("dplyr")

set.seed(42)

# parametres du jeux d'essai



# taille des strates UA (3=strate exhaustive)

N1 <- 100
N2 <- 50
N3 <- 10

# allocations par strate en UL

n1 <- 5
n2 <- 5
n3 <- 10 #strate 3 exhaustive


# nombre d'unités UB

M_EP_final <- 150



# UA


bds_s1 <- data.frame(
  idA = c(1:N1),
  CA = abs(rnorm(n=N1, mean = 100, sd = 50)),
  strate="S1")

bds_s2 <- data.frame(
  idA = c(N1+1:N2),
  CA = abs(rnorm(n=N2, mean = 1000, sd = 200)),
  strate="S2")

bds_s3 <- data.frame(
  idA = c(N1+N2+1:N3),
  CA = abs(rnorm(n=N3, mean = 2000, sd = 500)),
  strate="S3")



# aggregation des strates

bds <- rbind(bds_s1, bds_s2, bds_s3) %>%
  mutate(idB = round(runif(n = N1+N2+N3, min = 1, max = M_EP_final))) #construction des ep finales

tbis_dataPop <- select(bds,-idB)


# Contours

tbis_liens <- select(bds,idA,idB)



## plan de sondage stratif fictif
tbis_dataStrate <- data.frame("strate"=c("S1","S2","S3"), "Nh"=c(N1,N2,N3), "nh"=c(n1,n2,n3))


# Liens
tbis_liens <- tbis_dataPop %>% select(idA) %>% mutate(idB=paste0("idB",round(runif(50,1,10))))

# echantillon

library(sampling)
sA <- strata(tbis_dataPop,"strate",tbis_dataStrate$n,method="srswor")
tbis_sA <- getdata(tbis_dataPop,sA) %>% mutate(poids=1/Prob) %>% select(-ID_unit, -Prob, -Stratum)


# tests

library(tbis)

#exemple pour idB==21

tbis_liens[tbis_liens$idB=="21",]$idA

 proba_inclusion_indirecte(c("32","43","111"),
 dataPop = tbis_dataPop,
 dataStrate =tbis_dataStrate,
 identifiant = "idA",
 nomStrate = "strate",
  population = "Nh",
  echantillon =  "nh")


#double

# calcul de la probabilité d'inclusion double des unités liées c("51") et c("140","146")
proba_double_indirecte(c("32","43","111"),c("51","103"),
dataPop = tbis_dataPop,
dataStrate =tbis_dataStrate,
identifiant = "idA",
nomStrate = "strate",
population = "Nh",
echantillon =  "nh")


#mgpp

poids_mgpp(UA=tbis_dataPop, sA=tbis_sA, l_ji=tbis_liens, id_j="idA", w_j="poids", id_i="idB", aux_j="CA")


# sauvegarde des tables

usethis::use_data(tbis_dataPop)
usethis::use_data(tbis_dataStrate)
usethis::use_data(tbis_liens)
usethis::use_data(tbis_sA)
