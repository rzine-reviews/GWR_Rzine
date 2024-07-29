# PACKAGES

# Chargement, visualisation et manipulation de la données
library(here)
library(DT)
library(dplyr)

# Analyse et représentation statistique
library(car)
library(correlation)
library(corrplot)
library(ggplot2)
library(gtsummary)
library(GGally)
library(plotly)

# Manipulation et représentation de la données spatiales (cartographie)
library(sf)
library(mapsf)
library(rgeoda) #permet en plus de calculer les indices d'auto-corrélation spatiale
library(RColorBrewer)

# Calcul du voisinage et réalisation de la GWR
library(spdep)
library(GWmodel)

# 1 Présentation et préparation des données
###############################################################

# 1.1 Chargement des données sur le prix de l’immobilier par EPCI

# On situe le dossier dans lequel se trouve nos données
csv_path <- here("data", "donnees_standr.csv")
# lecture du CSV dans un dataframe
immo_df <- read.csv2(csv_path)
# Pour visualiser les 10 1ères lignes
datatable(head(immo_df, 10))

# 1.2 Chargement des données géographiques : les EPCI de France métropolitaine

# lecture du shapefile en entrée dans un objet sf
shp_path <- here("data", "EPCI.shp")
epci_sf <- st_read(shp_path)
# visualisation des données géographiques
mf_map(x = epci_sf)
# et la table attributaire correspondante
datatable(head(epci_sf, 5))

# 1.3 Jointure des données géographiques et tabulaires

nrow(immo_df)
nrow(epci_sf)
# l'option all.x = TRUE permet de garder toutes les lignes de epci_sf,
# même celles qui n'ont pas de correspondance dans immo_df
data_immo <- merge(x = epci_sf, y = immo_df, by.x = "CODE_SIREN", by.y = "SIREN", all.x = TRUE)
nrow(data_immo)
# on peut filtrer les données de la jointure pour ne voir que les epci n'ayant pas de correspondance dans le tableau immo_df
datatable(data_immo[is.na(data_immo$prix_med),])
mf_map(x = data_immo[is.na(data_immo$prix_med),])
# on refait la jointure en ne gardant que les EPCI ayant une correspondance dans le tableau de données
data_immo <- merge(x = epci_sf, y = immo_df, by.x = "CODE_SIREN", by.y = "SIREN")
nrow(data_immo)
# carte du prix médian par EPCI
mf_map(x = data_immo, 
       var = "prix_med", 
       type = "choro",
       breaks = "quantile",
       nbreaks = 7,
       pal = "Mint",
       lwd = 0.01,
       leg_title = "Discrétisation par quantile",
       leg_val_rnd = 0)
mf_title("Prix médian de l'immobilier au m² par EPCI")
mf_credits("Sources: Notaires de France, IGN Admin Express")
# aperçu des données INSEE
par(mfrow = c(3,3))
for (var in colnames(data_immo)[6:13]) {
  mf_map(x = data_immo,
         var = var,
         type = "choro",
         breaks = "quantile",
         nbreaks = 7,
         pal = "Purples",
         lwd = 0.01,
         leg_pos = NA)
  mf_title(var)
  mf_credits('Sources: INSEE, IGN Admin Express')
}
# conversion sf vers sp pour la suite
data_immo_sp <- as(data_immo, "Spatial")

# 2 Création du voisinage
###############################################################

# 2.1 Voisinage

# Création de la liste des voisins : avec l'option queen = TRUE, 
# sont considérés comme voisins 2 polygones possédant au moins 1 sommet commun
#help(poly2nb)
neighbours_epci <- poly2nb(data_immo, queen = TRUE)
# Obtention des coordonnées des centroïdes
coord <- st_coordinates(st_centroid(data_immo))
# Faire un graphe de voisinage
par(mfrow = c(1,1))
plot(neighbours_epci, coord)
# si on prend le 1er élément de neighbours_epci, on voit qu'il a pour voisins les polygones 62, 74 etc.
neighbours_epci[[1]]
# ce qu'on peut vérifier sur la carte :
neighbours1 <- data_immo[c(1,62,74,338,1135,1136,1137,1140),]
neighbours1$index <- rownames(neighbours1)
mf_map(x = neighbours1, border = "white")
# pour voir les numéros
mf_label(x = neighbours1, var = "index", halo = TRUE)
# pour ajouter les liens entre voisins, il faut convertir neighbours_epci en objet sf
proj4string_2154 <- "+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
neighbours_epci_sf <- spdep::nb2lines(neighbours_epci, coords=coord, proj4string=proj4string_2154, as_sf=T)
mf_map(x = neighbours_epci_sf[neighbours_epci_sf$i == 1,], col = "grey30", add = TRUE)


# 2.2 Création de la matrice de voisinage

# la fonction nb2listw attribue des poids à chaque voisin
# par ex. si un polygone a 4 voisins, ils auront chacun un poids de 1/4 = 0.25
#help("nb2listw")
neighbours_epci_w <- nb2listw(neighbours_epci)
# les poids sont stockés dans le 3ème élément de neighbours_epci_w
# par ex. si on veut connaître les poids des voisins du 1er élément :
neighbours_epci_w[[3]][1]

# 3 Approche statistique "classique"
###############################################################

# 3.1 Exploration des variables

# Distribution de la variable dépendante :
add_histogram(plot_ly(data_immo, x = ~prix_med))
# Distribution des variables indépendantes :
a <- add_histogram(plot_ly(data_immo, x = ~log(perc_log_vac), name = "perc_log_vac"))
b <- add_histogram(plot_ly(data_immo, x = ~log(perc_maison), name = "perc_maison"))
c <- add_histogram(plot_ly(data_immo, x = ~log(perc_tiny_log), name = "perc_tiny_log"))
d <- add_histogram(plot_ly(data_immo, x = ~log(dens_pop), name = "dens_pop"))
e <- add_histogram(plot_ly(data_immo, x = ~log(med_niveau_vis), name = "med_niveau_vis"))
f <- add_histogram(plot_ly(data_immo, x = ~log(part_log_suroccup), name = "part_log_suroccup"))
g <- add_histogram(plot_ly(data_immo, x = ~log(part_agri_nb_emploi), name = "part_agri_nb_emploi"))
h <- add_histogram(plot_ly(data_immo, x = ~log(part_cadre_profintellec_nbemploi), name = "part_cadre_profintellec_nbemploi"))
fig = subplot(a, b, c, d, e, f, g, h, nrows = 2)
fig

# 3.2 Etude des corrélations

# on commence par créer un dataframe identique à immo_df mais sans la colonne SIREN
data_cor <- immo_df %>% select(-SIREN)
# on peut ensuite créer la matrice de corrélation
immo_cor <- correlation(data_cor, redundant = TRUE)
# et afficher cette matrice
summary(immo_cor)

# pour mieux voir la matrice de corrélation :
mat_cor_comp <- summary(immo_cor, redundant = TRUE)
# Nom des lignes = valeurs de la première colonne ("Parameter")
rownames(mat_cor_comp ) <- mat_cor_comp[,1]
# Transformation du data.frame en objet matrice (+ suppression première colonne)
mat_cor<- as.matrix(mat_cor_comp[,-1])
# Calcul du nombre total d'individus
nb <- nrow(data_cor)
# Calcul des matrices de p-values et des intervalles de confiance
p.value <- cor_to_p(mat_cor, n = nb, method = "auto")
# Extraction de la matrice des p-value uniquement
p_mat <- p.value$p
# Affichage du corrélogramme
corrplot(mat_cor, 
         p.mat = p_mat, 
         type = "upper", 
         order = "hclust", 
         addCoef.col = "white", 
         tl.col = "gray",
         number.cex = 0.5,
         tl.cex= 1,
         tl.srt = 45, 
         col=brewer.pal(n = 8, name = "PRGn"), 
         sig.level = 0.000001, 
         insig = "blank", 
         diag = FALSE, )

# 3.3 Régression linéaire ou Méthode des moindre carrés ordinaire (MCO)

# Dans le fonctionnement sur R il est important de stocker la régression dans un objet.
# Pour lancer la régression on va utiliser la fonction lm() dont les 2 lettres sont l'acronyme pour linear model
mod.lm <- lm(formula = prix_med ~ perc_log_vac + perc_maison + perc_tiny_log + dens_pop + med_niveau_vis + part_log_suroccup + part_agri_nb_emploi + part_cadre_profintellec_nbemploi, 
             data = data_immo)
# On affiche les principaux résultats avec la fonction summary
summary(mod.lm)
# en + joli avec le package gtsummary
mod.lm %>% tbl_regression(intercept = TRUE)
# visu graphique des coefficients des VE avec GGally
GGally::ggcoef_model(mod.lm)

# 3.4 Diagnostic de notre modèle linéaire

# 3.4.1 Multicolinéarité

# calcul des VIF = Variance Inflation Factor
vif(mod.lm)
# ajout des VIF au résumé des coefficients obtenus avec gtsummary
mod.lm %>% tbl_regression(intercept = TRUE) %>% add_vif()

# représentation graphique des VIF
score_vif <- vif(mod.lm)
# création du graphique
par(mar=c(3, 12, 3, 1), mgp=c(1, 0.4, 0)) # pour pouvoir lire les noms des variables
x = barplot(height = score_vif, main = "VIF Values", horiz = TRUE, col = "steelblue", las = 1)
#ajout du seuil de 4
abline(v = 4, lwd = 3, lty = 2)
# et de la limite de 3
abline(v = 3, lwd = 3, lty = 2)

# pour retirer la variable avec le + fort VIF = % petits logements
mod.lm2 <- lm(formula = prix_med ~ perc_log_vac + perc_maison + dens_pop + 
                med_niveau_vis + part_log_suroccup + part_agri_nb_emploi + 
                part_cadre_profintellec_nbemploi, data = data_immo)
summary(mod.lm2)
mod.lm2 %>% tbl_regression(intercept = TRUE) %>% add_vif()
GGally::ggcoef_model(mod.lm2)

# 3.4.2 Principe de parcimonie

# régression pas à pas descendante et ascendante
step(mod.lm2, direction = "both")
# donc le nouveau modèle ne prend pas en compte part_agri_nb_emploi
mod.lm3 <- lm(formula = prix_med ~ perc_log_vac + perc_maison + dens_pop + 
                med_niveau_vis + part_log_suroccup + 
                part_cadre_profintellec_nbemploi, data = data_immo)

# 3.4.3 Analyser les résidus

# pour obtenir les résidus 
res_modlm <- mod.lm$residuals
datatable(as.data.frame(res_modlm))
# et pour les visualiser
par(mfrow=c(1,3))
# diagramme quantile-quantile qui permet de vérifier l'ajustement d'une distribution à un modèle théorique, ici la loi normale
qqPlot(mod.lm)
# Histogramme pour donner une autre indication sur la normalité
hist(rstudent(mod.lm), breaks = 50, col="darkblue", border="white", main="Analyse visuelle des résidus")
# un graphique pour visualiser l'homoscédasticité des résidus
plot(rstudent(mod.lm))
# Pour étudier la normalité on peut utiliser le test de Shapiro-Wilk
shapiro.test(mod.lm$residuals)
# Pour évaluer l'homoscédasticité on peut utiliser le test de Breusch-Pagan
# Le package car propose une fonction pour le réaliser
ncvTest(mod.lm)

# Aparté sur les outliers

# Pour visualiser les individus concernés
datatable(data_immo[c(36, 266),])
# Pour relancer un nouveau modèle sans l'individu le plus extrême
# Notez que l'on peut en supprimer plusieurs d'un coup avec subset=-c(36,266)
mod.lmx <- update(mod.lm, subset=-266)
# Etudier le nouveau modèle
summary(mod.lmx)
vif(mod.lmx)
par(mfrow=c(1,2))
plot(rstudent(mod.lmx)) 
qqPlot(mod.lmx)
# Il est possible de comparer les deux modèles et les coefficients
car::compareCoefs(mod.lm, mod.lmx, pvals = TRUE)

# 3.4.4 L’autocorrélation des résidus

# Test de Moran des résidus de la régression (H0 : pas d’autocorrélation spatiale)
lm.morantest(model = mod.lm, 
             listw = neighbours_epci_w)
# Test de Geary (H0 pas d’autocorrélation)
#  Attention : Pour avoir le  coefficient il faut faire 1-"Résultat test de Geary" (soit ici le coefficient est 0.67)
# Le coefficient de Geary s'étend de 0 à 2, 1 étant le "0" et signifiant aucune corrélation
# Par ailleurs, un score inférieur à 1 implique une corrélation positive et un score supérieur à 1 une corrélation négative.
geary(x = data_immo$prix_med, 
      listw = neighbours_epci_w,
      n = length(neighbours_epci), 
      n1 = length(neighbours_epci)-1, 
      S0 = Szero(neighbours_epci_w))

# 3.4.5 Cartographie des résidus de la régression

data_immo$res_reg <- mod.lm$residuals
# appel de la fonction discr pour les limites de classe
source("discr.R")
# valeur centrale 0, intervalle = écart-type/2, classes extrêmes regroupées
res_residus <- discr(data_immo$res_reg, 0, "class_center", sd(data_immo$res_reg)*0.5, 10)
breaks_residus <- res_residus[[1]]
nb_cl_sup0_res <- res_residus[[2]]
nb_cl_inf0_res <- res_residus[[3]]
# carto des résidus :
# on fonce légèrement la couleur de fond pour mieux voir la classe centrale
mf_theme("default", bg = "#dedede")
# création d'une palette divergente avec une couleur neutre centrale
palette = mf_get_pal(n = c(nb_cl_inf0_res, nb_cl_sup0_res), pal = c("Teal", "Peach"), neutral = "#f5f5f5")
# la carte :
par(mfrow = c(1,1))
mf_map(x = data_immo, 
       var = "res_reg", 
       type = "choro", 
       border = NA,
       lwd = 0.1,
       breaks = breaks_residus,
       pal = palette,
       leg_title = "Valeur centrale =  0\nIntervalle = σ / 2",
       leg_val_rnd = 1
       )
mf_title("Résidus de régression linéaire classique")
mf_credits("Sources: Notaires de France, INSEE, IGN Admin Express")
# réinitialisation du thème
mf_theme("default")

# 4 Analyse de l’autocorrélation spatiale
###############################################################

# 4.1 Niveau global

# carte du prix médian des logements par EPCI
mf_map(x = data_immo, 
       var = "prix_med", 
       type = "choro",
       breaks = "quantile",
       nbreaks = 7,
       pal = "Mint",
       lwd = 0.01,
       leg_title = "Discrétisation par quantile",
       leg_val_rnd = 0)
mf_title("Prix médian de l'immobilier au m² par EPCI")
mf_credits("Sources: Notaires de France, IGN Admin Express")
# Pour l'occasion on va standardiser notre prix médian
# cela permettra par la suite de le comparer à d'autres variables 
# si d'autres analyses d'autocorrélation spatiale sont réalisées
data_immo$prix_med_z<- (data_immo$prix_med-mean(data_immo$prix_med))/sd(data_immo$prix_med)

# Geary
# Attention à la lecture particulière des résultats de l'indice de Geary
geary.test(data_immo$prix_med_z, neighbours_epci_w, zero.policy = TRUE, randomisation = FALSE) 

# Moran
# On indique dans un premier temps la variable que l'on souhaite analyser
# Puis la matrice de voisinage
# L'argument zero.policy=TRUE permet de préciser que l'on souhaite intégrer à l'analyse les entités spatiales qui n'auraient pas de voisins
# L'argument randomisation=FALSE transmet l'instruction à la fonction que nous supposons que la distribution est normale
# Dans le cas contraire on devrait partir sur une solution de type Monte-Carlo qui repose sur la randomisation
moran.test(data_immo$prix_med_z, neighbours_epci_w, zero.policy = TRUE, randomisation = FALSE)
# diagramme de Moran
moran.plot(data_immo$prix_med_z,neighbours_epci_w, 
           labels = TRUE, 
           xlab="prix medians centrés réduits par epci" , 
           ylab="moyenne du prix médian centré réduit par epci des voisins")

# 4.2 Niveau local

# calcul moran local
# Pour utiliser la fonction local_moran du package rgeoda 2 pré-requis:
# 1- utiliser la fonction queen_weights du package rgeoda pour calculer une matrice de contiguïté de type queen 
queen_w <- queen_weights(data_immo)
# 2- Sortir la variable à étudier dans un vecteur
prix_med_z = data_immo["prix_med_z"]
lisa <- local_moran(queen_w, prix_med_z)
# Pour visualiser les résultats des LISA il faut les stocker dans des objets 
# ou dans des bases de données pour les représenter 
lisa_colors <- lisa_colors(lisa)
lisa_labels <- lisa_labels(lisa)
lisa_clusters <- lisa_clusters(lisa)
lisa_value <- lisa_values(lisa)
lisa_pvalue<- lisa_pvalues(lisa)

# carte moran locaux
# on récupère les moran locaux dans data_immo
data_immo$moranlocalvalue <- lisa_values(lisa)
# discrétisation standardisée avec des classes de un demi-écart-type
res_moranloc <- discr(data_immo$moranlocalvalue, 0, "class_break", sd(data_immo$moranlocalvalue)/2, 10)
breaks_moranloc <- res_moranloc[[1]]
nb_cl_sup0_moranloc <- res_moranloc[[2]]
nb_cl_inf0_moranloc <- res_moranloc[[3]]
# création d'une palette associée
palette = mf_get_pal(n = c(nb_cl_inf0_moranloc, nb_cl_sup0_moranloc), pal = c("Teal", "Reds"))
# la carte
mf_map(x = data_immo, 
       var = "moranlocalvalue", 
       type = "choro",
       breaks = breaks_moranloc,
       border = "gray",
       lwd = 0.2,
       pal = palette,
       leg_title = "Discrétisation standardisée\nvaleur centrale = 0\nintervalle = σ / 2", 
       leg_val_rnd = 1)
mf_title("Carte des LISA du prix médian du logement (Moran local)")
mf_credits("Sources: Notaires de France, INSEE, IGN Admin Express")

# Carte des p-value des moran locaux
data_immo$moranlocalpvalue<- lisa_pvalues(lisa)
# Pour plus de lisibilité dans la carte on va faire des classes des p-value
data_immo <- data_immo %>%  mutate(lisapvalue_fac = case_when(moranlocalpvalue <= 0.002 ~ "[0.001;0.002[",
                                                              moranlocalpvalue <= 0.01 ~ "[0.002;0.01[",
                                                              moranlocalpvalue <= 0.05 ~ "[0.01;0.05[",
                                                              TRUE ~ "[0.05;0.5]")) %>%
  mutate(lisapvalue_fac = factor(lisapvalue_fac,
                                 levels = c("[0.001;0.002[", "[0.002;0.01[",
                                            "[0.01;0.05[",
                                            "[0.05;0.5]")))

mypal <- mf_get_pal(n = 4, palette = "Reds")
mf_map(x = data_immo, 
       var = "lisapvalue_fac", 
       type = "typo", 
       border = "grey3", 
       lwd = 0.08, 
       pal = mypal, 
       leg_title = "P-value Local Moran")
mf_title("Carte de significativité des LISA")
mf_credits("Sources: Notaires de France, INSEE, IGN Admin Express")

# carte des LISA avec les 4 types de regroupements du diagramme de Moran
# (High-High, Low-Low, High-Low et Low-High)
data_immo$testmoran <- sapply(lisa_clusters, function(x){return(lisa_labels[[x+1]])})
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
mf_map(x = data_immo, 
       var = "testmoran", 
       type = "typo", 
       border = "black", 
       lwd = 0.1, 
       pal= colors,
       val_order = c("Not significant","Low-Low","Low-High","High-Low","High-High"),
       leg_title = "Lisa cluster")
mf_title("LISA clusters")
mf_credits("Sources: Notaires de France, INSEE, IGN Admin Express")

# 5 Régression géographiquement pondérée (GWR)
###############################################################

# 5.1 Calcul de la matrice des distance

# Le package GWmodel n'est pas compatible avec le format sf il a besoin d'un objet sp
# (contrairement à spgwr qui peut travailler avec un format sf)
# Pour construire la matrice de distances entre centroïdes des EPCI :
dm.calib <- gw.dist(dp.locat = coordinates(data_immo_sp))

# Définition de la bande passante (bandwidth en anglais) :
formula = prix_med ~ 
  perc_log_vac + 
  perc_maison + 
  perc_tiny_log + 
  dens_pop + 
  med_niveau_vis + 
  part_log_suroccup + 
  part_agri_nb_emploi + 
  part_cadre_profintellec_nbemploi
bw_g <- bw.gwr(data = data_immo_sp, 
               approach = "AICc", 
               kernel = "gaussian", 
               adaptive = TRUE, 
               dMat = dm.calib,
               formula = formula)
bw_g

# 5.3 Estimation du modèle

# un des problèmes de la GWR est de gérer des individus "aberrants" au niveau local
# 2 méthodes ont été définies pour gérer cela :
# Méthode 1 (filtered = TRUE) on filtre en fonction des individus standardisés
# L'objectif est de détecter les individus dont les résidus sont très élevés et de les exclure.
# Méthode 2 (filtered = FALSE) on diminue le poids des observations aux résidus élevés.
mod.gwr_g <- gwr.robust(data = data_immo_sp, 
                        dMat = dm.calib,
                        bw = bw_g,
                        kernel = "gaussian",
                        filtered = FALSE,
                        adaptive = TRUE,
                        formula = formula)
# pour comparer 2 formes de noyau
bw_tri <- bw.gwr(data = data_immo_sp, 
                 approach = "AICc", 
                 kernel = "tricube", 
                 adaptive = TRUE, 
                 dMat = dm.calib,
                 formula = formula)
mod.gwr_tri <- gwr.robust(data = data_immo_sp, 
                          dMat = dm.calib,
                          bw = bw_tri,
                          kernel = "gaussian",
                          filtered = FALSE,
                          adaptive = TRUE,
                          formula = formula)
Best_gwr <- cbind(
  rbind(bw_g, bw_tri),
  rbind(mod.gwr_g$GW.diagnostic$gw.R2,mod.gwr_tri$GW.diagnostic$gw.R2),
  rbind(mod.gwr_g$GW.diagnostic$AIC,mod.gwr_tri$GW.diagnostic$AIC)) %>% 
  `colnames<-`(c("Nb Voisins","R2","AIC")) %>% 
  `rownames<-`(c("GAUSSIAN","TRICUBE"))
Best_gwr

# 5.4 Interprétation des premiers résultats

# Pour voir les différents éléments qui composent notre modèle de GWR
summary(mod.gwr_g)
# Pour accéder aux résultats
mod.gwr_g

# L’ensemble des données est stocké dans le sous objet SDF de notre modèle
datatable(mod.gwr_g$SDF@data)
# Pour voir les variables qui le constituent
names(mod.gwr_g$SDF@data)

# 5.4.1 Étude des résidus

# on récupère les résidus dans data_immo
res_gwr <- mod.gwr_g$SDF$Stud_residual
data_immo$res_gwr <- res_gwr
# calcul des limites de classes avec la fonction discr, centrées sur 0
res_resgwr <- discr(data_immo$res_gwr, 0, "class_center", sd(data_immo$res_gwr)*0.5, 10)
breaks_gwr <- res_resgwr[[1]]
nb_cl_sup0_gwr <- res_resgwr[[2]]
nb_cl_inf0_gwr <- res_resgwr[[3]]
# création de la palette correspondante
palette = mf_get_pal(n = c(nb_cl_inf0_gwr, nb_cl_sup0_gwr), pal = c("Teal", "Peach"), neutral = "#f5f5f5")
# la carte des résidus
mf_map(x = data_immo, 
       var = "res_gwr", 
       type = "choro", 
       border = "gray", 
       lwd = 0.2, 
       breaks = breaks_gwr,
       pal = palette, 
       leg_title = "Discrétisation standardisée :\nvaleur centrale = 0\nintervalle = σ / 2", 
       leg_val_rnd = 1)
mf_title("Résidus GWR")
mf_credits("Sources: Notaires de France, INSEE, IGN Admin Express")

# 5.4.2 Étude des coefficients

# On ajoute à data_immo les coefficients
data_immo$agri.coef <- mod.gwr_g$SDF$part_agri_nb_emploi
data_immo$perc_maison.coef <- mod.gwr_g$SDF$perc_maison
data_immo$dens_pop.coef <- mod.gwr_g$SDF$dens_pop
data_immo$med_vie.coef <- mod.gwr_g$SDF$med_niveau_vis
data_immo$logvac.coef <- mod.gwr_g$SDF$perc_log_vac
data_immo$tinylog.coef <- mod.gwr_g$SDF$perc_tiny_log
data_immo$suroccup.coef <- mod.gwr_g$SDF$part_log_suroccup
data_immo$cadre.coef <- mod.gwr_g$SDF$part_cadre_profintellec_nbemploi
# les cartes
par(mfrow = c(4, 2)) 
for (var in colnames(data_immo)[22:29]) {
  # calcul des limites de classe
  res <- discr(data.frame(data_immo)[, var], 0, "class_center", sd(data.frame(data_immo)[, var])*0.5, 10)
  breaks <- res[[1]]
  # palette de couleurs
  nb_cl_sup0 <- res[[2]]
  nb_cl_inf0 <- res[[3]]
  if (nb_cl_inf0 > 0) {
    palette = mf_get_pal(n = c(nb_cl_inf0, nb_cl_sup0), pal = c("Teal", "Peach"), neutral = "#f5f5f5")
  } else { # cas de la médiane du niveau de vie où la valeur min est supérieure à 0
    palette = mf_get_pal(n = c(nb_cl_sup0), pal = c("Peach"))
  }
  # la carte
  mf_map(x = data_immo,
         var = var,
         type = "choro",
         border = "gray",
         lwd = 0.1,
         breaks = breaks,
         pal = palette,
         leg_pos = "left",
         leg_title = NA,
         leg_val_rnd = 0)
  mf_title(var)
}

# Pour voir par EPCI quelle variable sera la plus explicative dans la relation à notre VD
data_immo$agri.t <- mod.gwr_g$SDF$part_agri_nb_emploi_TV
data_immo$maison.t <- mod.gwr_g$SDF$perc_maison_TV
data_immo$dens.t <- mod.gwr_g$SDF$dens_pop_TV
data_immo$medvie.t <- mod.gwr_g$SDF$med_niveau_vis_TV
data_immo$logvac.t <- mod.gwr_g$SDF$perc_log_vac_TV
data_immo$tinylog.t <- mod.gwr_g$SDF$perc_tiny_log_TV
data_immo$suroccup.t <- mod.gwr_g$SDF$part_log_suroccup_TV
data_immo$cadre.t <- mod.gwr_g$SDF$part_cadre_profintellec_nbemploi_TV     
# Définir contrib max
df <- as.data.frame(data_immo)
# On passe les t-values en valeurs absolues pour voir la plus grande contribution dans un sens sens ou dans l'autre
data_immo$contribmax<- colnames(df[, c(30:37)])[max.col(abs(df[, c(30:37)]),ties.method="first")]
# Carte
par(mfrow = c(1, 1))
mf_map(x = data_immo, 
       var = "contribmax", 
       type = "typo", 
       pal = brewer.pal(6,'Set2'),
       border = "white",
       lwd = 0.2)
mf_title("Carte des variables contribuant le plus par epci")

# cartographie des R2 locaux
data_immo$r2local=mod.gwr_g$SDF$Local_R2
mf_map(x = data_immo, 
       var = "r2local", 
       type = "choro",
       breaks = "quantile",
       nbreaks = 11,
       pal= "Reds",
       border = "gray",
       lwd = 0.2,
       leg_title = "Discrétisation par quantile")
mf_title("R² locaux")

# étudie de la significativité des effets sur le territoire à partir des t-value
# Pour rappel si on a plus de 200 individus et le t-value > |1.96| 
# on pourra considérer le coefficient comme significatif au seuil de 0.05 
# (95% chances que ce ne soit pas dû au hasard)
data_immo$nbsignif_t <- rowSums(abs(df[, c(30:37)]) > 1.96)
mf_map(x = data_immo, 
       var = "nbsignif_t", 
       type = "typo",
       pal = "Reds",
       border = "gray",
       lwd = 0.2)
mf_title("Nombre de Betas significatifs par EPCI (t-value)")

# ou avec les p-value
# Les p-value ne sont pas fournis dans le modèle de la GWR
# on pourrait les calculer à partir de t-value et de l'erreur standard 
# mais le package GWmodel propose une fonction pour les obtenir
pvalue <- gwr.t.adjust(mod.gwr_g)
# On ajoute les p-value à notre fichier
data_immo$agri.p <- pvalue$SDF$part_agri_nb_emploi_p 
data_immo$maison.p <- pvalue$SDF$perc_maison_p
data_immo$dens.p <- pvalue$SDF$dens_pop_p
data_immo$medvie.p <- pvalue$SDF$med_niveau_vis_p
data_immo$logvac.p <- pvalue$SDF$perc_log_vac_p
data_immo$tinylog.p <- pvalue$SDF$perc_tiny_log_p
data_immo$suroccup.p <- pvalue$SDF$part_log_suroccup_p
data_immo$cadre.p <- pvalue$SDF$part_cadre_profintellec_nbemploi_p
df <- as.data.frame(data_immo)
data_immo$nbsignif_p <- rowSums(df[, c(41:48)] < 0.05)
mf_map(x = data_immo, 
       var = "nbsignif_p", 
       type = "typo",
       pal= "Reds",
       border = "gray",
       lwd = 0.2,)
mf_title("Nombre des Betas significatifs par EPCI (p-value)")

# Ici nous représenterons les p-value avec un découpage par classe de significativité
# et seulement les p-value de 2 VI
par(mfrow = c(1, 2))
# Par exemple les p-value des coefficients de la variable part de l'emploi agriculteur
data_immo<- data_immo %>%  mutate(agri.p_fac = case_when(agri.p<= 0.002 ~ "[0;0.002[",
                                                         agri.p <= 0.01 ~ "[0.002;0.01[",
                                                         agri.p <= 0.05 ~ "[0.01;0.05[",
                                                         agri.p <= 0.1 ~ "[0.05;0.1[",
                                                         TRUE ~ "[0.1;1]")) %>%
  mutate(agri.p_fac = factor(agri.p_fac,
                             levels = c("[0;0.002[", "[0.002;0.01[",
                                        "[0.01;0.05[",
                                        "[0.05;0.1[", "[0.1;1]")))

mypal2 <- mf_get_pal(n = 5, palette = "OrRd")
mf_map(x = data_immo, 
       var = "agri.p_fac", 
       type = "typo", 
       border = "grey3", 
       lwd = 0.1, 
       pal = mypal2, 
       leg_title = "Classe P-value")
mf_title("P-value du coefficient de la part d'emploi agriculteurs")

# Pour la densité de population
data_immo<- data_immo %>%  mutate(dens.p_fac = case_when(dens.p <= 0.002 ~ "[0;0.002[",
                                                         dens.p <= 0.01 ~ "[0.002;0.01[",
                                                         dens.p <= 0.05 ~ "[0.01;0.05[",
                                                         dens.p <= 0.1 ~ "[0.05;0.1[",
                                                         TRUE ~ "[0.1;1]")) %>%
  mutate(dens.p_fac = factor(dens.p_fac,
                             levels = c("[0;0.002[", "[0.002;0.01[",
                                        "[0.01;0.05[",
                                        "[0.05;0.1[", "[0.1;1]")))

mypal2 <- mf_get_pal(n = 5, palette = "OrRd")
mf_map(x = data_immo, 
       var = "dens.p_fac", 
       type = "typo", 
       border = "grey3", 
       lwd = 0.1, 
       pal=mypal2, 
       leg_title = "Classe P-value")
mf_title("P-value du coefficient de la densité de population")
