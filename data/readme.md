## Données de la formation iFoss

| Nom | Résumé | Source | Notes |
|--- |--- |--- |--- |
| coefs.csv  | ?  | ?  |   |
| coefs02.csv  | ?  | ?  |   |
| donnees_standr.csv  | ?  | ?  |   |
| epci_immo_ifoss.shp  | ?  | ?  | utiliser plutôt EPCI.shp ? |
| shape_sf.shp  | ?  | ?  | utiliser plutôt EPCI.shp ? |
| EPCI.shp  | EPCI France métropolitaine + Corse édition 2021 | IGN ADMIN-EXPRESS-COG édition 2021 par territoire https://geoservices.ign.fr/adminexpress | Les données de l'IGN ont été simplifiées avec [mapshaper]([https://mapshaper.org/) pour en réduire le poids, en utilisant l'algorithme *Visvalingam/weighted area* avec une valeur de 1 |

Détail de **donnees_standr.csv** :
Ce fichier a été constitué par Frédéric Audard et Alice Ferrari à partir de ? (sources). Il contient les variables suivantes :

- SIREN : code SIREN de l'EPCI
- prix_med : pris médian par EPCI à la vente (au m2 ?)
- perc_log_vac : % logements vacants
- perc_maison : % maisons
- perc_tiny_log : % petits logements (surface < ?)
- dens_pop : densité de population (nb habitants / km2 ?)
- med_niveau_vis : médiane du niveau de vie
- part_log_suroccup : % logements suroccupés
- part_agri_nb_emploi : % agriculteurs
- part_cadre_profintellec_nbemploi : % cadres et professions intellectuelles