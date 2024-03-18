# Données fiche GWR RZine

| Nom | Résumé | Source | Notes |
|--- |--- |--- |--- |
| donnees_standr.csv  | Prix de l’immobilier par EPCI (prix médian au m²) + variables INSEE | Données sur le prix de l'immobilier : base de données des notaires de France, extraction par Frédéric Audard et Alice Ferrari en 2018, données INSEE 2019 : [population](https://www.insee.fr/fr/statistiques/6456153?sommaire=6456166), [logement](https://www.insee.fr/fr/statistiques/6454155?sommaire=6454268) et [niveau de vie](https://www.insee.fr/fr/statistiques/6036907) | Voir le détail ci-dessous pour les données INSEE |
| EPCI.shp  | EPCI France métropolitaine + Corse en 2021 | IGN ADMIN-EXPRESS-COG édition 2021 par territoire France métropolitaine https://geoservices.ign.fr/adminexpress | Les données de l'IGN ont été simplifiées avec [mapshaper]([https://mapshaper.org/) pour en réduire le poids, en utilisant l'algorithme *Visvalingam/weighted area* avec une valeur de 1% |
| REGION.shp  | Régions France métropolitaine + Corse en 2021 | IGN ADMIN-EXPRESS-COG édition 2021 par territoire France métropolitaine https://geoservices.ign.fr/adminexpress | Les données de l'IGN ont été simplifiées avec [mapshaper]([https://mapshaper.org/) pour en réduire le poids, en utilisant l'algorithme *Visvalingam/weighted area* avec une valeur de 0.4% |

Détail des variables contenues dans **donnees_standr.csv** :

- SIREN : code SIREN de l'EPCI
- prix_med : pris médian par EPCI à la vente, au m²
- perc_log_vac : % logements vacants
- perc_maison : % maisons
- perc_tiny_log : % petits logements (surface < ?)
- dens_pop : densité de population (nb habitants / km² ?)
- med_niveau_vis : médiane du niveau de vie
- part_log_suroccup : % logements suroccupés
- part_agri_nb_emploi : % agriculteurs
- part_cadre_profintellec_nbemploi : % cadres et professions intellectuelles