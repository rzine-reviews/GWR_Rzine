# fonction pour créer des limites de classes à partir de :
# values : une liste de valeurs à discrétiser
# interval : la taille de chaque classe
# center : valeur centrale de la discrétisation
# pos_center : la position de la valeur centrale, "class_center" ou "class_break"
# (si class_center, une classe sera créée autour de cette valeur, de taille 2*interval)
# min_nb : si besoin les classes extrêmes seront fusionnées jusqu'à obtenir une classe
# avec un nb d'individus >= à min_nb
discr <- function(values, center, pos_center, interval, min_nb) {
  # calcul des limites de classes :
  if (pos_center == "class_break") { # valeur centrale = lim de classe
    breaks <- c(center)
    centermax <- center
    centermin <- center
  } else { # valeur centrale = centre de classe
    if (center < max(values)) {
      # breaks <- c(center - interval/2, center + interval/2)
      breaks <- c(center + interval/2)
      centermax <- center + interval/2
    }
    if (center > min(values)) {
      breaks <- append(breaks, center - interval/2)
      centermin <- center - interval/2
    }
  }
  # ...pour les limites > centre
  if (center < max(values)) {
    x <- 1
    while (centermax + x * interval < max(values)) {
      breaks <- append(breaks, centermax + x * interval)
      x <- x + 1
    }
  }
  # ...pour les limites < centre
  if (center > min(values)) {
    x <- 1
    while (centermin - x * interval > min(values)) {
      breaks = append(breaks, centermin - x * interval)
      x <- x + 1
    }
  }
  # ajout des min et max
  breaks = append(breaks, min(values))
  breaks = append(breaks, max(values))
  # et tri
  breaks = sort(breaks)
  # calcul des effectifs pour chaque classe
  nb_classes = length(breaks) - 1
  sizes = c()
  for (x in 1:nb_classes) {
    min_cl <- breaks[x]
    max_cl <- breaks[x+1]
    current_size <- 0
    for (value in values) {
      if (value >= min_cl & value < max_cl) {
        current_size <- current_size + 1
      } 
    }
    sizes = append(sizes, current_size)
  }
  # suppression des classes ayant un effectif trop faible :
  # ...en partant de la classe du bas
  x <- 1
  while (sizes[x] < min_nb) {
    # fusionne les 2 1ères classes en supprimant la limite qui les sépare
    breaks <- breaks[! breaks %in% c(breaks[x + 1])]
    # recalcule la 2ème valeur des effectifs
    sizes[2] = sizes[1] + sizes[2]
    # et supprime la 1ère valeur d'effectifs
    sizes = sizes[-1]
  }
  # ...en partant de la classe du haut
  x <- length(breaks)
  while (sizes[x - 1] < min_nb) {
    # fusionne les 2 dernières classes en supprimant la limite qui les sépare
    breaks <- breaks[! breaks %in% c(breaks[x-1])]
    # recalcule l'avant dernière valeur des effectifs
    sizes[length(sizes)-1] = sizes[length(sizes)] + sizes[length(sizes)-1]
    # et supprime la dernière valeur d'effectifs
    sizes = sizes[-length(sizes)]
    # réaffecte x
    x <- length(breaks)
  }
  # récupère le nb de classes d'un côté et de l'autre du centre
  if (pos_center == "class_break") {
    nb_cl_sup0 <- length(breaks[breaks > center])
    nb_cl_inf0 <- length(breaks[breaks < center])
  } else {
    if (center < max(values)) {
      nb_cl_sup0 <- length(breaks[breaks > center]) - 1
    } else {
      nb_cl_sup0 <- 0
    }
    if (center > min(values)) {
      nb_cl_inf0 <- length(breaks[breaks < center]) - 1
    } else {
      nb_cl_inf0 <- 0
    }
  }
  resultats <- list(breaks, nb_cl_sup0, nb_cl_inf0)
  return (resultats)
}