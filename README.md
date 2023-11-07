# puzzle-de-taquin


Ce code implémente un jeu de puzzle carré où seule la pièce 0 peut être déplacée/échangée avec une autre pièce afin de résoudre le puzzle en l'arrangeant dans un ordre croissant. Le but de ce code est d'arriver à résoudre le puzzle de taille NxN avec différentes méthodes.

Dans un premier temps, on utilise un parcours naïf (parcours1) où l'on explore tous les chemins possibles. Cependant, ce n'est pas forcément la bonne approche car la méthode peut prendre beaucoup de temps, surtout si le puzzle de départ est très désordonné/mélangé.

Pour l'améliorer, nous avons défini la fonction parcours2 qui elle aussi utilise un parcours naïf, mais avec un paramètre de plus qui est une liste où nous stockons tous les états de la grille déjà explorés. L'ajout de ce paramètre a permis d'éviter les parcours inutiles, ce qui a réduit considérablement le temps de traitement et a rendu le parcours plus efficace. Néanmoins, la fonction prend encore trop de temps pour résoudre les grilles de taille supérieure à 3.

Finalement, nous avons défini la fonction parcours3 qui utilise un parcours heuristique basé sur une fonction distance que nous avions définie auparavant. Cette fonction calcule le nombre de déplacements nécessaires pour passer d'un puzzle initial au puzzle final. La différence avec parcours2 est qu'elle ajoute les grilles à parcourir de manière triée en se basant sur la distance par rapport à la solution. Cette méthode a rendu le parcours plus efficace, optimisé et plus rapide que les parcours précédents.
