
(* exercice 1 *)

(* 1 *)
(* a *)
(* fonction qui retourne la position de la valeur x dans une grille.*)
let rec position grille x= match grille with 
                      [] -> failwith "Valeur non trouvé."
                      | y::r -> if y=x then 0
                                else 1+ position r x;;

(* position [1;4;2;3;0;5;6;7;8] 0;; *)

 (* b *)
 (* retourne la valeur de la position p dans une grille*)
let rec valeur grille p= match grille with
                        [] -> failwith "Positon introuvable."
                        | x::r -> if p=0 then x
                                  else valeur r (p-1);;
                                  
(* valeur [1;4;2;3;0;5;6;7;8] 4;; *)
                                  
                                  
 (* c *)
 (* Echange deux valeurs v1 et v2 dans une grille et la retourne *)
let rec echange grille v1 v2 = match grille with
                              [] -> []
                              | x::r -> if x=v1 then v2 :: (echange r v1 v2)
                                        else if x=v2 then v1:: (echange r v1 v2)
                                        else x::(echange r v1 v2);;
                                        
(* echange [0;1;2;3;4;5;6;7;8] 0 4;; *)
                                        
                                        
(* 2 *)            
type direction = Droite | Gauche | Bas | Haut;;               


(* Fonction qui verifie si un mouvement est valide *)
(* (grille,y) represente le couple puzzle *)
let mvt_valide (grille,y) direction = (* on recupere la position de 0 *)
                                      let p = position grille 0 in
                                        match direction with
                                        (* on verifie si 0 n'est pas dans la premiere ligne *)
                                        |Haut -> p >= y
                                        (* on verifie si 0 n'est pas dans la derniere ligne *)
                                        |Bas -> p < y*(y-1)
                                        (* on verifie si 0 n'est pas dans la premiere colonne *)
                                        |Gauche -> p mod y <> 0
                                        (* on verifie si 0 n'est pas dans la derniere colonne *)
                                        |Droite -> (p+1) mod y <> 0;;

(* Effectue un déplacement dans une direction donnée pour une grille et une taille y et retourne la grille *)
let deplacer (grille,y) direction = let p=(position grille 0)
                                  in
                                    (* On verifie si le déplacement est possible *)
                                        if mvt_valide (grille,y) direction then 
                                                (* calcule du decalage *)
                                                let deplacement = match direction with 
                                                                    Droite -> 1
                                                                    |Gauche -> -1
                                                                    |Bas -> y
                                                                    |Haut -> -y
                                                    in
                                             (* on echange 0 avec la valeur recuperer avec le decalage  *)       
                                            echange grille 0 (valeur grille (p+deplacement))
                                        else grille;; 
        
(* deplacer ([0;1;2;3;4;5;6;7;8],3) Gauche;; *)
        
                             
(* exercice 2 *)                            
                            
(* 3 *)
(* Mélange une grille (x,y) pour un nombre d'itérations et retourne la grille *)
let rec melange (x,y) iterations =  if iterations > 0 then 
                                    (* on genere un random pour determiner une direction *)
                                      let rand=Random.int 4
                                        in
                                            let grille = match rand with 
                                            | 0 -> deplacer (x,y) Droite
                                            | 1 -> deplacer (x,y) Haut
                                            | 2 -> deplacer (x,y) Gauche
                                            | 3 -> deplacer (x,y) Bas
                                            | _ -> failwith "Cas non determiner."
                                        in
                                            (* On verifie si un changement a été effectuer ou pas*)
                                            if grille=x then melange (x,y) iterations
                                            else melange (grille,y) (iterations-1) 
                                 
                                    else x;;
                             

 (* melange ([0;1;2;3;4;5;6;7;8],3) 100;; *)                            
                             
                                                          
(* 4 *)
(* Applique un chemin sur une grille et la retourne *)
let testerChemin (x,y) chemin =  List.fold_right (fun c acc -> deplacer (acc,y)  c) chemin x;;
 
 
(* testerChemin ([0;1;2;3;4;5;6;7;8],3) [Gauche;Bas;Droite];; *) 
 
 
(* exercice 3 *)
 
(* 5 *)
(* renvoie le chemin obtenu a l'issu d'un parcours naif vers grille_finale apartir de la liste prochaines_grilles *)                         
let rec parcours1 grille_finale taille prochaines_grilles=
                                    match prochaines_grilles with 
                                    | [] -> failwith "Pas de solution trouvé."
                                    | (grille, chemin)::r ->
                                                    (* on verifie si on a trouver le chemin *)
                                                    if grille = grille_finale then chemin
                                                    else 
                                                    (* on filtre pour garder les directions valides *)
                                            let deplacement = List.filter (fun direction -> mvt_valide
                                            (grille,taille) direction ) [Haut; Bas; Gauche; Droite] 
                                            in
                                             (* On genere les nouvelles grilles*)
                                            let nouvelles_grilles = 
                                                List.fold_left (fun acc direction ->
                                                (* pour chaque deplacement on cree une nouvelle grille et on l'ajoute avec sn chemin *)
                                                let nouvelle_grille = deplacer (grille,taille) direction in
                                                        (nouvelle_grille, direction::chemin)::acc) [] deplacement in
                                                (* On rappelle recursivement la fonction en parcourant le reste de la grille et en ajoutant les nouvelles grilles a parcourir *)
                                             parcours1 grille_finale taille (r @ nouvelles_grilles)  ;;  
                                            

 
 
(* 6 *)
(* renvoie le chemin obtenu a l'issu d'un parcours naif vers grille_finale apartir de la liste prochaines_grilles et en stockant les grilles deja parcouru afin d'avoir une version plus optimisé de parcours1 *) 
let rec parcours2 grille_finale taille prochaines_grilles grilles_parcourues= 
                                    match prochaines_grilles with 
                                    | [] -> failwith "Pas de solution trouvé."
                                    | (grille, chemin)::r -> 
                                                            if grille = grille_finale then chemin
                                                            (* on verifie si on a deja parcouru cette grille *)
                                                            else if List.mem grille grilles_parcourues then  parcours2 grille_finale taille r grilles_parcourues
                                                            else 
                                            (* on filtre pour garder les directions valides *)
                                            let deplacement = List.filter (fun direction -> mvt_valide
                                            (grille,taille) direction ) [Haut; Bas; Gauche; Droite] 
                                            in
                                             (* On genere les nouvelles grilles*)
                                            let nouvelles_grilles = 
                                                List.fold_left (fun acc direction -> 
                                                                    let nouvelle_grille = deplacer (grille,taille) direction in
                                                                    (nouvelle_grille, direction::chemin)::acc ) [] deplacement in
                                              (* On rappelle recursivement la fonction en parcourant le reste de la grille et en ajoutant les nouvelles grilles a parcourir et la grille a la liste des grilles deja parcourues*) 
                                             parcours2 grille_finale taille (r @ nouvelles_grilles)  (grille::grilles_parcourues);;   
                                            

                                            
                                            
(* 7 *)

(* fct valeur absolu *)
let absolu x = if x < 0 then -x else x;;

(* Calcule la distance (nombre de deplacements) pour passer d'une grille initiale a une grille finale *)
let distance grille_initiale grille_finale taille =
                            (* utilisation de fold_left pour accumuler la distance totale.*)
                            List.fold_left (fun acc x ->
                                (* position de x dans la grille grille_initiale *)
                                let position_initiale = position grille_initiale x in
                                (* position de x dans la grille grille_finale *)
                                let position_finale = position grille_finale x in
                               (* on verifie si l'element est deja dans la bonne position *)
                                if position_initiale = position_finale then acc
                                else
                                    (* On calcule la difference de colonne *)
                                    let diff_colonne = absolu ((position_finale mod taille) - (position_initiale mod taille)) in
                                    (* On calcule la difference de ligne *)
                                    let diff_ligne = absolu ((position_finale / taille) - (position_initiale / taille)) in
                                    (* on ajoute la somme totale a acc *)
                                    acc + diff_colonne + diff_ligne) 0 grille_initiale;;

                                    
(*  distance [4; 3; 2; 1; 0; 5; 6; 7; 8] [0; 1; 2; 3; 4; 5; 6; 7; 8] 3;; *)                                    
                                    
                            
(* 8 *)

(* Fonction qui insert un tuple dans une liste trier dans un ordre croissant en fonction de la distance et renvoie la liste *)
let rec inserer_trier (grille,chemin,distance) li =
                                    match li with
                                    (* si la liste est vide on insere l'element *)
                                    | [] -> [(grille, chemin, distance)]
                                    | (g, c, d) :: r -> 
                                       (* Si la distance est supieure ou egale a la distance de l'element a inserer on l'ajoute *)
                                        if d >= distance then 
                                            (grille,chemin,distance) :: (g, c, d) :: r
                                        else (g,c,d) :: inserer_trier (grille, chemin, distance) r;;


(* Renvoie le chemin obtenu à l'issu d'un parcours heuristique à partir d'une liste triée prochaines_grilles et d'une liste de grilles_parcourues afin d'obtenir une version plus optimisé et plus performante que parcours2 *)
let rec parcours3 grille_finale taille prochaines_grilles grilles_parcourues= 
                                    match prochaines_grilles with 
                                    | [] -> failwith "Pas de solution trouvé."
                                    | (grille, chemin,dist)::r -> 
                                                            if grille = grille_finale then chemin
                                                            (* on verifie si on a deja parcouru cette grille *)
                                                            else if List.mem grille grilles_parcourues then  parcours3 grille_finale taille r grilles_parcourues
                                                            else
                                            (* on filtre pour garder les directions valides *)
                                            let deplacement = List.filter (fun direction -> mvt_valide
                                            (grille,taille) direction ) [Haut; Bas; Gauche; Droite] 
                                            in
                                             (* On genere les nouvelles grilles en effectuant les deplacements possible*)
                                            let nouvelles_grilles = 
                                                List.fold_left (fun acc direction -> 
                                                let nouvelle_grille = deplacer (grille,taille) direction in
                                                let dist2=distance nouvelle_grille grille_finale taille in
                                                (* On insere  la nouvelle grille dans prochaines_grilles (acc) de maniere ordonne *)
                                                inserer_trier (nouvelle_grille,direction::chemin,dist2) acc ) prochaines_grilles deplacement
                                                in
                                               parcours3 grille_finale taille (nouvelles_grilles)  (grille::grilles_parcourues);;

                                               
(* fonction qui resout le puzzle a partir d'une grille_init et une grille_finale et leur taille en utilisant une methode de parcours specifique *)
let resoudre grille_init grille_finale taille parcours = 
    match parcours with
           (* prochaines_grilles est represente par une liste de couple et initialisé par la grille de depart *)
    | 1 -> parcours1 grille_finale taille [(grille_init, [])]
           (* prochaines_grilles est represente par une liste de couple (grille,chemin) et initialisé par la grille de depart et grilles_parcourues est representé par une liste vide*) 
    | 2 -> parcours2 grille_finale taille [(grille_init, [])] []
           (* prochaines_grilles est represente par une liste de tuple (grille,chemin,distance) et initialisé par la grille de depart avec sa distance et grilles_parcourues est representé par une liste vide*)
    | 3 -> parcours3 grille_finale taille [(grille_init, [], distance grille_init grille_finale taille)] []
    | _ -> failwith "Parcours non implémentés";;  
    
    
    

(* resoudre [3;1;2;4;7;5;0;6;8] [0;1;2;3;4;5;6;7;8] 3 1 ;; *)
(* resoudre [6;3;1;4;0;2;7;8;5] [0;1;2;3;4;5;6;7;8] 3 2 ;; *)
(* resoudre [10;12;4;5;9;14;8;3;0;11;6;15;13;7;2;1] [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15] 4 3;; *)                                               
                                               
                                               
                                               
                            
