(** Hydra Battles *)

(* Message utilisé lorsqu'un bug est détecté – par exemple, dans les fonctions de réplication *)

let the_msg = "ouah, le bug!"

(* Une hydre est représentée sous la forme d'un arbre enraciné dont les noeuds peuvent avoir un nombre quelconque mais néanmoins fini de filles. *)

type hydra = Node of hydra list

(* Quelques abréviations  simples *)

(* Hydre à une seule tête *)
let head = Node []
let is_head : hydra -> bool = fun h -> h = head

(* Nœuds à 1, 2 ou 3 filles *)
let single h = Node [h]
let bi h1 h2 = Node [h1;h2]
let tri h1 h2 h3 = Node [h1;h2;h3]

(* Idem, lorsque les filles sont identiques *)
let bisame h = bi h h
let trisame h = tri h h h

(* Liste des filles d'un nœud *)
let les_filles (Node hs) = hs

(* Exemples d'hydres *)

let baby_hydra = single head
let very_small_hydra = bi head head
let small_hydra = single (bi head head)
let my_hydra = tri head head  (single (single (tri head head head)))
let another_hydra = single (tri head head head)
let yet_another_hydra = single small_hydra
let goodstein_hydra = tri head head (tri head head head)

(* Exemple du sujet, page 1 *)
let example_hydra = tri head (bi (single (tri head head head)) head) head
(* Exemple du sujet après 2 coups d'Hercule et réplication en surface *)
let example_shallow = bi (bi (trisame very_small_hydra) head) head
(* Exemple du sujet après 2 coups d'Hercule et réplication en profondeur *)
let example_deep =
  let one = bisame very_small_hydra in
  let two = tri one one head in
  tri two head two
(* L'hydre qu'on aurait obtenue si la duplication en profondeur avait fait 2 copies *)
let example_deep_two_copies =
  let one = trisame very_small_hydra in
  let two = Node[one; one; one; head] in
  Node [two; two; two; head] ;;
(* Les hydres pouvant être assez grosses, il est utile de fournir quelques mesures  *)

(* Écrire une fonction donnant la taille d'une hydre (nombre total de noeuds) *)


 
let size h =
  let rec size_aux h acc =
    match h with
    | Node [] -> (acc + 1)
    | Node (t::q) -> size_aux t acc + size_aux (Node q) acc
  in size_aux h 0 ;;

  
size baby_hydra ;;
size very_small_hydra ;;  
size example_hydra ;;
size goodstein_hydra ;;
(* Écrire une fonction donnant la hauteur d'une hydre (longueur maximale d'un  chemin partant du pied) *)
  
let grand_daughter h =
  let rec aux l acc =
    match l with
    | [] -> acc
    | t::q -> aux q ((les_filles t)@acc)
  in aux (les_filles h) [] ;;


let _ = grand_daughter example_hydra ;;
  
let rec height h = match les_filles h with
  | [] -> 0
  | _ -> 1 + height (Node (grand_daughter h)) ;;

    
let _=height goodstein_hydra ;; 
let _=height very_small_hydra ;;
let _= height example_hydra ;;
(* Écrire une fonction qui calcule l'histogramme d'une hydre, nombre de noeuds à chaque niveau *)

  
let histogram h =
  let rec aux h acc =
    match les_filles h with
    | [] -> acc
    | _ -> aux (Node (grand_daughter h)) (List.length(les_filles h)::acc)
  in List.rev (aux h [1]) ;;

let _ = histogram example_deep ;;

(* Écrire une fonction qui compte le nombre de têtes à chaque niveau. *)
  
let heads_in_list l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | t::q -> if is_head t then aux q (acc + 1) else aux q acc
  in aux l 0 ;;

let histogram_heads h =
  let rec aux h acc =
    match les_filles h with
    | [] -> acc
    | _ -> aux (Node (grand_daughter h)) (heads_in_list(les_filles h)::acc)
  in List.rev (aux h [0]) ;;

let _ = histogram_heads example_deep ;;

(*
   Écrire une fonction qui retourne une liste triée d'arêtes de l'hydre, avec 
   les contraintes décrites dans le sujet.
 *)

let hydra_num h =
  let rec aux l tmp s =
    match tmp with
    | [] -> l
    | t::q -> if is_head t then aux l q (s+1)
              else aux ((s,(List.length(les_filles t)))::l) (List.append q (les_filles t)) (s+1) 
  in List.rev(aux [(0,List.length(les_filles h))] (les_filles h) 1) ;;
  

let _ = hydra_num example_hydra ;;

let  hydra_convers (a,b) next =
  let rec aux next l acc = if (acc = b) then l
                             else aux (next + 1) ((a+0,next)::l) (acc + 1 )
  in aux next [] 0 ;;

let t = (6,3) ;;
let _ = hydra_convers t 7 ;;

let hydra_edges h =
  let rec aux l res next =
    match l with
    | [] -> res
    | (a,b)::q -> aux q (List.append (hydra_convers (a,b) next) res) (next + b)
  in List.rev(aux (hydra_num h) [] 1) ;;

let _ = hydra_edges example_hydra ;;
  

(*
   Affiche une hydre h.
   Prérequis : la fonction hydra_edges doit avoir été écrite.
*)
let show_hydra h =
  (* Translates the list of edges in dot format, and outputs it to filename *)
  let hydra_to_dot h filename =    
    let rec edges_to_dot edges channel =
      match edges with
        [] -> ()
      | (a,b)::r -> Printf.fprintf channel "%d -- %d\n" a b; edges_to_dot r channel
    in
    let dot_preamble = "graph hydra {\n" ^
                       "\trankdir=BT;\n" ^
                       "\tnode [label=\"\" shape=point style=filled fixedsize=true];\n"
    in
    let dot_postamble = "\n}" in
    let edges = hydra_edges h in
    let channel = open_out filename in
    Printf.fprintf channel "%s\n" dot_preamble;
    edges_to_dot edges channel;
    Printf.fprintf channel "%s\n" dot_postamble;
    close_out channel
  in
  (* Get uname of the system to properly set the png viewer *)
  let uname() =
    let (inchannel, outchannel) = Unix.open_process "uname" in
    let name = input_line inchannel in
    close_in inchannel;
    close_out outchannel;
    name
  in
  (* Set viewer to Imagemagick "display" under Linux, or "open" under OSX, otherwise fail :)  *)
  let viewer = let uname = uname() in
    if uname = "Linux" then " display "
    else if uname = "Darwin" then " open "
    else failwith "Viewer not set under windows" in 
  (* Set style to view hydra's heads *)
  let style = "{style=\"invisible\",$.shape=\"none\",height=0.2,width=0.2,image=\"head.png\",label=\"\"}" in
  (* Prepare command *)
  let command = "gvpr -c 'N[$.outdegree==0] " ^ style ^ "' tmp.dot" (* post-process dot file to set style and view hydra's heads *)
                ^ "|" ^ "dot -T png -o tmp.png "                    (* Launch dot on resulting file *)
                ^ "&&" ^ viewer ^ " tmp.png" ^ "&"                  (* Launch viewer in bg *)
  in
  let _ = hydra_to_dot h "tmp.dot" in
  Unix.system command

(*
   Pour désigner un noeud ou une tête, on utilise une notation dite "de Dewey" : le chemin d'accés à un noeud
   est une liste d'indices qui représente le chemin à suivre depuis la racine ("le pied", si on préfère).
   un 0 signifie "aller vers la fille la plus à gauche", etc.
*)

type path = int list

(*
   Réactions de l'Hydre.
   Quand la tête de l'Hydre donnée par le chemin p est supprimée, l'Hydre
   effectue son algorithme de réplication.
*)

let rec repeat_concat n a l =
  if n <= 0
  then l
  else repeat_concat (n-1) a (a::l)

(* Supprime le i-ème élément de hs (si c'est une tête) *)

let rec remove_head i hs =
  match i,hs with
  | 0,(Node []) ::hs' -> hs'
  | i, h::hs' when i> 0 -> h :: remove_head (i-1) hs'
  |  _,_  -> failwith the_msg

(* Un tour de base : 
   - Hercule coupe une tête de l'Hydre h donnée par le chemin p.
   - L'Hydre se réplique n fois.
*)

type replication_fun = path -> hydra -> int -> hydra

(* Version en profondeur *)
let rec deep_replication : replication_fun = fun  p h n ->
  match p,h with
    [i], Node l -> Node (remove_head i l)
  | (_::_), Node l -> Node (deep_replication_list p l n)
  | _,_ -> failwith the_msg
and deep_replication_list p l n =
  match p,l with
    0::p', h::lh -> repeat_concat (1+n) (deep_replication p' h n) lh
  | i::p', h::lh when i> 0 -> h :: deep_replication_list (i-1::p') lh n
  | _,_ -> failwith the_msg

(* Version en surface *)
let rec shallow_replication : replication_fun = fun p h  n ->
  match p,h with
    [i], Node l -> Node (remove_head i l)
  | (_::_), Node l -> Node (shallow_replication_list p l n)
  | _,_ -> failwith the_msg
and shallow_replication_list p l n =
  match p,l with
    [0;i], Node l :: lh -> repeat_concat (1+n) (Node (remove_head  i l)) lh
  | 0::p',  h::lh -> shallow_replication p' h n :: lh
  | i::p', h::lh when i> 0 -> h :: shallow_replication_list (i-1::p') lh n
  | _,_ -> failwith the_msg

(* Les stratégies: Hercule et l'Hydre suivent chacun une stratégie *)

(*
   Une stratégie d'Hercule est, à partir d'une Hydre, de choisir une tête.
   Le programmeur qui définit une stratégie doit s'assurer qu'elle retourne
   toujours un chemin vers une tête.
*)

type hercules_strat =  hydra -> path

(*
   Suggestion: avant la fonction check_hercules_strategy, écrire une fonction sub_hydra
   telle que sub_hydra path h renvoie la sous-hydre de h donnée par le chemin path.
*)

let rec sub_hydra : path -> hydra -> hydra = fun path h ->
  match path with
  |[]->h
  |t::q->sub_hydra q (List.nth (les_filles h) t))
let _=sub_hydra [1;0;0;2] example_hydra;;

(* Écrire la fonction suivante qui teste si une stratégie choisit bien une tête  *)
let check_hercules_strategy : hercules_strat -> hydra -> bool = fun strat  h  ->
  failwith "A écrire"

(* Écrire la stratégie choisissant la tête la plus à gauche *)
let leftmost_head_strat : hercules_strat = fun  h  ->
  let rec aux h acc =
    match h with
    | Node [] -> acc
    | _ -> aux (List.nth(les_filles h) 0) [0]@acc
  in List.rev(aux h [])
let _=leftmost_head_strat small_hydra;;

(* Écrire la stratégie choisissant une tête de hauteur maximale *)
let highest_head_strat : hercules_strat = fun h ->
  failwith "A écrire"

(* Écrire une stratégie visant à choisir une tête le plus près du sol possible *)
let closest_to_ground_strat : hercules_strat = fun h  ->
  failwith "A écrire"

(* En apprenant à utiliser la bibliothèque Random, écrire une stratégie pour choisir une tête au hasard *)
let random_strat : hercules_strat = fun h ->
  failwith "A écrire"

(* Étant donnée une date, l'Hydre peut calculer un nombre de réplications >= 1 *)

type time = Time of int

type hydra_strat =  time -> int

let check_hydra_strategy : hydra_strat -> time -> bool = fun st t -> st t >= 1

(* Une stratégie classique (celle de la vidéo): à chaque tour, le nombre de réplications est incrémenté. *)

let original_hydra_strat : hydra_strat = function Time  t -> t + 1

(* Une stratégie plus amusante : attention à l'explosion de pile ! *)

let boum : hydra_strat = function (Time t) ->
  let rec exp2 i =
    if i = 0 then 1 else 2 * exp2 (i-1)
  in exp2 t

(* Genre de bataille *)

type genre_de_bataille = Battle_kind of replication_fun * hercules_strat * hydra_strat

(*  Le score final d'une bataille *)
type result =
    Hercules_wins of time       (* Nombre de tours effectués *)
  | Hercules_gives_up of hydra  (* Hydre restante *)

(* Écrire la fonction de simulation *)
let simulation : genre_de_bataille -> hydra -> time -> result =
  fun (Battle_kind(replication,hercules_strat, hydra_strat)) initial_hydra (Time(duration)) ->
  failwith "A écrire"

(*
   Écrire une fonction make_trace telle que make_trace measure bat h_init (Time t) donne la suite 
   des valeurs de la fonction measure sur les hydres obtenues en partant de l'hydre h_init et 
   en effectuant t tours de la bataille de genre bat.
*)

let make_trace : (hydra -> 'a) -> genre_de_bataille -> hydra -> time -> 'a list =
  fun measure (Battle_kind(replication,hercules_strat, hydra_strat)) initial_hydra (Time duration) ->
  failwith "A écrire"

(* Écrire ici vos tests *)
