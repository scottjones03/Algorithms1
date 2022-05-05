let _list = [1;4;32;4;5;4;5;4;2;5];;
let rec linear_search l x = 
  match l with
  shd :: stl -> 
    if shd==x 
    then [x]
    else linear_search stl x
  | [] -> []
;;

let items_found = linear_search _list 100 in
List.iter (fun item -> print_int item; print_newline()) items_found
;;

let search collection key  =
  let rec _search index key = function
      | [] -> None
      | h::t -> if h == key then Some index else
          _search (index+1) key t
  in _search 0 key collection
;;

let nums = [12; 2; 0; 81]
;;