let merge_sort list =
  let rec merge_one acc = function
    | ([], []) -> List.rev acc
    | (xs, [])
    | ([], xs) -> List.rev_append acc xs
    | ((x::xs as l1), (y::ys as l2)) ->
      if x < y
      then merge_one (x::acc) (xs, l2)
      else merge_one (y::acc) (l1, ys)
  in
  let rec merge acc = function
    | xs::ys::tl -> merge ((merge_one [] (xs, ys))::acc) tl
    | tl -> tl @ acc
  in
  let rec loop = function
    | _::_::_ as list -> loop (merge [] list)
    | list -> list
  in
  loop (List.map (fun x -> [x]) list);;


let rec merge_sort a =
  if Array.length a <= 1
  then a
  else
  
  (* Split array into two smaller ones and sort recursively*)
  let mid_idx = Array.length a / 2 in
  let a1 = merge_sort (Array.sub a 0 mid_idx) and a2 = merge_sort (Array.sub a mid_idx (((Array.length a - 1) - (mid_idx)) + 1)) in
  let a3 = Array.make (Array.length a) 0 in
  let i1 = ref (0) and i2 = ref (0) and i3 = ref (0) in
  while (!i1 < Array.length a1) && (!i2 < Array.length a2) do
    i3 := !i3 + 1;
    if (!i1 == Array.length a1) then 
    (a3.(!i3-1) <- a2.(!i2);
    i2 := !i2 + 1) 
    else if (!i2 == Array.length a2) then
      (a3.(!i3-1) <- a1.(!i1);
      i1 := !i1 + 1)
    else if a1.(!i1) >= a2.(!i2) then 
        (a3.(!i3-1) <- a2.(!i2);
        i2 := !i2 + 1)
    else 
        (a3.(!i3-1) <- a1.(!i1);
        i1 := !i1 + 1)
  done;
  
  a3
;;


(* let arr = [|10;4;1;5;100|];;
merge_sort arr;;
let () = Array.iter (fun el -> print_int el; print_string(" ")) arr;; *)

let merge a l m r =
  (* ASSERT subarrays a[p:q+1] and a[q+1:r+1] are sorted *)
  let n1 = m-l+1 and n2 = r-m in
  let a_l = Array.make (n1+1) 0 and a_r = Array.make (n2+1) 0 in
  for i=0 to n1-1 do
    a_l.(i) <- a.(l+i)
  done;
  for j=0 to n2-1 do
    a_r.(j) <- a.(m+j+1)
  done;
  a_l.(n1) <- 10000;
  a_r.(n2) <- 10000;
  print_newline();
  Array.iter (fun el -> print_int el; print_string(" ")) a_l;
  print_newline();
  Array.iter (fun el -> print_int el; print_string(" ")) a_r;
  let i=ref (0) and j=ref (0) in
  for k=l to r do
    print_string "...";
    print_int a_l.(!i);
    print_string "...";
    print_int a_r.(!j);
    print_string "...";
    if a_l.(!i) <= a_r.(!j)
    then 
    (a.(k) <- a_l.(!i); i := !i + 1)
    else
    (a.(k) <- a_r.(!j); j := !j + 1)
  done;
  print_newline();
  Array.iter (fun el -> print_int el; print_string(" ")) a;
;;

  
let rec merge_sort_2 a l r =
  if l<r then 
  (let m = l + (r-l) / 2 in
    print_string "midpoint";
    print_int m;
    merge_sort_2 a l m;
    merge_sort_2 a (m+1) r;
    let merge a l m r =
      (* ASSERT subarrays a[p:q+1] and a[q+1:r+1] are sorted *)
      let n1 = m-l+1 and n2 = r-m in
      let a_l = Array.make (n1+1) 0 and a_r = Array.make (n2+1) 0 in
      for i=0 to n1-1 do
        a_l.(i) <- a.(l+i)
      done;
      for j=0 to n2-1 do
        a_r.(j) <- a.(m+j+1)
      done;
      a_l.(n1) <- 10000;
      a_r.(n2) <- 10000;
      print_newline();
      Array.iter (fun el -> print_int el; print_string(" ")) a_l;
      print_newline();
      Array.iter (fun el -> print_int el; print_string(" ")) a_r;
      let i=ref (0) and j=ref (0) in
      for k=l to r do
        print_string "...";
        print_int a_l.(!i);
        print_string "...";
        print_int a_r.(!j);
        print_string "...";
        if a_l.(!i) <= a_r.(!j)
        then 
        (a.(k) <- a_l.(!i); i := !i + 1)
        else
        (a.(k) <- a_r.(!j); j := !j + 1)
      done;
      print_newline();
      Array.iter (fun el -> print_int el; print_string(" ")) a;
    in
    merge a 1 m r)
;;

(* 
MergeSort(arr[], l,  r)
  If r > l
     1. Find the middle point to divide the array into two halves:  
             middle m = l + (r-l)/2
     2. Call mergeSort for first half:   
             Call mergeSort(arr, l, m)
     3. Call mergeSort for second half:
             Call mergeSort(arr, m+1, r)
     4. Merge the two halves sorted in step 2 and 3:
             Call merge(arr, l, m, r) *)

let arr = [|10;4;1;5;100|];;
merge_sort_2 arr 0 (Array.length arr - 1);;
let () = Array.iter (fun el -> print_int el; print_string(" ")) arr;;


