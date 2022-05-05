(* Imperative Style *)
let select_sort a =
  for k = 0 to Array.length a - 1 do
    (* ASSERT: the array positions a[0 ... k-1] is sorted 
     -> loop invariant*)
    (* Find smallest item in a[k ... END]*)
    let iMin = ref (k) in
    for j = !iMin + 1 to Array.length a - 1 do
      if a.(j) < a.(!iMin)
      then iMin := j
    done;
    let tmp = a.(k) in
      a.(k) <- a.(!iMin);
      a.(!iMin) <- tmp
  done
;;

let _arr = [|10;4;32;4|] in 
  select_sort _arr;
  Array.iter (fun item -> print_int item; print_string ",") _arr
;;

(* Worst case permutation of the numbers from 1 to 7:
  7,6,5,4,3,2,1 *)