(* Imperative Style *)
let insertion_sort a =
  for j = 1 to Array.length a - 1 do
    let key = a.(j) and i = ref (j - 1) in
      while !i >= 0 && a.(!i) > key do
        a.(!i+1) <- a.(!i);
        i := !i - 1
      done;
    a.(!i+1) <- key
  done
;;

(* sort l takes a list and returns a sorted list that orders elements from the smallest to the biggest. *)
let rec sort l = 
	(* Functional Style - s = sorted list, h = element to be in inserted *)
	let rec insert s h =
  	match s with
			(* shd = sorted list head, stl = rest of sorted list *)
			(* [*shd] :: [*stl] should match with s*)
			shd :: stl -> 
				if shd > h then
					h :: shd :: stl
				else
					shd :: insert stl h
			(* Return h *)
			|_ -> [h]
		in
		match l with 
			[] -> []
			| hd :: tl -> insert (sort tl ) hd
;;

let unsorted_list = [2;1;5;0];;
let sorted_list = sort unsorted_list;;
let () = List.iter (fun item -> print_int item; print_newline()) sorted_list;;
print_newline();;

let reverse_sort a =
	for j=1 to Array.length a - 1 do
		let tmp=a.(j) and i=ref (j-1) in
			while !i>=0 && tmp > a.(!i) do
				a.(!i+1) <- a.(!i);
        i := !i - 1
      done;
    a.(!i+1) <- tmp
  done
;;

let arr = [|10;4;1;5;100|];;
reverse_sort arr;;
let () = Array.iter (fun el -> print_int el; print_newline()) arr;;

(* Insertion sort cost analysis:
let insertion_sort a =
  for j = 1 to Array.length a - 1 do
    let key = a.(j) and i = ref (j - 1) in
      while !i >= 0 && a.(!i) > key do
        a.(!i+1) <- a.(!i);
        i := !i - 1
      done;
    a.(!i+1) <- key
  done
;;

55 = n-1 times always
During jth iteration:
	56 = 1 times
	57 = depends on the number of swaps to be performed
	 Worst-case: j times (let j=3 -> i=2,1,0)
	58 = 1 times

f(n)=1+2+3+...+n-1<sum(1 ... n)=n(n+1)/2=O(n^2)

=> Insert sort has quadratic cost
*)

(* Binary insertsort
 Have array a[0 ... k] already sorted, consider k+1
 - Comparison : binary search done in lg(k)
 - Exchanges (insert): at most k exchanges
 Since computing for all k, Comparison = O(nlgn), Exchanges = n^2
*)

let binary_insert_sort a =
	for k = 1 to Array.length a - 1 do
		(* ASSERT: a[0 ... k-1] is already sorted*)

		let binary_search _a x =
			let idx_range=[|0; Array.length _a - 1|] in 
			while idx_range.(1) != idx_range.(0) do
				let mid_idx = (idx_range.(1)-idx_range.(0)) / 2 in
					if _a.(mid_idx) > x
					then idx_range.(1) <- mid_idx
					else if _a.(mid_idx) < x
					then idx_range.(0) <- mid_idx
					else 
					idx_range.(1) <- mid_idx;
					idx_range.(0) <- idx_range.(1)
			done;
			if _a.(idx_range.(0)) < x
			then idx_range.(0)+1
			else idx_range.(0)
		in 
		let found = binary_search (Array.sub a 0 k) (a.(k)) in
		if found != k 
		then begin 
			let tmp = a.(k) in
			for j=0 to k - (found + 1) do
				a.(k-j) <- a.(k-j-1)
			done;
			a.(found) <- tmp;
			end
	done

;;


			
let arr = [|10;4;1;5;100|];;
binary_insert_sort arr;;
let () = Array.iter (fun el -> print_int el; print_string(" ")) arr;;
				