let bubble_sort a =
  let didSwap = ref (true) in
  while !didSwap do
    didSwap := false;
    for k=0 to Array.length a - 2 do
      if a.(k) > a.(k+1)
      then begin
        let tmp=a.(k) in
        a.(k)<-a.(k+1);
        a.(k+1)<-tmp;
        didSwap := true
      end
    done;
  done
;;

let arr = [|10;4;1;5;100|];;
bubble_sort arr;;
let () = Array.iter (fun el -> print_int el; print_string(" ")) arr;;
				

(* By the time we are half way through the array, since
all elements before are lighter, they have gone past the heaviest
element so the heaviest element is at the start already.

Whereas, with the lighter, there may still be heavier items ahead of it
so it will need to drop to the bottom.*)

(* After ith each iteration,
the array a[(n-1)-i ... n-1] is sorted
so after n-1 iterations, the array a[0 ... n-1] is sorted
so bubblesort will never have to perform more than n passes of the outer loop*)