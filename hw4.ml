open String
(* Q1: A Rose by any Other Name Would Smell as Sweet *)

type 'a rose_tree = Node of 'a * ('a rose_tree) list

(* Find with exceptions *)

exception BackTrack

(* Implemented with partial evaluation as 'p' to search tree *)
let match_leaf (a : 'a)(b : 'a) : bool = (a=b);;
let even_leaf (a: int) = ((a mod 2) = 0);;

(* Q1.1 write a function that finds an element of the tree using backtracking with exceptions *)
let rec find_e (p : 'a -> bool) (t : 'a rose_tree) : 'a = match t with 
  |Node(leaf, []) -> if p leaf then leaf else raise BackTrack 
  |Node(leaf, h::tail) -> if p leaf then leaf else try find_e p h with BackTrack -> find_e p (Node(leaf, tail))

(* Q1.1: write this function and it helper functions *)
let find (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = try Some(find_e p t) with BackTrack -> None

(* Q1.2 Find with failure continuations *)
let rec find_k (p : 'a -> bool) (t : 'a rose_tree) (k : unit -> 'a option) : 'a option = match t with
  |Node(leaf, []) -> if p leaf then Some leaf else k ()
  |Node(leaf, h::tail) -> if p leaf then Some leaf else find_k p h (fun () -> find_k p (Node(leaf, tail)) k)

(* Q1.2: write this function and it helper functions *)
let find' (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = find_k p t (fun () -> None) (*  call find_k with the appropriate inital continuation *)

(* Find all with continuations *)
let rec find_all_k  (p : 'a -> bool) (t : 'a rose_tree) (k : 'a list -> 'b) : 'b = 
  let rec find_all_k_acc (p : 'a -> bool) (t : 'a rose_tree) (l: 'a list) (k : 'a list -> 'b) = match t with
    |Node(leaf, []) -> if p leaf then k (leaf::l) else k l
    |Node(leaf, h::tail) -> if p leaf then find_all_k_acc p h (leaf::l) (fun a -> find_all_k_acc p (Node(leaf, tail)) a k)
                            else find_all_k_acc p h l (fun a -> find_all_k_acc p (Node(leaf, tail)) a k)
  in find_all_k_acc p t [] k

(* Q1.3: write this function and it helper functions *)
let find_all p t = find_all_k p t (fun a -> a)

(* An example to use *)

let example = Node (7, [ Node (1, [Node(100, [])])
                         ; Node (2, [Node (71, [ Node(71, []); Node(50, [])])])
                         ; Node (71, [])
                         ; Node (9, [])
                         ; Node (11, [Node(43, [])])
                         ; Node (71, [])
                         ])

let is_big x =  x > 10

let x: int option = find' (match_leaf 43) example;;
let y: int option = find' (match_leaf 7) example;;
let z: int option = find' (match_leaf 1000000) example;;

let a: int list = find_all even_leaf example;;
let b: int list = find_all (match_leaf 71) example;;

(* Q2 : Rational Numbers Two Ways *)
type fraction = int * int

module type Arith =
  sig
    type t
    val epsilon : t             (* A suitable tiny value, like epsilon_float for floats *)

    val plus : t -> t -> t      (* Addition *)
    val minus : t -> t -> t     (* Substraction *)
    val prod : t -> t -> t      (* Multiplication *)
    val div : t -> t -> t       (* Division *)
    val abs : t -> t            (* Absolute value *)
    val lt : t -> t -> bool     (* < *)
    val le : t -> t -> bool     (* <= *)
    val gt : t -> t -> bool     (* > *)
    val ge : t -> t -> bool     (* >= *)
    val eq : t -> t -> bool     (* = *)
    val from_fraction : fraction -> t (* conversion from a fraction type *)
    val to_string : t -> string        (* generate a string *)
  end

module FloatArith : Arith =
struct
  type t = float
  let epsilon = 0.0000001
  let from_fraction (num, den) = float_of_int num /. float_of_int den

  let plus = (+.)
  let minus = (-.)
  let prod = ( *. )
  let div = ( /. )
  let abs = abs_float
  let lt = (<)
  let le = (<=)
  let gt = (>)
  let ge = (>=)
  let eq = (=)
  let to_string x = string_of_float x
end

(* Q2.1: Implement the Arith module using rational numbers (t = fraction) *)

(* A helper function that returns the Greatest Common Denominator of two numbers *)
let rec gcd (u : int) (v : int) : int =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

(* Useful function for reducing complex fractions *)
let reduceFraction (f: fraction) : fraction = match f with
  |(n, d) -> let div = gcd n d in (n/div, d/div)

module FractionArith : Arith = 
 struct

  type t = fraction
  let epsilon = (1,1000)
  let from_fraction (num, dem) = (num, dem)

  let plus f1 f2 = match f1, f2 with
    |(n1, d1), (n2, d2) -> ((n1*d2)+(n2*d1), d1*d2)

  let minus f1 f2 = match f1, f2 with
    |(n1, d1), (n2, d2) -> ((n1*d2)-(n2*d1), d1*d2)

  let prod f1 f2 = match f1, f2 with
    |(n1, d1), (n2, d2) -> (n1*n2, d1*d2)

  let div f1 f2 = match f1, f2 with
    |(n1, d1), (n2, d2) -> (n1*d2, d1*n2)

  let abs f1 = match f1 with
    |(n1, d1) -> (abs n1, abs d1)

  let lt f1 f2 = match (minus f1 f2) with
    |(n1, d1) -> if ((n1>0 && d1<0) || (n1<0 && d1>0)) then true else false

  let le f1 f2 = match (minus f1 f2) with
    |(n1, d1) -> if ((n1>0 && d1<0) || (n1<0 && d1>0) || (n1 = 0)) then true else false

  let gt f1 f2 = match (minus f1 f2) with
    |(n1, d1) -> if ((n1>0 && d1>0) || (n1<0 && d1<0)) then true else false 

  let ge f1 f2 = match (minus f1 f2) with
    |(n1, d1) -> if ((n1>0 && d1>0) || (n1<0 && d1<0) || (n1 = 0)) then true else false

  let eq f1 f2 = match (minus f1 f2) with
    |(n1, d1) -> if (n1 = 0) then true else false

  let to_string x = match x with
    |(n1, d1) -> (string_of_int n1) ^ "/" ^ (string_of_int d1)
end
  

module type NewtonSolver =
  sig
    type t

    val square_root : t -> t
  end

(* Q2.2: Implement a function that approximates the square root using  the Newton-Raphson method *)

module Newton (A : Arith) : (NewtonSolver with type t = A.t) = 
  struct 
    type t = A.t

    let square_root (num : t) = 
      let rec findroot (root : t) (acc : t) : t = 
        (*print_string (A.to_string root ^ "\n");*)
        if (A.lt (A.abs(A.minus num (A.prod root root))) acc) then root
        else let newRoot : t = A.div (A.plus (A.div num root) root) (A.from_fraction(2,1))
             in findroot newRoot acc
    in (findroot (A.from_fraction(1,1)) A.epsilon) (* Arbitrary starting point @ 1*)

  end 

(* Examples *)

module FloatNewton = Newton (FloatArith) 
module RationalNewton = Newton (FractionArith) 

let sqrt2 = FloatNewton.square_root (FloatArith.from_fraction (2, 1));;
print_string(FloatArith.to_string sqrt2 ^ "\n");;
let sqrt2_r = RationalNewton.square_root (FractionArith.from_fraction (2, 1));;
print_string(FractionArith.to_string sqrt2_r ^ "\n");;

(*let sqrt2_r2 = reduceFraction sqrt2_r*)



(* Q3 : Real Real Numbers, for Real! *)

type 'a stream = { head : 'a  ; tail : unit -> 'a stream}

let rec nth z = function
  | 0 -> z.head
  | n -> nth (z.tail()) (n - 1)

let rec constant x = {head = x ; tail = fun () -> constant x }

(* Some examples *)

let sqrt2 =
  {head = 1 ; tail = fun () -> constant 2} (* 1,2,2,2,2... *)

let golden_ratio = constant 1   (* 1,1,1,1,1,1 *)

let rec take n z =
  if n = 1 then [z.head]
  else z.head::(take (n-1) (z.tail()))

(* Q3.1: implement the function q as explained in the pdf *)
let rec q z n = assert false

(* Q3.2: implement the function r as in the notes *)
let rec r z n = assert false

(* Q3.3: implement the error function *)
let error z n = assert false

(* Q3.4: implement a function that computes a rational approximation of a real number *)
let rat_of_real z approx = assert false

let real_of_int n = { head = n ; tail = fun () -> constant 0}

(* Q3.5: implement a function that computes the real representation of a rational number   *)
let rec real_of_rat r = assert false


(* Examples *)

(* Approximations of the  irrational numbers we have *)

(* let sqrt_2_rat = rat_of_real sqrt2 1.e-5 *)
(* let golden_ratio_rat = rat_of_real golden_ratio 1.e-5 *)

(* To test the representation of rationals we can try this *)
(* let to_real_and_back n = rat_of_real (real_of_rat n) 0.0001 *)

(* e1 should be very close to 10 (it is exactly 10 in the model solution) *)
(* let e1 = to_real_and_back 10.0 *)

(* this is the float approximation of pi, not the real number pi *)
(* let not_pi = 2. *. acos 0. *)

(* This should share the same 4 decimals with not_pi *)
(* let not_pi' = to_real_and_back not_pi *)
