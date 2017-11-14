(* Q1: A Rose by any Other Name Would Smell as Sweet *)

type 'a rose_tree = Node of 'a * ('a rose_tree) list

(* Find with exceptions *)

exception BackTrack
exception NotFound

let match_leaf (a : 'a)(b : 'a) : bool = (a=b);;

(* Q1.1 write a function that finds an element of the tree using backtracking with exceptions *)
let rec find_e (p : 'a -> bool) (t : 'a rose_tree) : 'a = match t with 
  |Node(leaf, []) -> if p leaf then leaf else raise BackTrack 
  |Node(leaf, h::t) -> if p leaf then leaf else try find_e p h with BackTrack -> find_e p (Node(leaf, t))

(* Q1.1: write this function and it helper functions *)
let find (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = match t with
  |Node(leaf, []) -> if p leaf then Some leaf else raise NotFound
  |Node(leaf, h1::t) -> if p leaf then Some leaf else try Some (find_e p h1) with BackTrack -> Some (find_e p (Node(leaf, t)))

(* Q1.2 Find with failure continuations *)
let rec find_k (p : 'a -> bool) (t : 'a rose_tree) (k : unit -> 'a option) : 'a option = assert false

(* Q1.2: write this function and it helper functions *)
let find' (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = assert false (*  call find_k with the appropriate inital continuation *)

(* Find all with continuations *)

let rec find_all_k  (p : 'a -> bool) (t : 'a rose_tree) (k : 'a list -> 'b) : 'b =
  assert false

(* Q1.3: write this function and it helper functions *)
let find_all p t = assert false

(* An example to use *)

let example = Node (7, [ Node (1, [Node(100, [])])
                         ; Node (2, [Node (16, [Node(71, [])])])
                         ; Node (4, [])
                         ; Node (9, [])
                         ; Node (11, [Node(43, [])])
                         ; Node (15, [])
                         ])

let is_big x =  x > 10

let Some x = find (match_leaf 0) example;;
print_int x;

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
  let epsilon = epsilon_float
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

(* module FractionArith : Arith = *)
(* struct *)
(*   ... *)
(* end *)

module type NewtonSolver =
  sig
    type t

    val square_root : t -> t
  end

(* Q2.2: Implement a function that approximates the square root using  the Newton-Raphson method *)

(* module Newton (A : Arith) : (NewtonSolver with type t = A.t) = *)
(*   struct *)
(*     ... *)
(*   end *)

(* Examples *)

(* module FloatNewton = Newton (FloatArith) *)
(* module RationalNewton = Newton (FractionArith) *)

(* let sqrt2 = FloatNewton.square_root (FloatArith.from_fraction (2, 1)) *)
(* let sqrt2_r = RationalNewton.square_root (FractionArith.from_fraction (2, 1)) *)

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
