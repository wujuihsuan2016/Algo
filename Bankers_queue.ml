(* Implementation of lazy list *)

module Lzl = struct

  type 'a t = Susp of 'a node_t
  and 'a node_t = Nil | Cons of 'a * 'a t

  let force = fun (Susp x) -> x
  
  let rec app xs ys = 
    Susp 
      (match xs with 
        | Susp Nil -> force ys
        | Susp (Cons (x, xs')) -> Cons (x, app xs' ys))
  
  let rec take n xs = 
    Susp
      (match (n,xs) with
        | 0, _ -> Nil
        | _, Susp Nil -> Nil
        | _, Susp (Cons (x, xs')) -> Cons (x, take (n-1) xs'))
  
  let drop n xs =
    let rec drop' m ys = match (m, ys) with
      | 0, _ -> force ys
      | _, Susp Nil -> Nil
      | _, Susp (Cons (_, ys')) -> drop' (m-1) ys'
    in Susp (drop' n xs)

  let reverse xs = 
    let rec reverse' ys yl = match ys with
      | Susp Nil -> yl
      | Susp (Cons (y, ys')) -> reverse' ys' (Cons (y, Susp yl))
    in Susp (reverse' xs Nil)

end

(* An implementation of queue using the banker's method and the lazy list *)
module Bankers_queue = struct
  
  (* A banker's queue Q = (f,lenf,r,lenr)            *)
  (* Invariants : lenf = |f|, lenr = |r|, |f| >= |r| *)              
  type 'a t = 'a Lzl.t * int * 'a Lzl.t * int
   
  exception Empty

  let empty = (Lzl.Susp Lzl.Nil, 0, Lzl.Susp Lzl.Nil, 0)

  let is_empty (_, lenf, _, _) = lenf = 0

  (* The function queue is used to maintain the invariant |f| >= |r| *)
  let queue ((f, lenf, r, lenr) as q) =
    if lenr <= lenf then q 
      else ((Lzl.app f (Lzl.reverse r)), (lenf + lenr), Lzl.Susp Lzl.Nil, 0)

  let snoc (f, lenf, r, lenr) x =
    queue (f, lenf, Lzl.Susp (Lzl.Cons (x, r)), lenr + 1)

  let head (f, lenf, r, lenr) = match f with
    | Lzl.Susp Lzl.Nil -> raise Empty
    | Lzl.Susp (Lzl.Cons (x ,f')) -> x

  let tail (f, lenf, r, lenr) = match f with
    | Lzl.Susp Lzl.Nil -> raise Empty
    | Lzl.Susp (Lzl.Cons (x, f')) -> queue (f', lenf - 1, r, lenr)

end
