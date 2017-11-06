module type Ordered = sig
  type t 
  val leq : t -> t -> bool
end

module type Priority_queue = 
  functor (X : Ordered) -> sig 
    type t  (* type of the priority queue *)
    exception Empty
    val empty : t 
    val is_empty : t -> bool
    val insert : X.t -> t -> t
    val merge : t -> t -> t
    val find_min : t -> X.t  (* raises Empty if the queue is empty *)
    val delete_min : t -> t  (* raises Empty if the queue is empty *)
 end 

module Add_root (Q : Priority_queue) (X : Ordered) = struct
  
  module R = Q(X)

  type t = Emp | Root of X.t * R.t
    
  exception Empty

  let empty = Emp

  let is_empty = function
    | Emp -> true
    | Root _ -> false
 
  let insert x = function
    | Emp -> Root (x, R.empty)
    | Root (y, q) when X.leq x y -> Root (x, (R.insert) y q)
    | Root (y, q) -> Root (y, (R.insert) x q)

  let merge t1 t2 = match (t1, t2) with
    | Emp, _ -> t2
    | _, Emp -> t1
    | Root (x1, q1), Root(x2, q2) -> 
        if X.leq x1 x2 then Root(x1, (R.insert) x2 ((R.merge) q1 q2))
        else Root(x2, (R.insert) x1 ((R.merge) q1 q2))

  let find_min = function
    | Emp -> raise Empty
    | Root (x, _) -> x

  let delete_min = function 
    | Emp -> raise Empty
    | Root (x, q) -> 
        if (R.is_empty) q then Emp
        else Root ((R.find_min) q, (R.delete_min) q)

end
