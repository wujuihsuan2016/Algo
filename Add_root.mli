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

module Add_root (Q : Priority_queue) : Priority_queue  
