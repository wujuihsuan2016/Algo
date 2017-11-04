module type Ordered = sig
  type t 
  val leq : t -> t -> bool
end

module Make(X : Ordered): sig
  
  (* Type of Binomial Heaps : t list *)
  type t
  exception Empty
  val empty : t list
  val is_empty : t list -> bool
  val rank : t -> int
  val root : t -> X.t
  val subtrees : t -> t list
  val link : t -> t -> t
  val insert_tree : t -> t list -> t list 
  val insert : X.t -> t list -> t list
  val merge : t list -> t list -> t list
  val find_min : t list -> X.t
  val delete_min : t list -> t list

end
