module type Ordered = sig
  type t 
  val leq : t -> t -> bool
end

module Make(X : Ordered): sig
  type t
  exception Empty
  val empty : t
  val is_empty : t -> bool
  val insert : X.t -> t -> t
  val merge : t -> t -> t
  val find_min : t -> X.t
  val delete_min : t -> t
end
