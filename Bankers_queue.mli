module Lzl : sig
  
  type 'a t 
  type +'a node_t
  val force : 'a t -> 'a node_t
  val app : 'a t -> 'a t -> 'a t
  val take : int -> 'a t -> 'a t
  val drop : int -> 'a t -> 'a t 
  val reverse : 'a t -> 'a t

end

module Bankers_queue : sig

  type 'a t
  exception Empty
  val empty : 'a t
  val is_empty : 'a t -> bool
  val queue : 'a t -> 'a t
  val snoc : 'a t -> 'a -> 'a t
  val head : 'a t -> 'a 
  val tail : 'a t -> 'a t

end
