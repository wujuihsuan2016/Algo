module type Ordered = sig
  type t 
  val leq : t -> t -> bool
end

module Make(X : Ordered) = struct
  
  type t' = Node of int * X.t * t' list
  
  type t = t' list

  exception Empty

  let empty = []

  let is_empty ts = ts = []
  
  let rank (Node (r, _, _)) = r 
  
  let root (Node (_, x, _)) = x

  let subtrees (Node (_, _, sub)) = sub

  (* Merge two trees having the same rank *)
  let link (Node (r, x1, sub1) as t1) (Node (_, x2, sub2) as t2) = 
    if X.leq x1 x2 then Node (r + 1, x1, t2 :: sub1) 
    else Node (r + 1, x2, t1 :: sub2)
  
  let rec insert_tree t = function 
    | [] -> [t]
    | (t' :: ts') as ts -> 
      let r = rank t and r' = rank t' in
      if r < r' then t :: ts
      else if r = r' then insert_tree (link t t') ts'
           else t' :: (insert_tree t ts')

  let insert x ts = insert_tree (Node (0, x, [])) ts
  
  (* Merge two binomial heaps *)
  let rec merge ts1 ts2 = match (ts1, ts2) with
    | ([], _) -> ts2
    | (_, []) -> ts1
    | (t1 :: ts1', t2 :: ts2') -> 
      let r1 = rank t1 and r2 = rank t2 in
      if r1 < r2 then t1 :: (merge ts1' ts2)
      else if r1 = r2 then insert_tree (link t1 t2) (merge ts1' ts2')
           else t2 :: (merge ts1 ts2')
  
  let rec find_min = function
    | [] -> raise Empty
    | [t] -> root t
    | t :: ts' -> let r = root t and r' = find_min ts' in 
                  if X.leq r r' then r else r'

  let delete_min = function 
    | [] -> raise Empty
    | ts -> let rec get_min = function
              | [] -> raise Empty
              | [t] -> t, []
              | t :: ts' -> let t', ts'' = get_min ts' in
                            if X.leq (root t) (root t') then t, ts'
                            else t', t :: ts''
            in 
            let t, ts' = get_min ts in
            merge (List.rev (subtrees t)) ts'

end
