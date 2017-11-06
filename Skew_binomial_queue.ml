module type Ordered = sig
  type t 
  val leq : t -> t -> bool
end

module Make(X : Ordered) = struct
  
  (* t' : tree, t = t' list : queue *) 
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

  let skew_link (Node (_, x0, _) as t0) (Node (r1, x1, sub1) as t1) (Node (r2, x2 ,sub2) as t2) = 
    if X.leq x1 x0 && X.leq x1 x2 then Node (r1 + 1, x1, t0 :: t2 :: sub1)
    else if X.leq x2 x0 && X.leq x2 x1 then Node (r2 + 1, x2, t0 :: t1 :: sub2)
    else Node (r1 + 1, x0, [t1; t2])

  let rec insert_tree t = function
    | [] -> [t]
    | t' :: ts' -> if rank t < rank t' then t :: t' :: ts'
                   else insert_tree (link t t') ts' (* rank t <= rank t' *)
  
  let uniqify = function 
    | [] -> []
    | t :: ts -> insert_tree t ts (* eliminate initial duplicate *)
  let insert x = function
    | t1 :: t2 :: ts' as ts ->
         if rank t1 = rank t2 then (skew_link (Node (0, x, [])) t1 t2) :: ts'
         else Node (0, x, []) :: ts
    | ts -> Node (0, x, []) :: ts

  let rec merge_uniq ts1 ts2 = match (ts1, ts2) with
    | [], _ -> ts2
    | _, [] -> ts1
    | (t1 :: ts1', t2 :: ts2') -> 
        let r1 = rank t1 and r2 = rank t2 in
        if r1 < r2 then t1 :: merge_uniq ts1' ts2
        else if r2 < r1 then t2 :: merge_uniq ts1 ts2'
        else insert_tree (link t1 t2) (merge_uniq ts1' ts2')

  let merge ts1 ts2 = merge_uniq (uniqify ts1) (uniqify ts2)

  let rec find_min = function
    | [] -> raise Empty
    | [t] -> root t
    | t :: ts' -> 
        let x = find_min ts' in 
        if X.leq (root t) x then root t else x

  let rec delete_min = function
    | [] -> raise Empty
    | ts -> let rec get_min = function 
              | [] -> raise Empty
              | [t] -> t, []
              | t :: ts' -> let t', ts'' = get_min ts' in
                            if X.leq (root t) (root t') then t, ts'
                            else t', t :: ts''
            in
            let rec split = function
              | [] -> [], []
              | t :: ts -> let l1, l2 = split ts in
                           if rank t = 0 then t :: l1, l2
                           else l1, t :: l2
            in
            let t, ts' = get_min ts in
            let xs', ts'' = split (subtrees t) in
            List.fold_right insert_tree xs' (merge ts'' ts') 

end 
                  
              
