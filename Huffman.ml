(* Implementation of Huffman coding in OCaml *)
type code_tree = Leaf of char * int 
               | Node of char list * code_tree * code_tree * int

let get_freq = function
  | Leaf (_, f) -> f
  | Node (_, _, _, f) -> f

let get_chs = function 
  | Leaf (c, _) -> [c]
  | Node (clst, _, _ ,_) -> clst
 
let fusion t1 t2 = 
  Node ((get_chs t1 @ get_chs t2), t1, t2, (get_freq t1 + get_freq t2))

let init_leaf c f = Leaf (c, f)

(* Encode a single character *)
let encode_ch tree ch = 
  let rec encode_subtree = function
      | Leaf _ -> []
      | Node (_, ln, _, _) when List.mem ch (get_chs ln) -> 
          0 :: encode_subtree ln
      | Node (_, _, rn, _) -> 
          1 :: encode_subtree rn
  in encode_subtree tree

(* Encode a list of characters *)
let encode_chs tree chs = 
  List.fold_left (fun res ch -> res @ (encode_ch tree ch)) [] chs

(* Decode a list of characters *)  
let decode tree chs = 
  let rec decode_node node = function
    | [] -> []
    | curr :: next -> 
        let next_node = match (curr, node) with
          | (0, Node (_, ln, _, _)) -> ln
          | (1, Node (_, _, rn, _)) -> rn
          | _ -> failwith "Invalid code"
        in
        match next_node with 
          | Leaf (c, _) -> c :: (decode_node tree next)
          | _ -> decode_node next_node next
  in 
  decode_node tree chs

let leq tree1 tree2 =
  get_freq tree1 < (get_freq tree2)

(* Insert a tree into a sorted list of trees and keep the list sorted *)
let rec insert tree = function
  | [] -> [tree]
  | (tree' :: _) as tree_list when leq tree tree' -> tree :: tree_list
  | (tree' :: tree_list') -> tree' :: (insert tree tree_list')
   
let fusion_t1_t2 = function 
  | [] -> failwith "No trees in the list"
  | [tree] -> failwith "Only one tree in the list"
  | tree1 :: tree2 :: tree_list' -> 
    insert (fusion tree1 tree2) tree_list'

(* Initialize the tree list *)
let init_tree_list freq_list = 
  List.map (fun (c, f) -> init_leaf c f) freq_list
  
let create_huffman_tree freq_list = 
  let tree_list = init_tree_list freq_list in
  let rec huffman_from_tree_list = function
    | [] -> failwith "Empty list"
    | [tree] -> tree
    | _ as ts -> huffman_from_tree_list (fusion_t1_t2 ts)
  in
  huffman_from_tree_list tree_list  

(* Create a huffman code table *)
let huffman_coding freq_list = 
  let huffman_tree = create_huffman_tree freq_list in
  List.map (fun (ch, _) -> (ch, encode_ch huffman_tree ch)) freq_list
