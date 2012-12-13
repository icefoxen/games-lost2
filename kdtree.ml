(* a 2-dimensional Kd-tree. *)

open Vector2d;;

type 'a t =
   Node of vector2d * int * 'a t * 'a t * 'a
 | Leaf 
;;

let xdim = 0;;
let ydim = 1;;

let dimension locfunc item splt =
  let x,y = locfunc item in
  match splt with
      0 -> x
    | 1 -> y
    | _ -> raise (Failure "Kdtree.dimension")
;;

    

let splitAndRemove locfunc item items splt =
  let (xi,yi) = locfunc item in
  let rec loop left right = function
      [] -> (left, right)
    | hd :: tl ->
	if hd = item then loop left right tl
	else
	  let (x,y) = locfunc hd in
	    if splt = xdim then
	      if x < xi then loop (hd :: left) right tl
	      else loop left (hd :: right) tl
	    else
	      if y < yi then loop (hd :: left) right tl
	      else loop left (hd :: right) tl
  in
    loop [] [] items
;;

(* Extremely primitive pivot choosing at the moment. *)
let pivot locfunc dim items =
  match items with
      [] -> raise (Failure "Kdtree.pivot")
    | hd :: tl ->
	let splt = if dim = xdim then ydim else xdim in
	  (hd,splt)
;;

(* This takes a function that turns an item into the appropriate vector.
   Holy cats, it also works.  However, it is not tail recursive, so cannot
   build really BIG trees without stack overflowing.  Works fine for 100,000
   items, blows up on a million.  Oh well!
*)
let rec build dim locfunc items =
  if items = [] then
    Leaf
  else
    let item, splt = pivot locfunc dim items in
    let left, right = splitAndRemove locfunc item items splt in
    let kdleft = build dim locfunc left
    and kdright = build dim locfunc right in
      Node( locfunc item, splt, kdleft, kdright, item )
;;

let rec get kdtree loc =
  match kdtree with
      Leaf -> raise Not_found
    | Node( nloc, ndim, left, right, item ) ->
	if loc = nloc then
	  item
	else (
	  Printf.printf "Recursing...\n";
	  let x,y = loc
	  and nx, ny = nloc in
	    if ndim = xdim then
	      if x < nx then get left loc
	      else get right loc
	    else
	      if y < ny then get left loc
	      else get right loc
	)
;;

let vsquare (x,y) =
  (x*.x, y*.y)
;;

(*
let getNearest kdtree loc =
  let (x,y) = loc in
  let rec loop kd target hr maxdistsquared =
    match kd with
	Leaf -> (infinity, None)
      | Node( (nx,ny), ndim, left, right, item ) ->
	  let nearer,further =
	    if ndim = xdim then
	      if x < nx then (left,right)
	      else (right,left)
	    else
	      if y < ny then (left,right)
	    else (right,left)
	  in
	  let distsquared,nearest = loop nearer loc hr maxdistsquared in
	  let maxdistsquared = min distsquared maxdistsquared in
	  let dist =
	    if ndim = xdim then abs_float (x -. nx)
	    else abs_float (y -. ny)
	  in
	    if (dist *. dist) > maxdistsquared then
	      (distsquared,nearest)
	    else
	      let nearest = ...
	      and distsquared = vsquare (pivot ^- target) in
	      let tdist, tnearest = loop further loc hr distsquared in
 		if tdist < distsquared then
 		  (tdist, tnearest)
		else
		  (distsquared, nearest)
  in
    loop loc () infinity
;;
*)

let getWithin kdtree loc distance =
  let (x,y) = loc in
  let rec loop kd =
    match kd with
	Leaf -> []
      | Node( (nx,ny), ndim, left, right, item ) ->
	  (* Each node bounds an area.
	     If the bound on the given axis is within distance of the
	     location we are searching for... then the area the node
	     bounds is at least partially within that area.
	     If that is the case, then at least some of its subnodes
	     on both sides are within that area as well.
	  *)
	  let bound,search = if ndim = xdim then nx,x else ny,y in
	  let items =
	    if (abs_float (search -. bound)) < distance then 
	      (* both subtrees must be searched *)
	      let a = loop left
	      and b = loop right in
		a @ b
	    else (* only the nearer subtree must be searched, whichever it is *)
	      let nearer =
		if ndim = xdim then
		  if x < nx then left
		  else right
		else
		  if y < ny then left
		else right
	      in
		loop nearer
	  in
	    if within loc (nx,ny) distance then
	      item :: items
	    else
	      items
  in
    loop kdtree
;;
