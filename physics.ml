open Vector2d;;

(* Physics object... aka, nearly everything
   We can make immovable objects by giving them mass=infinity
*)
type ptype = {
    mass : float;
    moment : float;
    rad : float; (* bounding circle radius *)
    drag : float;
    rdrag : float;
  }

type pobj = {
    kind : ptype;
    vel : vector2d;
    rvel : float;
    loc : vector2d;
    facing : float;
  }

let push obj forcev =
  let accel = forcev ^/ obj.kind.mass in
    {obj with vel = obj.vel ^+ accel}
;;

let thrust obj force =
  let accel = (createDirMag obj.facing force) ^/ obj.kind.mass in
    {obj with vel = obj.vel ^+ accel}
;;

let rotate obj force =
  let accel = force /. obj.kind.moment in
    {obj with rvel = obj.rvel +. accel}
;;

let pcalc obj =
  {obj with
    vel = obj.vel ^* obj.kind.drag;
    rvel = obj.rvel *. obj.kind.rdrag;
    loc = obj.loc ^+ obj.vel;
    facing = obj.facing +. obj.rvel;
  }
;;


let initCollision objs locfunc =
  Kdtree.build Kdtree.xdim locfunc objs
;;

let getNearby col obj distance =
  Kdtree.getWithin col obj.loc distance
;;

(* For now we do simple bounding-circle collision *)
let colliding obj1 obj2 =
  let distance = magnitude (obj1.loc ^- obj2.loc) in
    distance < (obj1.kind.rad +. obj2.kind.rad)
;;
