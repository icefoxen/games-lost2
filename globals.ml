
open Vector2d

(* interval = 50 milliseconds, physics goes at 20 FPS 
 * Can always be tweaked.  *)
let calcInterval = 50
let calcIntervalf = 50.0


(* Linear interpolation.  It's your buddy! *)
let lerp oldVec newVec dt =
   let difference = newVec ^- oldVec in
   oldVec ^+ (difference ^* ((float_of_int dt) /. calcIntervalf))
;;

let lerpFloat oldfloat newfloat dt =
  let difference = newfloat -. oldfloat in
    oldfloat +. (difference *. ((float_of_int dt) /. calcIntervalf))
;;

let backgroundLayer = -1000
and weaponLayer = -100
and gateLayer = -10
and playerLayer = 0
and rockLayer = 60
and cloudLayer = 80
and explosionLayer = 90
and uiLayer = 100
