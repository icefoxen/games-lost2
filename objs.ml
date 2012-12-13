(* Outstanding issues:
   Structure vs. union types for each object type
   Getting stuff working asap vs. making things complete
   Gun locations
   Force fields
   Complex enemies with turrets
   Load properties from config files?
*)

open Vector2d
open Drawing

open Physics

exception TerribleError of string

(* Basic object properties ***********************************************)
let bigRockKind = {
    mass = 5000.;
    moment = 5000.;
    rad = 50.;
    drag = 1.0;
    rdrag = 1.0;
  };;

let smallRockKind = {
    mass = 2000.;
    moment = 2000.;
    rad = 10.;
    drag = 1.0;
    rdrag = 1.0;
  };;

let bigRockHits = 100;;
let smallRockHits = 30;;

(* Complex objects ***********************************************)
(* Simple enemies... ie, ones that don't have turrets. *)
type etype =
    Scout
    | Swarmer 
    | Fighter
    | Gunboat
    | Feral
    | Berserker
    | Spider
;;

(* XXX: How do we represent weapon firing points? ie, where the shot actually
   spawns.
*)
type enemy = {
    hits : int;
    thrust : float;
    rthrust : float;
    refire : int;
    ekind : etype;
  };;

let grenadeLauncher = 1 lsl 0
let pulsar = 1 lsl 1

let rocketPod = 1 lsl 2
let phantomHammer = 1 lsl 3
let pressorBeam = 1 lsl 4

let deflector = 1 lsl 5
let stealth = 1 lsl 6
let autoRepair = 1 lsl 7

let barrier = 1 lsl 8
let afterburner = 1 lsl 9
let hyperspace = 1 lsl 10

let armorbuff = 1 lsl 11
let thrustbuff = 1 lsl 12
let energybuff = 1 lsl 13
let regenbuff = 1 lsl 14
let rotationbuff = 1 lsl 15
let dragbuff = 1 lsl 16

(* These are physical projectile shots...  hitscan weapons are handled
   instantly and separately.
   Enemy weapons are denoted with an E at the front.
*)
type shot = 
    Vulcan
    | Grenade
    | Rocket
    | EShootgun
    | EVulcan
    | ERailgun
    | ERocket
    | EFireball
;;

(* OBJECT DEFINITIONS ***********************************************)
type obj = 
    (* hits, resources *)
    Rock of pobj * int * int

    (* Hits, vulnerability *)
    | Block of pobj * int * bool

    | Cloud of pobj

    (* Hits, force (positive = away, negative = towards) *)
    | Fan of pobj * int * float

    (* Hits, damage *)
    | EnergyBall of pobj * int * int

    (* Lifetime *)
    | Bubble of pobj * int

    (* Hits, refire *)
    | Turret of pobj * int * int

    (* Hits, refire *)
    | DeathTurret of pobj * int * int

    (* index, resources *)
    | Gate of pobj * int * int

    | Powerup of pobj * int

    | EBasicEnemy of pobj * enemy

    | Projectile of pobj * shot

    (* hits, refire delay *)
    | Player of pobj * int * int

(* XXX: Not sure how force field generators will work yet. *)
(*    | FieldGen of pobj *  *)
;;


(* METHODS ***********************************************)
(*
let _drawRock obj last dt =
  let loc = Globals.lerp last.rloc obj.rloc dt in
  let drawn = withMatrix 
    (translated loc 
	(colored (0.3,0.3,0.3) 
	    (scaled 0.05 (circle 6)))) in
    [(Globals.rockLayer, drawn)]
;;
*)


let calc obj g =
  match obj with
      Rock(o,hits,resources) -> (obj,g)
    | Block(o,hits,vulnerable) -> (obj,g)
    | Cloud(o) -> (obj,g)
    | Fan(o,hits,force) -> (obj,g)
    | EnergyBall(o,hits,damage) -> (obj,g)
    | Bubble(o,time) -> (obj,g)
    | Turret(o,hits,refire) -> (obj,g)
    | DeathTurret(o,hits,refire) -> (obj,g)
    | Gate(o,index,resources) -> (obj,g)
    | Powerup(o,index) -> (obj,g)
    | EBasicEnemy(o,kind) -> (obj,g)
    | Projectile(o,shot) -> (obj,g)
    | Player(o,hits,refire) -> (obj,g)
;;

let draw obj last dt =
  match obj with
      Rock(o,hits,resources) -> []
    | Block(o,hits,vulnerable) -> []
    | Cloud(o) -> []
    | Fan(o,hits,force) -> []
    | EnergyBall(o,hits,damage) -> []
    | Bubble(o,time) -> []
    | Turret(o,hits,refire) -> []
    | DeathTurret(o,hits,refire) -> []
    | Gate(o,index,resources) -> []
    | Powerup(o,index) -> []
    | EBasicEnemy(o,kind) -> []
    | Projectile(o,shot) -> []
    | Player(o,hits,refire) -> []
;;


(*
(* All times are in milliseconds! *)
let shotTime = 250;;

let shotVel = 0.2;;

let newShot loc facing = {
    sloc = loc;
    svel = createDirMag facing shotVel;
    sfacing = facing;
    stime = shotTime;
  };;


type missile = {
  mloc : vector2d;
  mvel : vector2d;
  mfacing : float;
  mtime : int;
};;

let missileTime = 1000;;
let missileVel = 0.01;;
let missileAccel = 0.05;;
let missileDrag = 0.9;;

let newMissile loc facing = {
    mloc = loc;
    mvel = createDirMag facing missileVel;
    mfacing = facing;
    mtime = missileTime;
  };;

let missileThrust m =
  let ax = missileAccel *. (cos m.mfacing)
  and ay = missileAccel *. (sin m.mfacing) in
    {m with mvel = m.mvel ^+ (ax,ay)}
;;

type explosion = {
  eloc : vector2d;
    (* How long the explosion has left to live *)
  etime : int;
  esize : float;
};;

let newExplosion loc time = {
    eloc = loc;
    etime = time;
    esize = 0.;
  };;

(* How fast the explosion grows each tick (absolute scale) *)
let explosionGrowth = 0.015;;


type cloud = {
  cloc : vector2d;
  cradius : float;
};;

let cloudSize = 1.0;;

let cloudDrag = 0.7;;

let newCloud loc = {
    cloc = loc;
    cradius = cloudSize;
  };;

type gate = {
    gloc : vector2d;
    gfacing : float;
};;

let gateRotation = -0.01;;

let newGate loc = {
    gloc = loc;
    gfacing = 0.0;
  };;

type baddie = {
  bloc : vector2d;
  bvel : vector2d;
  bfacing : float;
  bdamage : int;
  brefire : int;
};;

type player = {
  ploc : vector2d;
  pvel : vector2d;
  pfacing : float;
  protVel : float;  
  (* The time left before the player can fire again. *)
  prefire : int;
  pmissiles : int;
};;

let playerDrag = 0.9;;
let playerAccel = 0.01;;
let playerRotationDrag = 0.8;;
let playerRotationAccel = 0.05;;

let playerRefire = 250;;

let playerRotate p dir =
  {p with protVel = p.protVel +. (playerRotationAccel *. dir)}
;;

let newPlayer () = {
    ploc = (0.,0.);
    pvel = (0.01,0.);
    (* This is on a mathematical axis, so increasing theta rotates
     * counterclockwise, and theta = 0 is facing along the x axis.  *)
    pfacing = 0.;
    protVel = 0.0;
    prefire = playerRefire;
    pmissiles = 200;
  };;



type obj =
    Rock of rock
  | Shot of shot
  | Missile of missile
  | Explosion of explosion
  | Cloud of cloud
  | Gate of gate
  | Baddie of baddie
  | Player of player
;;
*)

type gamestate = {
    (* Game meta-data *)
    frames : int;
    lastCalc : int;
    continue : bool;

    (* Okay.  This is the game time, which is what all calculations
       are based off of, and is only updated when the game is focussed
       and unpaused.
       It differs from lastCalc in that lastCalc is a real time, compared
       to real times to know how long it has been since the last frame
       for graphical interpolation.
       ...hmmm.  It might be simpler just to just... well, calc a frame
       immediately upon unpausing/refocussing, which will get everything
       back in sync.  Have to clear the input state and such as well
       though...
    *)

    (* Game resources *)
    input : gamestate Input.inputContext;
    res : Resources.resourcePool;
    colltree : obj Kdtree.t;

    (* Graphical stuff *)
    zoom : float;
    loc : vector2d;

    (* Game objects *)
    objs : (obj * obj) list;
    deadobjs : obj list;
    newobjs : (obj * obj) list;
    level : Level.level;


  };;


let newObj obj g = {g with newobjs = (obj,obj) :: g.newobjs };;
let alterObj old obj g = { g with newobjs = (obj,old) :: g.newobjs };;
let removeObj obj g = {g with deadobjs = obj :: g.deadobjs};;
let nextFrame g = {g with objs = g.newobjs; deadobjs = []; newobjs = []};;

(*
let playerRotateLeft p g = (playerRotate p 1.0, g);;
let playerRotateRight p g = (playerRotate p (-1.0), g);;

let playerThrust p g = 
  let ax = playerAccel *. (cos p.pfacing)
  and ay = playerAccel *. (sin p.pfacing) in
    ({p with pvel = p.pvel ^+ (ax,ay)}, g)
;;

let playerFire p g =
  if p.prefire > 0 then
    (p,g)
  else
    let offset = createDirMag p.pfacing 0.1 in
    let sloc = p.ploc ^+ offset in
    let shot = newShot sloc p.pfacing in
    let newp = {p with prefire = playerRefire} in
    let g = newObj (Shot( shot )) g in
      (newp, g)
;;
let playerFireMissile p g =
  if (p.prefire > 0) or (p.pmissiles < 1) then
    (p,g)
  else
    let offset = createDirMag p.pfacing 0.1 in
    let sloc = p.ploc ^+ offset in
    let missile = newMissile sloc p.pfacing in
    let newp = {
	p with 
	  prefire = playerRefire; 
	  pmissiles = p.pmissiles - 1
      } in
    let g = newObj (Missile( missile )) g in
      (newp, g)
;;


(* Ow, my poor keyboard callback system... *)
let ifKeyPressed player g key func =
  if Input.isKeyPressed g.input key then func player g 
  else (player,g)
;;
let ifKeysPressed player g keyfuncs =
  let func (p,g) (key,f) = ifKeyPressed p g key f in
    List.fold_left func (player,g) keyfuncs
;;
let playerHandleInput p g =
  let keyfuncs = [
      (Sdlkey.KEY_LEFT, playerRotateLeft);
      (Sdlkey.KEY_RIGHT, playerRotateRight);
      (Sdlkey.KEY_UP, playerThrust);
      (Sdlkey.KEY_c, playerFire);
      (Sdlkey.KEY_x, playerFireMissile);
    ] in
  ifKeysPressed p g keyfuncs
;;

(* COLLISION ***********************************************)
(* This is how far apart objects will be in our first-pass collision
   detection.  It should be large enough such that centers of the two largest 
   objects can fall within it and just barely not touch.
*)
let collisionRange = 1.0;;
let rockRadius = 0.05;;
let shotRadius = 0.02;;
let missileRadius = 0.02;;
let gateRadius = 0.4;;
let baddieRadius = 0.05;;
let playerRadius = 0.05;;

let nearbyObjs point g =
  Kdtree.getWithin g.colltree point collisionRange
;;

let doCollide loc obj g f =
  let near : obj list = nearbyObjs loc g in
  List.fold_left (fun (p,g) other -> f g p other) (obj,g) near
;;
    

let _collideRock obj g other = 
  match other with 
    Rock( r ) -> (obj,g)
  | Shot( s ) -> (obj,g)
  | Missile( m ) -> (obj,g)
  | Explosion( e ) -> (obj,g)
  | Cloud( c ) -> (obj,g)
  | Gate( g ) -> (obj,g)
  | Baddie( b ) -> (obj,g)
  | Player( p ) -> (* (obj,g)*)
        if Collide.collideCircle p.ploc playerRadius obj.rloc rockRadius then
	let obj = {obj with rvel = invert obj.rvel} in
	  (obj,g)
      else
         (obj,g)
;;
let _collideShot obj g other =
  match other with 
    Rock( r ) -> (obj,g)
  | Shot( s ) -> (obj,g)
  | Missile( m ) -> (obj,g)
  | Explosion( e ) -> (obj,g)
  | Cloud( c ) -> (obj,g)
  | Gate( g ) -> (obj,g)
  | Baddie( b ) -> (obj,g)
  | Player( p ) -> (obj,g)
;;
let _collideMissile obj g other =
  match other with 
    Rock( r ) -> (obj,g)
  | Shot( s ) -> (obj,g)
  | Missile( m ) -> (obj,g)
  | Explosion( e ) -> (obj,g)
  | Cloud( c ) -> (obj,g)
  | Gate( g ) -> (obj,g)
  | Baddie( b ) -> (obj,g)
  | Player( p ) -> (obj,g)
;;
let _collideExplosion obj g other = 
  match other with 
    Rock( r ) -> (obj,g)
  | Shot( s ) -> (obj,g)
  | Missile( m ) -> (obj,g)
  | Explosion( e ) -> (obj,g)
  | Cloud( c ) -> (obj,g)
  | Gate( g ) -> (obj,g)
  | Baddie( b ) -> (obj,g)
  | Player( p ) -> (obj,g)
;;
let _collideCloud obj g other = 
  match other with 
    Rock( r ) -> (obj,g)
  | Shot( s ) -> (obj,g)
  | Missile( m ) -> (obj,g)
  | Explosion( e ) -> (obj,g)
  | Cloud( c ) -> (obj,g)
  | Gate( g ) -> (obj,g)
  | Baddie( b ) -> (obj,g)
  | Player( p ) -> (obj,g)
;;
let _collideGate obj g other = 
  match other with 
    Rock( r ) -> (obj,g)
  | Shot( s ) -> (obj,g)
  | Missile( m ) -> (obj,g)
  | Explosion( e ) -> (obj,g)
  | Cloud( c ) -> (obj,g)
  | Gate( g ) -> (obj,g)
  | Baddie( b ) -> (obj,g)
  | Player( p ) -> (obj,g)
;;
let _collideBaddie obj g other = 
  match other with 
    Rock( r ) -> (obj,g)
  | Shot( s ) -> (obj,g)
  | Missile( m ) -> (obj,g)
  | Explosion( e ) -> (obj,g)
  | Cloud( c ) -> (obj,g)
  | Gate( g ) -> (obj,g)
  | Baddie( b ) -> (obj,g)
  | Player( p ) -> (obj,g)
;;
let _collidePlayer obj g other =
  match other with 
    Rock( r ) -> 
      if Collide.collideCircle obj.ploc playerRadius r.rloc rockRadius then
	let obj = {obj with pvel = invert obj.pvel} in
	  (obj,g)
      else
         (obj,g)
  | Shot( s ) -> (obj,g)
  | Missile( m ) -> (obj,g)
  | Explosion( e ) -> (obj,g)
  | Cloud( c ) ->
      let playerRadius = 0.05 in
      let p = 
	if Collide.collideCircle obj.ploc playerRadius c.cloc (cloudSize /. 2.) then
	  {obj with pvel = obj.pvel ^* cloudDrag}
	else obj
      in
      (p,g)
  | Gate( ga ) -> (obj,g)
  | Baddie( b ) -> (obj,g)
  | Player( p ) -> (obj,g)
;;


(* DRAWING *************************************************)
let _drawRock obj last dt =
  let loc = Globals.lerp last.rloc obj.rloc dt in
  let drawn = withMatrix 
    (translated loc 
	(colored (0.3,0.3,0.3) 
	    (scaled 0.05 (circle 6)))) in
    [(Globals.rockLayer, drawn)]
;;
let _drawShot obj last dt =
  let loc = Globals.lerp last.sloc obj.sloc dt in
  let v = createDirMag obj.sfacing 1. in
  let v1 = loc ^- (v ^* 0.03)
  and v2 = loc ^- (v ^* 0.06)
  and v3 = loc ^- (v ^* 0.09) in
  let shot = [
      withMatrix (colored (0.8, 0.0, 0.0)
		     (affined ~t: loc ~r: obj.sfacing ~s: 0.020 triangle));
      withMatrix (colored (0.6, 0.0, 0.0)
		     (affined ~t: v1 ~r: obj.sfacing ~s: 0.018 triangle));
      withMatrix (colored (0.4, 0.0, 0.0)
		     (affined ~t: v2 ~r: obj.sfacing ~s: 0.016 triangle));
      withMatrix (colored (0.2, 0.0, 0.0)
		     (affined ~t: v3 ~r: obj.sfacing ~s: 0.014 triangle));
    ]
  in
    addListToScene emptyScene Globals.weaponLayer shot
;;



let _drawMissile obj last dt = 
  let loc = Globals.lerp last.mloc obj.mloc dt in
  let s = slice 8 260.0 20. in
  let missile = [
      withMatrix (colored (0.7, 0.7, 0.7)
		     (affined ~t: loc ~r: obj.mfacing ~s: 0.1 s))
    ]
  in
    addListToScene emptyScene Globals.weaponLayer missile
;;

let _drawExplosion obj last dt =
  let loc = Globals.lerp last.eloc obj.eloc dt in
  let size = Globals.lerpFloat last.esize obj.esize dt in
  let r = ring 16 0.4 in
  let exp = [
      withMatrix (colored (1.0, 0.0, 0.0)
		     (affined ~t: loc ~s: size r))
    ]
  in
    addListToScene emptyScene Globals.explosionLayer exp
;;
let _drawCloud obj last dt =
  let loc = Globals.lerp last.cloc obj.cloc dt in
  let c = circle 24 in
  let cloud = [
      withMatrix 
	(colored (0.01, 0.01, 0.01)
	    (affined ~t: loc ~s: obj.cradius c))
    ]
  in
    addListToScene emptyScene Globals.cloudLayer cloud
;;
let _drawGate obj last dt = 
  let loc = Globals.lerp last.gloc obj.gloc dt in
  let rot = Globals.lerpFloat last.gfacing obj.gfacing dt in
  let r = ring 6 0.4 in
  let gate = [
      withMatrix (colored (1.0, 1.0, 0.0)
		     (affined ~t: loc ~r: rot r))
    ]
  in
    addListToScene emptyScene Globals.gateLayer gate
;;
let _drawBaddie obj last dt = emptyScene;;
let _drawPlayer obj last dt = 
  let verts = [
      (0.0, 0.0);
      (-0.02, 0.02);
      (0.04, 0.0);
      (-0.02, -0.02)
    ]
  and loc = Globals.lerp last.ploc obj.ploc dt in
  let drawn = 
    withMatrix (translated loc
 		   (rotated obj.pfacing
		       (colored (1.0,0.0,0.0) 
			   withPrim `quads (fun () -> vertices verts)))) in
    [(Globals.playerLayer, drawn)]
;;


(* CALCING *************************************************)
(* Okay.  To keep things sane, we need two invariants on calculation:
   One, objects never alter anything, only define new things.
   Two, objects never change other objects, only themselves.  Thus, if
   a missile hits a ship, it is the missile's responsibility to go "boom",
   and the ship's responsibility to go "ow".
   Each calc function takes an object and the game state, and modifies
   the game state to have one or more new objects in its 'newobjs' list.
   Any objects that are destroyed are not put on the 'newobjs' list, but
   instead on the 'deadobjs' list.  This is so that the mainloop can go 
   through and call the appropriate die methods for each; thus, you do
   not have to call the die methods yourself.
   It seems that these invariants result in the rule that only the calc
   function ever alters an object; an object cannot be altered more than
   once.  So if anything ever happens, it has to happen in the calc function.
   Hmmmm.
*)
let _calcRock obj g = 
  let n = Rock( {obj with rloc = obj.rloc ^+ obj.rvel} ) in
    alterObj (Rock(obj)) n g
;;
let _calcShot obj g =
  let s = { 
      obj with
	sloc = obj.sloc ^+ obj.svel;
	stime = obj.stime - Globals.calcInterval;
    } in
    if s.stime < 1 then
      removeObj (Shot( obj )) g
    else
      alterObj (Shot(obj)) (Shot( s )) g
;;

let _calcMissile obj g =
  let t = obj.mtime - Globals.calcInterval in
    if t < 1 then
      removeObj (Missile( obj )) g
    else
      let m = missileThrust obj in
      let rot = (Random.float 0.5) -. 0.25 in
      let n = Missile( {
	  m with
	    mloc = m.mloc ^+ m.mvel;
	    mvel = m.mvel ^* missileDrag;
	    mtime = t;
	    mfacing = m.mfacing +. rot;
	} ) in
	alterObj (Missile( obj )) n g
;;
      
let _calcExplosion obj g =
  let t = obj.etime - Globals.calcInterval in
    if t < 1 then
      removeObj (Explosion( obj )) g
    else
      let e = Explosion( {
	  obj with
	    etime = t;
	    esize = obj.esize +. explosionGrowth;
	} ) in
	alterObj (Explosion( obj)) e g
;;
let _calcCloud obj g = newObj (Cloud( obj )) g;;
let _calcGate obj g =
  let ga = Gate( {
      obj with
	gfacing = obj.gfacing +. gateRotation;
    }) in
    alterObj (Gate( obj )) ga g
;;
let _calcBaddie obj g = g;;

let _calcPlayer obj g =
  let (p,g) = playerHandleInput obj g in
  let near : obj list = nearbyObjs p.ploc g in
  let (p,g) =  List.fold_left 
  (fun (p,g) other -> _collidePlayer p g other) (p,g) near
  in
  let n = { 
      p with
	ploc = p.ploc ^+ p.pvel;
	pvel = p.pvel ^* playerDrag;
	pfacing = p.pfacing +. p.protVel;
	protVel = p.protVel *. playerRotationDrag;
	prefire = max 0 (p.prefire - Globals.calcInterval);
    } in
    alterObj (Player( obj )) (Player( n )) g
;;

(* DYING ***************************************************)

let _dieRock obj g = g;;
let _dieShot obj g = 
  let e = Explosion( newExplosion obj.sloc 500 ) in
    newObj e g
;;
let _dieMissile obj g =
  let e = Explosion( newExplosion obj.mloc 1000 ) in
    newObj e g
;;
let _dieExplosion obj g = g;;
let _dieCloud obj g = g;;
let _dieGate obj g = g;;
let _dieBaddie obj g = g;;
let _diePlayer obj g = g;;


(* LOCATION ************************************************)
let _locRock obj = obj.rloc;;
let _locShot obj = obj.sloc;;
let _locMissile obj = obj.mloc;;
let _locExplosion obj = obj.eloc;;
let _locCloud obj = obj.cloc;;
let _locGate obj = obj.gloc;;
let _locBaddie obj = obj.bloc;;
let _locPlayer obj = obj.ploc;;

(* METHODS *************************************************)
let draw (cur:obj) (prev:obj) (dt:int) = 
  match cur,prev with
      Rock( r ),Rock(o) -> _drawRock r o dt
    | Shot( s ),Shot(o) -> _drawShot s o dt
    | Missile( m ),Missile(o) -> _drawMissile m o dt
    | Explosion( e ),Explosion(o) -> _drawExplosion e o dt
    | Cloud( c ),Cloud(o) -> _drawCloud c o dt
    | Gate( g ),Gate(o) -> _drawGate g o dt
    | Baddie( b ),Baddie(o) -> _drawBaddie b o dt
    | Player( p ),Player(o) -> _drawPlayer p o dt
    | _ -> raise (TerribleError "Objs.draw")
;;

let calc (obj : obj) (g : gamestate) : gamestate =
  match obj with
    Rock( r ) -> _calcRock r g
  | Shot( s ) -> _calcShot s g
  | Missile( m ) -> _calcMissile m g
  | Explosion( e ) -> _calcExplosion e g
  | Cloud( c ) -> _calcCloud c g
  | Gate( ga ) -> _calcGate ga g
  | Baddie( b ) -> _calcBaddie b g
  | Player( p ) -> _calcPlayer p g
;;

let die obj g =
  match obj with
    Rock( r ) -> _dieRock r g
  | Shot( s ) -> _dieShot s g
  | Missile( m ) -> _dieMissile m g
  | Explosion( e ) -> _dieExplosion e g
  | Cloud( c ) -> _dieCloud c g
  | Gate( ga ) -> _dieGate ga g
  | Baddie( b ) -> _dieBaddie b g
  | Player( p ) -> _diePlayer p g
;;

let loc = function
    Rock( r ) -> _locRock r
  | Shot( s ) -> _locShot s
  | Missile( m ) -> _locMissile m
  | Explosion( e ) -> _locExplosion e
  | Cloud( c ) -> _locCloud c
  | Gate( g ) -> _locGate g
  | Baddie( b ) -> _locBaddie b
  | Player( p ) -> _locPlayer p
;;

(* Could make this multivariate. *)
(*
let collide obj1 obj2 =
  match obj1 with
    Rock( r ) -> Rock( _collideRock r obj2 )
  | Shot( s ) -> Shot( _collideShot s obj2 )
  | Missile( m ) -> Missile( _collideMissile m obj2 )
  | Explosion( e ) -> Explosion( _collideExplosion e obj2 )
  | Cloud( c ) -> Cloud( _collideCloud c obj2 )
  | Gate( g ) -> Gate( _collideGate g obj2 )
  | Baddie( b ) -> Baddie( _collideBaddie b obj2 )
  | Player( p ) -> Player( _collidePlayer p obj2 )
;;
*)

let getPlayer g =
  let rec loop = function
      [] -> raise (TerribleError "Objs.getPlayer")
    | (Player(nw),Player(old)) :: _ -> (nw,old)
    | _ :: tl -> loop tl
  in
    loop g.objs
;;


(* MISC ****************************************************)
let randomVec x y =
  let xr = (Random.float (x *. 2.)) -. x
  and yr = (Random.float (y *. 2.)) -. y in
    (xr, yr)
;;

let randomRock () =
  Rock( newRock (randomVec 5.0 5.0) (randomVec 0.01 0.01) )
;;
let bunchOf n item =
  let rec loop n accm =
    if n = 0 then accm
    else let i = item () in loop (n - 1) ((i, i) :: accm)
  in
    loop n []
;;

*)
