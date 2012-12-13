open Vector2d
open Objs


let initInput res =
  let ic = Input.createContext () in
  let ic = Input.bindQuit ic (fun g -> {g with continue = false}) in
  let ic = Input.bindMany Input.bindButtonPress ic
    [(Sdlkey.KEY_q, (fun _ g -> {g with continue = false}));
     (Sdlkey.KEY_ESCAPE, (fun _ g -> {g with continue = false}))]
  in
    ic
;;


let initGamestate ic res =
  let o = Objs.bunchOf 100 Objs.randomRock in
  let p = Player( newPlayer () ) in
  let g = Gate( newGate (1.0, 1.0) ) in
  let c = Cloud( newCloud (-1.0, -1.0) ) in
  let o = (c,c) :: (g,g) :: (p,p) :: o in
  { 
    frames = 0;
    lastCalc = 0;
    continue = true;

    input = ic;
    res = res; 
    colltree = Kdtree.Leaf;

    loc = (0.,0.); 
    zoom = 100.;

    objs = o;
    newobjs = []; 
    deadobjs = [];
    level = {Level.background = Level.basicLevelDraw; Level.radius = 1000.;
    Level.generate=ignore};

      }


let getScreenBounds g loc =
    let x, y = loc in 
      (* 4:3 aspect ratio *)
    let x1 = x +. (1.25 *. g.zoom)
    and x2 = x -. (1.25 *. g.zoom)
    and y1 = y +. g.zoom
    and y2 = y -. g.zoom in
      (x2,x1,y1,y2)

let setView g loc =
  let x1, x2, y1, y2 = getScreenBounds g loc in
    Drawing.setView x1 x2 y1 y2


let drawMark v =
  (Globals.uiLayer, Drawing.translated v (Drawing.colored (1., 1., 1.) (Drawing.scaled 0.01 (Drawing.triangle))))
;;

let doDrawing now g =
  let dt = now - g.lastCalc in
  let objScene = 
    List.concat (List.map
		    (fun (cur,prev) -> Objs.draw cur prev dt) g.objs)
  and levelScene = g.level.Level.background () 
(*  and markScene = List.map drawMark g.marks*)
  in

  let finalScene = Drawing.mergeScene objScene levelScene in
(*  let finalScene = Drawing.mergeScene markScene finalScene in*)

  let p,pold = getPlayer g in
    setView g (Globals.lerp pold.ploc p.ploc dt);
    Drawing.drawScene finalScene;
    Gl.flush ();
;;

(*
let doCollision g =
(* BRUTE FORCE.  It works. *)
  let pl,_ = getPlayer g in
  let closeObjs = List.filter
    (fun (x,_) -> (magnitude ((Objs.loc x) ^- pl.ploc)) < 0.1) g.objs in
  let closeObjs = List.map (fun (x,_) -> Objs.loc x) closeObjs in
    {g with marks = closeObjs}
;;
*)

let doDeath g = List.fold_left (fun x y -> die y x) g g.deadobjs;;

(*
let doMarks g =
  let o = List.map fst g.objs in
  let k = Kdtree.build 0 loc o in 
  let p,_ = getPlayer g in 
  let l = p.ploc in
  let m = Kdtree.getWithin k l 0.1 in
    {g with marks = List.map loc m}
;;
*)

let buildCollisionTree g =
  let o = List.map fst g.objs in
  let t = Kdtree.build Kdtree.xdim loc o in
    {g with colltree = t}
;;

let doCalc now g =
  if (now - g.lastCalc) >= Globals.calcInterval then
    let ic, g = Input.doInput g.input g in
    let g = buildCollisionTree g in
    let g = List.fold_left (fun x (y,_) -> calc y x) g g.objs in
(*    let g = doMarks g in *)
    let g = doDeath g in
    let g = nextFrame g in
      (* This GC call is harmonious and auspicious.  It makes the game
       * run slightly faster.
       * I still wish I could have the option of turning the thing off 
       * entirely, though.
       *)
      Gc.minor ();
      {g with 
	input = ic;
	lastCalc = now; 
      }
  else
    g
;;


(* This kinda nukes the framerate calculation accuracy, but seems
   to work otherwise.
   We should make framerate calcs immediate rather than total, anyway.
*)
let rec doPause g =
  let ic, g = Input.doInput g.input g in
  let s = Sdlevent.get_app_state () in
    if not (List.mem Sdlevent.INPUTFOCUS s) then begin
	Sdltimer.delay 50;
	doPause g
      end
    else
      let now = Sdltimer.get_ticks () in
      let ic = Input.clearInputstate ic in
      doCalc now {g with input = ic; lastCalc = (now - Globals.calcInterval)}
;;

let rec doMainLoop g =
  let now = Sdltimer.get_ticks () in
    if g.continue then begin
	let g = {g with frames = g.frames + 1} in
	let g = doCalc now g in
	let s = Sdlevent.get_app_state () in
	let g = if not (List.mem Sdlevent.INPUTFOCUS s) then doPause g
	  else g
	in
	  doDrawing now g;
	  doMainLoop g;
      end
    else begin
	let frames = float_of_int g.frames
	and now = (float_of_int now) /. 1000. in
	  Printf.printf "FPS: %f\n" (frames /. now);
      end


let main () =
  let res = Resources.createResourcePool () in

  let mainConf = Resources.getConfig res "cfg/main.cfg" in
  let x = Cfg.getInt mainConf "screen.x"
  and y = Cfg.getInt mainConf "screen.y" in
    Util.init x y;
    
    let ic = initInput res in
    let g = initGamestate ic res in

      Gc.set { (Gc.get ()) with Gc.minor_heap_size = 0x100000};

      doMainLoop g; 

      Util.quit ()

;;
let treeTest () =
  let getAndPrint k x =
    let i = Kdtree.get k (loc x) in
      Printf.printf " Got: %s\n" (string_of_vector2d (loc i));
  in
  let o = Objs.bunchOf 100000 Objs.randomRock in
  let o = List.map fst o in
  let k = Kdtree.build Kdtree.xdim loc o in

    List.iter (getAndPrint k) o;
;;

let treeTest2 () =
  let getAndPrint k x =
    let i = Kdtree.getWithin k (loc x) 1. in
      Printf.printf " Got %d items near %s:\n" (List.length i) (string_of_vector2d (loc x));
      List.iter
	(fun x -> Printf.printf "  %s\n" (string_of_vector2d (loc x))) i;
  in
  let o = Objs.bunchOf 1000 Objs.randomRock in
  let o = List.map fst o in
  let k = Kdtree.build Kdtree.xdim loc o in

    List.iter (getAndPrint k) o;
;;

let _ = main ()
