open Drawing
type level = {
  background : (unit -> Drawing.scene);
  radius : float;
  generate : (unit -> unit);
}


let bounds = 1000.
let hline y = 
   let verts = [(-.bounds,y +. 0.01); (-.bounds, y -. 0.01);
                (bounds,y +. 0.01);  (bounds, y -. 0.01)] in
   withMatrix (colored (0.,0.,1.0) (withPrim `quads (fun () -> vertices verts)))
and vline x =
   let verts = [(x +. 0.01,-.bounds); (x -. 0.01, -.bounds);
                (x +. 0.01, bounds); (x -. 0.01, bounds)] in
   withMatrix (colored (0.,0.,1.0) (withPrim `quads (fun () -> vertices verts)))

(* XXX: This sucks, make it better *)
let basicLevelDraw () =
   let bounds = 10. in
   let rec loopx x accm =
      if x > bounds then
         accm
      else
         loopx (x +. 1.) ((Globals.backgroundLayer, vline x) :: accm)
   and loopy y accm =
      if y > bounds then
         accm
      else
         loopy (y +. 1.) ((Globals.backgroundLayer, hline y) :: accm)
   in
   mergeScene (loopx (-.bounds) []) (loopy (-.bounds) [])
