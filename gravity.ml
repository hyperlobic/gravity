open Vector
open Graphics
open Thread

type entity = {
  position : Vec2.t;
  velocity : Vec2.t;
  diameter : float;
  mass : float;
}

type time_dir =
| Forward
| Reverse
| Paused

type state = {
  entities : entity list;
  dir : time_dir;
}

type window = {
  height : int;
  width : int;
}

let e1 = {
  position = Vec2.make 340. 150.;
  velocity = Vec2.make 100. 30.;
  diameter = 10.;
  mass = 2000.;
}

let e2 = {
  position = Vec2.make 375. 320.;
  velocity = Vec2.make 40. (-150.);
  diameter = 10.;
  mass = 50.;
}

let e3 = {
  position = Vec2.make 340. 125.;
  velocity = Vec2.make 52. 0.;
  diameter = 5.;
  mass = 10.;
}

let e4 = {
  position = Vec2.make (640.0 /. 2.0) (640.0 /. 2.0);
  velocity = Vec2.make 0.0 0.0;
  diameter = 20.0;
  mass = 50000.0;
}

let win = {
  height = 640;
  width = 640;
}

let draw_entity e =
  let open Vec2 in
  Graphics.draw_circle (int_of_float e.position.x) (int_of_float e.position.y) (int_of_float e.diameter)

let draw_entities entities = 
  List.iter draw_entity entities

let bound_value value bound = 
  match value with
  | value when value < 0. -> 0.
  | value when value > bound -> bound
  | _ -> value

(* arbitrary gravitational constant *)
let g = 0.5

let limit_distance epos ~pos ~limit =
  let dist = Vec2.sub epos pos |> Vec2.mag in
  if dist > limit then dist
  else limit

let grav_force e1 e2 = 
  let dist = limit_distance e2.position e1.position 10.0 in
  (g *. e1.mass *. e2.mass) /. (dist ** 2.0) 
    
let grav_accel e1 e2 =
  (grav_force e1 e2) /. e1.mass

let grav_accel_vec e1 e2 =
  let open Vec2 in
  let a = grav_accel e1 e2 in
  Vec2.sub e2.position e1.position
  |> Vec2.normalize
  |> Vec2.scale ~scalar:a

let accum_accel entity entities =
  List.fold_left 
    (fun acc e -> 
      if entity != e then Vec2.add acc (grav_accel_vec entity e) 
      else acc) 
    entity.velocity entities

let sub_accel entity entities =
  List.fold_left 
    (fun acc e -> 
      if entity != e then Vec2.sub acc (grav_accel_vec entity e) 
      else acc) 
    entity.velocity entities

let advance_entity entity entities time =
  let open Vec2 in
  let vel = accum_accel entity entities in
  let vel_vec = Vec2.scale vel time in
  let pos = Vec2.add entity.position vel_vec in
  { entity with position = pos; velocity = vel }

let advance_entities entities time =
  List.map (fun e -> advance_entity e entities time) entities 

let rewind_entity entity time =
  let open Vec2 in
  let rev_vel = Vec2.reverse entity.velocity in
  let rev_vel_vec = Vec2.scale rev_vel time in
  let pos = Vec2.add entity.position rev_vel_vec in
  { entity with position = pos }

let rewind_entity_velocity entity entities =
  let v = sub_accel entity entities in
  { entity with velocity = v }

let rewind_entities entities time =
  let rewound_entity_pos = List.map (fun e -> rewind_entity e time) entities in
  List.map (fun e -> rewind_entity_velocity e rewound_entity_pos) rewound_entity_pos

let time_per_frame = 1. /. 60.

let time = Unix.gettimeofday

let rec play state frame_time = 
  Graphics.auto_synchronize false;
  Graphics.clear_graph();
  draw_entities state.entities;
  Graphics.auto_synchronize true;
  let new_dir = 
    if Graphics.key_pressed () then
      match Graphics.read_key () with
      | 'f' -> print_endline "Forward"; Forward
      | 'r' -> print_endline "Reverse"; Reverse
      | 'p' -> print_endline "Paused"; Paused
      | _ -> state.dir
    else state.dir 
  in
  let rec delay () = 
    let duration = frame_time +. time_per_frame -. Unix.gettimeofday() in
    if duration > 0. then
      try
	Thread.delay duration
      with Unix.Unix_error (_, _, _) -> delay()
  in 
  delay();
  let e = match new_dir with
    | Forward -> advance_entities state.entities time_per_frame
    | Reverse -> rewind_entities state.entities time_per_frame
    | Paused -> state.entities
  in
  let t = time () in
  play { entities = e; dir = new_dir } t

let _ =
  Graphics.open_graph " 640x640";
  play { entities = [e1; e2; e3; e4]; dir = Forward } (time ())
