module Vec2 = struct
  type t = {
    x : float;
    y : float;
  }

  let make x y = { x = x; y = y }

  let add v1 v2 =  
    { x = v1.x +. v2.x; 
      y = v1.y +. v2.y }

  let sub v1 v2 = 
    { x = v1.x -. v2.x; 
      y = v1.y -. v2.y }

  let scale v ~scalar =
    { x = v.x *. scalar;
      y = v.y *. scalar; }

  let distance v1 v2 = 
    let v = sub v1 v2 in
    sqrt ((v.x ** 2.0) +. (v.y ** 2.0))
    
  let mag v =
    sqrt (v.x ** 2.0 +. v.y ** 2.0)

  let normalize v = 
    let m = mag v in
    if m = 0.0 then { x = 0.0; y = 0.0 }
    else scale v (1.0 /. m)

  let reverse v = 
    { x = -. v.x;
      y = -. v.y; }

  let string_of v =
    "{ " ^ string_of_float v.x ^ " , " ^ string_of_float v.y ^ " }"
end
