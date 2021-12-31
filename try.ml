let decorate ~t ~d = d ^ "{" ^ t ^ "}"
let bolden = decorate ~d:"\textbf"
let colour ~c = decorate ~d:("\color{" ^ c ^ "}")

let node ~text ~node_name ~offset ~origin ~tc (* text colour *)
         ~fc (* fill colour *)=
  let offset_x, offset_y = offset and origin_x, origin_y = origin in
  let x = offset_x +. origin_x and y = offset_y +. origin_y in
  let strx = (string_of_float x) and stry = (string_of_float y) in
    "\node[draw=Black, fill=" ^ fc ^ ", rounded corners, minimum width = 0.6cm]"
    ^ "(" ^ node_name ^ ")" ^ "at (" ^ strx ^ "," ^ stry ^ ")"
    ^ "{" ^ (colour ~t:text ~c:tc) ^ "};"

let day_node ~day ~offset ~origin ~tc (* text colour *) ~fc (* fill colour *)=
  let strd = string_of_int day in
  node ~text:strd ~node_name:("d" ^ strd) ~offset:offset ~origin:origin ~tc:tc
       ~fc:fc

let normal_day ~day ~offset ~origin =
  let tc = "Black" and fc = "none" in
  day_node ~day:day ~offset:offset ~origin:origin ~tc:tc ~fc:fc

let special_day ~day ~offset ~origin =
  let tc = "Blue" and fc = "Brown!20" in
  day_node ~day:day ~offset:offset ~origin:origin ~tc:tc ~fc:fc

let holi_day ~day ~offset ~origin =
  let tc = "Black" and fc = "none" in
  day_node ~day:day ~offset:offset ~origin:origin ~tc:tc ~fc:fc

let weekday_name ~day ~offset ~origin =
  let tc = "Black" and fc = "Red!20" in
  node ~text:(bolden day) ~node_name:day ~offset:offset ~origin:origin ~tc:tc ~fc:fc
