let rec take i = function
  | [] -> []
  | x::xs ->
      if i > 0 then x :: take (i - 1) xs
      else []
let rec drop i = function
  | [] -> []
  | x::xs ->
      if i > 0 then drop (i-1) xs
      else x::xs
let rec merge x y=
    match x with
    []->y
    |x::xs->match y with
            []->x::xs
            |y::ys->if x>y then y::(merge (x::xs) ys)
                    else x::(merge xs (y::ys))
let rec mergeSort l=
    let t=List.length l in
    if t=0 || t=1 then l
    else
    let x=mergeSort (take (t/2) l) in
    let y=mergeSort (drop (t/2) l) in
    merge x y
