let rec bubble xs=
    match xs with
    []->xs
    |[x]->xs
    |x::y::ys->if x>y then y::(bubble (x::ys))
                else x::(bubble (y::ys))
let rec bubbleSort l=
    match l with
    []->l
    |[x]->l
    |_->match (List.rev (bubbleRev l)) with y::ys->List.rev (y::(List.rev (bubbleSort ys)))
