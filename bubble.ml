let rec bubble xs=
    match xs with
    []->xs
    |[x]->xs
    |x::y::ys->if x>y then y::(bubble (x::ys))
                else x::(bubble (y::ys))
             | Lorry
let rec bubble_tr xs fr=
    match xs with
    []->List.rev fr
    |[x]->List.rev (x::fr)
    |x::y::ys->if x>y then bubble_tr (x::ys) (y::fr)
                else bubble_tr (y::ys) (x::fr)
let rec bubbleSort l=
    match l with
    []->l
    |[x]->l
    |_->match (List.rev (bubbleRev l)) with y::ys->List.rev (y::(List.rev (bubbleSort ys)))
