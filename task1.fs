// tests dependent function
let test3 () =
    let pi_type = Pi(Star, fun x -> Pi(x, fun a -> x))
    let id = Lam(fun A -> Lam(fun x -> x)) 
    let pi_ann = Ann(id, pi_type)

    try
        let inferred = infer 0 [] pi_ann
        printfn "Test 3: Type of dependent function: %s" (pp 0 inferred)
        assert (equate 0 (inferred, pi_type))
    with
        | ex -> printfn "%s" (ex.Message)

// tests bool type 
let test4 ()= 
    let bool_type = Pi(Star, fun x -> Pi(x, fun y -> Pi(x, fun y -> x)))
    let true_elem = Lam(fun x -> Lam(fun y -> x))
    let false_elem = Lam(fun x -> Lam(fun y -> y))

    let t_ann = Ann(true_elem, bool_type)
    let f_ann = Ann(false_elem, bool_type)

    try
        let result_f = infer 0 [] t_ann
        let result_t = infer 0 [] t_ann
        printfn "Test 4: Type of false: %s" (pp 0 result_f)
        printfn "Test 4: Type of true: %s" (pp 0 result_t)

        assert (equate 0 (t_ann, bool_type))
        assert (equate 0 (f_ann, bool_type))
    with
        | ex -> printfn "%s" (ex.Message)


test3 ()
test4 ()
