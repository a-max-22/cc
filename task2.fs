
//...
let test5 () =
    let id_func_type = Pi(Star, fun a -> Pi(a, fun _ -> a))
    let id_func = Lam(fun A -> Lam(fun x -> x)) 
    let id_func_ann = Ann(id_func, id_func_type)

    try
        let inferred = infer 0 [] id_func_ann
        printfn "Test 5: Type of idientity function: %s" (pp 0 inferred)
        assert (equate 0 (inferred, id_func_type))
    with
        | ex -> printfn "%s" (ex.Message)
