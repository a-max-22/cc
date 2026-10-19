let rec apply (value:Term) (succ:Term) (n:int) = 
    match n with
        | 0 -> value
        | _ -> Appl(succ, (apply value succ (n - 1)))

let church_encoded_add (m:int) (n:int) = 
    let zero = Lam(fun A -> Lam(fun s -> Lam(fun z -> z)))
    let succ = Lam(fun n -> 
                    Lam(fun A -> 
                        Lam(fun s -> 
                        Lam(fun z -> 
                            Appl(s, Appl(Appl(Appl(n, A), s), z))))))
    
    let sum = apply (apply zero succ m) succ n
    try
        printfn "church_encoded_add %s" (pp 0 sum)
    with
        | ex -> printfn "%s" (ex.Message)
