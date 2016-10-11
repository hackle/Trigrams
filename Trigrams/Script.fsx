// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

// Define your library scripting code here

let rec infinite n = 
    seq {
        yield n
        yield! infinite (n+1)
    }


let rec printInfinite inf stop count =
    if stop count
    then
        printf "(END with counter)"
    else
        if Seq.isEmpty inf
        then printf "(END)"
        else
            printf "%A," (inf |> Seq.head)
            printInfinite (inf |> Seq.tail) stop (count + 1)

printInfinite (infinite 0) (fun s -> s = 50) 0