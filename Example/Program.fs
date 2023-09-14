open CpUtils

open CpUtils.ModelBuilder

// Solving sudoku.

// Load an input file.
let input: int64[][] =
    System.IO.File.ReadAllLines "sample.txt"
    |> Array.map (fun line -> line.Split() |> Array.map int64)

// generate an array x of variables and build the sudoku model.
let x, model =
    modelBuilder {
        let! x = vars2D "x" 1 9 9 9

        for i in 0..8 do
            for j in 0..8 do
                if input.[i].[j] > 0 then
                    yield x.[i, j] =% C input.[i].[j]

        for i in 0..8 -> AllDifferent [ for j in 0..8 -> Expr.expr x.[i, j] ]
        for j in 0..8 -> AllDifferent [ for i in 0..8 -> Expr.expr x.[i, j] ]

        for i in 0..2 do
            for j in 0..2 ->
                AllDifferent
                    [ for k in 0..2 do
                          for l in 0..2 do
                              Expr.expr x.[i * 3 + k, j * 3 + l] ]

        return x
    }
    |> ModelState.run Model.empty

// Solve the model.
printfn "calculating..."
let _, _, solutions = model.SolveAll()
printfn "done"

// Print the solutions.
solutions
|> Seq.iteri (fun i solution ->
    printfn "solution %d" (i + 1)

    [ for i in 0..8 ->
          [ for j in 0..8 -> solution.[x.[i, j]] ]
          |> Seq.map (sprintf "%d")
          |> String.concat " " ]
    |> String.concat "\n"
    |> printfn "%s")
