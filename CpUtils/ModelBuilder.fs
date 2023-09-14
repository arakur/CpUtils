namespace CpUtils

//

type ModelBuilder() =
    member __.Bind(x, f) : 'a ModelState = ModelState.bind x f

    member __.Return x : 'a ModelState = ModelState(fun model -> x, model)

    member this.Zero() : unit ModelState = this.Return()

    member __.Combine(x: 'a ModelState, y: 'b ModelState) =
        ModelState(fun model ->
            let _, model' = ModelState.run model x
            ModelState.run model' y)

    member __.Delay f : 'a ModelState = f ()

    member this.For(seq, f: 'a -> 'b ModelState) =
        seq |> Seq.map f |> Seq.reduceBack (fun x y -> this.Combine(x, y))

    member this.While(f, x) =
        if f () then
            this.Combine(x, this.While(f, x))
        else
            this.Zero()

    member __.Yield(var: IntVar) : unit ModelState =
        ModelState(fun model -> (), model.AddVariable var)

    member __.Yield(constr: Constraint) : unit ModelState =
        ModelState(fun model -> (), model.AddConstraint constr)

    member __.YieldFrom(variables: IntVar seq) : unit ModelState =
        ModelState(fun model -> (), variables |> Seq.fold (fun model var -> model.AddVariable var) model)

    member __.YieldFrom(constraints: Constraint seq) : unit ModelState =
        ModelState(fun model -> (), constraints |> Seq.fold (fun model constr -> model.AddConstraint constr) model)

    member __.YieldFrom(other: Model) : unit ModelState =
        ModelState(fun model ->
            model
            |> Seq.foldBack (fun var model -> model.AddVariable var) other.Variables.Values
            |> Seq.foldBack (fun constr (model: Model) -> model.AddConstraint constr) other.Constraints
            |> fun model' -> (), model')

//

module ModelBuilder =
    let modelBuilder = ModelBuilder()

    //

    let var name min max =
        let v = IntVar.Raw(name, min, max)
        ModelState(fun model -> v, model.AddVariable v)

    let vars (seq: seq<string * int64 * int64>) =
        seq
        |> Seq.map (fun (name, min, max) -> IntVar.Raw(name, min, max))
        |> Seq.fold
            (fun state var -> ModelState.bind state (fun vars -> ModelState.return_ (var :: vars)))
            (ModelState.return_ [])

    let varMap<'k when 'k: comparison> (m: Map<'k, string * int64 * int64>) : Map<'k, IntVar> ModelState =
        m
        |> Map.map (fun k (name, min, max) -> k, IntVar.Raw(name, min, max))
        |> Map.fold
            (fun state k (k', var) -> ModelState.bind state (fun vars -> ModelState.return_ (Map.add k var vars)))
            (ModelState.return_ Map.empty)

    let vars2D name min max width height =
        modelBuilder {
            let vars2D =
                Array2D.create width height ()
                |> Array2D.mapi (fun i j _ -> IntVar.Raw(sprintf "%s[%d,%d]" name i j, min, max))

            for i in 0 .. width - 1 do
                for j in 0 .. height - 1 do
                    yield vars2D.[i, j]

            return vars2D
        }
