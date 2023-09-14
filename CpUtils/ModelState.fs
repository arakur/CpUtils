namespace CpUtils

type 'a ModelState = ModelState of (Model -> 'a * Model)

module ModelState =
    let unwrap (ModelState f) = f

    let inline run<'a> (model: Model) (state: 'a ModelState) = unwrap state model

    let inline buildFrom<'a> (model: Model) (state: 'a ModelState) = state |> run model |> snd

    let inline build<'a> (state: 'a ModelState) = buildFrom Model.empty state

    let return_ x = ModelState(fun model -> x, model)

    let bind (state: 'a ModelState) (f: 'a -> 'b ModelState) =
        ModelState(fun model ->
            let x, model' = run model state
            run model' (f x))

    let get = ModelState(fun s -> s, s)

    let addVariable (var: IntVar) =
        ModelState(fun model ->
            let model' = model.AddVariable var
            (), model')

    let addConstraint (constr: Constraint) =
        ModelState(fun model ->
            let model' = model.AddConstraint constr
            (), model')

    let map (f: 'a -> 'b) (state: 'a ModelState) =
        ModelState(fun model ->
            let x, model' = run model state
            f x, model')
