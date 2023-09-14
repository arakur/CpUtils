namespace CpUtils

open Utils

open Google.OrTools

module Model =
    type Solution(intVarMap: Map<string, int64>) =
        new(seq: seq<string * int64>) = Solution(seq |> Map.ofSeq)

        member __.IntVarMap = intVarMap

        member __.Item(name: string) = intVarMap.[name]
        member __.Item(var: IntVar) = intVarMap.[var.Name]

    type SolutionCollector(varMap: Map<string, Sat.IntVar>) =
        inherit Sat.CpSolverSolutionCallback()

        let mutable solutions = []

        member __.Solutions = solutions

        override this.OnSolutionCallback() =
            let sol = varMap |> Map.map (fun name var -> this.Value var) |> Solution
            solutions <- sol :: solutions

//

open Model

type Model(variables: Map<string, IntVar>, constraints: Constraint list) =
    member __.Variables = variables
    member __.Constraints = constraints

    static member empty = Model(Map.empty, [])

    member __.AddVariable(var: IntVar) =
        Model(variables |> Map.add var.Name var, constraints)

    member __.AddConstraint(constr: Constraint) = Model(variables, constr :: constraints)

    member __.constructCpModel() =
        let cpModel = Sat.CpModel()

        let varMap: Map<string, Sat.IntVar> =
            variables
            |> Map.toSeq
            |> Seq.map (fun (name, var) -> name, cpModel.NewIntVar(var.Min, var.Max, name))
            |> Map.ofSeq

        let exprForCp (expr: Expr) (tempMap: Map<string, Sat.IntVar>) =
            let addTerm (expr', tempMap': Map<string, Sat.IntVar>) (term: Multiset<IntVar> * int64) =
                let vs, c = term

                if vs.m.IsEmpty then
                    expr' + c, tempMap'
                elif vs.m.Values |> Seq.toList = [ 1 ] then
                    expr' + varMap.[(vs.m.Keys |> Seq.head).Name] * c, tempMap'
                else
                    let varProd = IntVar.prodMany (vs.m |> Map.toSeq)

                    let cpVar, newTempMap =
                        tempMap'.TryFind varProd.Name
                        |> Option.map (fun cpVar -> cpVar, tempMap')
                        |> Option.defaultWith (fun () ->
                            let cpVar = cpModel.NewIntVar(varProd.Min, varProd.Max, varProd.Name)

                            let cpProd =
                                seq {
                                    for (v, count) in vs.m |> Map.toSeq do
                                        yield! varMap.[v.Name] :> Sat.LinearExpr |> Seq.replicate count
                                }

                            cpModel.AddMultiplicationEquality(cpVar, cpProd) |> ignore
                            cpVar, tempMap' |> Map.add varProd.Name cpVar)

                    expr' + cpVar * c, newTempMap

            expr.Terms()
            |> Map.toSeq
            |> Seq.fold addTerm (Sat.LinearExpr.Constant 0, tempMap)

        let rec addConstr (tempMap: Map<string, Sat.IntVar>) (constr: Constraint) =
            match constr with
            | Eq(expr0, expr1) ->
                let expr0', tempMap' = exprForCp expr0 tempMap
                let expr1', tempMap'' = exprForCp expr1 tempMap'
                cpModel.Add(Sat.LinearExpr.(=) (expr0', expr1')) |> ignore
                tempMap''
            | Le(expr0, expr1) ->
                let expr0', tempMap' = exprForCp expr0 tempMap
                let expr1', tempMap'' = exprForCp expr1 tempMap'
                cpModel.Add(Sat.LinearExpr.(<=) (expr0', expr1')) |> ignore
                tempMap''
            | Lt(expr0, expr1) ->
                let expr0', tempMap' = exprForCp expr0 tempMap
                let expr1', tempMap'' = exprForCp expr1 tempMap'
                cpModel.Add(Sat.LinearExpr.(<) (expr0', expr1')) |> ignore
                tempMap''
            | Ne(expr0, expr1) ->
                let expr0', tempMap' = exprForCp expr0 tempMap
                let expr1', tempMap'' = exprForCp expr1 tempMap'
                cpModel.Add(Sat.LinearExpr.(<>) (expr0', expr1')) |> ignore
                tempMap''
            | AllDifferent exprs ->
                let exprs', tempMap' =
                    (([], tempMap), exprs)
                    ||> Seq.fold (fun (exprs', tempMap) expr ->
                        let expr', tempMap' = exprForCp expr tempMap
                        expr' :: exprs', tempMap')

                cpModel.AddAllDifferent(exprs') |> ignore
                tempMap'

        constraints |> Seq.fold addConstr Map.empty |> ignore

        cpModel, varMap

    member this.Solve() =
        let cpModel, varMap = this.constructCpModel ()
        let solver = Sat.CpSolver()
        let status = solver.Solve(cpModel)

        let sol =
            this.Variables.Keys
            |> Seq.map (fun var -> var, solver.Value varMap.[var])
            |> Solution

        status, solver, sol

    member this.SolveAll() =
        let cpModel, varMap = this.constructCpModel ()
        let solver = Sat.CpSolver()
        solver.StringParameters <- "enumerate_all_solutions:true"

        use collector = new SolutionCollector(varMap)
        let status = solver.Solve(cpModel, collector)

        let solutions = collector.Solutions |> List.rev

        status, solver, solutions
