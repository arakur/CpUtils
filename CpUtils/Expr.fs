namespace CpUtils

open Utils

type IntVar =
    { Name: string
      Min: int64
      Max: int64 }

    static member Raw(name: string, min: int64, max: int64) = { Name = name; Min = min; Max = max }

    static member ToExpr(v) = V v

    static member neg(v: IntVar) = Neg(V v)

    static member (+)(v0: IntVar, v1: IntVar) : Expr = Add(V v0, V v1)
    static member (+)(v0: IntVar, v1: int64) : Expr = Add(V v0, C v1)
    static member (+)(v0: int64, v1: IntVar) : Expr = Add(C v0, V v1)
    static member (+)(v0: IntVar, v1: int) : Expr = Add(V v0, C(int64 v1))
    static member (+)(v0: int, v1: IntVar) : Expr = Add(C(int64 v0), V v1)

    static member (-)(v0: IntVar, v1: IntVar) : Expr = Add(V v0, Mul(C(-1), V v1))
    static member (-)(v0: IntVar, v1: int64) : Expr = Add(V v0, C(-v1))
    static member (-)(v0: int64, v1: IntVar) : Expr = Add(C v0, Mul(C(-1), V v1))
    static member (-)(v0: IntVar, v1: int) : Expr = Add(V v0, C(-int64 v1))
    static member (-)(v0: int, v1: IntVar) : Expr = Add(C(int64 v0), Mul(C(-1), V v1))

    static member (*)(v0: IntVar, v1: IntVar) : Expr = Mul(V v0, V v1)
    static member (*)(v0: IntVar, v1: int64) : Expr = Mul(V v0, C v1)
    static member (*)(v0: int64, v1: IntVar) : Expr = Mul(C v0, V v1)
    static member (*)(v0: IntVar, v1: int) : Expr = Mul(V v0, C(int64 v1))
    static member (*)(v0: int, v1: IntVar) : Expr = Mul(C(int64 v0), V v1)

    //

    static member power (v: IntVar) (count: int) =
        let pow (x: int64) (n: int) =
            let rec loop acc x n =
                if n = 0 then acc
                elif n % 2 = 0 then loop acc (x * x) (n / 2)
                else loop (acc * x) (x * x) (n / 2)

            loop 1L x n

        let name = sprintf "%s ^ %d" v.Name count

        let min', max' =
            if count % 2 = 0 then
                if v.Min >= 0L then pow v.Min count, pow v.Max count
                elif v.Max <= 0L then pow v.Max count, pow v.Min count
                else 0L, max (pow v.Min count) (pow v.Max count)
            else
                pow v.Min count, pow v.Max count

        IntVar.Raw(name, min', max')

    static member prod2 (v0: IntVar) (v1: IntVar) =
        let isV0NegPos = v0.Min <= 0L && 0L <= v0.Max
        let isV0Neg = v0.Max <= 0L
        let isV1NegPos = v1.Min <= 0L && 0L <= v1.Max
        let isV1Neg = v1.Max <= 0L

        let min', max' =
            if isV0NegPos then
                if isV1NegPos then
                    // v0.Min, v1.Min <= 0L <= v0.Max, v1.Max
                    min (v0.Min * v1.Max) (v0.Max * v1.Min), max (v0.Min * v1.Min) (v0.Max * v1.Max)
                elif isV1Neg then
                    // v0.Min, v1.Min, v1.Max <= 0L <= v0.Max
                    v0.Max * v1.Min, v0.Min * v1.Min
                else
                    // v0.Min <= 0L <= v0.Max, v1.Min, v1.Max
                    v0.Min * v1.Max, v0.Max * v1.Max
            elif isV0Neg then
                if isV1NegPos then
                    // v0.Min, v0.Max, v1.Min <= 0L <= v1.Max
                    v0.Min * v1.Max, v0.Min * v1.Min
                elif isV1Neg then
                    // v0.Min, v0.Max, v1.Min, v1.Max <= 0L
                    v0.Max * v1.Max, v0.Min * v1.Min
                else
                    // v0.Min, v0.Max <= 0L <= v1.Min, v1.Max
                    v0.Min * v1.Max, v0.Max * v1.Min
            else if isV1NegPos then
                // v1.Min <= 0L <= v0.Min, v0.Max, v1.Max
                v0.Max * v1.Min, v0.Min * v1.Min
            elif isV1Neg then
                // v1.Min, v1.Max <= 0L <= v0.Min, v0.Max
                v0.Max * v1.Min, v0.Min * v1.Max
            else
                // 0L <= v0.Min, v0.Max, v1.Min, v1.Max
                v0.Min * v1.Min, v0.Max * v1.Max

        let name = sprintf "%s * %s" v0.Name v1.Name

        IntVar.Raw(name, min', max')

    static member prodMany(vs: (IntVar * int) seq) =
        match vs |> Seq.toList with
        | [] -> failwith "Empty sequence"
        | (v, count) :: vs' ->
            let v' = IntVar.power v count

            vs'
            |> Seq.fold (fun acc (v, count) -> IntVar.prod2 acc (IntVar.power v count)) v'

and Expr =
    | C of int64
    | V of IntVar
    | Neg of Expr
    | Add of Expr * Expr
    | Mul of Expr * Expr

    static member ToExpr(expr) = expr

    static member (~-)(x: Expr) = Neg x

    static member neg(x: Expr) = Neg x

    static member (+)(x: Expr, y: Expr) = Add(x, y)
    static member (+)(x: Expr, y: IntVar) = Add(x, V y)
    static member (+)(x: IntVar, y: Expr) = Add(V x, y)
    static member (+)(x: Expr, y: int64) = Add(x, C y)
    static member (+)(x: int64, y: Expr) = Add(C x, y)
    static member (+)(x: Expr, y: int) = Add(x, C(int64 y))
    static member (+)(x: int, y: Expr) = Add(C(int64 x), y)

    static member (-)(x: Expr, y: Expr) = Add(x, Mul(C(-1), y))
    static member (-)(x: Expr, y: IntVar) = Add(x, Mul(C(-1), V y))
    static member (-)(x: IntVar, y: Expr) = Add(V x, Mul(C(-1), y))
    static member (-)(x: Expr, y: int64) = Add(x, C(-y))
    static member (-)(x: int64, y: Expr) = Add(C x, Mul(C(-1), y))
    static member (-)(x: Expr, y: int) = Add(x, C(-int64 y))
    static member (-)(x: int, y: Expr) = Add(C(int64 x), Mul(C(-1), y))

    static member (*)(x: Expr, y: Expr) = Mul(x, y)
    static member (*)(x: Expr, y: IntVar) = Mul(x, V y)
    static member (*)(x: IntVar, y: Expr) = Mul(V x, y)
    static member (*)(x: Expr, y: int64) = Mul(x, C y)
    static member (*)(x: int64, y: Expr) = Mul(C x, y)
    static member (*)(x: Expr, y: int) = Mul(x, C(int64 y))
    static member (*)(x: int, y: Expr) = Mul(C(int64 x), y)

    member this.Terms() =
        match this with
        | C i -> Map.ofSeq [ Multiset.empty, i ]
        | V var -> Map.ofSeq [ Multiset.singleton var, 1L ]
        | Neg expr -> expr.Terms() |> Map.map (fun _ c -> -c)
        | Add(expr0, expr1) ->
            (expr0.Terms(), expr1.Terms())
            ||> Map.fold (fun acc vs c ->
                let c' = acc.TryFind vs |> Option.defaultValue 0L
                acc |> Map.add vs (c + c'))
        | Mul(expr0, expr1) ->
            (expr0.Terms(), expr1.Terms())
            ||> Seq.allPairs
            |> Seq.map (fun (t0, t1) ->
                let vs0, c0 = t0.Key, t0.Value
                let vs1, c1 = t1.Key, t1.Value
                vs0.Union vs1, c0 * c1)
            |> Map.ofSeq

//

module Expr =
    let inline expr (x: 'a when 'a: (static member ToExpr: 'a -> Expr)) = 'a.ToExpr x
