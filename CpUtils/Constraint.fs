namespace CpUtils

open Expr

type BoolVar =
    { Name: string }

    static member New(name: string) = { Name = name }

type Constraint =
    | Eq of Expr * Expr
    | Le of Expr * Expr
    | Lt of Expr * Expr
    | Ne of Expr * Expr
    | AllDifferent of Expr seq

// static member (%&&)(constr0: Constraint, constr1: Constraint) = And(constr0, constr1)

// static member (%||)(constr0: Constraint, constr1: Constraint) = Or(constr0, constr1)

// static member (%=>)(constr0: Constraint, constr1: Constraint) = Not(constr0) %|| constr1

// static member (%<=>)(constr0: Constraint, constr1: Constraint) =
//     (constr0 %=> constr1) %&& (constr1 %=> constr0)

// static member (%^^)(constr0: Constraint, constr1: Constraint) = constr0 %<=> Not constr1

//

[<AutoOpen>]
module Operators =
    let inline (=%)
        (x: 'a when 'a: (static member ToExpr: 'a -> Expr))
        (y: 'b when 'b: (static member ToExpr: 'b -> Expr))
        =
        Eq(expr x, expr y)

    let inline (<=%)
        (x: 'a when 'a: (static member ToExpr: 'a -> Expr))
        (y: 'b when 'b: (static member ToExpr: 'b -> Expr))
        =
        Le(expr x, expr y)

    let inline (>=%)
        (x: 'a when 'a: (static member ToExpr: 'a -> Expr))
        (y: 'b when 'b: (static member ToExpr: 'b -> Expr))
        =
        y <=% x

    let inline (<%)
        (x: 'a when 'a: (static member ToExpr: 'a -> Expr))
        (y: 'b when 'b: (static member ToExpr: 'b -> Expr))
        =
        Lt(expr x, expr y)

    let inline (>%)
        (x: 'a when 'a: (static member ToExpr: 'a -> Expr))
        (y: 'b when 'b: (static member ToExpr: 'b -> Expr))
        =
        y <% x

    let inline (<>%)
        (x: 'a when 'a: (static member ToExpr: 'a -> Expr))
        (y: 'b when 'b: (static member ToExpr: 'b -> Expr))
        =
        Ne(expr x, expr y)

//

module NumericLiteralZ =
    let FromZero () = C 0L
    let FromOne () = C 1L
    let FromInt32 = int64 >> C
    let FromInt64 = C
