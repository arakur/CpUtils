module CpUtils.Utils

type Multiset<'v when 'v: comparison> =
    { m: Map<'v, int> }

    static member empty: 'v Multiset = { m = Map.empty }

    member this.Count(v: 'v) =
        this.m |> Map.tryFind v |> Option.defaultValue 0

    static member addN (v: 'v) (num: int) (ms: 'v Multiset) =
        { m = ms.m |> Map.add v (ms.m.TryFind v |> Option.defaultValue 0 |> (+) num) }

    static member add(v: 'v) = Multiset.addN v 1

    static member ofSeq(seq: ('v * int) seq) : 'v Multiset =
        let addN' acc (v, count) = Multiset.addN v count acc
        seq |> Seq.fold addN' Multiset.empty

    static member singleton(v: 'v) = Multiset.add v Multiset.empty

    member this.Union(ms: 'v Multiset) =
        (this, ms.m) ||> Map.fold (fun acc v count -> acc |> Multiset.addN v count)

    static member map<'w when 'w: comparison> (f: 'v -> 'w) (ms: 'v Multiset) =
        { m = ms.m |> Map.toSeq |> Seq.map (fun (v, count) -> f v, count) |> Map.ofSeq }
