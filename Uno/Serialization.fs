module Serialisation

open Game
open Newtonsoft.Json
open FSharp.Reflection
open System

type SingleCaseConverter() =
    inherit JsonConverter()

    override __.CanConvert t =
        FSharpType.IsUnion t &&
        (FSharpType.GetUnionCases t |> Array.length) = 1
    override __.CanRead = true
    override __.CanWrite = true

    override __.WriteJson(w,o,s) = 
        let _,fields = FSharpValue.GetUnionFields(o, o.GetType())
        s.Serialize(w, fields.[0])
    
    override __.ReadJson(r,t,_,s) =
        let case = FSharpType.GetUnionCases t |> Array.head
        let v = s.Deserialize(r,case.GetFields().[0].PropertyType)
        FSharpValue.MakeUnion(case, [| v |])

            
            


type FSharpListConverter() =
    inherit JsonConverter()

    override __.CanConvert t =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ list>
    override __.CanRead = true
    override __.CanWrite = true

    override __.WriteJson(w,o,s) = 
        w.WriteStartArray()
        for v in  unbox o do
            s.Serialize(w,o)
        w.WriteEndArray()
    
    override __.ReadJson(r,t,_,s) =
        let ti = t.GenericTypeArguments.[0]
        r.Read() |> ignore
        List.unfold (fun _ ->
        if r.TokenType = JsonToken.EndArray then
            r.Read() |> ignore
            None
        else
            let v = s.Deserialize(r,ti)
            r.Read() |> ignore
            Some(v,())) ()
        |> box
            

type UnionConverter() =
    inherit JsonConverter()

    override __.CanConvert t =
        FSharpType.IsUnion t 
    override __.CanRead = true
    override __.CanWrite = true

    override __.WriteJson(w,o,s) = 
        let case, fields = FSharpValue.GetUnionFields(o, o.GetType())
        if fields.Length = 0 then
            w.WriteValue(case.Name)
        else
            w.WriteStartObject()
            w.WritePropertyName(case.Name)
            if fields.Length = 1 then
                let t = case.GetFields().[0].PropertyType
                s.Serialize(w, fields.[0], t)

            else
                w.WriteStartArray()
    
                for field, t in Array.zip fields (case.GetFields()) do
                    s.Serialize(w, field, t.PropertyType)
                w.WriteEndArray()
            w.WriteEndObject()
        
    override __.ReadJson(r,t,_,s) =
        let cases = FSharpType.GetUnionCases t 
        if r.TokenType = JsonToken.String then
            let name = string r.Value
            r.Read() |> ignore

            let case = cases |> Array.find (fun c -> c.Name = name) 
            FSharpValue.MakeUnion(case, null)
        else 
            r.Read() |> ignore
            let name = string r.Value
            let case = cases |> Array.find (fun c -> c.Name = name) 
            let fields = case.GetFields()
            r.Read() |> ignore
            let value = 
                if fields.Length = 1 then
                    let v = s.Deserialize(r, fields.[0].PropertyType)
                    FSharpValue.MakeUnion(case, [|v|])
                else 
                    r.Read() |> ignore
                    let values =
                        fields
                        |> Array.map (fun f -> 
                            s.Deserialize(r, f.PropertyType))
                    r.Read() |> ignore
                    FSharpValue.MakeUnion(case, values)
            r.Read() |> ignore
            value 


let serializer =
    JsonSerializer.Create(
        JsonSerializerSettings(
            Converters = [|
                SingleCaseConverter()
                FSharpListConverter()
                UnionConverter()
            |]
        ))

let serialize o =
    use stream = new IO.StringWriter()
    serializer.Serialize(stream, o)
    stream.ToString()

let deserialize s =
    use stream = new IO.StringReader(s)
    use reader = new JsonTextReader(stream)
    serializer.Deserialize<'t>(reader)

type GameId = GameId of int

// serialize <| GameId 2

// let (GameId n) = deserialize "2"

// let s : string list = deserialize """["2","1"]"""

// let c = Digit(Four, Green)
// let case, fs = FSharpValue.GetUnionFields(c, c.GetType())

// serialize (StartGame { Players = 3; FirstCard = Digit(Four, Green)}  )
// let (cmd: Command) = deserialize """{"StartGame":{"Players":3,"FirstCard":{"Digit":["Four","Green"]}}}"""


// let (d: Digit) = deserialize "\"Four\""

// let cs = FSharpType.GetUnionCases typeof<Digit>

// let r = new IO.StringReader("\"Four\"")
// let jr = new JsonTextReader(r)

module GameEvents = 
    let serialize event = 
            match event with
            | CardPlayed e -> "CardPlayed", serialize e
            | GameStarted e -> "GameStarted", serialize e
            | WrongCardPlayed e -> "WrongCardPlayed", serialize e
            

    let deserialize (eventType, data) =
        match eventType with
        | "CardPlayed" -> [CardPlayed (deserialize data)]
        | "GameStarted" -> [GameStarted (deserialize data)]
        | "WrongCardPlayed" -> [WrongCardPlayed (deserialize data)]
        | _ -> []
     