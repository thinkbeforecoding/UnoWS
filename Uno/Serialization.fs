module Serialisation

open Game
open Newtonsoft.Json // nuget : Newtonsoft.Json
open FSharp.Reflection
open System

let serializer = JsonSerializer.Create()

let serialize o =
    use stream = new IO.StringWriter()
    serializer.Serialize(stream, o)
    stream.ToString()

let deserialize s =
    use stream = new IO.StringReader(s)
    use reader = new JsonTextReader(stream)
    serializer.Deserialize<'t>(reader)

type GameId = GameId of int

type CardDto = {
    Value: string
    Color: string 
}

type GameStartedDto = {
    Players: int
    FirstCard: CardDto  
}

type CardPlayedDto = {
    Player: int
    Card: CardDto
}

module GameEvents = 
    open Game

    let toCardDto = function
        | Digit(d, c ) -> { Value = string d; Color = string c}
        | Skip(c) -> { Value = "Skip"; Color = string c }

    let (|Color|_|) c =
        match c with
        | "Red" -> Some Red
        | "Green" -> Some Green
        | "Blue" -> Some Blue
        | "Yellow" -> Some Yellow
        | _ -> None
        
    let (|Digit|_|) d =
        match d with
        | "Zero" -> Some Zero
        | "One" -> Some One
        | "Two" -> Some Two 
        | "Three" -> Some Three
        | "Four" -> Some Four
        | "Five" -> Some Five
        | "Six" -> Some Six
        | "Seven" -> Some Seven
        | "Height" -> Some Height
        | "Nine" -> Some Nine
        | _ -> None
    let (|Card|_|)  =
        function
        | { Value = "Skip"; Color = Color c } -> Some (Skip(c))
        | { Value = Digit d; Color = Color c } -> Some (Digit(d,c))
        | _ -> None 

    let serialize event = 
            match event with
            | GameStarted e -> 
                "GameStarted", 
                    { GameStartedDto.Players = e.Players
                      FirstCard = toCardDto e.FirstCard } 
                    |> serialize
            | CardPlayed e -> 
                "CardPlayed", 
                    { CardPlayedDto.Player = e.Player
                      Card = toCardDto e.Card }
                    |> serialize 
            

    let deserialize (eventType, data) =
        match eventType with
        | "CardPlayed" -> 
            data
            |> deserialize
            |> function 
                | { CardPlayedDto.Player = player; Card = Card card } ->
                    [CardPlayed { Player = player; Card = card }]
                | _ -> []

        | "GameStarted" -> 
            data
            |> deserialize
            |> function 
                    | { GameStartedDto.Players = players; FirstCard = Card card } ->
                        [GameStarted (deserialize data)]
                    | _ -> []
        | _ -> []
     