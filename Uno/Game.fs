module Game

type Command = 
    | StartGame of StartGame

and StartGame = {
    Players: int
    FirstCard: Card }

type Event = 
    | GameStarted of GameStarted

and GameStarted = {
    Players: int
    FirstCard: Card
}

type State = 
    | InitialState
    | Started

// let decide (command: Command) (state: State) : Event list = []

type GameError =
    | GameAlreadyStarted

let decide command state = 
    match state, command with
    | InitialState, StartGame cmd -> 
        Ok [ GameStarted { Players = cmd.Players; FirstCard = cmd.FirstCard } ]
    | Started, StartGame _ ->
        Error GameAlreadyStarted
        
let evolve state event = 
    match event with
    | GameStarted _ -> Started
    | _ -> state

