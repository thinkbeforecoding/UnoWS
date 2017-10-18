module Game

type Command = 
    | StartGame of StartGame

and StartGame = {
    Players: int
    FirstCard: Card }

type Event = 
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed

and GameStarted = {
    Players: int
    FirstCard: Card
}
and CardPlayed = {
    Player: int
    Card: Card
}

type State = 
    | InitialState

type GameError =
    | GameAlreadyStarted

type Decide = Command -> State -> Result<Event list, GameError>
type Evolve = State -> Event -> State


let decide : Decide = fun  _ _ -> failwith "Not Implemented"
       
let evolve : Evolve =
    fun _ _ -> failwith "Not Implemented"

