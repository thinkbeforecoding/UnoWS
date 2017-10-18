module CommandHandler

[<Struct>]
type StreamId = StreamId of string

[<Struct>]
type EventNumber = EventNumber of int64
    with
    static member Start = EventNumber 0L


type Slice<'t> =
    | Done of 't list * EventNumber
    | Continue of 't list * EventNumber

type Read<'t> = StreamId -> EventNumber -> Slice<'t> Async



open Game
let handler (read: _ Read) append stream command =
    let rec load initialState startVersion =
        async {
            let! slice = read stream startVersion
            match slice with
            | Done(events, finalVersion) ->
                let finalState = List.fold evolve initialState events
                return finalState, finalVersion
            | Continue(events, nextVersion) ->
                let intermediateState = List.fold evolve initialState events
                return! load intermediateState nextVersion
        }
    async {
        let! game, version = load InitialState EventNumber.Start
        match decide command game with
        | Ok newEvents ->
            do! append stream version newEvents
            return Ok()
        | Error error -> 
            return Error error }

type Agent<'t> = MailboxProcessor<'t>

module EventNumber =
    let dist (EventNumber x) (EventNumber y) = x - y  

let game read append loadSnapshot writeSnapshot stream =
    Agent.Start (fun mailbox ->
        let rec loop state version snapshotVersion =
            async {
                let! command, reply = mailbox.Receive()
                let newEvents = decide command state
                match newEvents with
                |Ok newEvents ->
                    let! newVersion = append stream version newEvents
                    let newState = List.fold evolve state newEvents
                    reply (Ok())

                    let newSnapshotVersion =
                        if EventNumber.dist newVersion snapshotVersion > 1000L then
                            writeSnapshot "snap-game" newState newVersion
                            newVersion
                        else
                            snapshotVersion

                    return! loop newState newVersion newSnapshotVersion
                | Error err ->
                    reply (Error err)
                    return! loop state version snapshotVersion    }
        let rec load initialState startVersion =
            async {
                let! slice = read stream startVersion
                match slice with
                | Done(events, finalVersion) ->
                    let finalState = List.fold evolve initialState events
                    return finalState, finalVersion
                | Continue(events, nextVersion) ->
                    let intermediateState = List.fold evolve initialState events
                    return! load intermediateState nextVersion }
        async {
            let! snapshotState, snapshotVersion = loadSnapshot "snap-game"
            let! state, version = load snapshotState snapshotVersion
            return! loop state version snapshotVersion }) 

// load InitialState 0 |> writeSnapshot "..."

// let game1 = game x x "Game-1"
// async {
//     let! result = game1.PostAndAsyncReply(fun channel -> StartGame { Players = 4; FirstCard = Digit(Five,Red) }, channel.Reply)
//     let! otherResult =
//         game1.PostAndAsyncReply (fun channel -> PlayCard { Player = 1; Card = Digit(Three,Red) }, channel.Reply)
// } |> Async.Start


module EventStore =
    open EventStore.ClientAPI // from nuget: EventStore.Client
    let read (store:IEventStoreConnection) (StreamId stream) (EventNumber version) =
        async {
            let! slice =
                store.ReadStreamEventsForwardAsync(stream, version, 1000, true )
                |> Async.AwaitTask

            let events =
                slice.Events
                |> Array.toList
                |> List.collect(fun event -> 
                    let eventType = event.Event.EventType
                    let data = System.Text.Encoding.UTF8.GetString(event.Event.Data)
                    Serialisation.GameEvents.deserialize(eventType, data)
                )

            if slice.IsEndOfStream then
                return Done( events , EventNumber slice.LastEventNumber)
            else
                return Continue( events , EventNumber slice.NextEventNumber)
        }
    let append (store: IEventStoreConnection) (StreamId stream) (EventNumber expectedVersion) (events: Event list) =
        async {
            let eventData =
                events
                |> List.map(fun event -> 
                    let eventType, data = Serialisation.GameEvents.serialize event
                    EventData(
                        System.Guid.NewGuid(),
                        eventType,
                        true,
                        System.Text.Encoding.UTF8.GetBytes(data),
                        null))
                |> List.toArray
            
                
            let! result =
                store.AppendToStreamAsync(stream,expectedVersion,null,eventData)
                |> Async.AwaitTask
            //result.NextExpectedVersion
            return()
        }
