[<AutoOpen>]
module Prelude

type Result<'t,'e> =
    | Ok of 't
    | Error of 'e

