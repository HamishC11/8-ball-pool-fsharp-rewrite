namespace MyGame
open System
open Nu

// this module contains our user-defined events.
[<RequireQualifiedAccess>]
module Events =

    // event raised by Gameplay screen that lets the game know its time to go back to the title screen
    let QuitEvent = stoa<unit> "Quit/Event"

    // event raised by Controls screen that lets the game know its time to start gameplay
    let StartGameEvent = stoa<unit> "StartGame/Event"

    // events for starting singleplayer and multiplayer games
    let StartSingleplayerGame = stoa<unit> "StartSingleplayerGame/Event"
    let StartMultiplayerGame = stoa<unit> "StartMultiplayerGame/Event"