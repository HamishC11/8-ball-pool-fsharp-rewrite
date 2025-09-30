namespace MyGame
open System
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    // splash screen
    let Splash = Game / "Splash"

    // title screen
    let Title = Game / "Title"
    let TitleGui = Title / "Gui"
    let TitleSingleplayer = TitleGui / "Singleplayer"
    let TitleMultiplayer = TitleGui / "Multiplayer"
    let TitleExit = TitleGui / "Exit"

    // credits screen
    let Credits = Game / "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsBack = CreditsGui / "Back"

    // Controls
    let Controls = Game / "Controls"
    let ControlsGui = Controls / "Gui"
    let ControlsStart = Controls / "Start"

    // gameplay screen
    let Gameplay = Game / "Gameplay"
    let GameplayGui = Gameplay / "Gui"
    let GameplayQuit = GameplayGui / "Quit"
    let GameplayScene = Gameplay / "Scene"