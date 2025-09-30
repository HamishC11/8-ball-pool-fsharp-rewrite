namespace MyGame
open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    // these are assets from the Gui package. Note that we don't actually have any assets here yet, but they can be
    // added to the existing package at your leisure!
    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"

        let backButtonImage = asset<Image> PackageName "back_button"
        let backButtonHoverImage = asset<Image> PackageName "back_button_hover"
        let continuebuttonImage2 = asset<Image> PackageName "continue_button"
        let continuebuttonHoverImage = asset<Image> PackageName "continue_button_hover"

        let singleplayerButtonImage = asset<Image> PackageName "1_player_button"
        let singleplayerButtonHoverImage = asset<Image> PackageName "1_player_button_hover"
        let multiplayerButtonImage = asset<Image> PackageName "2_players_button"
        let multiplayerButtonHoverImage = asset<Image> PackageName "2_players_button_hover"


        let playerVsPlayer = asset<Image> PackageName "2_players_button"
        let playerVsPlayerHover = asset<Image> PackageName "2_players_button_hover"
        let playerVsComputer = asset<Image> PackageName "1_player_button"
        let playerVsComputerHover = asset<Image> PackageName "1_player_button_hover"
        let continuebuttonImage = asset<Image> PackageName "continue_button"
        let controlsImage = asset<Image> PackageName "controls"

        let backgroundFont = asset<Font> PackageName "impact"



    // these are assets from the Gui package. Also no assets here yet.
    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"

        let cueImage = asset<Image> PackageName "spr_stick"
        let poolTable1 = asset<Image> PackageName "spr_background4"
        let cueBallImage = asset<Image> PackageName "spr_ball2"
        let redBallImage = asset<Image> PackageName "spr_redBall2"
        let yellowBallImage = asset<Image> PackageName "spr_yellowBall2"
        let blackBallImage = asset<Image> PackageName "spr_blackBall2"
        let pauseImage = asset<Image> PackageName "main_menu_background"
        let continuebuttonImage = asset<Image> PackageName "continue_button"