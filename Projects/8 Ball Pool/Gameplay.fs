namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

type Cue =
    {Position : Vector3
     Size : Vector3 }

    static member initial =
        { Position = v3 0.0f -100.0f 0.0f
          Size = v3 400.0f 9.382f 0.0f}

type Ball =
    {Position : Vector3
     Size : Vector3
     Velocity : Vector3
     }

     static member initial =
        { Position = v3 0.0f 0.0f 0.0f
          Size = v3 25.0f 25.0f 0.0f
          Velocity = v3 0.0f 0.0f 0.0f}

     member this.positionNext =
        this.Position + this.Velocity

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState 
      Cue : Cue 
      Ball : Ball}

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quit 
          Cue = Cue.initial 
          Ball = Ball.initial}

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        { Gameplay.empty with
            GameplayState = Playing }

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.TimeUpdateEvent => TimeUpdate]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | StartPlaying ->
            let gameplay = Gameplay.initial
            just gameplay

        | FinishQuitting ->
            let gameplay = Gameplay.empty
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =
        match command with
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world

    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, _) =

        [// the scene group while playing
         if gameplay.GameplayState = Playing then
            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Multiplayer.nugroup" []
                [Content.staticSprite "Cue"
                    [Entity.Position := gameplay.Cue.Position
                     Entity.Size == gameplay.Cue.Size
                     Entity.StaticImage == Assets.Gameplay.cueImage]
                 Content.staticSprite "PoolTable"
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Size == v3 640f 360f 0.0f
                     Entity.StaticImage == Assets.Gameplay.poolTable1]
                 Content.staticSprite "CueBall"
                    [Entity.Position := v3 -144.5f 0.0f 0.0f
                     Entity.Size == gameplay.Ball.Size
                     Entity.StaticImage == Assets.Gameplay.cueBallImage]]

         // the gui group
         Content.group Simulants.GameplayGui.Name []

            [// quit
             Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -200.0f 0.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]]
