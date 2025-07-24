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

type CueBall =
    {Position : Vector3
     Size : Vector3
     Velocity : Vector3
     MovingCheck : Boolean
     }

     static member initial =
        { Position = v3 -144.5f 0.0f 0.0f
          Size = v3 25.0f 25.0f 0.0f
          Velocity = v3 0.0f 0.0f 0.0f
          MovingCheck =  false}

     member this.positionNext =
        this.Position + this.Velocity

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState 
      Cue : Cue 
      CueBall : CueBall}

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quit 
          Cue = Cue.initial 
          CueBall = CueBall.initial}

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        { Gameplay.empty with
            GameplayState = Playing }

    static member update gameplay world = 
        match gameplay.GameplayState with
        | Playing ->

            //update cue
            let gameplay =
                let cueBall = gameplay.CueBall
                //let mousePos = World.getMousePosition world

                //direction of mouse relative to cueball
                // idk crap bruh this hard as hell im gonna need to think about it. Maybe create a travelable circle around the cueball that the cue follows? position determined by mousePos idk man
                //let direction = Vector2.Normalize(mousePos - (v2 cueBall.Position.X cueball.Position.Y)

                let cue =
                    let cue = gameplay.Cue
                    if not cueBall.MovingCheck then
                        // Keep Cue behind Cueball
                        { cue with Position = v3 (cueBall.Position.X - 220.0f) cueBall.Position.Y cueBall.Position.Z }
                    else
                        cue
                { gameplay with Cue = cue}

            //update cueball movement
            let gameplay =
                // update pos
                let cueBall = gameplay.CueBall
                let cueBall = 
                    { cueBall with Position = cueBall.positionNext }

                // friction
                let friction = 0.98f
                let cueBall = {cueBall with Velocity = cueBall.Velocity * friction}

                // placeholder mover
                let cueBall =
                    if cueBall.Velocity.X < 0.1f && cueBall.Velocity.X > -0.1f then
                        if World.isKeyboardKeyDown KeyboardKey.Space world then
                            // Launch in random direction
                            { cueBall with 
                                Velocity = (v3 (0.5f - Gen.randomf) (0.5f - Gen.randomf) 0.0f) * 40.0f 
                                MovingCheck = true }
                        else
                            {cueBall with MovingCheck = false}
                    else
                        cueBall

                { gameplay with CueBall = cueBall }

            //handle wall collision
            let gameplay =
                let cueBall = gameplay.CueBall
                let cueBall =
                    // short walls
                    if cueBall.positionNext.X <= -285.5f || cueBall.positionNext.X >= 286.5f then
                        { cueBall with Velocity = cueBall.Velocity.MapX negate}
                    else cueBall
                let cueBall =
                    // long walls
                    if cueBall.positionNext.Y <= -145.5f || cueBall.positionNext.Y >= +145.5f then
                        { cueBall with Velocity = cueBall.Velocity.MapY negate}
                    else cueBall
                {gameplay with CueBall = cueBall}
            //end
            gameplay

        | Quit -> gameplay

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | Update
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
         Screen.UpdateEvent => Update
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

        | Update ->
            let gameplay = Gameplay.update gameplay world
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
                     Entity.StaticImage == Assets.Gameplay.cueImage
                     Entity.Elevation == 2.0f]
                 Content.staticSprite "PoolTable"
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Size == v3 640f 360f 0.0f
                     Entity.StaticImage == Assets.Gameplay.poolTable1]
                 Content.staticSprite "CueBall"
                    [Entity.Position := gameplay.CueBall.Position
                     Entity.Size == gameplay.CueBall.Size
                     Entity.StaticImage == Assets.Gameplay.cueBallImage
                     Entity.Elevation == 1.0f]]

         // the gui group
         Content.group Simulants.GameplayGui.Name []

            [// quit
             Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -200.0f 0.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]]
