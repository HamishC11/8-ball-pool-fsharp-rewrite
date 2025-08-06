namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame
open MyGame.Constants

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

type Cue =
    {Position : Vector3
     Size : Vector3 
     Rotation : Quaternion 
     Power : single}

    static member initial =
        { Position = v3 0.0f -100.0f 0.0f
          Size = v3 400.0f 9.382f 0.0f
          Rotation = Quaternion(0.0f, 0.0f, 0.0f, 0.0f)
          Power = 1.0f}

type BallType = Cue | Red | Yellow | Black

type Ball =
    {Position : Vector3
     Size : Vector3
     Velocity : Vector3
     Type : BallType
     IsMoving : Boolean}

    static member make (position: Vector3) (balltype: BallType) = 
        { Position = position
          Size = v3 20.0f 20.0f 0.0f
          Velocity = v3 0.0f 0.0f 0.0f
          Type = balltype
          IsMoving =  false }

    member this.positionNext =
        this.Position + this.Velocity


        

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState 
      Cue : Cue
      Balls : Map<String, Ball>}

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quit 
          Cue = Cue.initial 
          Balls = Map.empty}

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =

        // generate balltype order
        let ballTypes =
            [ yield! List.replicate 7 BallType.Red
              yield! List.replicate 7 BallType.Yellow
              yield BallType.Black ]

        // use position generator
        let trianglePositions = generateTrianglePositions ()

        //create balls, mapping each one to the triangle
        let triangleBalls =
            List.mapi (fun i pos ->
                let id = "Ball " + string i
                let ballType = List.item i ballTypes
                (id, Ball.make pos ballType)
            ) trianglePositions
 
        let cueBall = ("Cue Ball", Ball.make(v3 -144.5f 0.0f 0.0f) BallType.Cue)

        let balls =
            cueBall :: triangleBalls
            |> Map.ofList
            
        { Gameplay.empty with
            GameplayState = Playing
            Balls = balls}

    static member update gameplay world = 
        match gameplay.GameplayState with
        | Playing ->
            //update cue
            let gameplay =
                let cue = gameplay.Cue
                let cueBall = gameplay.Balls.["Cue Ball"]
                let mousePos = World.getMousePosition2dScreen world
                let cueBallPos2D = v2 cueBall.Position.X cueBall.Position.Y

                //direction of mouse relative to cueball
                let direction2D = Vector2.Normalize(mousePos - cueBallPos2D)

                //offset cue to cueball (account for power)
                let cuePos2D = (cueBallPos2D + (direction2D * (-220.0f + -cue.Power)))

                //get angle of direction vector
                let angleRad = atan2 direction2D.Y direction2D.X
                //convert to qauternion for Nu rotation
                let rotationQuat = Quaternion.CreateFromAxisAngle(Vector3.UnitZ, angleRad)

                let cue =
                    // handle all cue movements when no balls are moving
                    if not cueBall.IsMoving then
                        let cue = 
                            // min max func
                            let clampedPower power = min MaxPower (max MinPower power)
                            if World.isKeyboardKeyDown KeyboardKey.S world then
                                { cue with Power = clampedPower (cue.Power - 1.0f) }
                            elif World.isKeyboardKeyDown KeyboardKey.W world then
                                { cue with Power = clampedPower (cue.Power + 1.0f) }
                            else cue
                        { cue with 
                            Position = v3 cuePos2D.X cuePos2D.Y cueBall.Position.Z 
                            Rotation = rotationQuat}
                    else
                        
                        let cue =
                            {cue with
                                Power = Cue.initial.Power}
                        cue
                { gameplay with Cue = cue}

            //update cueball movement
            let gameplay =
                // update pos
                let cueBall = gameplay.Balls.["Cue Ball"]

                // Mover
                let mousePos = World.getMousePosition2dScreen world
                let cueBallPos2D = v2 cueBall.Position.X cueBall.Position.Y
                //direction of mouse relative to cueball
                let direction2D = Vector2.Normalize(mousePos - cueBallPos2D)
                let cueBall =
                    if cueBall.Velocity.X < 0.1f && cueBall.Velocity.X > -0.1f then
                        if World.isMouseButtonClicked MouseButton.MouseLeft world then
                            // Launch in cue direction
                            { cueBall with 
                                Velocity = (v3 direction2D.X direction2D.Y 0.0f) * gameplay.Cue.Power
                                IsMoving = true}

                        else
                            {cueBall with IsMoving = false}
                    else
                        cueBall

                //update Ball list
                let updatedBalls = Map.add "Cue Ball" cueBall gameplay.Balls
                { gameplay with Balls = updatedBalls}

            //ball movement
            let gameplay =
                let balls = Map.toList gameplay.Balls
                
                let updatedBallList =
                    [ for ballId, ball in balls do
                        let updatedBall = 
                            {ball with
                                //update pos
                                Position = ball.positionNext
                                //friction
                                Velocity = ball.Velocity * FrictionFactor}
                        (ballId, updatedBall) ]
                //update Ball list
                let updatedBalls = Map.ofList updatedBallList
                { gameplay with Balls = updatedBalls }


            //handle wall collision
            let gameplay =
                let balls = Map.toList gameplay.Balls
                let updatedBallList =
                    [ for ballId, ball in balls do
                        // long walls
                        let velocityX =
                            if ball.positionNext.X <= -285.5f || ball.positionNext.X >= 286.5f then
                                ball.Velocity.MapX negate
                            else ball.Velocity

                        // short walls
                        let velocityY =
                            if ball.positionNext.Y <= -145.5f || ball.positionNext.Y >= 145.5f then
                                velocityX.MapY negate
                            else velocityX

                        let updatedBall = { ball with Velocity = velocityY }
                        (ballId, updatedBall)
                    ]
                
                //update Ball list
                let updatedBalls = Map.ofList updatedBallList
                { gameplay with Balls = updatedBalls }

            
            //collision
            let handleCollision (ballA: Ball) (ballB: Ball) =
                let distance = Vector3.Distance(ballA.Position, ballB.Position)
                if distance <= BallRadius then
                    // direction from between balls
                    let dir = Vector3.Normalize(ballA.Position - ballB.Position)
                    let relVel = ballA.Velocity - ballB.Velocity
                    // projected combined speed of balls
                    let speed = Vector3.Dot(dir, relVel)
                    let impulse = dir * speed
                    // distribute impulse
                    let vA' = ballA.Velocity - impulse
                    let vB' = ballB.Velocity + impulse

                    // overlap correction
                    let overlap = BallRadius - distance
                    let correction = dir * (overlap / 2.0f)
                    let newPosA = ballA.Position + correction
                    let newPosB = ballB.Position - correction

                    { ballA with 
                        Velocity = vA'
                        Position = newPosA},
                    { ballB with 
                        Velocity = vB'
                        Position = newPosB}
                else
                    ballA, ballB

            //ball collision
            let gameplay = 
                let balls = Map.toList gameplay.Balls
                let totalBalls = List.length balls

                let mutable ballMap = Map.ofList balls


                // iterate over every ball in list
                for i in 0 .. totalBalls - 1 do
                    //get current ball
                    let (ballIdA, _ballAOriginal) = List.item i balls
                    let ballA = Map.find ballIdA ballMap
                    let mutable updatedBallA = ballA

                    // iterate over all other balls
                    for k in i + 1 .. totalBalls - 1 do
                        let (ballIdB, _) = List.item k balls
                        let ballB = Map.find ballIdB ballMap

                        let (newA, newB) = handleCollision updatedBallA ballB

                        // update both in the map
                        updatedBallA <- newA
                        ballMap <- ballMap |> Map.add ballIdB newB

                    // update ballA after all interactions
                    ballMap <- ballMap |> Map.add ballIdA updatedBallA

                { gameplay with Balls = ballMap }
                            

            // pocketing
            let gameplay =
                let cueBall = gameplay.Balls.["Cue Ball"]

                let cueBall =
                    if IsInsideHole(cueBall.Position) then
                        {cueBall with 
                            Position = (v3 -144.0f 0.0f 0.0f)
                            Velocity = (v3 0.0f 0.0f 0.0f) }
                    else
                        cueBall

                //update Ball list
                let updatedBalls = Map.add "Cue Ball" cueBall gameplay.Balls
                { gameplay with Balls = updatedBalls }
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
                     Entity.Elevation == 2.0f
                     Entity.Rotation := gameplay.Cue.Rotation]
                 Content.staticSprite "PoolTable"
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Size == v3 640f 360f 0.0f
                     Entity.StaticImage == Assets.Gameplay.poolTable1]
                 for (ballId, ball) in Map.toList gameplay.Balls do
                    Content.staticSprite ballId
                        [Entity.Position := ball.Position
                         Entity.Size == ball.Size
                         Entity.Elevation == 1.0f
                         if ball.Type = BallType.Red then
                            Entity.StaticImage == Assets.Gameplay.redBallImage
                         elif ball.Type = BallType.Yellow then
                            Entity.StaticImage == Assets.Gameplay.yellowBallImage
                         elif ball.Type = BallType.Black then
                            Entity.StaticImage == Assets.Gameplay.blackBallImage
                         elif ball.Type = BallType.Cue then
                            Entity.StaticImage == Assets.Gameplay.cueBallImage]]


         // the gui group
         Content.group Simulants.GameplayGui.Name []

            [// quit
             Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -200.0f 0.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]]
