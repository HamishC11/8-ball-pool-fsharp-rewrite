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
    | Paused
    | Controls
    | Quit

type Mode = Singleplayer | Multiplayer

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

type BallType = Cue | Red | Yellow | Black | Null

type Ball =
    {Position : Vector3
     Size : Vector3
     Velocity : Vector3
     Type : BallType
     IsMoving : Boolean
     Pocketed : Boolean}

    static member make (position: Vector3) (balltype: BallType) = 
        { Position = position
          Size = GlobalBallSize
          Velocity = v3 0.0f 0.0f 0.0f
          Type = balltype
          IsMoving = false
          Pocketed = false}

    member this.positionNext =
        this.Position + this.Velocity

type Player =
    { Name : string
      Score : int
      Colour : BallType
      WinCount : int}

    static member initial name =
        { Name = name
          Score = 0 
          Colour = BallType.Null
          WinCount = 0}


        

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      GameMode : Mode
      Cue : Cue
      Balls : Map<String, Ball>
      Turn : string
      NextTurn : string
      TurnPlayed : bool
      Aiming : bool
      Player1 : Player
      Player2 : Player
      AITurn : bool
      FirstHit : BallType
      BallPocketed : bool
      lastBallPocketed : Ball
      AITurnCooldown : int
      ResumeTimer : int64}

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quit
          GameMode = Mode.Singleplayer
          Cue = Cue.initial
          Balls = Map.empty
          Player1 = Player.initial "Player 1"
          Player2 = Player.initial "Player 2"
          AITurn = true
          Turn = "P1"
          NextTurn = ""
          TurnPlayed = true
          Aiming = true
          FirstHit = BallType.Null
          BallPocketed = false
          lastBallPocketed = Ball.make (v3 0.0f 0.0f 0.0f) BallType.Null
          AITurnCooldown = 0
          ResumeTimer = 0L}

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        // generate balltype order
        let ballTypes =
            [yield! List.replicate 2 BallType.Red
             yield! List.replicate 2 BallType.Yellow
             yield BallType.Black
             yield! List.replicate 2 BallType.Red
             yield BallType.Yellow
             yield BallType.Red
             yield! List.replicate 2 BallType.Yellow
             yield BallType.Red
             yield BallType.Yellow
             yield BallType.Red
             yield BallType.Yellow]

        // use position generator
        let trianglePositions = generateTrianglePositions ()

        //create balls, mapping each one to the triangle
        let triangleBalls =
            List.mapi (fun i pos ->
                let id = "Ball " + string i
                let ballType = List.item i ballTypes
                (id, Ball.make pos ballType)
            ) trianglePositions
 
        let cueBall = ("Cue Ball", Ball.make cueBallStartingPos BallType.Cue)

        let balls =
            cueBall :: triangleBalls
            |> Map.ofList
            
        { Gameplay.empty with
            GameplayState = Controls
            Balls = balls}

    static member update gameplay world = 
        match gameplay.GameplayState with
        | Playing | Paused | Controls ->
            let ballsWereMoving = gameplay.Aiming
            //update cue
            let gameplay =
                let cue = gameplay.Cue
                let cueBall = gameplay.Balls.["Cue Ball"]
                let mousePos = World.getMousePosition2dScreen world
                let cueBallPos2D = v2 cueBall.Position.X cueBall.Position.Y
                let aiming = gameplay.Aiming

                // Handle pause toggle
                let gameplay =
                    if World.isKeyboardKeyDown KeyboardKey.Q world then
                        if gameplay.GameplayState = Playing then
                            { gameplay with GameplayState = Paused }
                        else
                            gameplay
                    else
                        gameplay


                //direction of mouse relative to cueball
                let direction2D = Vector2.Normalize(mousePos - cueBallPos2D)

                //offset cue to cueball (account for power)
                let cuePos2D = (cueBallPos2D + (direction2D * (-220.0f + -cue.Power)))

                //get angle of direction vector
                let angleRad = atan2 direction2D.Y direction2D.X
                //convert to quaternion for Nu rotation
                let rotationQuat = Quaternion.CreateFromAxisAngle(Vector3.UnitZ, angleRad)

                let cue =
                    // handle all cue movements when no balls are moving and game is not paused
                    if aiming && gameplay.GameplayState = Playing then
                        if gameplay.AITurn && gameplay.GameMode = Mode.Singleplayer then
                            // Hide cue during AI turn
                            let offscreenPos = v2 999.0f 999.0f
                            { cue with Position = v3 offscreenPos.X offscreenPos.Y cueBall.Position.Z }
                        else
                            let clampedPower power = min MaxPower (max MinPower power)
                            let cueWithPower =
                                if World.isKeyboardKeyDown KeyboardKey.S world then
                                    { cue with Power = clampedPower (cue.Power - PowerIncrement) }
                                elif World.isKeyboardKeyDown KeyboardKey.W world then
                                    { cue with Power = clampedPower (cue.Power + PowerIncrement) }
                                else cue
                            { cueWithPower with 
                                Position = v3 cuePos2D.X cuePos2D.Y cueBall.Position.Z
                                Rotation = rotationQuat }
                    else
                        // Reset cue power when not aiming
                        { cue with Power = Cue.initial.Power }

                { gameplay with Cue = cue}

            //update cueball movement
            let gameplay =
                let cueBall = gameplay.Balls.["Cue Ball"]
                let aiming = gameplay.Aiming
                let cueBall, AIcooldown =
                    // Human Shoot Calc
                    let mousePos = World.getMousePosition2dScreen world
                    let cueBallPos2D = v2 cueBall.Position.X cueBall.Position.Y
                    //direction of mouse relative to cueball
                    let direction2D = Vector2.Normalize(mousePos - cueBallPos2D)

                    match gameplay.GameMode, gameplay.Turn with
                    | Multiplayer, _ ->
                        if aiming then
                            if World.isMouseButtonClicked MouseButton.MouseLeft world && gameplay.GameplayState = Playing then
                                // Launch in cue direction
                                { cueBall with 
                                    Velocity = (v3 direction2D.X direction2D.Y 0.0f) * gameplay.Cue.Power
                                    IsMoving = true}, 0

                            else
                                {cueBall with IsMoving = false}, 0
                        else
                            cueBall, 0
                    | Singleplayer, "P1" when aiming ->
                        if World.isMouseButtonClicked MouseButton.MouseLeft world && gameplay.GameplayState = Playing then
                            { cueBall with 
                                Velocity = (v3 direction2D.X direction2D.Y 0.0f) * gameplay.Cue.Power
                                IsMoving = true }, DefAIcooldown
                        else
                            {cueBall with IsMoving = false}, DefAIcooldown

                    | Singleplayer, "P2" when aiming ->
                        if gameplay.AITurnCooldown <= 0 && gameplay.GameplayState = Playing then
                            //AI Shoot Calc
                            let targetBalls =
                                gameplay.Balls
                                |> Map.toList
                                |> List.map snd
                                |> List.filter (fun b -> b.Type = gameplay.Player2.Colour)

                            let target =
                                if targetBalls.Length > 0 then // ai suit on the table
                                    targetBalls
                                    |> List.minBy (fun ball ->
                                        Vector3.Distance(cueBall.Position, ball.Position)) // nearest ball of AI's suit
                                else
                                    if gameplay.Player2.Colour = BallType.Null then // suit unselected
                                        gameplay.Balls
                                        |> Map.toList
                                        |> List.map snd
                                        |> List.filter (fun b -> b.Type <> BallType.Cue)
                                        |> List.minBy (fun ball ->
                                        Vector3.Distance(cueBall.Position, ball.Position)) // nearest ball overall
                                    else 
                                        gameplay.Balls.["Ball 4"] //all ai suit pocketed - target black ball

                            // direction cueball -> target
                            let direction = target.Position - cueBall.Position
                            let variationVector = (v2 (Gen.randomf - 0.5f) (Gen.randomf - 0.5f)) * VariationFactor // create a random vector we'll add to the direct one to give some variation
                            let direction2D = Vector2.Normalize(Vector2(direction.X, direction.Y)) + variationVector

                            // Launch in direction
                            { cueBall with 
                                Velocity = (v3 direction2D.X direction2D.Y 0.0f) * (Gen.randomf + 0.1f) * (MaxPower - (MinPower + 20.0f)) //random power
                                IsMoving = true }, DefAIcooldown
                        else
                            // decrease cooldown
                            cueBall, gameplay.AITurnCooldown - 1
                            
                    | _ -> cueBall, gameplay.AITurnCooldown
                //update Ball list
                let updatedBalls = Map.add "Cue Ball" cueBall gameplay.Balls
                { gameplay with 
                    Balls = updatedBalls
                    AITurnCooldown = AIcooldown}

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

                //outer rec function to process every balls interaction with another ball
                let rec processBalls i (ballMap, firstHit) =
                    match i with
                    | index when index >= totalBalls -> ballMap, firstHit// end once gone through all balls
                    | _ ->  
                        let (ballIdA, _) = List.item i balls
                        let ballA = Map.find ballIdA ballMap

                        // inner rec function to handle collisions between remaining balls
                        let rec processCollisions k updatedA updatedMap firstHitSoFar =
                            match k with
                            | index when index >= totalBalls -> updatedA, updatedMap, firstHitSoFar 
                            | _ ->
                                let (ballIdB, _) = List.item k balls
                                let ballB = Map.find ballIdB updatedMap

                                let (newA, newB) = handleCollision updatedA ballB
                                let distance = Vector3.Distance(updatedA.Position, ballB.Position)
                                //store firstballhit
                                let newFirstHit =
                                    if distance <= BallRadius then
                                            if firstHitSoFar = BallType.Null then
                                                if ballIdA = "Cue Ball" then ballB.Type
                                                elif ballIdB = "Cue Ball" then ballA.Type
                                                else firstHitSoFar
                                            else firstHitSoFar
                                    else firstHitSoFar

                                let updatedMap = updatedMap |> Map.add ballIdB newB
                                processCollisions (k + 1) newA updatedMap newFirstHit

                        let (finalA, updatedMap, firstHitAfter) =
                            processCollisions (i + 1) ballA ballMap firstHit

                        let updatedMap = updatedMap |> Map.add ballIdA finalA
                        processBalls (i + 1) (updatedMap, firstHitAfter)

                // call func on balls
                let updatedBalls, newFirstHit = processBalls 0 (gameplay.Balls, gameplay.FirstHit)

                { gameplay with 
                    Balls = updatedBalls
                    FirstHit = newFirstHit }

            //handle pocketing and turns
            let gameplay =
                // accumulator for fold
                let initAcc = 
                    ( Map.empty<string, Ball>,                     // updated balls
                      Ball.make (v3 0.0f 0.0f 0.0f) BallType.Null, // last ball pocketed in this frame
                      gameplay.Player1.Score,                     // p1 score
                      gameplay.Player2.Score,                     // p2 score
                      gameplay.Turn,                              // turn
                      gameplay.BallPocketed,                       // pocketed
                      gameplay.Player1.Colour,                     //p1 ball colour
                      gameplay.Player2.Colour,                     //p2 ball colour
                      gameplay.AITurn)                            //AI turn check

                // fold over all balls
                let updatedBalls, lastPocketedThisFrame, score1, score2, turn, pocketed, p1colour, p2colour, aIturn =
                    gameplay.Balls
                    |> Map.fold (fun (ballsMap, last, p1, p2, currentTurn, pocketed, p1colour, p2colour, aIturn) key ball ->
                        if IsInsideHole ball.Position then
                            // set players colours if not set yet
                            let p1Col, p2Col =
                                if currentTurn = "P1" && gameplay.Player1.Colour = BallType.Null then
                                    if ball.Type = BallType.Red then
                                       ball.Type, BallType.Yellow
                                    elif ball.Type = BallType.Yellow then
                                       ball.Type, BallType.Red
                                    else BallType.Null, BallType.Null
                                elif currentTurn = "P2" && gameplay.Player2.Colour = BallType.Null then
                                    if ball.Type = BallType.Red then
                                       BallType.Yellow, ball.Type
                                    elif ball.Type = BallType.Yellow then
                                       BallType.Red, ball.Type
                                    else BallType.Null, BallType.Null
                                else gameplay.Player1.Colour, gameplay.Player2.Colour
                            let newP1, newP2, newTurn, newAIturn =
                                // handle score/turn updates for pocketed balls
                                 match currentTurn with
                                    //p1
                                    | "P1" when ball.Type = p1Col -> p1 + 1, p2, currentTurn, false 
                                    | "P1" when ball.Type = p2Col -> p1, p2 + 1, "BallInHandP2", true // account for balls pocketed on wrong turn
                                    | "P1" when ball.Type = BallType.Cue -> p1, p2, "BallInHandP2", true
                                    | "P1" when ball.Type = BallType.Black && p1 < 7 -> p1, p2, "P2Win", false //p1 loss
                                    | "P1" when ball.Type = BallType.Black && p1 = 7 -> p1, p2, "P1Win", false //p1 win
                                    //p2
                                    | "P2" when ball.Type = p2Col -> p1, p2 + 1, currentTurn, true
                                    | "P2" when ball.Type = p1Col -> p1 + 1, p2, "BallInHandP1", false // account for balls pocketed on wrong turn
                                    | "P2" when ball.Type = BallType.Cue -> p1, p2, "BallInHandP1", false
                                    | "P2" when ball.Type = BallType.Black && p2 < 7 -> p1, p2, "P1Win", false //p2 loss
                                    | "P2" when ball.Type = BallType.Black && p2 = 7 -> p1, p2, "P2Win", false //p1 win

                                    | _ -> p1, p2, currentTurn, aIturn

                            // pocket all coloured balls and reset cueball
                            let updatedBall =
                                if ball.Type <> BallType.Cue then
                                    { ball with Pocketed = true }
                                else
                                    { ball with Position = cueBallStartingPos; Velocity = v3 0.0f 0.0f 0.0f }

                            // update acc (lastPocketed = updatedBall)
                            (Map.add key updatedBall ballsMap, updatedBall, newP1, newP2, newTurn, true, p1Col, p2Col, newAIturn)
                        else
                            (Map.add key ball ballsMap, last, p1, p2, currentTurn, pocketed, p1colour, p2colour, aIturn)
                    ) initAcc
    

                // remove pocketed balls
                let remainingBalls = updatedBalls |> Map.filter (fun _ ball -> not ball.Pocketed)

                // update gameplay
                { gameplay with
                    Balls = remainingBalls
                    Player1 = { gameplay.Player1 with 
                                    Score = score1
                                    Colour = p1colour}
                    Player2 = { gameplay.Player2 with 
                                    Score = score2
                                    Colour = p2colour}
                    NextTurn = turn
                    lastBallPocketed = lastPocketedThisFrame
                    BallPocketed = pocketed 
                    AITurn = aIturn
                    }

            // handle fouls (ball in hand)
            let gameplay =
                match gameplay.Turn with
                | "BallInHandP1" | "BallInHandP2" ->
                    let player = if gameplay.Turn = "BallInHandP1" then "P1" else "P2"

                    let mousePos = World.getMousePosition2dScreen world
                    let desiredPos = v3 mousePos.X mousePos.Y 0.0f

                    // check if within bounds of the table
                    let withinBounds =
                        desiredPos.X >= -285.0f && desiredPos.X <= 285.0f &&
                        desiredPos.Y >= -145.0f && desiredPos.Y <= 145.0f

                    // check not overlapping another ball
                    let overlapping =
                        gameplay.Balls
                        |> Map.exists (fun id ball ->
                            id <> "Cue Ball" &&
                            not ball.Pocketed &&
                            Vector3.Distance(desiredPos, ball.Position) < BallRadius * 1.2f)

                    // only update if valid
                    let cueBall = gameplay.Balls.["Cue Ball"]
                    let updatedCueBall =
                        if withinBounds && not overlapping then
                            { cueBall with Position = desiredPos; Velocity = v3 0.0f 0.0f 0.0f }
                        else
                            cueBall

                    // commit placement on click
                    let nextTurn =
                        if World.isMouseButtonClicked MouseButton.MouseLeft world then
                            if player = "P1" then "P2" else "P1"
                        else
                            gameplay.Turn

                    { gameplay with
                        Balls = Map.add "Cue Ball" updatedCueBall gameplay.Balls
                        Turn = nextTurn
                        Aiming = (nextTurn = "P1" || nextTurn = "P2") } // allow aiming only after placement
                | _ -> gameplay


            // turn determination based off fouls / pockets
            let gameplay =
                let P1 = gameplay.Player1
                let P2 = gameplay.Player2
                let newTurn, turnPlayed, AIturn =
                    if gameplay.TurnPlayed then
                        if gameplay.NextTurn = gameplay.Turn then // if predetermined balls from pocketed ball didnt change, run firsthit calculations
                            match gameplay.Turn with
                            | "P1" ->
                                if P1.Colour <> BallType.Null && gameplay.FirstHit <> P1.Colour && gameplay.FirstHit <> BallType.Null then// make sure hit fouls only count after suit is decided
                                    if P1.Score < 7 then // firsthit blackball is ok if others pocketed
                                        // foul - wrong ball hit first
                                        "BallInHandP2", false, true
                                    else "P2", false, true //normal turn switch
                                elif gameplay.BallPocketed then
                                    "P1", false, false
                                else
                                    // normal turn switch
                                    "P2", false, true


                            | "P2" ->
                                if P2.Colour <> BallType.Null && gameplay.FirstHit <> P2.Colour && gameplay.FirstHit <> BallType.Null then// make sure hit fouls only count after suit is decided
                                    if P2.Score < 7 then
                                        // foul - wrong ball hit first
                                        "BallInHandP1", false, false
                                    else "P1", false, false
                                elif gameplay.BallPocketed then
                                    "P2", false, true
                                else
                                    // normal turn switch
                                    "P1", false, false

                            | _ -> gameplay.Turn, gameplay.TurnPlayed, gameplay.AITurn
                        else gameplay.NextTurn, false, gameplay.AITurn // use correct turn if balls are pocketed
                    else gameplay.Turn, gameplay.TurnPlayed, gameplay.AITurn

                let firstHit, ballPocketed =
                    if gameplay.TurnPlayed then
                        BallType.Null, false
                    else
                        gameplay.FirstHit, gameplay.BallPocketed
                { gameplay with
                    Turn = newTurn
                    TurnPlayed = turnPlayed
                    BallPocketed = ballPocketed
                    FirstHit = firstHit 
                    AITurn = AIturn}

            // handle turnPlayed
            let gameplay =
                let balls = gameplay.Balls
                let turn = gameplay.Turn

                let velocityLimit = 0.01f

                // update IsMoving for all balls based on limit
                let updatedBalls =
                    balls
                    |> Map.map (fun _ ball ->
                        let isMoving =
                            abs ball.Velocity.X >= velocityLimit ||
                            abs ball.Velocity.Y >= velocityLimit ||
                            abs ball.Velocity.Z >= velocityLimit
                        { ball with IsMoving = isMoving }
                    )

                let ballsNotMoving =
                    gameplay.Balls
                    |> Map.forall (fun _ ball -> not ball.IsMoving)

                let aiming =
                    if ballsNotMoving && (turn = "P1" || turn = "P2") then
                        true
                    else
                        false

                let ballsNotMovingThisFrame = 
                    if ballsNotMoving && not ballsWereMoving then
                        true
                    else
                        false

                let turnPlayed =
                    if ballsNotMovingThisFrame && (turn = "P1" || turn = "P2") then
                        true
                    else
                        gameplay.TurnPlayed

                // update gameplay
                { gameplay with 
                    Balls = updatedBalls
                    Aiming = aiming
                    TurnPlayed = turnPlayed}


            // win/lose
            let gameplay =
                let P1 = gameplay.Player1
                let P2 = gameplay.Player2
                if gameplay.Turn = "P1Win" then {gameplay with Player1.WinCount = P1.WinCount + 1}
                elif gameplay.Turn = "P2Win" then {gameplay with Player2.WinCount = P2.WinCount + 1}
                else gameplay
            // Handle resume timer
            let gameplay =
                if gameplay.ResumeTimer > 0L then
                    let newTimer = gameplay.ResumeTimer - 1L
                    if newTimer = 0L then
                        // Timer finished, set state to Playing directly
                        { gameplay with ResumeTimer = newTimer; GameplayState = Playing }
                    else
                        { gameplay with ResumeTimer = newTimer }
                else
                    gameplay

            //end
            gameplay

        | Quit -> gameplay


// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartSingleplayer
    | StartMultiplayer
    | FinishQuitting
    | Update
    | TimeUpdate
    | Continue
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
        [Screen.DeselectingEvent => FinishQuitting
         Screen.UpdateEvent => Update
         Screen.TimeUpdateEvent => TimeUpdate
         Events.StartSingleplayerGame => StartSingleplayer
         Events.StartMultiplayerGame => StartMultiplayer]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | StartSingleplayer -> just { Gameplay.initial with GameMode = Singleplayer }
        | StartMultiplayer -> just { Gameplay.initial with GameMode = Multiplayer }

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

        | Continue ->
            // Start the resume timer (1 second = 60 frames at 60 FPS)
            let gameplay = { gameplay with ResumeTimer = 60L }
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =
        match command with
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world

    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, _) =

        [// the scene group while playing or paused
         if gameplay.GameplayState = Playing || gameplay.GameplayState = Paused then
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
                  // load all balls
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
                             Entity.StaticImage == Assets.Gameplay.cueBallImage]
                  
                  // pause overlay when paused
                  if gameplay.GameplayState = Paused then

                       Content.staticSprite "Pause"
                        [Entity.Position == v3 0.0f 0.0f 0.0f
                         Entity.Size == v3 640f 360f 0.0f
                         Entity.StaticImage == Assets.Gameplay.pauseImage
                         Entity.Elevation == 4.0f]
   
                       // pause menu content
                       Content.text "PauseTitle"
                          [Entity.Text == "Classic 8-Ball"
                           Entity.Position == v3 -150.0f 100.0f 0.0f
                           Entity.Size == v3 300f 100f 0f
                           Entity.Elevation == 5.0f
                           Entity.Font == Assets.Default.Font]
   
                       // Continue button
                       Content.button "ContinueButton"
                          [Entity.Position == v3 -150.0f 35.0f 0.0f
                           Entity.Size == v3 177f 46f 0.0f
                           Entity.UpImage == Assets.Gui.continuebuttonImage2
                           Entity.DownImage == Assets.Gui.continuebuttonHoverImage
                           Entity.Elevation == 5.0f
                           Entity.ClickEvent => Continue]
   
                       // Singleplayer button
                       Content.button "SingleplayerButton"
                          [Entity.Position == v3 -150.0f -30.0f 0.0f
                           Entity.Size == v3 177f 46f 0.0f
                           Entity.UpImage == Assets.Gui.playerVsPlayer
                           Entity.DownImage == Assets.Gui.playerVsPlayerHover
                           Entity.Elevation == 5.0f
                           Entity.ClickEvent => StartMultiplayer]
   
                       // Multiplayer button
                       Content.button "MultiplayerButton"
                          [Entity.Position == v3 -150.0f -95.0f 0.0f
                           Entity.Size == v3 177f 46f 0.0f
                           Entity.UpImage == Assets.Gui.playerVsComputer
                           Entity.DownImage == Assets.Gui.playerVsComputerHover
                           Entity.Elevation == 5.0f
                           Entity.ClickEvent => StartSingleplayer]
   
                       // Exit button
                       Content.button "ExitButton"
                          [Entity.Position == v3 225.0f -150.0f 0.0f
                           Entity.Text == "Exit"
                           Entity.Elevation == 5.0f
                           Entity.ClickEvent => StartQuitting]]
             // the gui group
             Content.group Simulants.GameplayGui.Name []
                 [// Draw current turn
                  let ogColour = Color(0.0f, 104.0f/255.0f, 52.0f/255.0f, 1.0f)
                  let turnText = "PLAYER " + (if gameplay.Turn = "P1" then "1" else "2")
                  Content.text "TurnText"
                      [ Entity.Text := turnText
                        Entity.Position == v3 0.0f 72.0f 0.0f
                        Entity.Size == v3 100.0f 50f 0.0f
                        Entity.Elevation == 0.5f
                        Entity.TextColor := ogColour //same colour as og game
                        Entity.Font == Assets.Gui.backgroundFont
                        Entity.FontSizing == Some(25)]

                  // Draw player total scores
                  Content.text "Player1Wins"
                      [ Entity.Text := gameplay.Player1.WinCount.ToString()
                        Entity.Position == v3 30.0f 32.0f 0.0f
                        Entity.TextColor := ogColour
                        Entity.Font == Assets.Gui.backgroundFont
                        Entity.FontSizing == Some(60)
                        Entity.Size == v3 100.0f 100.0f 0.0f ]
                  Content.text "Player2Wins"
                      [ Entity.Text := gameplay.Player2.WinCount.ToString()
                        Entity.Position == v3 -29.0f 32.0f 0.0f
                        Entity.TextColor := ogColour
                        Entity.Font == Assets.Gui.backgroundFont
                        Entity.FontSizing == Some(60)
                        Entity.Size == v3 100.0f 100.0f 0.0f]
    
                 //score display
                  Content.text "ScoreP1"
                     [Entity.Text := "Player 1: " + gameplay.Player1.Score.ToString()
                      Entity.Position == v3 -144f 168.0f 0.0f]
                  Content.text "ScoreP2"
                     [Entity.Text := "Player 2: " + gameplay.Player2.Score.ToString()
                      Entity.Position == v3 130f 168.0f 0.0f]

                  // icons for turn indication
                  let iconForColour colour =
                    match colour with
                    | BallType.Red -> Assets.Gameplay.redBallImage
                    | BallType.Yellow -> Assets.Gameplay.yellowBallImage
                    | BallType.Null -> Assets.Gameplay.cueBallImage
                    | _ -> Assets.Gameplay.cueBallImage

                  Content.staticSprite "P1 icon"
                     [Entity.Elevation == 5.0f
                      Entity.Position == v3 -184.0f 168.0f 0.0f
                      Entity.Size == GlobalBallSize
                      Entity.StaticImage := iconForColour gameplay.Player1.Colour]
                  Content.staticSprite "P2 icon"
                     [Entity.Elevation == 5.0f
                      Entity.Position == v3 90f 168.0f 0.0f
                      Entity.Size == GlobalBallSize
                      Entity.StaticImage := iconForColour gameplay.Player2.Colour]
                  // quit
                  Content.button Simulants.GameplayQuit.Name
                     [Entity.Position == v3 232.0f -200.0f 0.0f
                      Entity.Text == "Quit"
                      Entity.ClickEvent => StartQuitting]]
                   
         elif gameplay.GameplayState = Controls then
            Content.group Simulants.ControlsGui.Name []
                [// Background
                 Content.staticSprite "Background"
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Size == v3 300f 300f 0.0f
                     Entity.StaticImage == Assets.Gui.controlsImage
                     Entity.Elevation == 1.0f
                     Entity.Visible == true]
             
                 // Clickable area to start game
                 Content.button Simulants.ControlsStart.Name
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Size == v3 240f 360f 0.0f
                     Entity.UpImage == Assets.Default.EmptyImage
                     Entity.DownImage == Assets.Default.EmptyImage
                     Entity.Elevation == 2.0f
                     Entity.Visible == true
                     Entity.ClickEvent => Continue]]]

