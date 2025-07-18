﻿namespace JumpBox
open System
open System.Numerics
open Prime
open Nu

// this extends the Game API to expose user-defined properties.
[<AutoOpen>]
module JumpBoxExtensions =
    type Game with
        member this.GetCollisions world : int = this.Get (nameof Game.Collisions) world
        member this.SetCollisions (value : int) world = this.Set (nameof Game.Collisions) value world
        member this.Collisions = lens (nameof Game.Collisions) this this.GetCollisions this.SetCollisions

// this is the dispatcher that customizes the top-level behavior of our game.
type JumpBoxDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Game.Collisions 0]

    // here we define the game's behavior
    override this.Process (game, world) =

        // declare screen and group
        World.beginScreen "Screen" true Vanilla [] world |> ignore
        World.beginGroup "Group" [] world

        // declare a block
        World.doBlock2d "Block" [Entity.Position .= v3 128.0f -64.0f 0.0f] world |> ignore

        // declare a box and then handle its body interactions for the frame
        let (boxBodyId, results) = World.doBox2d "Box" [Entity.Position .= v3 128.0f 64.0f 0.0f] world
        for result in results do
            match result with
            | BodyPenetrationData _ -> game.Collisions.Map inc world
            | _ -> ()

        // declare a control panel with a flow layout
        let layout = Flow (FlowDownward, FlowUnlimited)
        World.beginPanel "Panel" [Entity.Position .= v3 -128.0f 0.0f 0.0f; Entity.Layout .= layout] world

        // declare a collision counter
        let collisions = game.GetCollisions world
        World.doText "Collisions" [Entity.Text @= "Collisions: " + string collisions] world

        // declare a jump button
        let canJump = World.getBodyGrounded boxBodyId world
        if World.doButton "Jump!" [Entity.EnabledLocal @= canJump; Entity.Text .= "Jump!"] world then
            World.jumpBody false 8.0f boxBodyId world

        // declare a bar that fills based on up to 10 collisions and a text that displays when the bar is full
        World.doFillBar "FillBar" [Entity.Fill @= single collisions / 10.0f] world
        if collisions >= 10 then
            World.doText "Full!" [Entity.Text .= "Full!"] world

        // finish declaring the control panel, group, and screen
        World.endPanel world
        World.endGroup world
        World.endScreen world

        // handle Alt+F4 while unaccompanied
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world