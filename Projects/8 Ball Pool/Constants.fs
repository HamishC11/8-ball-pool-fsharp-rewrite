namespace MyGame
open System
open System.Numerics
open Prime
open Nu

module Constants =
    [<Literal>]
    let FrictionFactor = 0.98f

    // Cue Power limits
    let MaxPower = 50.0f
    let MinPower = 1.0f

    // used for pocket collision
    let HoleRadius = 15.0f
    // used for ball collision
    let BallRadius = 10.0f
    // Hole centres
    let TopLeftHolePos = v3 -288.0f 148.33f 0.0f
    let TopMiddleHolePos = v3 0.67f 160.0f 0.0f
    let TopRightHolePos = v3 291.0f 150.0f 0.0f
    let BottomRightHolePos = v3 291.0f -150.0f 0.0f
    let BottomMiddleHolePos = v3 0.67f -160.0f 0.0f
    let BottomLeftHolePos = v3 -288.0f -148.33f 0.0f

    //holechecker
    let IsInsideHole (pos: Vector3) =
        let holesWithRadius =
            [ (TopMiddleHolePos, HoleRadius + 4.0f)
              (BottomMiddleHolePos, HoleRadius + 4.0f)
              (TopLeftHolePos, HoleRadius)
              (TopRightHolePos, HoleRadius)
              (BottomLeftHolePos, HoleRadius)
              (BottomRightHolePos, HoleRadius) ]
        holesWithRadius
        |> List.exists (fun (holePos, radius) -> Vector3.Distance(pos, holePos) <= radius)
