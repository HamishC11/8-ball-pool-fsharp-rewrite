namespace MyGame
open System
open System.Numerics
open Prime
open Nu

module Constants =
    [<Literal>]
    let FrictionFactor = 0.98f

    let MaxPower = 50.0f
    let MinPower = 1.0f

    let HoleRadius = 15.0f
    let TopLeftHolePos = v3 -288.0f 148.33f 0.0f
    let TopMiddleHolePos = v3 0.67f 160.0f 0.0f
    let TopRightHolePos = v3 291.0f 150.0f 0.0f
    let BottomRightHolePos = v3 291.0f -150.0f 0.0f
    let BottomMiddleHolePos = v3 0.67f -160.0f 0.0f
    let BottomLeftHolePos = v3 -288.0f -148.33f 0.0f

    //holechecker
    let IsInsideHole (pos: Vector3) =
        let holesWithRadius =
            [ (TopMiddleHolePos, HoleRadius + 6.0f)
              (BottomMiddleHolePos, HoleRadius + 6.0f)
              (TopLeftHolePos, HoleRadius)
              (TopRightHolePos, HoleRadius)
              (BottomLeftHolePos, HoleRadius)
              (BottomRightHolePos, HoleRadius) ]

        holesWithRadius
        |> List.exists (fun (holePos, radius) -> Vector3.Distance(pos, holePos) <= radius)
