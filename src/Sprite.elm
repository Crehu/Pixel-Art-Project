port module Sprite exposing (Memory, init, main, update, view)

import Arithmetic exposing (isEven)
import Array exposing (Array)
import List exposing (map, range)
import Playground exposing (..)
import Playground.Extra exposing (tile)
import Playground.Internal exposing (Msg(..))
import Tuple exposing (first, second)



-- MAIN


main : Program () (Playground Memory) Msg
main =
    game view update init (subscriptions init)



-- PORTS


port isPaused : (Bool -> msg) -> Sub msg


init : Memory
init =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = 1
    , day = True
    , initial = True
    }


type alias Memory =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Float
    , day : Bool
    , initial : Bool
    }



-- VIEW


view : Computer -> Memory -> List Shape
view computer sprite =
    let
        w =
            computer.screen.width

        h =
            computer.screen.height

        b =
            computer.screen.bottom

        generateGround =
            map
                (\g ->
                    tile 96 96 grass 0
                        |> move (-(w / 2) + 48 + (96 * toFloat g)) (b + 48)
                )
                (range 0 (ceiling (w / 96)))

        generateStars =
            map
                (\g ->
                    let
                        r =
                            if isEven g then
                                70

                            else
                                60

                        t =
                            if g < 24 then
                                ( -(w / 2) + 250 + (200 * toFloat g), (h / 2) - 116 )

                            else if g < 48 then
                                ( -(w / 2) + 100 + (200 * toFloat (g - 24)), (h / 2) - 286 )

                            else if g < 72 then
                                ( -(w / 2) + 150 + (200 * toFloat (g - 48)), (h / 2) - 456 )

                            else
                                ( -(w / 2) + 100 + (200 * toFloat (g - 72)), (h / 2) - 626 )
                    in
                    tile 96 96 starSheet (getStarFrame computer.time r)
                        |> scale 1
                        |> move (first t) (second t)
                )
                (range 0 96)

        clouds =
            group
                [ image 120 120 cloudOne
                    |> scale 1.5
                    |> move -100 100
                , image 120 120 cloudOne
                    |> move 300 100
                , image 120 120 cloudTwo
                    |> scale 1.75
                    |> move 100 200
                , image 120 120 cloudTwo
                    |> move -300 200
                ]

        bushes =
            group
                [ image 192 192 bush
                    |> move 0 (b + 78)
                , image 192 192 bush
                    |> move (-(w / 2) + 240) (b + 78)
                , image 32 32 tree
                    |> scale 8
                    |> move ((w / 2) - 240) (b + 178)
                ]

        daySprites =
            [ rectangle (rgb 60 195 255) w h
            , clouds
                |> move -400 0
            , clouds
                |> move 400 0
            , clouds
                |> move -1200 0
            , clouds
                |> move 1200 0
            , bushes
            , image 32 32 sun
                |> scale 5.25
                |> move ((w / 2) - 116) ((h / 2) - 116)
            ]

        nightSprites =
            [ rectangle (rgb 12 1 99) w h
            , image 96 96 moon
                |> scale 1.75
                |> move (-(w / 2) + 116) ((h / 2) - 116)
            , bushes
            ]
                ++ generateStars
    in
    if computer.paused then
        daySprites ++ generateGround

    else
        (if sprite.day then
            daySprites

         else
            nightSprites
        )
            ++ [ tile 96 96 spriteSheet (getSpriteFrame sprite computer.time)
                    |> scale 1
                    |> scaleX sprite.dir
                    |> move sprite.x (b + 90 + sprite.y)
               ]
            ++ generateGround


getSpriteFrame : Memory -> Time -> number
getSpriteFrame sprite time =
    let
        frame =
            .now time // 90 |> remainderBy 8
    in
    if sprite.y > 0 then
        6

    else if sprite.vx /= 0 then
        Array.get frame run |> Maybe.withDefault 0

    else
        0


getStarFrame : Time -> Int -> number
getStarFrame time rate =
    let
        frame =
            .now time // rate |> remainderBy 13
    in
    Array.get frame star |> Maybe.withDefault 0



-- UPDATE


update : Computer -> Memory -> Memory
update computer sprite =
    let
        h =
            computer.screen.height

        w =
            computer.screen.width

        dt =
            toFloat (.delta computer.time) / 7

        vx = 
            toX computer.keyboard

        vy =
            if sprite.y == 0 then
                if computer.keyboard.up || computer.keyboard.wKey then
                    6

                else
                    0

            else
                sprite.vy - dt / 12

        x =
            if sprite.x > (w / 2) then
                -(w / 2)

            else if sprite.x < -(w / 2) then
                w / 2

            else
                sprite.x

        y =
            if sprite.initial then
                (h / 2) - 250

            else
                sprite.y + dt * vy

        day =
            if computer.keyboard.qKey then
                True

            else if computer.keyboard.eKey then
                False

            else
                sprite.day
    in
    { x = x + dt * vx * 2
    , y = max 0 y
    , vx = vx
    , vy = vy
    , dir =
        if vx == 0 then
            sprite.dir

        else if vx < 0 then
            -1

        else
            1
    , day = day
    , initial = False
    }


-- Subscriptions


subscriptions : model -> Sub Msg
subscriptions _ =
    isPaused Paused


run : Array number
run =
    Array.fromList [ 1, 2, 3, 4, 1, 2, 3, 4 ]


star : Array number
star =
    Array.fromList [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]


spriteSheet : String
spriteSheet =
    "data:image/png;base64,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"


cloudOne : String
cloudOne =
    "data:image/png;base64,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"


cloudTwo : String
cloudTwo =
    "data:image/png;base64,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"


starSheet : String
starSheet =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABOAAAABgCAYAAAC0RA27AAADDklEQVR4nO3YIa4CQRBFUfZvkMhZBhKJZClIZH81pOVXfUn6nKR8hxQ1r+pyAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYGPX63WcVb9lR7fbbZxVv2VHx3GMs+q37OjxeIyz6rfs6Pl8jrPqt+xI/7fM/5b82ZI/ASAgALUEoJYFrOUA0XKAa+n/lvnfkj9b8icABASglgDUsoC1HCBaDnAt/d8y/1vyZ0v+BDI+AK3X6zXOqt+yi7nn3+/3t/wX1phDz+fz+ZYwtMa8dM0sY2vc7/dx1swxYo35d545xq2h/1vmf0v+bMmfv8P+y9YM/ZYBtJ4A1BKAWhawlgNEywGupf9b5n9L/mzJn7/D/svWDP2WAbSeANQSgFoWsJYDRMsBrqX/W+Z/S/5syZ+/w/4LsCmhpyX0tCxdLUeHlqNbS/+3zP+W/NmSPwEgIAC1BKCWBazlANFygGvp/5b535I/W/InAAQEoJYA1LKAtRwgWg5wLf3fMv9b8mdL/gQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+Kc/P7VHJGQQHRAAAAAASUVORK5CYII="


moon : String
moon =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAYAAADimHc4AAABYUlEQVR4nO3WS4rDMBBF0ex/gb2JXkR62uAYS+jzyvE5kJmRrLpg5fUCAAAAAAAAAJ7h5/f9XvlLn688AcIE2GT1oIW5kB64AAWG/ogALYdPeUQYAcIECEgNetZet48hQJgAYSOD6P372LJO7zO3vxsECBMgYNZ3f+f90RtJgMkE6DjwCl8boPfwO4d+9g69z6fnfSBAmAABqcGNGHnncveBAAJ0E+DDYUak7gwBJq/Tu5cAk9fp3ev2Ac4ONjKI1L4CCFBjEKl9bx+ggpb3F2AhAcIeEaBypJazCLCQAGFfG2DnPbFzfQEuBrR6fQEuBrR6/RIB/ksFSAVOz/tAgDABwloOP2tYI+vM2jc97wMBwgQoZNa3+GzQK9ZveSY912YChAkQtvOv4Qpn75+eazMBwgQopHKMljsmPb9hAoQJUFTL4Xf+0vPYLj1wAQoM/dEBzhh0mABhAgAAAAAAwHR/BfTWPDRN3SsAAAAASUVORK5CYII="


grass : String
grass =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAYAAADimHc4AAAAzElEQVR4nO3VsQ0CMRBE0Q0upwx0dbgH+qNSX3SUwIDmPcnJRJZ+sDMAAAAAAAAAAAAAAAAAAADwV473Y3u5J4AA3U8AAbqfAAJ0PwEE6H6znrPv91rn59m/swsgQPc+v/CJ5l0AAbp3NyC8CyBA9+4GhHcBBOje3YDwLoAA3bsbEN4FEKB7dwPCuwACdO9uQHgXQIDu3Q0I7wII0L27AeFdAAG6dzcgvAsgQPfuBoR3AQTo3t2A8C6AAN27GxDeBRCge3cDwrsAAnTvF8YvcGECEfxKAAAAAElFTkSuQmCC"


bush : String
bush =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAMAAAADACAYAAABS3GwHAAACuklEQVR4nO3WW47dIBAEUK8iq8mys7/kL7mKBgbbPBr6HKl+fRmokua6AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgOP8bggcywBIzQCYqqVwI4r45ncNg24MgNQMgBRGFO5u+W598+evH38z6DwkYgCkZgCkMLroQ/NZ+lIav0VSy0v8JgbAW8tL/CYGwBNDC/fyf/HuZzAG/mcABpCaARhAakNL1msMC8/A4SKXL8IZOFzk8kU4A4v1+n/51jdHFO5u+Uaf4c0gB70LXzAAA0jNAAzgWCsvPnzhop1nQtJZfeGhCxftPBOSzuoLD124aOeZkBSGlnXENzMMoOVsk8d2LAMwAAN4GwMwgJ0sKeLM3zopM++q8jZHMYCNMvOuKm9zFAPYKDPvqvI221tS+giPuntm3lVjP7ZkAJtm5l019mNLBrBpZt5VYz+2Ear0BhA/jV3ZhgGIAbQmwkWvLkD2NHYltLClN4C9UulNaAYgQ99oYpcfMQAZ+kYTu/xI2NLLXql0KDQDkC6pdCg0A5AuqXQoHKWX7qn0KRwDkO6p9CkcA5DuqfQpHKWX7in1alHHqwxAuqfUq0UdrzIA6Z5SrxZ1vMoApHtKvVrU8SoDkO4p9WpRx6sMQLqn1KtFHa8yAOmSUpcuA5AMKXXpMgDJkFKXLgOQU1PqTyXhGIA8Tqk/lYRjAPI4pf5UEs63h159yRIrLZ25gpf+kwHIrbR05jIAOTUtnbkMQE5KS0+ujUr/yQDk27T05DIAOTUtPbk2HcCnJ3+obJ7spf+0/DFkfgzgn+WPIfNjAF9b/jASNimsvmSJmxRWX7LEDRxF0UnNAEjNAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAmf4Aob/9qFnfqdQAAAAASUVORK5CYII="


sun : String
sun =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAYAAADimHc4AAAA+klEQVR4nO3WQYrDMBQFQZ90Djx3mLMkm1kEgkGbpL/lKtDeeg1GxwEAAAAAAAB82OP3eBSnvvcYAsQECFSji/GvHlyAAaPfLsDpAH8/zblbDAFiAgTGjX63GALEBAhcZvRdYwgQEyBw+dGvHkOAmACxrUZfiFHv/UaAmACBbf/7CwFGxBBAAAEEEEAAAQQQQAABBBBAAAEEEECAfCgBBowlwIZHAAEEEGBGgGz0VwLEBBhEgJgAMQEG2SrGtKfnCgFiAsQu/zw9+34BBFgiwCCjY+w09BkBYgIMtXL5b556j6+rBxdgwOi3DvDK6DEBYgIAAAAAAADArp5JWBGJaBotAwAAAABJRU5ErkJggg=="


tree : String
tree =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAYAAADimHc4AAABw0lEQVR4nO3avU0EMRBA4cnIKeAkEiiBkDKISSmBOmiANmjuSE4+B+tbA2s/7/o9aZIVSOx8aH90FzFGZ2jskgBwAgClRTx/P6R5+rpP0/p4TI4hAJwAcOnkey49Px4CCEAmAJz3ADgB4ATo1OobKbh0AUKA5gkAhD9ilo6XJir+UaBd/ikB4ASAwx8xa5b+24kdYQgAJwDQIZcugADVCQCHP3oKIACaAHDIPUCAawLACTBQTTE6L3o3S88TAE4AuE0eST9eT5vMka/1pQSAE2Cgaj/uO0fEZksvTeXfcagEgBNgB1EAdkkAOAEaVnOdFaBhAsAJAJdO+O3lLk0UYD7fH9Pki8uP10zpd0MAAXonwECtYpQWl/98zZQwQgAByAQYqEWM/yy9BiMESAkAJ8BALT6GdgaoeWM/bALACQBHAdS8pU+RAHACwCEAsy89TwA4AeC6Abj05QSAEwAuLcLPA5gEgBMAbvEe4Lci+iUAnABwi/cAvxvaLwHgBIATAE4AOAHgBIATAE4AOAHgBIATAKjb0m8ATI0hAJwADbt1ot2XPuO9QQA4AYCGXe4sGALACQC3et3f6ewmelECHHSa9gNPcAyztR+kAQAAAABJRU5ErkJggg=="
