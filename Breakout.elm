import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Time (..)
import Window
import Text
import List

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type alias UserInput = { dir : Int }

userInput : Signal UserInput
userInput = Signal.map UserInput (Signal.map .x Keyboard.arrows)

type alias Input =
    { timeDelta : Float
    , userInput : UserInput
    }

{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}

(gameWidth,gameHeight) = (400, 600)
(halfWidth,halfHeight) = (200, 300)

type alias Positioned a = { a | x:Float, y:Float }
type alias Moving     a = { a | vx:Float, vy:Float }
type alias Sized      a = { a | w:Float, h:Float }
type alias Box          = Sized (Positioned {})

type alias Ball = Moving (Positioned { r:Float })
type alias Player = Moving Box
type alias Brick = Box

brick : Float -> Float -> Float -> Float -> Brick
brick x y w h = { x=x, y=y, w=w, h=h }
(brickWidth, brickHeight) = (40,20)


type alias GameState =
  { ball : Ball
  , player : Player
  , bricks : List Brick
  }

defaultGame : GameState
defaultGame =
  { ball =
      { x = 0
      , y = 0
      , vx = 200
      , vy = 200
      , r = 8
      }
  , player =
      { x = 0
      , y = 20 - halfHeight
      , vx = 0
      , vy = 0
      , w = 40
      , h = 10
      }
  , bricks = List.map (\x -> brick (brickWidth * 2 * x) (halfWidth - 40) brickWidth brickHeight) [-2..2]
  }

{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} ({ball,player,bricks} as game) =
  let
    (ball', bricks') = stepBall timeDelta ball player bricks
  in
    { game | ball <- ball'
           , bricks <- bricks'
           , player <- updatePlayer timeDelta userInput.dir player
    }

stepBall : Time -> Ball -> Player -> List Brick -> (Ball, List Brick)
stepBall delta ({x,y,vx,vy} as ball) player bricks =
  let
    hitPlayer = (ball `within` player)
    hitCeiling = (y > halfHeight - ball.r)
    ball' = physicsUpdate delta
      { ball | vx <- stepV vx (x < ball.r - halfWidth) (x > halfWidth - ball.r)
             , vy <- stepV vy hitPlayer hitCeiling
      }
  in
    (List.foldr goBrickHits (ball',[]) bricks)

goBrickHits : Brick -> (Ball,List Brick) -> (Ball,List Brick)
goBrickHits brick (ball,bricks) =
  let
    hit = ball `within` brick
    bricks' = if hit then bricks else brick::bricks
    ball' = if hit then { ball | vy <- -ball.vy } else ball
  in
    (ball', bricks')

near k c n =
    n >= k-c && n <= k+c

within ball box = (ball.x |> near box.x (ball.r + box.w / 2))
               && (ball.y |> near box.y (ball.r + box.h / 2))

stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

updatePlayer deltaTime dir player =
  let player1 = physicsUpdate deltaTime { player | vx <- toFloat dir * 200 }
  in
    { player1 |
        x <- clamp (22-halfWidth) (halfWidth-22) player1.x
    }

physicsUpdate t ({x,y,vx,vy} as obj) =
  { obj |
      x <- x + vx * t,
      y <- y + vy * t
  }

{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) ({ball,player,bricks} as gameState) =
  container w h middle <|
    collage 1000 1000 <|
      [ rect gameWidth gameHeight
          |> filled green
      , circle ball.r
            |> make ball
      , rect player.w player.h
          |> make player
      , group <| List.map (\b -> rect b.w b.h |> make b) bricks
      ]

make obj shape =
  shape
    |> filled white
    |> move (obj.x, obj.y)

{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Float
delta =
    Signal.map inSeconds (fps 30)


input : Signal Input
input =
    Signal.sampleOn delta (Signal.map2 Input delta userInput)


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


main : Signal Element
main =
    Signal.map2 display Window.dimensions gameState
