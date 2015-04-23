import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal (..)
import Time (fps, inSeconds, Time)
import Window
import Text
import List

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type alias UserInput = {}


userInput : Signal UserInput
userInput = constant {}


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

type alias GameState = {}

defaultGame : GameState
defaultGame =
    {}



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} gameState =
    gameState



near k c n = n >= k-c && n <= k+c

within ball box = (ball.x |> near box.x (ball.r + box.w / 2))
               && (ball.y |> near box.y (ball.r + box.h / 2))

stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

stepObj t ({x,y,vx,vy} as obj) =
  { obj |
      x <- x + vx * t,
      y <- y + vy * t
  }

{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) gameState =
  container w h middle <|
    Text.asText gameState

make obj shape =
  shape
    |> filled white
    |> move (obj.x, obj.y)


{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Float
delta = inSeconds <~ fps 30


input : Signal Input
input = sampleOn delta (Input <~ delta ~ userInput)

gameState : Signal GameState
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
