module DemosceneConcentration where

{-| The classical memory game with old school demoscene effects.

The game begins when the player turns over
the first card. Maximally two cards at a time can be turned over.
If a pair it found, it stays image up. After all pairs are found the
needed time inversly indicates the players performance. ;-)
-}

import Touch
import Window

import Effect(Effect)
import Effect

import Common(Point, Positioned, Boxed, Box, point2D, box2D, roundTo,
              shuffle)

import Card
import Card(Card)
import Cube
import EulerSpiral
import Moire
import Plasma
import Particles
import Sinescroller
import Tunnel

-- todo: still show cards a second or so after a pair is found
--       cardstate Done + Gone
--       gamestate Won + End
-- todo: move all into src directory. move effects into effects directory.

-- /---------------------\
-- | model configuration |
-- \---------------------/

{-| The game field extends from -100 to +100 in x and y coordinates. -}
(gameWidth,gameHeight) = (200,200)
framesPerSecond = 60


-- /--------------------\
-- | view configuration |
-- \--------------------/

timeTextHeight = 5
timeTextPosY = 99
fpsTextHeight = 3
fpsTextPosY = 100
fpsTextPosX = -98



-- /--------\
-- | inputs |
-- \--------/

data Action = Tap Point (Int,Int) | Step Float
type Input = { action:Action }

speed : Signal Time
speed = fps framesPerSecond

input : Signal Input
input = (Input <~ actions)

steps : Signal Action
steps = lift Step speed

flips : Signal Action
flips =
  let
    f t winDims = Tap (point2D (toFloat t.x) (toFloat t.y)) winDims
  in
    f <~ Touch.taps ~ Window.dimensions |> sampleOn Touch.taps |> dropRepeats

actions = merge steps flips


-- /-------\
-- | model |
-- \-------/


type Cards = [Card.Card]


{-| Creation of one single row of cards with equidistant gaps. -}
cardBoxRow : Float -> [Box]
cardBoxRow y =
  let
    cols = 4
    distX = 60
    xOff = -distX * (toFloat cols - 1) / 2
    cardWidth = 55
    cardHeight = 55
  in
    map (\x -> box2D (distX * x + xOff) y cardWidth cardHeight) [0..cols-1]

cardBoxes =
  let
    rows = 3
    distY = 65
    yOff = -distY * (toFloat rows - 1) / 2
  in
    map (((+) yOff ) . (*) distY) [0..rows-1] |> map cardBoxRow |> concat

effects = [ EulerSpiral.make
          , Plasma.make
          , Particles.make
          , Sinescroller.make
          , Moire.make
          , Tunnel.make ]




cards : Cards
cards =
  let
    -- todo generate random numbers with datetime as seed
    numbers = [8635,5184,5783,5522,5062,2341,7018,1802,7294,1161,6711,554,9897,4878,1072,7806,1564,27,4190,5863,4225,1753,8459,9459,8976,9639,8788,61,1208,7347,919,5433,7202,3736,8016,9236,806,2671,2557,7685,1760,4639,3951,1358,3814,1430,6744,8496,7156,2862,6181,3920,1641,638,5599,4246,9720,5298,7272,9991,8614,381,3271,9097,3115,8203,7670,4919,5633,1981,2662,5365,4608,8450,5162,2984,4340,6581,9633,2855,4358,3907,2792,8743,548,953,2944,4465,1044,6914,335,1399,1929,1742,7521,2665,1960,3315,9416,5175,4534,6346,758,4987,462,2904,3720,2316,6458,2799,8961,5724,6652,762,4742,5783,4968,6420,3451,4622,1162,8409,7591,2050,6892,1551,7643,1610,4549,8664,6406,5913,5851,7360,7250,8445,523,359,8201,6838,7769,140,7754,4842,8711,8193,4622,6819,3850,7466,8647,6451,9387,3637,4952,6780,4388,3427,2422,2005,7136,4039,3825,1601,7277,7933,7348,9953,2988,5398,8959,8890,68,5381,2278,8320,5836,1259,9252,2796,4158,7141,2342,3875,5406,7482,5036,3193,4223,4269,3709,740,810,7641,8389,4076,8845,3584,3166,105,5987,6025,6313,5786,6984,490,3075,7619,2437,6827,7947,5820,291,2146,4977,5092,1506,4245,2494,4100,5444,2281,1362,9278,2868,8338,5455,4231,1053,823,5344,3061,753,3124,5864,6253,1932,7706,5738,1391,3725,3031,3897,3390,9274,9442,6769,8342,8106,4437,360,6836,2833,195,7427,3593,3639,5550,3037,9158,1040,5284,3633,2940,5256,8722,9385,6585,7013,77,9675,5178,9447,7679,8617,3016,235,1393,5338,5519,1593,251,278,9056,4621,917,1824,9114,9131,8467,5646,5072,274,42,424,4056,5842,1831,7224,849,5102,289,6273,5518,3550,4476,9158,7424,6173,4622,6853,9059,3351,4708,2560,6300,772,8724,5016,6244,2329,198,9777,337,6163,8481,9467,2280,1898,2339,8152,1506,9899,2913,3278,4503,2068,3778,5801,3182,5409,4430,2016,2182,8748,4769,8249,3456,2167,7458,5168,7199,2399,1171,9967,7400,502,1205,8266,7261,886,2050,1811,8552,7621,3241,480,8385,7809,223,528,2290,6719,9608,6643,2885,5248,8390,6331,689,2376,5020,8158,9140,177,1994,6594,2694,5681,1125,2307,9309,3687,3575,645,538,8574,2559,9337,7814,9114,2034,6053,8127,6983,1667,2739,3240,3417,7065,5086,843,2699,1556,8172,4358,1038,7446,5126,4300,7176,7477,6537,472,8153,7432,8251,2957,4743,6429,3443,2031,696,9478,5124,4187,6745,5830,794,9981,108,1847,8992,6893,2314,8452,5001,297,7617,6742,7400,1083,3700,9417,3493,234,4585,9039,2392,3087,782,3937,2626,3805,4926,486,3608,2050,2969,6476,2298,6485,8156,2571,1423,3532,746,8293,8884,5256,8089,208,3410,4453,5544,4823,3607,9478,1046,6915,7235,5792,1451,3342,8518,5279,3994,4704,2404,6578,8057,9874,3241,6406,5085,341,2739,108,304,1177,9433,4979,2967,1513,9206,9954,3362,3115,5856,5798,220,9674,6848,1972,4046,5817,8845,9858,7819,3285,5004,2738,2755,5226,463,424,9289,503,4369,6032,4179,7552,7261,6549,1236,7328,2592,5642,5474,1439,8626,5011,5781,814,2263,2488,1159,8174,6579,7742,5366,3376,5499,5553,8398,1844,3133,8865,2344,3741,5433,2571,8498,9069,4521,1068,5941,91,6484,8614,3752,6956,9726,7203,2918,1790,4755,9393,7436,3904,4470,6124,2666,8994,4114,6749,513,8478,9029,1994,4195,5540,5852,177,2637,1033,4744,7758,1937,9366,8057,3711,9464,7937,5093,2768,6052,5635,9159,9966,9450,2343,9274,1860,4374,1384,7380,4647,1811,3185,85,2724,3906,6501,2471,2030,5145,5541,8905,9889,7944,3143,9109,493,5597,254,8115,8186,878,5064,6786,7398,8617,5716,9580,6893,4857,7000,1427,8399,552,4546,1554,8095,2934,1377,8554,8100,29,4231,1089,1389,5649,2837,7261,9497,9166,1230,9106,5486,8420,1497,7280,3329,6582,7815,2095,6369,8594,6698,6947,1531,7772,9125,9452,6947,2259,5276,3177,4651,7878,2187,5009,5157,3613,2059,1234,1910,6743,2706,1221,355,1017,8852,5408,5530,6691,3047,1338,7063,1680,7388,4270,812,4561,577,3030,3645,4331,1555,3053,7603,136,986,6209,2579,4672,3381,4376,5437,22,7950,7531,3673,6363,2918,1360,4734,9258,9928,6995,4525,4369,1489,9160,3883,2,981,6481,9713,1902,9551,3932,8785,1257,212,4721,9187,8798,4632,6787,4811,7119,9310,4364,6027,224,3688,2135,7346,2552,2589,5305,162,4703,8046,2245,2925,2110,8098,2209,3454,4128,1059,4037,7821,7642,9429,5383,6872,1680,7604,5416,2551,1387,5119,3055,7343,3869,123,3343,2307,2822,3999,9079,9285,4023,116,7947,1738,3275,6996,4044,1308,1705,5604,6530,5157,6426,8305,3535,214,7010,9052,8074,5914,7036,9816,1253,328,7130,4135,6440,1582,6410,3740,2113,9401,7758,2787,3146,6270,6214,5260,3870,3405,1778,475,4777,9937,9176,5949,7341,7957,2208,5526,889,1009,1903,1184,9445,8882,2153,7182,5098,9467,7349,3001,6803,9844,5278,1781,261,2894,2324,6787,8757,3311,8179,5539,3299,364,5009,1382,9471,2411,7268,2744,8237,8291,1481,257,523,2654,2191,5278,1188,2704,8835,8973,544,6095,7971,9013,1952,9435,8015,714,5002,6330,6372,9149,2148,9117,3523,9261,9675,2443,3655,4358,9975,7769,8347,5487,7286,6127,9276,3859,7619,5877,8678,1748,2436,8946,1324,1667,8076,2688,4393,8329,2660,2348,2991,3016,9984,7444,3077,5132,8102,3240,6768,5812,1278,6026,5242,6704,8582,67,8982,8300,8657,7667,5940,7652,4674,6894,530,51,3300,998,7893,9747,7091,4306,9679,3404,4108,2648,8119,5209,8888,3472,8273,3409,4731,7701,2693,5014,4909,5033]
    shuffledBoxes = shuffle numbers cardBoxes
  in
    zipWith (\effect box -> Card.make effect box) (effects ++ effects) shuffledBoxes



data State = Start | Play | Won



type Game = { state:State
            , cards:Cards
            , wonEffect:Effect
            , time:Time
            , currentFPS:Int }



defaultGame : Game
defaultGame =
  { state = Start
  , cards = cards
  , wonEffect = Cube.make effects (rgb 64 64 64)
  , time = 0
  , currentFPS = 0 }


-- /---------\
-- | updates |
-- \---------/

{-| Since the game is always scaled maximally into the window
(keeping its aspect ratio), the mouse and touch positions
have to be converted to game positions. -}
winPosToGamePos : Positioned a -> (Int,Int) -> Point
winPosToGamePos pos size =
  let
    intPairToFloatPair (a, b) = (toFloat a, toFloat b)
    (winX, winY) = (pos.x, pos.y)
    (sizeX, sizeY) = intPairToFloatPair size
    (middleX, middleY) = (sizeX / 2, sizeY / 2)
    factor = gameScale size (gameWidth,gameHeight)
  in
    point2D ((winX - middleX) / factor) ((middleY - winY) / factor)

{-| Calculate factor by which the game is scaled visually onto the screen. -}
gameScale : (Int,Int) -> (Float,Float) -> Float
gameScale (winW, winH) (gameW,gameH) =
  min (toFloat winW / gameW) (toFloat winH / gameH)

gameState : Signal Game
gameState = foldp stepGame defaultGame input

inBox : Positioned a -> Boxed b -> Bool
inBox pos box =
  let
    xDist = abs (pos.x - box.x)
    yDist = abs (pos.y - box.y)
  in
    xDist <= box.w/2 && yDist <= box.h/2

goflipCardsHit : Point -> Card -> Cards -> Cards
goflipCardsHit tapPos ({status} as card) acc =
  let
    hit = tapPos `inBox` card
    status' = if not hit then status else case status of
                                            Card.Done -> Card.Done
                                            Card.FaceUp -> Card.FaceDown
                                            Card.FaceDown -> Card.FaceUp
    card' = { card | status <- status' }
  in
    card'::acc

flipCardsHit : Point -> Cards -> Cards
flipCardsHit tapPos cards =
    foldr (goflipCardsHit tapPos) [] cards

allEqual : Cards -> Bool
allEqual cards =
  let
    es = map .effect cards
    equalName (Effect e1) (Effect e2) = e1.name == e2.name
  in
    if length es < 2 then True
      else all (equalName (head es)) (tail es)

-- todo: simplify
stepTap : Point -> Game -> Game
stepTap gameTapPos ({state,cards} as game) =
  let
    (cardsDone, cardsNotDone) = partition (((==) Card.Done) . (.status)) cards
    cardsNotDoneFaceDown = map (\c -> {c | status <- Card.FaceDown}) cardsNotDone
    faceUpCount = cards |> filter (((==) Card.FaceUp) . (.status)) |> length
    cardsNotDone' = flipCardsHit gameTapPos <|
                      if faceUpCount == 2 then cardsNotDoneFaceDown
                                          else cardsNotDone
    (cardsNotDoneUp', cardsNotDoneDown') =
       partition (((==) Card.FaceUp) . (.status)) cardsNotDone'
    foundPair = length cardsNotDoneUp' == 2 && allEqual cardsNotDoneUp'
    cardsNotDoneUpDone' = map (\c -> {c | status <- Card.Done}) cardsNotDoneUp'
    allDone = all
    cards' = cardsDone ++ cardsNotDoneDown'
             ++ if foundPair then cardsNotDoneUpDone' else cardsNotDoneUp'
    state' = case state of
               Won -> Won
               Start -> Play
               Play -> Play
               Play -> if all (((==) Card.Done) . .status) cards' then Won
                                                                  else Play
  in
    { game | state <- state',
             cards <- cards' }



stepCards : Float -> Cards -> Cards
stepCards delta cards = map (Card.step delta) cards

stepWon : Float -> Game -> Game
stepWon delta ({wonEffect} as game) =
  { game | wonEffect <- Effect.step wonEffect delta }

stepDelta : Float -> Game -> Game
stepDelta delta ({cards, state, time} as game) =
  let game' =
    case state of
      Won -> stepWon delta game
      _   -> { game | cards <- stepCards delta cards
                    , time <- case state of
                                Play -> time + delta
                                _ -> time }
  in
    { game' | currentFPS <- round(1000/delta) }

stepGame : Input -> Game -> Game
stepGame ({action}) ({state, time} as game) =
  case action of
    Step delta -> stepDelta delta game
    Tap tapPos winDims -> stepTap (winPosToGamePos tapPos winDims) game





-- /---------\
-- | display |
-- \---------/




displayCards : Time -> Cards -> Form
displayCards time cards =
  map (Card.display time) cards |> group


txt : (Text -> Text) -> String -> Element
txt f = text . f . monospace . Text.color lightBlue . toText

displayFPS : Game -> Form
displayFPS {currentFPS} =
  txt (Text.height fpsTextHeight) ("FPS: " ++ (show <| currentFPS))
                     |> toForm |> move (fpsTextPosX, fpsTextPosY)

displayWon : Game -> Form
displayWon ({wonEffect} as game) = Effect.display wonEffect

{-| Draw game into a form with size (gameWidth,gameHeight). -}
displayNotYetDone : Game -> Form
displayNotYetDone ({time,cards} as game) =
  let
    timeRounded = (toFloat . round) (time / 100) / 10
    timeTextForm = txt (Text.height timeTextHeight) (show timeRounded)
                     |> toForm |> move (0, timeTextPosY)
  in
    group [ displayCards time cards
          , timeTextForm ]

display : Game -> Form
display ({state} as game) =
  group [
    case state of
      Won -> displayWon game
      _   -> displayNotYetDone game
  , displayFPS game ]


{-| Draw game maximized into the window. -}
displayFullScreen : (Int,Int) -> Game -> Element
displayFullScreen (w,h) game =
  let
    factor = gameScale (w,h) (gameWidth,gameHeight)
  in
    collage w h [ display game |> scale factor ]

main = displayFullScreen <~ Window.dimensions ~ gameState