module Main where

--import StandardLibrary
import Rattus.Primitives
import Rattus.Stream (Str(..), tl)
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace as D
import qualified Data.Set as Set
import Simple

{-
type World = (Maybe InputValue, String, O(Str String))

initial' :: World
initial' = (Nothing, "", textStr (accumulatorStr kbStr) resetStr)

render' :: World -> Picture
render' (_, text, _) = Translate (-(fromIntegral sizex)/2.0) 0 (Text text)

handleEvent' :: Event -> World -> World
handleEvent' (EventKey (Char c) Down _ _) (_, text, str) = (Just (1, CharValue c), trace ("This is the text now:" ++ text) text, str)
handleEvent' (EventKey (SpecialKey KeyF1) Down _ _) (_, text, str) = (Just (2, BoolValue True), trace ("This is a reset:" ++ text) text, str)
handleEvent' e world = trace ("nothing interesting happend: " ++ show e) world

step' :: Float -> World -> World
step' _ (Just input, _, laterStr) = (Nothing, a, as)
    where (a ::: as) =  adv' laterStr input 
step' _ world = world


-- Sizes
sizex = 1512 
sizey = 982 

-}

main :: IO ()
main = do
    {-
    print "THIS IS BEFORE"
    let a = addOne (Delay (Set.singleton 1) (\(1, IntValue i) -> D.trace ("LATER RETURNING: " ++ show i) i))
    print "THIS IS AFTER A IS DEFINED"
    let i = adv' a (1, IntValue 400)
    print i

    print "THIS IS BEFORE Q P"
    let q = Delay (Set.singleton 3) (\(3, IntValue i) -> D.trace ("Q LATER RETURNING: " ++ show i) i)
    let p = Delay (Set.singleton 4) (\(4, IntValue i) -> D.trace ("P LATER RETURNING: " ++ show i) i)

    let y = Delay (Set.singleton 5) (\(5, IntValue i) -> D.trace ("P LATER RETURNING: " ++ show i) i)
    
    print "THIS IS BEFORE S"
    let s = describe q p
    let z = describe q q

    print "TRY TO ADV S ON CHANEL 3"
    let k = adv' s (3, IntValue 300)
    print "TRY TO ADV S ON CHANEL 4"
    let j = adv' s (4, IntValue 400)
    print "RESULTS"
    print j
    print "AFTER J"
    print k

    print "BOTH"
    let o = adv' z (3, IntValue 3)
    print o

    print "Naive"
    let n = naiveIf False (Delay (Set.singleton 1) (\(1, IntValue i) -> i + 1)) (Delay (Set.singleton 2) (\(2, IntValue i) -> i + 2))
    let l = adv' n (2, IntValue 1)
    print l
    -}
    
    print $ input "keyboard" (CharValue 'c') describeKeyboard
    let (x ::: xs) = input "num" (IntValue 8) mappedStr
    print x
    let (x2 ::: xs2) = input "num" (IntValue 800) xs
    print x2
    let (y ::: ys) = input "num" (IntValue 10) (tl scannedStr)
    print y
    let (y2 ::: ys2) = input "num" (IntValue 20) ys
    print y2
    let (z ::: zs) = input "num" (IntValue 21) scanMappedStr
    print z
    let (z2 ::: zs2) = input "num" (IntValue 43) zs
    print z2
    let (z3 ::: zs3) = input "num" (IntValue 1) zs2
    print z3

    print $ input "num" (IntValue 100) const47Later
    print $ input "num" (IntValue 100) const48Later
    print $ input "num" (IntValue 100) idLater
    print $ input "num" (IntValue 300) myFunkyExample
    print $ input "num" (IntValue 350) myFunkyExample2
    print $ depend describeKeyboard
    let laterDescribe = describe const47Later const49Later
    let laterLater = describe laterDescribe const50Later
    print $ depend laterLater
    print $ inputMaybe "num2" (IntValue 350) const50Later
    
    -- Fails as expected
    --print $ input "num2" (IntValue 350) const50Later

{-
main = play
       (InWindow "text writer 9000" (sizex, sizey) (0,0))
       white
       10
       initial'
       render'
       handleEvent'
       step'
-}