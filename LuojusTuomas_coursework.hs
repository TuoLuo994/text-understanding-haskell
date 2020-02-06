-- ghc --make LuojusTuomas_coursework
-- ./LuojusTuomas_coursework <taskfile>

-- Tuomas Luojus
-- Advanced Functional Programming course work 2019-04-26
-- University of Tampere

import Control.Exception
import System.IO
import System.IO.Error
import System.Environment
import Data.Typeable
import Data.List
-- cabal install split
import Data.List.Split

data Person = Person { name :: String
                     , location :: [(String, Bool)]
                     , holding :: [String]
                     } deriving (Show, Eq)

prompt :: String -> IO String
prompt text = do
    putStrLn text
    hFlush stdout
    getLine

main :: IO ()
main = do
    (fileName:args) <- getArgs
    let readFrom = fileName
    readItems <- readFile readFrom
    question <- prompt "Question:"
    print $ answerTask (splitOn " " question) (splitOn "\n" readItems)
    main

splitAll :: [String] -> [[String]]
splitAll [x] = [splitOn " " x]
splitAll (x:xs) = [splitOn " " x] ++ splitAll xs

-- Directs the question and input list to right function
answerTask :: [String] -> [String] -> String
answerTask ["Is", a, "in", "the", b, "?"] p = personInLocation a b (splitAll p)
answerTask ["Where", "is", "the", a, "?"] p = objectLocation a (reverse $ splitAll p) (reverse $ splitAll p)
answerTask ["How", "many", "objects", "is", a, "carrying", "?"] p =  show $ itemNumber a p
answerTask ["Where", "was", a, "after", "the", b, "?"] p = locationAfter a b p False
answerTask ["Where", "was", a, "before", "the", b, "?"] p = locationAfter a b (reverse p) False
answerTask ["How", "do", "you", "go", "from", "the", a, "to", "the", b, "?"] p = fromTo a b (splitAll p) (splitAll p)
answerTask q p = "Can not understand your question"

-- Finds a route from location a to location b and returns it as String
fromTo :: String -> String -> [[String]] -> [[String]] -> String
fromTo a b [["The", x, "is", z, "of", "the", y]] all
    | x == a && y == b = oppositeDir z
    | y == a && x == b = z

    | x == a && deadEnd y all False = " 1 "
    | y == a && deadEnd x all False = " 2 "

    | x == a && not (deadEnd y all False) = oppositeDir z ++ ", " ++ fromTo y b all all
    | y == a && not (deadEnd x all False) = z ++ ", " ++ fromTo x b all all

    | otherwise = "  "
fromTo a b (["The", x, "is", z, "of", "the", y]:ls) all
    | x == a && y == b = oppositeDir z
    | y == a && x == b = z

    | x == a && deadEnd y all False = fromTo a b ls all
    | y == a && deadEnd x all False = fromTo a b ls all

    | x == a && not (deadEnd y all False) = oppositeDir z ++ ", " ++ fromTo y b all all
    | y == a && not (deadEnd x all False) = z ++ ", " ++ fromTo x b all all
    | otherwise = fromTo a b ls all
fromTo a b [l] all = "1 "
fromTo a b (l:ls) all = fromTo a b ls all
fromTo a b c d = "xxx"

-- check if room is a dead end (only one door leading to the room)
deadEnd :: String -> [[String]] -> Bool -> Bool
deadEnd room [["The", x, "is", _, "of", "the", y]] True
    | x == room = False
    | y == room = False
    | otherwise = True
deadEnd room [["The", x, "is", _, "of", "the", y]] False = True

deadEnd room (["The", x, "is", _, "of", "the", y]:xs) True
    | x == room = False
    | y == room = False
    | otherwise = deadEnd room xs True
deadEnd room (["The", x, "is", _, "of", "the", y]:xs) False
    | x == room = deadEnd room xs True
    | y == room = deadEnd room xs True
    | otherwise = deadEnd room xs False
deadEnd a (x:xs) False = deadEnd a xs False
deadEnd a (x:xs) True = deadEnd a xs True
deadEnd a [] _ = True

oppositeDir :: String -> String
oppositeDir "east" = "west"
oppositeDir "south" = "north"
oppositeDir "west" = "east"
oppositeDir "north" = "south"
oppositeDir _ = ""

-- Finds where a person was before given location
locationAfter :: String -> String -> [String] -> Bool -> String
locationAfter _ _ [p] False = "don't know"
locationAfter person place [p] True
    | name (addPersons (splitOn " " p)) == person = fst $ (location (addPersons (splitOn " " p)) !! 0)
    | otherwise = "don't know"
locationAfter person place (p:ps) False
    | name (addPersons (splitOn " " p)) == person && location (addPersons (splitOn " " p)) == [(place, True)] = locationAfter person place ps True
    | otherwise = locationAfter person place ps False
locationAfter person place (p:ps) True
    | name (addPersons (splitOn " " p)) == person && location (addPersons (splitOn " " p)) /= [] = fst $ (location (addPersons (splitOn " " p)) !! 0)
    | otherwise = locationAfter person place ps True

-- Calculates how many objects <name> is carrying (Task 3)
itemNumber :: String -> [String] -> Int
itemNumber name [p]
    | (init (splitOn " " p) == [name, "took", "the"]) = 1
    | (init (splitOn " " p) == [name, "got", "the"]) = 1
    | (init (splitOn " " p) == [name, "picked", "up", "the"]) = 1
    | (head (splitOn " " p)) == name && ((splitOn " " p) !! 1 == "handed") = -1
    | (last (splitOn " " p)) == name && ((splitOn " " p) !! 1 == "handed") = 1
    | otherwise = 0
itemNumber name (p:ps)
    | (init (splitOn " " p) == [name, "took", "the"]) = 1 + itemNumber name ps
    | (init (splitOn " " p) == [name, "got", "the"]) = 1 + itemNumber name ps
    | (init (splitOn " " p) == [name, "picked", "up", "the"]) = 1 + itemNumber name ps
    | (head (splitOn " " p)) == name && ((splitOn " " p) !! 1 == "handed") = -1 + itemNumber name ps
    | (last (splitOn " " p)) == name && ((splitOn " " p) !! 1 == "handed") = 1 + itemNumber name ps
    | otherwise = 0 + itemNumber name ps

-- Returns location of object (Task 2)
objectLocation :: String -> [[String]] -> [[String]] -> String
objectLocation item [p] all
    | item `elem` (holding $ addPersons p) = fst ((location $ addPersons p) !! 0)
    | otherwise = "Don't know"
objectLocation item (p:ps) all
    | item `elem` (holding $ addPersons p) = fst (getLocation (name $ addPersons p) (reverse all) !! 0)
    | otherwise = objectLocation item ps all

-- Returns "yes", "no", or "maybe" depending on person location (task 1 & 4)
personInLocation :: String -> String -> [[String]] -> String
personInLocation per loc [] = "?"
personInLocation per loc [p]
    | per == (name (addPersons p)) && [(loc, True)] == getLocation per [p] = "yes"
    | per == (name (addPersons p)) &&  (loc, False) `elem` (getLocation per [p]) = "no"
    | per == (name (addPersons p)) && ((loc, True) `elem` getLocation per [p] && (length (getLocation per [p])>1)) = "maybe"
    | per == (name (addPersons p)) && (length (getLocation per [p])>0) && snd (getLocation per [p] !! 0) == True = "no"
    | otherwise = "maybe"
personInLocation per loc (p:ps)
    | per == (name (addPersons p)) && [(loc, True)] == (getLocation per (p:ps)) = "yes"
    | per == (name (addPersons p)) && (loc, False) `elem` (getLocation per (p:ps)) = "no"
    | per == (name (addPersons p)) && ((loc, True) `elem` getLocation per (p:ps) && (length (getLocation per (p:ps))>0)) = "maybe"
    | per == (name (addPersons p)) && (length (getLocation per [p])>0) && snd (getLocation per (p:ps) !! 0) == True = "no"
    | otherwise = personInLocation per loc (ps)

-- Returns final location of person
getLocation :: String -> [[String]] -> [(String, Bool)]
getLocation per [] = []
getLocation  per p
    | per == (name (addPersons (last p))) && (location $ addPersons (last p)) /= [] = location (addPersons (last p))
    | otherwise = getLocation per (init p)

-- Takes a row from the input text and inteprets it into a person
addPersons :: [String] -> Person
addPersons [n, _, "to", "the", l] = Person {name=n, location=[(l, True)], holding=[]}
addPersons [n, "is", "in", "the", l] = Person n [(l, True)] []

addPersons [n, "took", "the", i] = Person n [] [i]
addPersons [n, "got", "the", i] = Person n [] [i]
addPersons [n, "picked", "up", "the", i] = Person n [] [i]
addPersons [n, "discarded", "the", i] = Person n [] []
addPersons [n, "dropped", "the", i] = Person n [] []
addPersons [a, "handed", "the", i, "to", b] = Person b [] [i]

addPersons [n, "is", "either", "in", "the", a, "or", "the", b] = Person n [(a, True), (b, True)] []
addPersons [n, "is", "no", "longer", "in", "the", a] = Person n [(a, False)]  []
addPersons x = Person "nameless" [] []
