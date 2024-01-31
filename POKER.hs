import System.Random
import Data.List
import Data.Function
import Data.List (delete)
import Data.Array.IO
import Data.Maybe

types :: [(Int,String)]
types = [(1,"Royal Flush"),(2,"Straight Flush"),(3,"Four of a kind"),(4,"Full House"),(5,"Flush"),(6,"Straight"),(7,"Three of a kind"),(8,"Two Pair"),(9,"Pair"),(10,"High card")]

numbers :: [(Int,String)]
numbers = [(2,"Two"),(3,"Three"),(4,"Four"),(5,"Five"),(6,"Six"),(7,"Seven"),(8,"Eight"),(9,"Nine"),(10,"Ten"),(11,"Jack"),(12,"Queen"),(13,"King"),(14,"Ace")]

prettyPrint :: (String,[(String,Int)]) -> (String,[String])
prettyPrint x = ((fst x), cardPrint(snd x))

cardPrint :: [(String,Int)] -> [String]
cardPrint x =  map (\z ->(fromJust (lookup (snd z) numbers)) ++ " of " ++ (fst z))   (x)

fun :: (Int,[(String,Int)]) -> (String,[(String,Int)])
fun x = (fromJust(lookup (fst x) types),(snd x))

priority_order :: [(String,Int)]-> (Int,[(String,Int)])
priority_order x 
  | length (nub (fst (unzip x))) == 1 && (sort (snd (unzip x))) == [10,11,12,13,14] = (1,x)
  | length (nub (fst (unzip x))) == 1 &&  ((take (length x) [head (sort (snd (unzip x)))..]) == (sort (snd (unzip x))) || (sort (snd (unzip x))) == [2,3,4,5,6] ) = (2,x)
  | length (filter (== head(sort (snd (unzip x)))) (sort (snd (unzip x)))) `elem` [1,4] && length (nub (snd (unzip x))) == 2 = (3,x)
  | length (filter (== head(sort (snd (unzip x)))) (sort (snd (unzip x)))) `elem` [2,3] && length (nub (snd (unzip x))) == 2 = (4,x)
  | length (nub (fst (unzip x))) == 1 = (5,x)
  | (take (length x) [head (sort (snd (unzip x)))..]) == (sort (snd (unzip x))) || (sort (snd (unzip x))) == [2,3,4,5,6] = (6,x)
  | 3 `elem` map (\k -> length (filter (== (snd k)) (sort (snd (unzip x))))) x  && length (nub (snd (unzip x))) == 3 = (7,x)
  | length (filter (== head(sort (snd (unzip x)))) (sort (snd (unzip x)))) `elem` [1,2] && length (nub (snd (unzip x))) == 3 = (8,x)
  | length (nub (snd (unzip x))) == 4 = (9,x)
  | otherwise = (10,x)

orderer_1 :: [(Int,[(String,Int)])] -> (Int,[(String,Int)])
orderer_1 x = orderer_2 (filter (\k -> minimum(fst (unzip x)) == (fst k)) x) 0

orderer_2 :: [(Int,[(String,Int)])] -> Int -> (Int,[(String,Int)])
orderer_2 [x] _ = x
orderer_2 x n = orderer_2 (filter (\k -> head ((snd (splitAt n (nub(reverse (sort (snd (unzip (concat(snd (unzip x))))))))))) `elem` (snd (unzip (snd k)))) x) (n+1)

best_comb :: [(String,Int)] -> [(String,Int)] -> (Int,[(String,Int)]) 
best_comb x y = orderer_1(map (priority_order) [ y++z |  z <- combinations 3 x])

combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs


finaliser :: [(Int,[(String,Int)])] -> Bool
finaliser x = orderer_1 x == head x

type Card = (String, Int)
type Hand = [Card]


generateDeck :: [Card]
generateDeck = [(suit, rank) | suit <- suits, rank <- [2..14]]
    where suits = ["Spade", "Heart", "Diamond", "Club"]


randomCard :: [Card] -> IO (Card, [Card])
randomCard availableCards = do
    index <- randomRIO (0, length availableCards - 1)
    let card = availableCards !! index
    let newAvailableCards = delete card availableCards
    return (card, newAvailableCards)


generateHand :: Int -> [Card] -> IO ([Card], [Card])
generateHand n availableCards = do
    let (hand, remainingCards) = splitAt n availableCards
    return (hand, remainingCards)

askFold :: IO Bool
askFold = do
    putStrLn "Do you want to fold? (yes/no)"
    input <- getLine
    return (input == "yes")

revealCard :: [Card] -> IO (Card, [Card])
revealCard deck = do
    (card, remainingDeck) <- randomCard deck
    putStrLn $ "A card is revealed: " ++ show card
    return (card, remainingDeck)


playGame :: [Card] -> [Card] -> [Card]-> IO ()
playGame playerHand computerHand deck = do
    putStrLn "Welcome to the card game!"
    putStrLn "Player's initial hand:"
    print (cardPrint playerHand)
    (newCard, remainingDeck) <- revealCard deck
    (newCard', remainingDeck') <- revealCard remainingDeck
    (newCard'', remainingDeck'') <- revealCard remainingDeck'
    let updatedPlayerdeck = [newCard]++[newCard']++[newCard'']
    putStrLn "Deck:"
    print (cardPrint updatedPlayerdeck)
    fold <- askFold
    if fold
        then 


            endGame playerHand computerHand updatedPlayerdeck
    else playRound 3 playerHand   computerHand remainingDeck'' updatedPlayerdeck


playRound :: Int -> [Card] -> [Card] -> [Card] ->  [Card] ->IO ()
playRound n playerHand computerHand deck actualdeck
    | n >= 5 = endGame playerHand computerHand actualdeck
    | otherwise = do
        putStrLn $ "Round " ++ show n ++ ":"
        putStrLn "Revealing a card..."
        (newCard, remainingDeck) <- revealCard deck
        let updatedPlayerDeck = newCard : actualdeck
        putStrLn "Deck:"
        print (cardPrint updatedPlayerDeck)
        fold <- askFold
        if fold
            then endGame playerHand computerHand updatedPlayerDeck
            else playRound (n + 1) playerHand computerHand remainingDeck updatedPlayerDeck 

endGame :: [Card] -> [Card] -> [Card] -> IO ()
endGame playerHand computerHand deck = do
    let playerWin = finaliser [(best_comb deck playerHand), (best_comb deck computerHand)]
    putStrLn "Your hand :\n"
    print (cardPrint playerHand)    
    putStrLn "Your best comb :\n"
    print (prettyPrint (fun (best_comb deck playerHand)))
    putStrLn "My hand :\n"
    print (cardPrint computerHand)    
    putStrLn "My best comb:\n"
    print (prettyPrint (fun (best_comb deck computerHand)))
    putStrLn $ if playerWin then "You win!" else "You lose."

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

main :: IO ()
main = do
    let fullDeck = generateDeck
    shuffledDeck <- shuffle fullDeck 
    let playerHand = [head shuffledDeck] ++  [head (tail shuffledDeck)]
    let computerHand = [head(tail(tail shuffledDeck))] ++  [head (tail (tail( tail (shuffledDeck))))]
    playGame playerHand computerHand ((shuffledDeck \\ playerHand)\\ computerHand)
