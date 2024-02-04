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

prob_pair :: [(Int,[(String,Int)])] ->   Double
prob_pair x = ((15 * fromIntegral(length filtered)) * (recip(fromIntegral(length x)*47)))

    where
    filtered = filter (\z -> (fst z)==10) x 

prob_2pair ::  [(Int,[(String,Int)])] -> Double
prob_2pair x =  ((9 * fromIntegral(length filtered)) * (recip(fromIntegral((length x) *47))))

    where
    filtered = filter (\z -> (fst z)==9) x 

prob_3kind ::  [(Int,[(String,Int)])] ->   Double
prob_3kind x =  ((2 * fromIntegral(length filtered)) * ((recip(fromIntegral(length x)*47))))

    where
    filtered = filter (\z -> (fst z)==9) x    

straight :: [Int] -> Bool 
straight x =  True `elem` (map (\n -> sort (x ++ [n]) == take ((length x) +1) [head(sort(x++[n]))..]) [2..14])

royal :: [Int] -> Bool 
royal x =  True `elem` (map (\n -> sort (x ++ [n]) == take ((length x) +1) [head(sort(x++[n]))..]) [10..14])

prob_straight :: [(Int,[(String,Int)])] ->   Double
prob_straight x =   ((sum(map  ((\z -> if (True `elem` (map (\k -> straight k )(combinations 4 (snd(unzip z))))) then ((4*recip(fromIntegral 47))) else 0)) (snd (unzip (filter (\z -> (fst z)>6) x ))))) * (recip (fromIntegral(length x))))

prob_flush :: [(Int,[(String,Int)])] ->   Double
prob_flush x = (sum (map (\z -> if ((filter (\k -> length k == 1)(map nub (combinations 4 (fst (unzip z)))))/=[]) then ((9*recip(fromIntegral 47))) else 0 ) (snd (unzip (filter (\z -> (fst z)>5) x )))))* (recip (fromIntegral(length x)))

prob_fullHouse :: [(Int,[(String,Int)])] ->   Double
prob_fullHouse x =  ((4 * fromIntegral(length filtered1)) * (recip(fromIntegral(length x)*47))) + ((6 * fromIntegral(length filtered2)) * (recip(fromIntegral(length x)*47))) 
    where
    filtered1 = filter (\z -> (fst z)==8) x  
    filtered2 = filter (\z -> (fst z)==7) x  

prob_4OfaKind :: [(Int,[(String,Int)])] ->   Double
prob_4OfaKind x = ((1 * fromIntegral(length filtered)) * ((recip(fromIntegral(length x)*47))))

    where
    filtered = filter (\z -> (fst z)==7) x 

prob_straightFlush ::  [(Int,[(String,Int)])] ->   Double
prob_straightFlush x = (prob_straight x) *  (prob_flush x)

prob_royalFlush :: [(Int,[(String,Int)])] ->   Double
prob_royalFlush x = (prob_flush x) * ((sum(map  ((\z -> if (True `elem` (map (\k -> royal k )(combinations 4 (snd(unzip z))))) then ((4*recip(fromIntegral 47))) else 0)) (snd (unzip (filter (\z -> (fst z)>2) x ))))) * (recip (fromIntegral(length x))))

probability :: [(Int,[(String,Int)])] ->   (Double,(String,Double))
probability x = (y,(l,z)) where
    m = (filter (\k -> minimum(fst (unzip x)) == (fst k)) x) 
    y = sum[(prob_pair m),(prob_2pair m),(prob_3kind m),(prob_straight m),(prob_flush m),(prob_fullHouse m),(prob_4OfaKind m),(prob_straightFlush m),(prob_royalFlush m)] 
    z = maximum[(prob_pair m),(prob_2pair m),(prob_3kind m),(prob_straight m),(prob_flush m),(prob_fullHouse m),(prob_4OfaKind m),(prob_straightFlush m),(prob_royalFlush m)] 
    k = head[p | p <- (zip [1..9] (reverse [(prob_pair m),(prob_2pair m),(prob_3kind m),(prob_straight m),(prob_flush m),(prob_fullHouse m),(prob_4OfaKind m),(prob_straightFlush m),(prob_royalFlush m)])), (snd p) == z]
    l = fromJust (lookup (fst k) types)

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
orderer_2 x n 
  | x == filter (\k -> head ((snd (splitAt n (nub(reverse (sort (snd (unzip (concat(snd (unzip x))))))))))) `elem` (snd (unzip (snd k)))) x  = head x
  | otherwise = orderer_2 (filter (\k -> head ((snd (splitAt n (nub(reverse (sort (snd (unzip (concat(snd (unzip x))))))))))) `elem` (snd (unzip (snd k)))) x) (n+1)
  
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


playGame :: [Card] -> [Card] -> [Card]-> Int -> IO ()
playGame playerHand computerHand deck x = do
    putStrLn "Welcome to the card game!"
    putStrLn "You start with"
    print (show x)
    putStrLn "Player's initial hand:"
    print (cardPrint playerHand)
    (newCard, remainingDeck) <- revealCard deck
    (newCard', remainingDeck') <- revealCard remainingDeck
    (newCard'', remainingDeck'') <- revealCard remainingDeck'
    let updatedPlayerdeck = [newCard]++[newCard']++[newCard'']
    putStrLn "Deck:"
    print (cardPrint updatedPlayerdeck)
    putStrLn "Your current best possible combination:"
    print (fst $ fun (best_comb updatedPlayerdeck playerHand))
    putStrLn "Probability of getting a better hand with the next card is:" 
    print (show (fst (probability (map (priority_order) [ playerHand++z |  z <- combinations 3 updatedPlayerdeck]))))
    putStrLn "Highest probability hand"
    print (show (fst (snd (probability (map (priority_order) [ playerHand++z |  z <- combinations 3 updatedPlayerdeck])))))  
    putStrLn "Probability of that hand"
    print (show ( snd (snd (probability (map (priority_order) [ playerHand++z |  z <- combinations 3 updatedPlayerdeck])))))            
    fold <- askFold
    if fold 
        then 
            do
                putStrLn "OK, this is what you have left"
                print x
                putStrLn "Play another round"
                playGame playerHand computerHand deck x
        else do
            again (3) playerHand computerHand remainingDeck'' updatedPlayerdeck x 0

again :: Int -> [Card] -> [Card] -> [Card] -> [Card] -> Int -> Int -> IO()
again n playerHand computerHand deck actualdeck x y = do 
            putStrLn "How much more are you betting? (atleast 100 bucks)"
            
            if x<100 then do
                putStrLn "You have too little, so you have to bet everything"
                input <- getLine
                if (read input) /= x then do 
                    putStrLn "That is not everything"
                    again n playerHand computerHand deck actualdeck x y       
                else do  
                    playRound (n + 1) playerHand computerHand deck actualdeck (x - (read input)) (y + (read input))

            else do        
                input <- getLine
                if (read input) <100 then do 
                    putStrLn "Bet too low"
                    again n playerHand computerHand deck actualdeck x y 
                else do 

                    if (x - (read input))< 0 then do 
                        putStrLn "Youre too broke for that, try again"
                        again n playerHand computerHand deck actualdeck x y

                    else 
                        playRound (n + 1) playerHand computerHand deck actualdeck (x - (read input)) (y + (read input))




playRound :: Int -> [Card] -> [Card] -> [Card] ->  [Card] -> Int-> Int ->IO ()
playRound n playerHand computerHand deck actualdeck x y
    | n > 5 = endGame playerHand computerHand actualdeck x y
    | otherwise = do
        putStrLn "Revealing a card..."
        (newCard, remainingDeck) <- revealCard deck
        let updatedPlayerDeck = newCard : actualdeck
        putStrLn "Your hand:"
        print (cardPrint playerHand)   
        putStrLn "Deck:"
        print (cardPrint updatedPlayerDeck)
        putStrLn "Your current best possible combination:"
        print (fst $ fun (best_comb updatedPlayerDeck playerHand))
        putStrLn "Probability of getting a better hand with the next hand is:" 
        print (show (fst (probability (map (priority_order) [ playerHand++z |  z <- combinations 3 updatedPlayerDeck]))))
        putStrLn "Highest probability hand"
        print (show (fst (snd (probability (map (priority_order) [ playerHand++z |  z <- combinations 3 updatedPlayerDeck])))))  
        putStrLn "Probability of that hand"
        print (show ( snd (snd (probability (map (priority_order) [ playerHand++z |  z <- combinations 3 updatedPlayerDeck])))))      
        fold <- askFold
        if fold
            then do             
            putStrLn "OK, this is what you have left"
            print x
            putStrLn "Play another round"
            playGame playerHand computerHand deck x
        else do again (n) playerHand computerHand remainingDeck updatedPlayerDeck x y

endGame :: [Card] -> [Card] -> [Card] -> Int -> Int -> IO ()
endGame playerHand computerHand deck x y = do
    putStrLn "final deck :\n"
    print (cardPrint deck)    
    let playerWin = finaliser [(best_comb deck playerHand), (best_comb deck computerHand)]
    putStrLn "Your hand :\n"
    print (cardPrint playerHand)    
    putStrLn "Your best comb :\n"
    print (prettyPrint (fun (best_comb deck playerHand)))
    putStrLn "My hand :\n"
    print (cardPrint computerHand)    
    putStrLn "My best comb:\n"
    print (prettyPrint (fun (best_comb deck computerHand)))
    putStrLn $ if playerWin then "You win! And you now  " ++ (show (x+y*3)) else "You lose. You now have " ++ (show (x))
    if  (x > 0) || (playerWin)  then do
        putStrLn "another round? (y/n)"
        input <- getLine 
        if input == "y"
            then if playerWin then do 
                    let fullDeck = generateDeck
                    shuffledDeck <- shuffle fullDeck 
                    let playerHand = [head shuffledDeck] ++  [head (tail shuffledDeck)]
                    let computerHand = [head(tail(tail shuffledDeck))] ++  [head (tail (tail( tail (shuffledDeck))))]
                    playGame playerHand computerHand ((shuffledDeck \\ playerHand)\\ computerHand) (x + y*3)
                 else do 
                    let fullDeck = generateDeck
                    shuffledDeck <- shuffle fullDeck 
                    let playerHand = [head shuffledDeck] ++  [head (tail shuffledDeck)]
                    let computerHand = [head(tail(tail shuffledDeck))] ++  [head (tail (tail( tail (shuffledDeck))))]
                    playGame playerHand computerHand ((shuffledDeck \\ playerHand)\\ computerHand) (x)
        else putStrLn "Goodbye!"
    else putStrLn "Goodbye, you have reached bankruptcy"


shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

initial_money :: Int 
initial_money = 100

main :: IO ()
main = do
    let fullDeck = generateDeck
    shuffledDeck <- shuffle fullDeck 
    let playerHand = [head shuffledDeck] ++  [head (tail shuffledDeck)]
    let computerHand = [head(tail(tail shuffledDeck))] ++  [head (tail (tail( tail (shuffledDeck))))]

    playGame playerHand computerHand ((shuffledDeck \\ playerHand)\\ computerHand) 1000
