import System.IO
import Control.Monad 

areEqual lst1 lst2
 |lst2 == [] = "Could't find you animal"
 |lst1 == fst (head lst2) = "Is your animal " ++ snd (head lst2) ++ " ?"
 |otherwise = areEqual lst1 (tail lst2)

-- Make a list of questions to ask
questions = ["Are you ready to play", "Is it a mammal? ", "Is it a bird? ", "Does it have 4 legs?", "Does it have 2 legs?", "Is it a carnivore?", "Is it furry?"]
answers1 = []

--List of animals
elephant = ["yes", "no", "yes", "no", "yes", "no"]
shark = ["no", "no", "no", "no", "no", "no"]
eagle = ["no", "yes", "no", "yes", "no", "yes"]
cat = ["yes", "no", "yes", "no", "no", "yes"]
lion = ["yes", "no", "yes", "no", "no", "yes"]

listAnimals = [(elephant, "elephant") , (shark, "shark"), (eagle, "eagle") , (cat, "cat") , (lion, "lion")]

--User input that is put into a list of answers - Answers1
main = do   
    answer: answers1 <- forM questions (\a -> do  
        putStrLn $ show a   
        answer <- getLine
        return answer)
    putStrLn (areEqual answers1 listAnimals)
    input <- getLine
    if input == "Yes" then putStrLn "Great!" else putStrLn "Could't find your animal :("
    putStrLn "Do you want to play again?"
    input1 <- getLine
    if input1 == "Yes" then main else putStrLn "See you next time!"


   --if areEqual answers1 listAnimals then print answers1 else putStrLn "Could't guess your animal :("

