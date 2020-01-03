import System.IO
import Control.Monad 

append' lst1 lst2 = lst1 ++ lst2

-- Make a list of questions to ask
questions = ["Are you ready to play", "Is it a mammal? ", "Is it a bird? ", "Does it have 4 legs?", "Does it have 2 legs?", "Is it a carnivore?", "Is it furry?"]
answers1 = []

--List of animals
elephant = ["yes", "no", "yes", "no", "yes", "no"]
shark = ["no", "no", "no", "no", "no", "no"]
eagle = ["no", "yes", "no", "yes", "no", "yes"]
cat = ["yes", "no", "yes", "no", "no", "yes"]
lion = ["yes", "no", "yes", "no", "no", "yes"]

--User input that is put into a list of answers - Answers1
main = do   
    answers: answers1 <- forM questions (\a -> do  
        putStrLn $ show a   
        answer <- getLine
        return answer)
    print answers1
