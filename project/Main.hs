module Main where
import DecisionTree
import DBWork
import Control.Monad
import Database.HDBC.Sqlite3
import Database.HDBC
-- ограничение въпросите да са уникални!

getDirection answer
  | answer == "y" || answer == "yes" = (traceRight, 1)
  | answer == "n" || answer == "no" = (traceLeft, 0)
  | otherwise = error "Invalid answer"

askUser EmptyTree currentPath conn = do
  putStrLn "I give up!"
  animal <- askForAnimal
  withTransaction conn (addAnimalGivenUp animal currentPath)

askUser (Leaf animals) currentPath conn = guessAnimal animals currentPath conn
askUser tree currentPath conn = do
    let questionElement = getQuestion tree
    putStrLn $ question questionElement
    answer <- getLine
    let (direction, answerAnimal) = getDirection answer
    askUser (direction tree) (((idQuestion questionElement), answerAnimal):currentPath) conn

main = do
    conn <- connectSqlite3 "animals_new.db"
    animals <- fetchAnimals conn
    answers <- fetchAnswers conn
    questions <- fetchQuestions conn
    --putStrLn $ show $ buildTree questions answers animals
    askUser (buildTree questions answers animals) [] conn

guessAnimal [animal] path conn = do
  putStrLn $ "Is it " ++ (show animal)
  answer <- getLine
  case answer of
    "yes" -> putStrLn "Yeey!"
    "no"  -> guessedWrong
  where
    guessedWrong = do
      trueAnimal <- askForAnimal
      addAnimalGuessedWrong (idAnimal animal) trueAnimal path conn

guessAnimal a@(animal:animals) path conn = do
  putStrLn $ "Is it one of these: " ++ (show a)
  answer <- getLine
  case answer of
    "yes" -> guessedRightAmong
    "no" -> guessedWrong
  where
    guessedWrong = do
      trueAnimal <- askForAnimal
      addAnimalGuessedWrong (idAnimal animal) trueAnimal path conn
    guessedRightAmong = do
      putStrLn "Yeey"
      guessAnimal [animal] path conn

askForAnimal = do
  putStrLn "What is it?"
  animal <- getLine
  return animal

addAnimalGivenUp animalName answers conn = do
  insertAnimalInDb animalName conn
  animalDb <- getAnimalId animalName conn
  insertManyAnswers (map (\(q, a) -> [a, (idAnimal (extract animalDb)), q]) answers) conn
  commit conn

addAnimalGuessedWrong oursAnimalId animalName path conn = do
  putStrLn "Give me a questions to make difference between these two?"
  question <- getLine
  putStrLn "Which answers 'yes' to it? (ours - 1/ yours - 2)"
  which <- getLine
  withTransaction conn (updateData oursAnimalId animalName question which path)
  return ()

extract (Just a) = a

whichAnimalTrue :: String -> Int -> Int -> (Int, Int)
whichAnimalTrue which oursAnimalId animalId = case which of
    "1" -> (oursAnimalId, animalId)
    "2" -> (animalId, oursAnimalId)

-- Освен това да има списък със всички зададени въпроси до сега и тях да ги запише, както е било отговорено.
updateData oursAnimalId animalName question which path conn = do
  insertAnimalInDb animalName conn
  animalDb <- getAnimalId animalName conn
  insertQuestionInDb question conn
  questionDB <- getQuestionId question conn
  let questionId = (idQuestion.extract) questionDB
  let (trueAnimal, falseAnimal) = whichAnimalTrue which oursAnimalId ((idAnimal.extract) animalDb)
  let otherAnswers = map (\(q, a)-> [a, trueAnimal, q]) path
  insertManyAnswers ([[1, trueAnimal, questionId], [0, falseAnimal, questionId]] ++ otherAnswers) conn
  commit conn






-- ако са няколко, питаме някое от тези ли е? кое? как мога да различавам (head от останалите) от него - задай ми въпрос. Кое отговаря да на въпроса?
-- Записване на въпрос в базата данни
-- добавяне на отговор в базата данни

-- ако е едно и питаме това ли е? кое е тогава? как мога да различавам новото от предположеното?
-- добавяне на животно в базата данни
-- Записване на въпрос в базата данни
-- добавяне на отговор в базата данни

-- ако отгатнем, highfive! играта е свършена
