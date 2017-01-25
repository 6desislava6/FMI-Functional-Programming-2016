module Main where
import DecisionTree
import DBWork
import Control.Monad
import Database.HDBC.Sqlite3
import Database.HDBC
-- ограничение въпросите да са уникални!

getDirection answer
  | answer == "y" || answer == "yes" = traceRight
  | answer == "n" || answer == "no" = traceLeft
  | otherwise = error "Invalid answer"

askUser EmptyTree _ = putStrLn "I give up!"
askUser (Leaf animals) conn = guessAnimal animals conn
askUser tree conn = do
    (putStrLn.question.getQuestion) tree
    answer <- getLine
    askUser ((getDirection answer) tree) conn

main = do
    conn <- connectSqlite3 "animals.db"
    animals <- fetchAnimals conn
    answers <- fetchAnswers conn
    questions <- fetchQuestions conn
    askUser (buildTree questions answers animals) conn

guessAnimal [animal] conn = do
  putStrLn $ "Is it " ++ (show animal)
  answer <- getLine
  case answer of
    "yes" -> putStrLn "Yeey!"
    "no"  -> guessedWrong
  where
    guessedWrong = do
      trueAnimal <- askForAnimal
      addAnimal (idAnimal animal) trueAnimal conn

guessAnimal a@(animal:animals) conn = do
  putStrLn $ "Is it one of these: " ++ (show a)
  answer <- getLine
  case answer of
    "yes" -> guessedRightAmong
    "no" -> guessedWrong
  where
    guessedWrong = do
      trueAnimal <- askForAnimal
      addAnimal (idAnimal animal) trueAnimal conn
    guessedRightAmong = do
      putStrLn "Yeey"
      guessAnimal [animal] conn

askForAnimal = do
  putStrLn "What is it?"
  animal <- getLine
  return animal

addAnimal oursAnimalId animalName conn = do
  putStrLn "Give me a questions to make difference between these two?"
  question <- getLine
  putStrLn "Which answers 'yes' to it? (ours - 1/ yours - 2)"
  which <- getLine
  withTransaction conn (updateData oursAnimalId animalName question which)
  return ()

extract (Just a) = a

whichAnimalTrue :: String -> Int -> Int -> (Int, Int)
whichAnimalTrue which oursAnimalId animalId = case which of
    "1" -> (oursAnimalId, animalId)
    "2" -> (animalId, oursAnimalId)

updateData oursAnimalId animalName question which conn = do
  insertAnimalInDb animalName conn
  animalDb <- getAnimalId animalName conn
  insertQuestionInDb question conn
  questionDB <- getQuestionId question conn
  let questionId = (idQuestion.extract) questionDB
  let (trueAnimal, falseAnimal) = whichAnimalTrue which oursAnimalId ((idAnimal.extract) animalDb)
  insertManyAnswers [[1, trueAnimal, questionId], [0, falseAnimal, questionId]] conn
  commit conn






{-
insertAnswers which oursAnimalId animal questionId = [[1, trueAnimal, questionId], [0, falseAnimal, questionId]
  where (trueAnimal, falseAnimal) = whichAnimalTrue which oursAnimalId ((idAnimal.extract) animalDb)-}











-- ако са няколко, питаме някое от тези ли е? кое? как мога да различавам (head от останалите) от него - задай ми въпрос. Кое отговаря да на въпроса?
-- Записване на въпрос в базата данни
-- добавяне на отговор в базата данни

-- ако е едно и питаме това ли е? кое е тогава? как мога да различавам новото от предположеното?
-- добавяне на животно в базата данни
-- Записване на въпрос в базата данни
-- добавяне на отговор в базата данни

-- ако отгатнем, highfive! играта е свършена
