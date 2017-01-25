-- {-# LANGUAGE FlexibleInstances #-}
module DecisionTree where
import Database.HDBC
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

class Record a where
  transform :: [(String, SqlValue)] -> a
  getId :: a -> Int
  content :: a -> Either String Int

instance Record Animal where
  transform [(_, x), (_, y)] = Animal (fromSql x) (fromSql y)
  getId = idAnimal
  content o = Left $ nameAnimal o

instance Record Question where
  transform [(_, x), (_, y)] = Question (fromSql x) (fromSql y)
  getId = idQuestion
  content o = Left $ question o

instance Record Answer where
  transform record = toTupleOfFour $ map snd record
  getId = idAnswer
  content o = Right $ answer o

toTupleOfFour [x, y, z, t] = Answer (fromSql x) (fromSql y) (fromSql z) (fromSql t)

data Animal = Animal {idAnimal:: Int, nameAnimal :: String } deriving(Show, Read, Eq, Ord)
data Question = Question {idQuestion :: Int, question :: String } deriving(Show, Read, Eq, Ord)
data Answer = Answer {idAnswer:: Int, answer:: Int, animalId :: Int, questionId :: Int} deriving(Show, Read, Eq, Ord)

data DTree = EmptyTree | Node Question DTree DTree | Leaf [Animal] deriving(Show)


answersToMap :: [Answer] -> Map.Map Int [Answer]
answersToMap answers = Map.fromList $ map questionMap $ ((groupBy combine).(sortBy sorting)) answers
  where
    sorting answerFirst answerSecond = compare (questionId answerFirst) (questionId answerSecond)
    combine answerFirst answerSecond = (questionId answerFirst) == (questionId answerSecond)
    questionMap r = ((questionId.head) r, r)


-- id of the question, result from lookup in Map of answers, total number of animals
entropy :: Floating a => [a1] -> Maybe [Answer] -> a
entropy animals Nothing = 0
entropy animals (Just answers) = ((calc_fraction positive) + (calc_fraction negative)) * (fromIntegral (positive + negative)) / (fromIntegral total)
  where
    positive = length $ filter ((==1).answer) answers
    negative =  length $ filter ((==0).answer) answers
    total = length animals
    calc_fraction x = (-1 *  (fromIntegral x))/(fromIntegral total) * (log ((fromIntegral x)/(fromIntegral total)))

-- returns the question with biggest entropy
biggestEntropy :: [Question] -> [a1] -> [Answer] -> Question
biggestEntropy questions animals answers = fst $ maximumBy comparing $ map (\ q -> (q, entropy animals (Map.lookup (idQuestion q) mapAnswers))) questions
  where
    mapAnswers = answersToMap answers
    comparing (_, e1) (_, e2) = compare e1 e2

getAnimalsUnknownAnswer :: [Animal] -> [Animal] -> [Animal] -> [Animal]
getAnimalsUnknownAnswer animals positiveAnimals negativeAnimals = Set.toList(Set.difference (Set.fromList animals) union)
  where
    positiveAnimalsSet = Set.fromList positiveAnimals
    negativeAnimalsSet = Set.fromList negativeAnimals
    union = Set.union positiveAnimalsSet negativeAnimalsSet

--getAnimalsOnAnswer Nothing _ _ = []
getAnimalsOnAnswer :: [Answer] -> [Animal] -> Int -> Int -> [Animal]
getAnimalsOnAnswer answers animals targetAnswer targetQuestion = map (\id -> head $ (filter ((==id).(idAnimal)) animals)) answeredAnimals
  where
    answeredAnimals = map animalId $ filter (\a -> (answer a)==targetAnswer && (questionId a) ==targetQuestion) answers

splitAnswer  :: Question -> [Answer] -> [Animal] -> ([Animal], [Animal])
splitAnswer nextQuestion answers animals = (negativeAnimals ++ unknownAnimals, positiveAnimals ++ unknownAnimals)
  where
    questionId = idQuestion nextQuestion
    positiveAnimals = getAnimalsOnAnswer answers animals 1 questionId
    negativeAnimals = getAnimalsOnAnswer answers animals 0 questionId
    unknownAnimals = getAnimalsUnknownAnswer animals positiveAnimals negativeAnimals

-- Builds the decision tree based on questions, animals and answers
buildTree :: [Question] -> [Answer] -> [Animal] -> DTree
buildTree _ _ [] = EmptyTree
buildTree _ [] animals = EmptyTree
buildTree [] _ animals = Leaf animals
buildTree _ _ [animal] = Leaf [animal]
buildTree questions answers animals = Node nextQuestion (buildTree filteredQuestions answersLeft leftAnimals) (buildTree filteredQuestions answersRight rightAnimals)
  where
    nextQuestion = biggestEntropy questions animals answers
    (leftAnimals, rightAnimals) = splitAnswer nextQuestion answers animals
    leftAnimalsIds = map idAnimal leftAnimals
    filteredQuestions = delete nextQuestion questions
    answersLeft = filterAnswers leftAnimals answers
    answersRight = filterAnswers rightAnimals answers

-- filters the answers only for specific animals
filterAnswers :: [Animal] -> [Answer] -> [Answer]
filterAnswers animals answers = filter (\a -> (animalId a) `elem` animalsIds) answers
  where animalsIds = map idAnimal animals


data Result = EndResult [Animal] | NoResult | Result DTree

traceTree _ EmptyTree = NoResult
traceTree direction (Leaf animals) = EndResult animals
traceTree direction tree = Result (direction tree)

traceLeft (Node _ tree _) = tree
traceRight (Node _ _ tree) = tree
getQuestion (Node question _ _) = question


{-prettyprint (Leaf [Animal]) = show [Animal]
prettyprint (Node question left right) = unlines (prettyprint_helper (Node question left right n h))
-}
{-prettyprint_helper (Leaf animals) = [show animals]
prettyprint_helper (Node question left right) = (show question) : (prettyprint_subtree left right)
        where
            prettyprint_subtree left right =
                ((pad "+- " "|  ") (prettyprint_helper right))
                    ++ ((pad "`- " "   ") (prettyprint_helper left))
            pad first rest = zipWith (++) (first : repeat rest)-}


