module DBWork where
import DecisionTree
import Database.HDBC.Sqlite3
import Database.HDBC
import Data.List


-- Константи?

transfromData :: (Record a) => [[(String, SqlValue)]] -> [a]
transfromData results = map transform results

fetchData :: (Record a, IConnection conn) => String -> conn -> IO [a]
fetchData table conn = do
  query <- prepare conn $ "SELECT * FROM " ++ table
  execute query []
  results <- fetchAllRowsAL query
  return $ transfromData results

fetchAnimals :: (Record a, IConnection conn) => conn -> IO [a]
fetchAnimals = fetchData "Animals"
fetchQuestions :: (Record a, IConnection conn) => conn -> IO [a]
fetchQuestions = fetchData "Questions"
fetchAnswers :: (Record a, IConnection conn) => conn -> IO [a]
fetchAnswers = fetchData "Answers"

insertInDb table values conn = do
  run conn ("INSERT INTO " ++ table ++ " VALUES (" ++ stmt ++ ")") values
  where
    stmt = intercalate ", " (take (length values) (repeat "?"))

insertAnimalInDb name = insertInDb "Animals" [SqlNull, toSql name]
insertQuestionInDb question = insertInDb "Questions" [SqlNull, toSql question]

insertAnswerInDb answer animalId questionId = insertInDb "Answers" [SqlNull, toSql answer, toSql animalId, toSql questionId]

getObjectId::(Record a, IConnection conn) => (conn -> IO [a]) -> (a -> String) -> String -> conn -> IO (Maybe a)
getObjectId objectsFunc contentFunc feature conn = do
  objects <- objectsFunc conn
  return $ find (\o -> (contentFunc o) == feature) objects

getAnimalId :: (IConnection conn) => String -> conn -> IO (Maybe Animal)
getAnimalId = getObjectId fetchAnimals nameAnimal
getQuestionId :: (IConnection conn) => String -> conn -> IO (Maybe Question)
getQuestionId = getObjectId fetchQuestions question


insertMany table values conn = do
  stmt <- prepare conn $ "INSERT INTO " ++ table ++ " VALUES (" ++ partStm ++ ")"
  executeMany stmt values
  where
    partStm = intercalate ", " (take ((length.head) values) (repeat "?"))

insertManyAnswers values = insertMany "Answers" $ map ((SqlNull:).(map toSql)) values
