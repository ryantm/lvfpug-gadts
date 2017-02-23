import Data.List
import Control.Error

data DBCon = DBCon [User]
data User = User String
  deriving Show

users = [User "Mike", User "Ryan", User "Asa"]

connectDB :: String -> Either String DBCon
connectDB "correct" = Right (DBCon users)
connectDB _ = Left "Your DB credentials were bad"

getUser :: String -> [User] -> Maybe User
getUser n users = find (\(User n2) -> n == n2) users

lookupUserId :: String -> DBCon -> Either String User
lookupUserId n (DBCon users) =
  note ("Username " ++ n ++ " is not in the database")
       (getUser n users)

connectAndLookup :: String -> String -> Either String User
connectAndLookup dbcred name = do
  dbcon <- connectDB dbcred
  lookupUserId name dbcon

tests :: IO ()
tests = do
  print (connectAndLookup "correct" "Ryan")
  print (connectAndLookup "incorrect" "Ryan")
  print (connectAndLookup "correct" "Asab")
