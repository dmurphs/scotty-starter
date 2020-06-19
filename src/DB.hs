{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module DB where

import Data.Aeson
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics
import GHC.Int

type TodoId = Int64

data Todo = Todo { id :: TodoId, name :: String, completed :: Bool } deriving (Generic)
instance FromJSON Todo
instance ToJSON Todo

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

-- DB stuff
getTodos :: IO [Todo]
getTodos = withConnection "todos.db" $ \conn ->
  query_ conn "SELECT id, name, completed from todos" :: IO [Todo]

getTodoById :: TodoId -> IO (Maybe Todo)
getTodoById todoId = withConnection "todos.db" $ \conn ->
  do
    results <- queryNamed conn "SELECT id, name, completed from todos WHERE id = :todoId" [":todoId" := todoId] :: IO [Todo]
    return $ case results of
      []  -> Nothing
      x:_ -> Just x

insertTodo :: String -> IO TodoId
insertTodo todoName = withConnection "todos.db" $ \conn ->
  do
    executeNamed conn "INSERT INTO todos (name, completed) VALUES (:name, 0)" [":name" := todoName]
    lastInsertRowId conn

markTodoComplete :: TodoId -> IO ()
markTodoComplete todoId = withConnection "todos.db" $ \conn ->
  executeNamed conn "UPDATE todos SET completed = 1 WHERE id = :todoId" [":todoId" := todoId]
