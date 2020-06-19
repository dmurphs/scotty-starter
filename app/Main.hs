{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Data.Text.Internal.Lazy
import GHC.Generics
import GHC.Int
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types.Status (status201, status204, status404)
import Web.Scotty (
  middleware,
  scotty,
  json,
  get,
  post,
  patch,
  param,
  status,
  jsonData)
import System.ReadEnvVar (readEnvDef)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- Data models
type TodoId = Int64

data Todo = Todo { id :: TodoId, name :: String, completed :: Bool } deriving (Generic)
instance FromJSON Todo
instance ToJSON Todo

newtype InsertTodoRequest = InsertTodoRequest { name :: String } deriving (Generic)
instance FromJSON InsertTodoRequest
instance ToJSON InsertTodoRequest

newtype InsertTodoResponse = InsertTodoResponse { id :: Int64 } deriving (Generic)
instance FromJSON InsertTodoResponse
instance ToJSON InsertTodoResponse

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

-- App
main :: IO ()
main = do
  port <- readEnvDef "APP_PORT_NUMBER" 8080
  scotty port $ do
    middleware logStdout

    get "/todos" $ do
      todos <- liftIO getTodos
      Web.Scotty.json todos

    get "/todos/:id" $ do
      todoId <- param "id"
      todo <- liftIO $ getTodoById todoId
      case todo of
        Just t -> Web.Scotty.json todo
        _      -> status status404

    post "/todos" $ do
      insertRequest <- jsonData
      let todoName = (name :: InsertTodoRequest -> String) insertRequest
      newTodoId <- liftIO $ insertTodo todoName
      status status201
      Web.Scotty.json $ InsertTodoResponse newTodoId

    patch "/todos/complete/:id" $ do
      todoId <- param "id"
      liftIO $ markTodoComplete todoId
      status status204
