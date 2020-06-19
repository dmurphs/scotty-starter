{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Data.Text.Internal.Lazy
import GHC.Generics
import DB (
  TodoId,
  Todo,
  getTodos,
  getTodoById,
  insertTodo,
  markTodoComplete)
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

-- Data models
newtype InsertTodoRequest = InsertTodoRequest { name :: String } deriving (Generic)
instance FromJSON InsertTodoRequest
instance ToJSON InsertTodoRequest

newtype InsertTodoResponse = InsertTodoResponse { id :: TodoId } deriving (Generic)
instance FromJSON InsertTodoResponse
instance ToJSON InsertTodoResponse

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
