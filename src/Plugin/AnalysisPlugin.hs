{-# LANGUAGE OverloadedRecordDot #-}
module Plugin.AnalysisPlugin (plugin) where

import GHC.Plugins
    ( Plugin(installCoreToDos),
      Outputable(..),
      GenModule(moduleName, moduleUnit),
      Module,
      defaultSDocContext,
      showSDocOneLine,
      putMsgS,
      defaultPlugin,
      liftIO,
      CoreM,
      CoreToDo(CoreDoPluginPass),
      CommandLineOption,
      ModGuts(mg_binds, mg_module) )
import System.Directory ( listDirectory, removeFile )
import Control.Monad ( forM_ )
import Data.List ( init, dropWhileEnd, takeWhile )
import Data.Char ( isDigit )

import Core ( getDependenciesFromCoreBinds ) 
import Json ( encodeFile )
import ReadableHelper 


plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [absolutePath] todo = do 
  return (CoreDoPluginPass "Find Dependencies" (pass absolutePath) : todo)
install xs todo = do 
  putMsgS $ "incorrect input args: doing nothing. args: " ++ show xs
  putMsgS "Please provide an absolute path"
  return todo

pass :: FilePath -> ModGuts -> CoreM ModGuts
pass absolutePath guts = do
  let dependencies = getDependenciesFromCoreBinds guts.mg_module guts.mg_binds

      fileName = modNameU guts.mg_module
      path = absolutePath ++ "/" ++ fileName ++ ".json"  
  liftIO $ encodeFile path dependencies
  putMsgS $ "Another functional dependency subgraph for " ++ fileName ++ " written to" ++ path

  pure guts
