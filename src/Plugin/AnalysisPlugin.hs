{-# LANGUAGE OverloadedRecordDot #-}
module Plugin.AnalysisPlugin (plugin) where

import GHC.Plugins
import System.Directory

import Core 
import Dependency
import Json


plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [absolutePath] todo = do 
  liftIO $ putStrLn "hey there!"
  return (CoreDoPluginPass "Find Dependencies" (pass absolutePath): todo)
install xs todo = do 
  putMsgS $ "incorrect input args: doing nothing. args: " ++ show xs
  putMsgS "Please provide an absolute path"
  return todo

pass :: FilePath -> ModGuts -> CoreM ModGuts
pass absolutePath guts = do
  let dependencies = getDependenciesFromCoreBinds guts.mg_module guts.mg_binds
  
      moduleName = showSDocOneLine defaultSDocContext $ ppr guts.mg_module
      path = absolutePath ++ "/" ++ moduleName ++ ".json"  
  liftIO $ encodeFile path dependencies
  putMsgS $ "Another functional dependency subgraph for " ++ moduleName ++ " written to" ++ path

  pure guts
