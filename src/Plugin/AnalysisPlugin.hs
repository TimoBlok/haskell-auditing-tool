{-# LANGUAGE OverloadedRecordDot #-}
module Plugin.AnalysisPlugin (plugin) where

import GHC.Plugins
import Core 
import Dependency

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [outputPath] todo = return (CoreDoPluginPass "Find Dependencies" pass : todo)
install _ todo = do 
  putMsgS "incorrect input args: doing nothing"
  return todo

pass :: FilePath -> ModGuts -> CoreM ModGuts
pass outputPath guts = do
  let dependencies = getDependenciesFromCoreBinds guts.mg_module guts.mg_binds
  
  liftIO $ writeFile outputPath $ encode dependencies

  pure guts
