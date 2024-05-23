{-# LANGUAGE OverloadedRecordDot #-}

-- | the plugin responsible for analysing the Core representation
-- and turning it into a call graph (one subgraph per module)
module Plugin.AnalysisPlugin (plugin) where

import GHC.Plugins
    ( Plugin(..),
      Outputable(..),
      GenModule(moduleName, moduleUnit),
      Module,
      purePlugin,
      defaultSDocContext,
      showSDocOneLine,
      putMsgS,
      putMsg,
      defaultPlugin,
      liftIO,
      CoreM,
      CoreToDo(CoreDoPluginPass),
      CommandLineOption,
      ModGuts(mg_binds, mg_module) )
import System.Directory ( listDirectory, removeFile, removeDirectoryRecursive, createDirectory, createDirectoryIfMissing )
import Control.Monad ( forM_ )
import Data.List ( init, dropWhileEnd, takeWhile )
import Data.Char ( isDigit )

import Core ( getDependenciesFromCoreBinds )
import Json ( encodeFile )
import ReadableHelper 

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install,
  pluginRecompile = purePlugin}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [absolutePath] todo = do 
  return (CoreDoPluginPass "Find Dependencies" (pass absolutePath) : todo)
install xs todo = do 
  putMsgS $ "incorrect input args: doing nothing. args: " ++ show xs
  putMsgS "Please provide an absolute path"
  return todo

pass :: FilePath -> ModGuts -> CoreM ModGuts
pass absolutePath guts = do
      -- read the core binds
  let dependencies = getDependenciesFromCoreBinds guts.mg_module guts.mg_binds
  --putMsg $ ppr $ getTopVars guts.mg_binds

      -- create filepath
  let fileName = modNameU guts.mg_module
      path = absolutePath ++ "/" ++ fileName ++ ".json"  

  -- write to file
  liftIO $ encodeFile path dependencies

  -- log
  putMsgS $ "Another functional dependency subgraph for " ++ fileName ++ " written to" ++ path

  -- don't change the program
  pure guts
