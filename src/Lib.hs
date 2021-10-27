{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( runApp
  ) where

import qualified Control.Foldl      as F
import           Control.Monad
import qualified Data.Text          as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Opt
import           Turtle
import           Turtle.Prelude

runApp :: Opt -> IO ()
runApp opt = eval (optCommand opt)

eval (Backup isDryrun) = do
  d <- Lib.date
  pools <- run "zpool list -H -o name,health"
  let pools' =
        filter (not . T.isInfixOf "backup") $
        pickBy (T.isSuffixOf "ONLINE") pools
  print pools'
  snapshots <- run "LANG=C zfs list -r -t snapshot -o name,creation"
  forM_ pools' $ \pool -> do
    let snapshots' = pickBy (T.isPrefixOf pool) snapshots
    print pool
    execute $ Dry isDryrun $ "zfs snapshot -r " <> pool <> "@" <> T.pack d
eval (Check isDryrun diskOpt poolOpt) = do
  let diskF =
        case diskOpt of
          Just disk -> T.isPrefixOf $ T.pack disk <> " "
          Nothing   -> T.isInfixOf "disk"
  disks <- run "lsblk"
  let disks' = pickBy diskF disks
  forM_ disks' $ \disk ->
    execute $
    Cmds
      [ Cmd ("smartctl -i /dev/" <> disk)
      , Dry isDryrun $ "smartctl -t long /dev/" <> disk
      , Cmd ("echo smart tested:" <> disk)
      ]
  let poolF x =
        T.isSuffixOf "ONLINE" x &&
        case poolOpt of
          Just pool -> T.isSuffixOf (T.pack pool <> " ") x
          Nothing   -> True
  pools <- run "zpool list -H -o health,name"
  let pools' = pickBy poolF pools
  forM_ pools' $ \pool ->
    execute $
    Cmds [Dry isDryrun $ "zpool scrub " <> pool, Cmd $ "echo scrubed:" <> pool]

-- helpers
date :: IO String
date = formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime

data Cmd
  = Cmd Text
  | Dry Bool
        Text
  | Cmds [Cmd]
  deriving (Show, Eq)

execute (Cmd cmd)      = run' cmd
execute (Dry True cmd) = execute (Cmd (convert True cmd))
execute (Dry _ cmd)    = execute (Cmd cmd)
execute (Cmds [h])     = execute h
execute (Cmds (h:t))   = execute h .&&. execute (Cmds t)

run cmd = fold (inshell cmd empty) F.list

run' cmd = shell cmd empty

pickBy f = map (head . T.words) . filter f . map lineToText

convert isDryrun cmd =
  (if isDryrun
     then "echo Dryrun:"
     else "") <>
  cmd
