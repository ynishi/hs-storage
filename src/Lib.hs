{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( runApp
  ) where

import qualified Control.Foldl      as F
import           Control.Monad
import qualified Data.List          as L
import qualified Data.Maybe         as M
import qualified Data.Text          as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Opt
import           Turtle
import           Turtle.Prelude

runApp :: Opt -> IO ()
runApp opt = eval (optCommand opt)

eval (Backup isDryrun backupPool) = do
  d <- Lib.date
  let dryOrDo = Dry isDryrun
  pools <- executeWithResult (Cmd "zpool list -H -o name,health")
  let pools' =
        filter (not . T.isInfixOf "backup") $
        pickBy (T.isSuffixOf "ONLINE") pools
  print pools'
  snapshots <-
    executeWithResult (Cmd "LANG=C zfs list -r -t snapshot -o name,creation")
  forM_ pools' $ \pool -> do
    let snapshots' = L.sort $ pickBy (T.isPrefixOf $ pool <> "@") snapshots
    let original = safeHead snapshots'
    let current = pool <> "@" <> T.pack d
    let backupPool' = T.pack backupPool <> "/" <> pool
    print $
      T.intercalate " " [pool, T.pack $ show original, current, backupPool']
    execute $ dryOrDo $ "zfs snapshot -r " <> current
    let sendTarget = maybe "" (\orig -> "-i " <> orig <> " ") original
    execute $
      dryOrDo $
      "zfs send " <> sendTarget <> current <> " | zfs recv -F " <> backupPool'
eval (Check isDryrun diskOpt poolOpt) = do
  let diskF =
        case diskOpt of
          Just disk -> T.isPrefixOf $ T.pack disk <> " "
          Nothing   -> T.isInfixOf "disk"
  disks <- executeWithResult (Cmd "lsblk")
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
  pools <- executeWithResult (Cmd "zpool list -H -o health,name")
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

execute (Cmd cmd)      = shell cmd empty
execute (Dry True cmd) = execute (Cmd ("echo Dryrun: \'" <> cmd <> "\'"))
execute (Dry _ cmd)    = execute (Cmd cmd)
execute (Cmds [h])     = execute h
execute (Cmds (h:t))   = execute h .&&. execute (Cmds t)

executeWithResult (Cmd cmd) = fold (inshell cmd empty) F.list

pickBy f = map (head . T.words) . filter f . map lineToText

safeHead []    = Nothing
safeHead (h:_) = Just h
