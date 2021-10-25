{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( check
  ) where

import qualified Control.Foldl  as F
import           Control.Monad
import qualified Data.Text      as T
import           Turtle
import           Turtle.Prelude

run cmd = fold (inshell cmd empty) F.list

run' cmd = shell cmd empty

pickBy f = map (head . T.words) . filter f . map lineToText

check :: IO ()
check = do
  disks <- run "lsblk"
  let disks' = pickBy (T.isInfixOf "disk") disks
  forM_ disks' $ \disk ->
    run' ("smartctl -i /dev/" <> disk) .&&.
    run' ("smartctl -t long /dev/" <> disk) .&&.
    run' ("echo smart tested:" <> disk)
  pools <- run "zpool list -H -o health,name"
  let pools' = pickBy (T.isSuffixOf "ONLINE") pools
  forM_ pools' $ \pool ->
    run' ("zpool scrub " <> pool) .&&. run' ("echo scrubed:" <> pool)
