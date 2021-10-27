{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- https://qiita.com/kwhrstr1206/items/23085abfaf310d5fae2e
-- Command をGADTsして、Operationalでモナドにできないか？
module Opt
  ( Opt(..)
  , Command(..)
  , parse
  ) where

import           Options.Applicative

data Opt = Opt
  { optCommand :: Command
  }

data Command
  = Check { isDryrun :: Bool
          , diskOpt  :: Maybe String
          , poolOpt  :: Maybe String }
  | Backup { isDryrun :: Bool }

parse :: IO Opt
parse = execParser opts
  where
    opts =
      info
        (optParser <**> helper)
        (fullDesc <> progDesc "Run some command for storage maintenance" <>
         header "hs-storage - a storage management cli")

optParser :: Parser Opt
optParser =
  Opt <$>
  subparser
    (command "check" (info checkOptions (progDesc "check storage")) <>
     command "backup" (info backupOptions (progDesc "backup storage")))

backupOptions :: Parser Command
backupOptions =
  Backup <$> switch (long "dryrun" <> short 'r' <> help "whether to dry run")

checkOptions :: Parser Command
checkOptions =
  Check <$> switch (long "dryrun" <> short 'r' <> help "whether to dry run") <*>
  optional
    (strOption
       (long "disk" <> short 'd' <> metavar "TARGET(,...)" <>
        help "set target disk, support comma separation")) <*>
  optional
    (strOption
       (long "pool" <> short 'p' <> metavar "TARGET(,...)" <>
        help "set disable pool, support comma separation"))
