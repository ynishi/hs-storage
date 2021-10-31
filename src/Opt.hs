{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Opt
  ( Opt(..)
  , Command(..)
  , parse
  ) where

import           Options.Applicative

newtype Opt = Opt
  { optCommand :: Command
  }

data Command
  = Check { isDryrun :: Bool
          , diskOpt  :: Maybe String
          , poolOpt  :: Maybe String }
  | Backup { isDryrun      :: Bool
           , backupPool    :: String
           , keepSnapshots :: Int }

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
  Backup <$> switch (long "dryrun" <> short 'r' <> help "whether to dry run") <*>
  strOption
    (long "backupPool" <> short 'b' <> metavar "BACKUP_POOL" <>
     value "backup-tank" <>
     help "set target backup pool for zfs recv") <*>
  option
    auto
    (long "keepSnapshots" <> short 'k' <> metavar "INT" <> value 360 <>
     help "set max number of snapshot keep(when no delete, set -1)")

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
