{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}

module Main where


import Import
import Yesod
import Yesod.Static
import Foundation
import Handlers
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql



connStr = "dbname=d6eits0lfgc829 host=ec2-54-221-225-242.compute-1.amazonaws.com user=lzpiqwomncxkls password=3oSCMoOnRLv6OZDW6hTlAmIDtE port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 8080 (Pagina t pool)
       