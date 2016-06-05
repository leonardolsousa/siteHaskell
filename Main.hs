{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}

module Main where


import Yesod
import Yesod.Static
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative

import Rotas
import Tabelas
import CadastroLoja


connStr = "dbname=d6u0i7sja7bad0 host=ec2-54-243-249-176.compute-1.amazonaws.com user=pibvccpjrprgfb password=nMw0gAWUxdfJkNiL38JKbkuOBo port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 8080 (Pagina t pool)
       