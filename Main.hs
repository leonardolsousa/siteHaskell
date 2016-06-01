{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
             

module Main where

import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative


data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Loja json
    nomeFantasia Text
    cnpj Text
    logradouro Text
    numero Text
    cep Text
    bairro Text
    cidade Text
    estado Text
    telefone Text
    email Text
    nomeDoResponsavel Text
    cpfDoResponsavel Text
    rgDoResponsavel Text

|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET POST
/loja/cadastro LojaR GET POST
/loja/checar/#LojaId ChecarLojaR GET
/lojas LojasR GET
/erro ErroR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage
------------------------


connStr = "dbname=d6u0i7sja7bad0 host=ec2-54-243-249-176.compute-1.amazonaws.com user=pibvccpjrprgfb password=nMw0gAWUxdfJkNiL38JKbkuOBo port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)