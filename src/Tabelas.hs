{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, EmptyDataDecls, ViewPatterns #-}

module Tabelas where
import Rotas
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Pagina = Pagina {getStatic :: Static, connPool :: ConnectionPool }

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
    
Registro json
    nomeCompleto Text
    nomePetshop Text
    telefone Text
    email Text
    senha Text
    

|]

staticFiles "static"

mkYesodData "Pagina" pRoutes

mkMessage "Pagina" "messages" "pt-br"

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
       
instance Yesod Pagina where

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage