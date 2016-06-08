{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
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
    
Users json
   nome Text
   login Text
   senha Text
   UniqueUsers login
   deriving Show
    

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
--------------------------------------TESTE-------------------------------------

    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Voce precisa ser admin para entrar aqui"

--------------------------------------TESTE-------------------------------------


type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage