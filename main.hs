{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
             


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
/ HomeR GET
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


formLoja :: Form Loja
formLoja = renderDivs $ Loja <$>
           areq textField "Nome Fantasia: " Nothing <*>
           areq textField "CNPJ: " Nothing <*>
           areq textField "Logradouro: " Nothing <*>
           areq textField "Número: " Nothing <*>
           areq textField "CEP: " Nothing <*>
           areq textField "Bairro: " Nothing <*>
           areq textField "Cidade: " Nothing <*>
           areq textField "Estado: " Nothing <*>
           areq textField "Telefone: " Nothing <*>
           areq textField "E-mail: " Nothing <*>
           areq textField "Nome do Responsável: " Nothing <*>
           areq textField "CPF do Responsável: " Nothing <*>
           areq textField "RG do Responsável: " Nothing

getLojaR :: Handler Html
getLojaR = do
           (widget, enctype) <- generateFormPost formLoja
           defaultLayout $ do 
           toWidget [cassius|
               label
                   color:black;
                   font-weight: bold;
           |]
           [whamlet|
                 <form method=post enctype=#{enctype} action=@{LojaR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
                     
           |]

postLojaR :: Handler Html
postLojaR = do
           ((result, _), _) <- runFormPost formLoja
           case result of 
               FormSuccess loja -> (runDB $ insert loja) >>= \piid -> redirect (ChecarLojaR piid)
               _ -> redirect ErroR

getLojasR :: Handler Html
getLojasR = do
            lojas <- runDB $ selectList ([]::[Filter Loja]) []
            defaultLayout $ [whamlet|
                <ul>
                    $forall Entity id loja <- lojas
                        <li>[#{fromSqlKey id}] #{lojaNomeFantasia loja}
            |]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Where's Pet!|]

{-getHomeR :: Handler Html
getHomeR = do defaultLayout
defaultLayout $ do 
           toWidget [cassius|
               label
                color:black;
                font-weight: bold;
                |]
-}




getChecarLojaR :: LojaId -> Handler Html
getChecarLojaR pid = do
    loja <- runDB $ get404 pid
    defaultLayout [whamlet|
        <p><b> Nome Fantasia: #{lojaNomeFantasia loja}  
        <p><b> CNPJ: #{lojaCnpj loja}
        <p><b> Endereço: #{lojaLogradouro loja}, #{lojaNumero loja}, #{lojaCep loja} - #{lojaBairro loja} - #{lojaCidade loja} - #{lojaEstado loja}
        <p><b> Contato: #{lojaTelefone loja} - #{lojaEmail loja}
        <p><b> Responsável: #{lojaNomeDoResponsavel loja} / RG: #{lojaRgDoResponsavel loja} / CPF: #{lojaCpfDoResponsavel loja}
        
    |]

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    cadastro deu pau com sucesso
|]


connStr = "dbname=d6u0i7sja7bad0 host=ec2-54-243-249-176.compute-1.amazonaws.com user=pibvccpjrprgfb password=nMw0gAWUxdfJkNiL38JKbkuOBo"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)
       