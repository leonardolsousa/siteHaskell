{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
 
module CadastroLoja where

import Yesod
import Yesod.Core
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql
import Tabelas


mkYesodDispatch "Pagina" pRoutes
instance Yesod Pagina where

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
              <p>
                cadastro deu pau com sucesso
|]
