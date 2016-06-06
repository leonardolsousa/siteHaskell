{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
 
module CadastroLoja where
import Rotas
import Yesod
import Yesod.Core
import Tabelas
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

mkYesodDispatch "Pagina" pRoutes

formLoja :: Form Loja
formLoja = renderDivs $ Loja <$>
           areq textField (fieldSettingsLabel MsgNomeFantasia) Nothing <*>
           areq textField  FieldSettings{fsId=Just "hident2",
                           fsLabel="CNPJ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","18")]} Nothing <*>
           areq textField  (fieldSettingsLabel MsgLogradouro) Nothing <*>
           areq textField  (fieldSettingsLabel MsgNumero) Nothing <*>
           areq textField  FieldSettings{fsId=Just "hident5",
                           fsLabel="CEP",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","9")]} Nothing <*> 
           areq textField  (fieldSettingsLabel MsgBairro) Nothing <*>
           areq textField  (fieldSettingsLabel MsgCidade) Nothing <*>
           areq textField  (fieldSettingsLabel MsgEstado) Nothing <*>
           areq textField  (fieldSettingsLabel MsgTelefone) Nothing <*>
           areq textField  (fieldSettingsLabel MsgEmail) Nothing <*>
           areq textField (fieldSettingsLabel MsgNomeDoResponsavel) Nothing <*>
           areq textField  FieldSettings{fsId=Just "hident11",
                           fsLabel="CPF do Responsável",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","14")]} Nothing <*>
           areq textField  FieldSettings{fsId=Just "hident12",
                           fsLabel="RG do Responsável",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","12")]} Nothing


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

