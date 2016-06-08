{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql

mkYesodDispatch "Pagina" pRoutes

formLoja :: Form Loja
formLoja = let
             cnpj = (fieldSettingsLabel MsgCnpj){fsId=Just "hident2",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","18")]}

             cep = (fieldSettingsLabel MsgCep){fsId=Just "hident5",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","9")]}

             cpf = (fieldSettingsLabel MsgCpfDoResponsavel){fsId=Just "hident11",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","14")]}

             rg = (fieldSettingsLabel MsgRgDoResponsavel){fsId=Just "hident12",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","12")]}
           in
           renderDivs $ Loja <$>
           areq textField (fieldSettingsLabel MsgNomeFantasia) Nothing <*>
           areq textField  cnpj Nothing <*>
           areq textField  (fieldSettingsLabel MsgLogradouro) Nothing <*>
           areq textField  (fieldSettingsLabel MsgNumero) Nothing <*>
           areq textField  cep Nothing <*> 
           areq textField  (fieldSettingsLabel MsgBairro) Nothing <*>
           areq textField  (fieldSettingsLabel MsgCidade) Nothing <*>
           areq textField  (fieldSettingsLabel MsgEstado) Nothing <*>
           areq textField  (fieldSettingsLabel MsgTelefone) Nothing <*>
           areq textField  (fieldSettingsLabel MsgEmail) Nothing <*>
           areq textField  (fieldSettingsLabel MsgNomeDoResponsavel) Nothing <*>
           areq textField  cpf Nothing <*>
           areq textField  rg  Nothing


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
                addStylesheet $ StaticR style_css
                $(whamletFile "templates/menu.hamlet")
                $(whamletFile "templates/home.hamlet")


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



getChecarLojaR :: LojaId -> Handler Html
getChecarLojaR pid = do
    loja <- runDB $ get404 pid
    defaultLayout [whamlet|
        <p><b> _{MsgNomeFantasia}: #{lojaNomeFantasia loja}  
        <p><b> _{MsgCnpj}: #{lojaCnpj loja}
        <p><b> _{MsgEndereco}: #{lojaLogradouro loja}, #{lojaNumero loja}, #{lojaCep loja} - #{lojaBairro loja} - #{lojaCidade loja} - #{lojaEstado loja}
        <p><b> _{MsgContato}: #{lojaTelefone loja} - #{lojaEmail loja}
        <p><b> _{MsgResponsavel}: #{lojaNomeDoResponsavel loja} / RG: #{lojaRgDoResponsavel loja} / CPF: #{lojaCpfDoResponsavel loja}
        
    |]

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
              <p>
               _{MsgErro}
|]

formUser :: Form Users
formUser = renderDivs $ Users <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
          toWidget $ $(luciusFile "templates/perfil.lucius")
          $(whamletFile "templates/perfil.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR


getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
    <h1> Bem-vindo meu Rei!
|]

getLoginR :: Handler Html
getLoginR = do
           deleteSession "_ID"
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout $ $(whamletFile "templates/login.hamlet")

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)

getErroCadR :: Handler Html
getErroCadR = defaultLayout [whamlet|
     <h1> Erro de cadastro
|]

getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> ADEUS!
     |]
     