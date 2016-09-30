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

---------------------------------------------------------------------------------------

formProcedimentos :: Form Procedimentos
formProcedimentos = renderDivs $ Procedimentos <$>
           areq textField (fieldSettingsLabel MsgProcedimentos) Nothing 
   
---------------------------------------------------------------------------------------

formFornecedores :: Form Fornecedores
formFornecedores = let
             cnpj = (fieldSettingsLabel MsgCnpjFornecedor){fsId=Just "hident2",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","18")]}

           in
           renderDivs $ Fornecedores <$>
           areq textField (fieldSettingsLabel MsgNomeFornecedor) Nothing <*>
           areq textField cnpj Nothing <*>
           areq textField (fieldSettingsLabel MsgTelefoneFornecedor) Nothing <*>
           areq textField (fieldSettingsLabel MsgEmail) Nothing 

---------------------------------------------------------------------------------------

formProdutos :: Form Produtos
formProdutos = renderDivs $ Produtos <$>
               areq textField (fieldSettingsLabel MsgProdutos) Nothing

---------------------------------------------------------------------------------------
       
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
           addStylesheet $ StaticR style_css
           $(whamletFile "templates/menu.hamlet")
           $(whamletFile "templates/home.hamlet")

--------------------------------------------------------------------------------------

getLojaR :: Handler Html
getLojaR = do 
           (widget, enctype) <- generateFormPost formLoja
           defaultLayout $ do
           addStylesheet $ StaticR style_css
           $(whamletFile "templates/menu.hamlet")
           $(whamletFile "templates/cadastrarLoja.hamlet")


postLojaR :: Handler Html
postLojaR = do
           ((result, _), _) <- runFormPost formLoja
           case result of 
               FormSuccess loja -> (runDB $ insert loja) >>= \piid -> redirect (ChecarLojaR piid)
               _ -> redirect ErroR

{-
getLojasR :: LojaId -> Handler Html
getLojasR pid = do 
                lojas <- runDB $ selectList ([]::[Filter Loja]) []
                defaultLayout $ do
                addStylesheet $ StaticR style_css
                $(whamletFile "templates/menu.hamlet")
                $(whamletFile "templates/lojas.hamlet")-}


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
       defaultLayout $ do
       addStylesheet $ StaticR style_css
       $(whamletFile "templates/menu.hamlet")
       $(whamletFile "templates/checarLoja.hamlet")

{-
getChecarLojaR :: LojaId -> Handler Html
getChecarLojaR pid = do
    loja <- runDB $ get404 pid
    defaultLayout [whamlet|
        <p><b> _{MsgNomeFantasia}: #{lojaNomeFantasia loja}  
        <p><b> _{MsgCnpj}: #{lojaCnpj loja}
        <p><b> _{MsgEndereco}: #{lojaLogradouro loja}, #{lojaNumero loja}, #{lojaCep loja} - #{lojaBairro loja} - #{lojaCidade loja} - #{lojaEstado loja}
        <p><b> _{MsgContato}: #{lojaTelefone loja} - #{lojaEmail loja}
        <p><b> _{MsgResponsavel}: #{lojaNomeDoResponsavel loja} / RG: #{lojaRgDoResponsavel loja} / CPF: #{lojaCpfDoResponsavel loja}
        
    |]-}

-------------------------------------------------------------------------------------------------

getProcedimentosR :: Handler Html
getProcedimentosR = do 
                (widget, enctype) <- generateFormPost formProcedimentos
                defaultLayout $ do
                addStylesheet $ StaticR style_css
                $(whamletFile "templates/menu.hamlet")
                $(whamletFile "templates/procedimentos.hamlet")


postProcedimentosR :: Handler Html
postProcedimentosR = do
           ((result, _), _) <- runFormPost formProcedimentos
           case result of 
               FormSuccess procedimento -> (runDB $ insert procedimento) >>= \piid -> redirect (ChecarProcedimentosR piid)
               _ -> redirect ErroR


getChecarProcedimentosR :: ProcedimentosId -> Handler Html
getChecarProcedimentosR pid = do
       procedimentos <- runDB $ get404 pid
       defaultLayout $ do
       addStylesheet $ StaticR style_css
       $(whamletFile "templates/menu.hamlet")
       $(whamletFile "templates/checarProcedimentos.hamlet")



--------------------------------------------------------------------------------------------------------------

getFornecedoresR :: Handler Html
getFornecedoresR = do 
                (widget, enctype) <- generateFormPost formFornecedores
                defaultLayout $ do
                addStylesheet $ StaticR style_css
                $(whamletFile "templates/menu.hamlet")
                $(whamletFile "templates/cadastrarFornecedores.hamlet")


postFornecedoresR :: Handler Html
postFornecedoresR = do
           ((result, _), _) <- runFormPost formFornecedores
           case result of 
               FormSuccess fornecedor -> (runDB $ insert fornecedor) >>= \piid -> redirect (ChecarFornecedoresR piid)
               _ -> redirect ErroR

getChecarFornecedoresR :: FornecedoresId -> Handler Html
getChecarFornecedoresR pid = do
       fornecedores <- runDB $ get404 pid
       defaultLayout $ do
       addStylesheet $ StaticR style_css
       $(whamletFile "templates/menu.hamlet")
       $(whamletFile "templates/checarFornecedores.hamlet")

---------------------------------------------------------------------------------------------------------------

getProdutosR :: Handler Html
getProdutosR = do 
                (widget, enctype) <- generateFormPost formProdutos
                defaultLayout $ do
                addStylesheet $ StaticR style_css
                $(whamletFile "templates/menu.hamlet")
                $(whamletFile "templates/cadastrarProdutos.hamlet")


postProdutosR :: Handler Html
postProdutosR = do
           ((result, _), _) <- runFormPost formProdutos
           case result of 
               FormSuccess produto -> (runDB $ insert produto) >>= \piid -> redirect (ChecarProdutosR piid)
               _ -> redirect ErroR


getChecarProdutosR :: ProdutosId -> Handler Html
getChecarProdutosR pid = do
       produtos <- runDB $ get404 pid
       defaultLayout $ do
       addStylesheet $ StaticR style_css
       $(whamletFile "templates/menu.hamlet")
       $(whamletFile "templates/checarProdutos.hamlet")


-----------------------------------------------------------------------------------------------

getErroR :: Handler Html
getErroR = defaultLayout $ do
           addStylesheet $ StaticR style_css
           $(whamletFile "templates/menu.hamlet")
           $(whamletFile "templates/erro.hamlet")

-----------------------------------------------------------------------------------------------
                
getSobreR :: Handler Html
getSobreR = defaultLayout $ do
            addStylesheet $ StaticR style_css
            $(whamletFile "templates/menu.hamlet")
            $(whamletFile "templates/sobre.hamlet")

