{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
                      TemplateHaskell, GADTs, FlexibleInstances,
                      MultiParamTypeClasses, DeriveDataTypeable,
                      GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}

module RegistroUsuario where
import Import
import Yesod
import Yesod.Core
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

mkYesodDispatch "Registro" pRoutes
formRegistro :: Form Registro
formRegistro = renderDivs $ Registro <$>
                areq textField "Nome Completo: " Nothing <*>
                areq textField "Nome PetShop:" Nothing <*>
                areq textField "Telefone: " Nothing <*>
                areq textField "E-mail: " Nothing <*>
                areq textField "Confirmar E-mail: " Nothing <*>
                areq textField "Senha: " Nothing <*>
                areq textFiled "Confirmar Senha: " Nothing <*>