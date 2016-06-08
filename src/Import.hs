{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
pRoutes = [parseRoutes|
   / HomeR GET
   /loja/cadastro LojaR GET POST
   /loja/checar/#LojaId ChecarLojaR GET
   /lojas LojasR GET
   /erro ErroR GET
   /static StaticR Static getStatic
   /login LoginR GET POST
   /erroCadastro ErroCadR GET
   /usuario UsuarioR GET POST
   /perfil/#UsersId PerfilR GET
   /admin AdminR GET
   /logout LogoutR GET
|]
