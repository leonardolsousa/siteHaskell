{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}

module Rotas where

import Yesod

pRoutes = [parseRoutes|
   / HomeR GET
   /loja/cadastro LojaR GET POST
   /loja/checar/#LojaId ChecarLojaR GET
   /lojas LojasR GET
   /erro ErroR GET
   /static StaticR Static getStatic
|]