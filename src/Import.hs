{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
pRoutes = [parseRoutes|
   / HomeR GET
   /loja/cadastro LojaR GET POST
   /loja/checar/#LojaId ChecarLojaR GET
   /lojas LojasR GET
   /procedimentos/cadastro ProcedimentosR GET POST
   /procedimentos/checar/#ProcedimentosId ChecarProcedimentosR GET
   /fornecedores/cadastro FornecedoresR GET POST
   /fornecedores/checar/#FornecedoresId ChecarFornecedoresR GET
   /produtos/cadastro ProdutosR GET POST
   /produtos/checar/#ProdutosId ChecarProdutosR GET
   /sobre SobreR GET
   /erro ErroR GET
   /static StaticR Static getStatic
   

|]