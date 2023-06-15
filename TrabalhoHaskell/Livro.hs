module Livro (mainLivro, LivroInfo, livroTemCodigo) where

import Data.List.Split
import Data.Char

type Codigo = Int
type Titulo = String
type Autor = String
type Editora = String
type AnoPublicacao = Int

data LivroInfo = Livro Codigo Titulo [Autor] Editora AnoPublicacao
    deriving (Show)

criarLivro :: Codigo -> Titulo -> [Autor] -> Editora -> AnoPublicacao -> LivroInfo
criarLivro cod tit aut edt ano = Livro cod tit aut edt ano

adicionarLivro :: [LivroInfo] -> IO [LivroInfo]
adicionarLivro livros = do
    putStrLn "Digite o código do livro:"
    codigoStr <- getLine
    let codigo = read codigoStr :: Codigo
    putStrLn ""

    putStrLn "Digite o título do livro:"
    titulo <- getLine
    putStrLn ""

    putStrLn "Digite os autores do livro (separados por vírgula):"
    autoresStr <- getLine
    let autores = splitOn "," autoresStr
    putStrLn ""

    putStrLn "Digite a editora do livro:"
    editora <- getLine
    putStrLn ""

    putStrLn "Digite o ano de publicação do livro:"
    anoPublicacaoStr <- getLine
    putStrLn ""

    let anoPublicacao = read anoPublicacaoStr :: AnoPublicacao
    
    let novoLivro = criarLivro codigo titulo autores editora anoPublicacao
    putStrLn "Livro adicionado:"
    print novoLivro
    putStrLn ""

    putStrLn "Deseja adicionar outro livro? (s/n):"
    resposta <- getLine
    putStrLn ""

    if resposta == "s"
        then do
            let novaListaLivros = novoLivro : livros
            adicionarLivro novaListaLivros
        else do
            putStrLn "Livro(s) adicionado(s)!"
            return (novoLivro : livros)

removerLivro :: Codigo -> [LivroInfo] -> IO [LivroInfo]
removerLivro codigo livros = do
    let livrosFiltrados = filter (\livro -> getCodigo livro /= codigo) livros
    putStrLn ""
    putStrLn "Livro removido com sucesso!"
    return livrosFiltrados
  where
    getCodigo (Livro cod _ _ _ _) = cod

exibirLivros :: [LivroInfo] -> IO ()
exibirLivros [] = putStrLn "Nenhum livro adicionado."
exibirLivros livros = do
    mapM_ print livros

livroTemCodigo :: Codigo -> LivroInfo -> Bool
livroTemCodigo codigo (Livro cod _ _ _ _) = codigo == cod

loopPrincipal :: [LivroInfo] -> IO ()
loopPrincipal listaLivros = do
    putStrLn ""
    putStrLn "Para interagir com as funcionalidades relacionadas aos livros, escolha uma das opções a baixo."
    putStrLn "(1) - Para adicionar um ou mais livros."
    putStrLn "(2) - Para remover um ou mais livros."
    putStrLn "(3) - Para exibir todos os livros."
    putStrLn "Sair - Qualquer tecla que não seja uma opção."
    putStrLn ""

    putStrLn "Digite:"
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            novaListaLivros <- adicionarLivro listaLivros
            loopPrincipal novaListaLivros
        "2" -> do
            putStrLn "Digite o código do livro a ser removido:"
            codigoStr <- getLine
            let codigo = read codigoStr :: Codigo
            novaListaLivros <- removerLivro codigo listaLivros
            loopPrincipal novaListaLivros
        "3" -> do
            putStrLn "Exibindo todos os livros:"
            exibirLivros listaLivros
            loopPrincipal listaLivros
        _ -> putStrLn "Opção inválida. Retorno as opções sobre livros."

mainLivro :: IO ()
mainLivro = loopPrincipal []
