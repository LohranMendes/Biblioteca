module Livro (mainLivro, LivroInfo, livroTemCodigo, carregarLivros, exibirLivroInfo, getCodigo, getTitulo, getAutor, getEditora, getAno) where

import Data.List.Split (splitOn)
import System.IO
import Data.List (any, intercalate)
import Data.Char (isSpace)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

type Codigo = Int
type Titulo = String
type Autor = String
type Editora = String
type AnoPublicacao = Int

data LivroInfo = Livro Codigo Titulo Autor Editora AnoPublicacao
    deriving (Show)

criarLivro :: Codigo -> Titulo -> Autor -> Editora -> AnoPublicacao -> LivroInfo
criarLivro cod tit aut edt ano = Livro cod tit aut edt ano

-- Função para carregar a lista de livros de um arquivo
carregarLivros :: IO [LivroInfo]
carregarLivros = do
    let nomeArquivo = "livros.txt"
    arquivoExiste <- doesFileExist nomeArquivo
    if not arquivoExiste
        then do
            putStrLn "O arquivo de livros não existe. Criando um novo arquivo."
            writeFile nomeArquivo ""  -- Cria um arquivo vazio
            return []  -- Retorna uma lista vazia de livros
        else do
            conteudo <- readFile nomeArquivo
            if null conteudo
                then do
                    putStrLn "O arquivo de livros está vazio."
                    return []  -- Retorna uma lista vazia de livros
                else do
                    let linhas = lines conteudo
                    mapM (return . parseLivro) linhas
  where
    parseLivro :: String -> LivroInfo
    parseLivro linha =
        case splitOn ", " (trimQuotes linha) of
            [_, codigoStr, titulo, autor, editora, anoPubliStr] ->
                let codigo = read codigoStr :: Codigo
                    anoPubli = read (trimQuotes anoPubliStr) :: AnoPublicacao
                in Livro codigo (removerAspas titulo) (removerAspas autor) (removerAspas editora) anoPubli
            _ -> error "Formato inválido de entrada"     

removerAspas :: String -> String
removerAspas str = filter (\c -> c /= '\"' && c /= '\\') str

trimQuotes :: String -> String
trimQuotes = filter (not . isQuote)
  where isQuote c = c == '"' || c == '\\'

livroTemCodigo :: Codigo -> LivroInfo -> Bool
livroTemCodigo cod (Livro codigo _ _ _ _) = cod == codigo

adicionarLivro :: [LivroInfo] -> IO [LivroInfo]
adicionarLivro livros = do
    putStrLn "Digite o código do livro:"
    codigoStr <- getLine
    case readMaybe codigoStr :: Maybe Codigo of
        Just codigo -> do
            putStrLn ""

            -- Verificar se o código já existe na lista carregada do arquivo
            let codigoExistente = any (livroTemCodigo codigo) livros
            if codigoExistente
                then do
                    putStrLn "O identificador do livro já existe. Tente novamente."
                    adicionarLivro livros
                else do
                    putStrLn "Digite o título do livro:"
                    titulo <- getLine
                    putStrLn ""

                    putStrLn "Digite o autor do livro:"
                    autor <- getLine
                    putStrLn ""

                    putStrLn "Digite a editora do livro:"
                    editora <- getLine
                    putStrLn ""

                    putStrLn "Digite o ano de publicação do livro:"
                    anoPublicacaoStr <- getLine
                    case readMaybe anoPublicacaoStr :: Maybe AnoPublicacao of
                        Just anoPublicacao -> do
                            putStrLn ""

                            let novoLivro = criarLivro codigo titulo autor editora anoPublicacao
                            putStrLn "Livro adicionado:"
                            print novoLivro
                            putStrLn ""

                            putStrLn "Deseja adicionar outro livro? (s/n):"
                            resposta <- getLine
                            putStrLn ""

                            let novaListaLivros = novoLivro : livros
                            if resposta == "s"
                                then adicionarLivro novaListaLivros
                                else do
                                    putStrLn "Livro(s) adicionado(s)!"
                                    salvarLivros novaListaLivros
                                    return novaListaLivros
                        Nothing -> do
                            putStrLn "Ano de publicacao inválido. Por favor, tente novamente."
                            putStrLn ""
                            adicionarLivro livros          
        Nothing -> do
            putStrLn "Identificador de livro inválido. Por favor, tente novamente."
            putStrLn ""
            adicionarLivro livros                      

getCodigo :: LivroInfo -> Codigo
getCodigo (Livro codigo _ _ _ _) = codigo

getTitulo :: LivroInfo -> Titulo
getTitulo (Livro _ titulo _ _ _) = titulo

getAutor :: LivroInfo -> Autor
getAutor (Livro _ _ autor _ _) = autor

getEditora :: LivroInfo -> Editora
getEditora (Livro _ _ _ editora _) = editora

getAno :: LivroInfo -> AnoPublicacao
getAno (Livro _ _ _ _ ano) = ano

exibirLivroInfo :: LivroInfo -> IO ()
exibirLivroInfo livro = do
  putStrLn ""    
  putStrLn "Informações do livro:"
  putStrLn $ "Código: " ++ show (getCodigo livro)
  putStrLn $ "Título: " ++ getTitulo livro
  putStrLn $ "Autor: " ++ getAutor livro
  putStrLn $ "Editora: " ++ getEditora livro
  putStrLn $ "Ano de Publicação: " ++ show (getAno livro)

removerLivro :: Codigo -> [LivroInfo] -> IO [LivroInfo]
removerLivro codigo livros = do
    let livrosFiltrados = filter (not . livroTemCodigo codigo) livros
    if null livrosFiltrados
        then do
            putStrLn "Não foi encontrado nenhum livro com o código informado."
            return livros
        else do
            putStrLn "Livro(s) removido(s) com sucesso!"
            salvarLivros livrosFiltrados
            return livrosFiltrados

exibirLivros :: [LivroInfo] -> IO ()
exibirLivros [] = putStrLn "Nenhum livro adicionado."
exibirLivros livros = do
    putStrLn "Exibindo todos os livros:"
    let livrosInvertidos = reverse livros
    mapM_ (putStrLn . formatLivro) livrosInvertidos

formatLivro :: LivroInfo -> String
formatLivro (Livro cod tit aut edt anoPubli) =
    "ID Livro: " ++ show cod ++ ", Titulo: " ++ tit ++ ", Autor(es): " ++ aut ++ ", Editora: " ++ edt ++ ", Ano: " ++ show anoPubli

-- Função para salvar a lista de livros em um arquivo
salvarLivros :: [LivroInfo] -> IO ()
salvarLivros livros = do
    let nomeArquivo = "livros.txt"
    withFile nomeArquivo WriteMode $ \arquivo -> do
        mapM_ (hPrint arquivo . formattLivro) livros
    putStrLn $ "Livros salvos com sucesso no arquivo: " ++ nomeArquivo

formattLivro :: LivroInfo -> String
formattLivro (Livro cod tit aut edt anoPubli) =
    intercalate ", " ["Livro", show cod, tit, aut, edt, show anoPubli]    

loopPrincipal :: [LivroInfo] -> IO ()
loopPrincipal livros = do
    putStrLn ""
    putStrLn "Para interagir com as funcionalidades relacionadas aos livros, escolha uma das opções abaixo."
    putStrLn "(1) - Para adicionar um ou mais livros."
    putStrLn "(2) - Para remover um livros."
    putStrLn "(3) - Para exibir todos os livros."
    putStrLn "Sair - Qualquer tecla que não seja uma opção."
    putStrLn ""

    putStrLn "Digite:"
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            novaListaLivros <- adicionarLivro livros
            loopPrincipal novaListaLivros
        "2" -> do
            putStrLn "Digite o código do livro a ser removido:"
            codigoStr <- getLine
            case readMaybe codigoStr :: Maybe Codigo of
                Just codigo -> do
                    novaListaLivros <- removerLivro codigo livros
                    loopPrincipal novaListaLivros
                Nothing -> do
                    putStrLn ""
                    putStrLn "Identificador de livro inválido."
                    putStrLn ""
                    loopPrincipal livros    
        "3" -> do
            exibirLivros livros
            loopPrincipal livros
        _ -> putStrLn "Encerrando as funcionalidades de livros."

mainLivro :: IO ()
mainLivro = do
    putStrLn "Bem-vindo às funcionalidades sobre livro."
    livros <- carregarLivros
    loopPrincipal livros
