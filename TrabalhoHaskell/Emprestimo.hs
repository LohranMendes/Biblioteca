module Emprestimo (mainEmprestimo, livrosDisponiveis, carregarEmprestimos, exibirEmprestimos, exibirAlunoEspecifico) where

import Data.List.Split
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime, formatTime)
import Livro (LivroInfo, livroTemCodigo, carregarLivros, exibirLivroInfo, getCodigo, getTitulo, getAutor, getEditora, getAno)
import Aluno (AlunoInfo, alunoTemId, carregarAlunos)
import System.IO
import Data.List (any)
import Data.Char (isSpace)
import Text.Read (readMaybe)

type IdEmprestimo = Int
type Codigo = Int
type IdAluno = Int

data EmprestimoInfo = Emprestimo IdEmprestimo Codigo IdAluno Day Day
    deriving (Show)

criarEmprestimo :: IdEmprestimo -> Codigo -> IdAluno -> Day -> Day -> EmprestimoInfo
criarEmprestimo id cod idAluno dtEmp dtDev = Emprestimo id cod idAluno dtEmp dtDev

nullDay :: Day
nullDay = fromGregorian 0 0 0

-- Função para carregar a lista de emprestimos de um arquivo
carregarEmprestimos :: IO [EmprestimoInfo]
carregarEmprestimos = do
  let nomeArquivo = "emprestimos.txt"
  conteudo <- readFile nomeArquivo
  if null conteudo
      then do
          putStrLn "O arquivo de emprestimos está vazio."
          return []  -- Retorna uma lista vazia de emprestimos
      else do
          let linhas = lines conteudo
          return (map parseEmprestimo linhas)

-- Função para carregar a lista de emprestimos devolvidos de um arquivo.
carregarEmprestimosDev :: IO [EmprestimoInfo]
carregarEmprestimosDev = do
  let nomeArquivo = "emprestimosdevolvidos.txt"
  conteudo <- readFile nomeArquivo
  if null conteudo
      then do
          putStrLn "O arquivo de emprestimos devolvidos está vazio."
          return []  -- Retorna uma lista vazia de emprestimos
      else do
          let linhas = lines conteudo
          return (map parseEmprestimo linhas)

parseEmprestimo :: String -> EmprestimoInfo
parseEmprestimo linha =
    case splitOn " " linha of
    [_, idEmprestimoStr, idLivroStr, idAlunoStr, dtEmpStr, dtDevStr] ->
        let idEmprestimo = read idEmprestimoStr :: IdEmprestimo
            idLivro = read idLivroStr :: Codigo
            idAluno = read idAlunoStr :: IdAluno
            dtEmp = case parseDataVerificar dtEmpStr of
                    Just d -> d
                    Nothing -> nullDay
            dtDev = case parseDataVerificar dtDevStr of
                    Just d -> d
                    Nothing -> nullDay         
        in Emprestimo idEmprestimo idLivro idAluno dtEmp dtDev
    _ -> error "Formato inválido de entrada"

parseDataVerificar :: String -> Maybe Day
parseDataVerificar = parseTimeM True defaultTimeLocale "%Y-%m-%d" :: String -> Maybe Day

-- Função para fazer o parsing da data no formato "dd-mm-aaaa"
parseData :: String -> Maybe Day
parseData = parseTimeM True defaultTimeLocale "%d-%m-%Y" :: String -> Maybe Day

-- Função para verificar se um aluno existe no alunos.txt com base no identificador
alunoExiste :: IdAluno -> [AlunoInfo] -> IO Bool
alunoExiste idAluno alunos = do
  let alunoExistente = any (alunoTemId idAluno) alunos
  if alunoExistente
    then return True
    else do
      putStrLn "O identificador do aluno não existe. Tente novamente."
      return False

-- Função para verificar se um livro existe no livros.txt com base no identificador
livroExiste :: Codigo -> [LivroInfo] -> IO Bool
livroExiste codigo livros = do
  let livroExistente = any (livroTemCodigo codigo) livros
  if livroExistente
    then return True
    else do
      putStrLn "O identificador do livro não existe. Tente novamente."
      return False

-- Função para verificar se um livro existe no emprestimos.txt com base no identificador
livroEsta :: Codigo -> EmprestimoInfo -> Bool
livroEsta codigo (Emprestimo _ cod _ _ _) = codigo == cod

emprestimoTemId :: IdEmprestimo -> EmprestimoInfo -> Bool
emprestimoTemId idEmprestimo (Emprestimo id _ _ _ _) = idEmprestimo == id

adicionarEmprestimo :: [EmprestimoInfo] -> IO [EmprestimoInfo] 
adicionarEmprestimo emprestimos = do
    alunos <- carregarAlunos -- Carrega os alunos do arquivo alunos.txt
    livros <- carregarLivros -- Carrega os livros do arquivo livros.txt
    putStrLn "Digite o identificador do emprestimo:"
    idemprestimoStr <- getLine
    case readMaybe idemprestimoStr :: Maybe IdEmprestimo of
      Just idemprestimo -> do
        putStrLn ""

        let emprestimoExistente = any (emprestimoTemId idemprestimo) emprestimos
        if emprestimoExistente
            then do
                putStrLn "O identificador do emprestimo já existe. Tente novamente."
                adicionarEmprestimo emprestimos
            else do
              empDev <- carregarEmprestimosDev
              let emprestimoDevExistente = any (emprestimoTemId idemprestimo) empDev
              if emprestimoDevExistente
                then do
                  putStrLn "O identificador do emprestimo já existe nos devolvidos. Tente novamente."
                  adicionarEmprestimo emprestimos
                else do
                  putStrLn "Digite o identificador do livro:"
                  codigoStr <- getLine
                  case readMaybe codigoStr :: Maybe Codigo of
                    Just codigo -> do
                      putStrLn ""

                      livroEx <- livroExiste codigo livros
                      if livroEx
                        then do
                          let livroSalvo = any (livroEsta codigo) emprestimos
                          if livroSalvo
                            then do
                              putStrLn "O identificador do livro já existe no arquivo. Tente novamente."
                              adicionarEmprestimo emprestimos
                            else do
                              putStrLn "Digite o identificador do aluno:"
                              idalunoStr <- getLine
                              case readMaybe idalunoStr :: Maybe IdAluno of
                                Just idaluno -> do
                                  putStrLn ""

                                  alunoEx <- alunoExiste idaluno alunos
                                  if alunoEx
                                    then do
                                      putStrLn "Digite a data do emprestimo do livro (dd-mm-aaaa):"
                                      input <- getLine
                                      case parseData input of
                                        Just dataemprestimo -> do
                                          putStrLn ""
                                          putStrLn "Digite a data da devolução do livro (dd-mm-YYYY):"
                                          dtDev <- getLine
                                          case parseData dtDev of
                                            Just datadevolucao -> do
                                              putStrLn ""
                                              let novaEmprestimo = criarEmprestimo idemprestimo codigo idaluno dataemprestimo datadevolucao
                                              putStrLn "Emprestimo adicionado:"
                                              print novaEmprestimo
                                              putStrLn ""

                                              putStrLn "Deseja adicionar outro emprestimo? (s/n):"
                                              resposta <- getLine
                                              putStrLn ""

                                              let novaListaEmprestimo = novaEmprestimo : emprestimos
                                              if resposta == "s"
                                                then do
                                                  adicionarEmprestimo novaListaEmprestimo
                                                else do
                                                  putStrLn "Emprestimo(s) adicionado(s)!"
                                                  salvarEmprestimo novaListaEmprestimo
                                                  return (novaEmprestimo : emprestimos)
                                            Nothing -> do
                                                  putStrLn "Formato de data inválido. Tente novamente."
                                                  putStrLn ""
                                                  adicionarEmprestimo emprestimos
                                        Nothing -> do
                                              putStrLn "Formato de data inválido. Tente novamente."
                                              putStrLn ""
                                              adicionarEmprestimo emprestimos
                                    else do
                                      adicionarEmprestimo emprestimos
                                Nothing -> do
                                  putStrLn "Identificador de aluno inválido. Tente novamente."
                                  putStrLn ""
                                  adicionarEmprestimo emprestimos        
                        else do
                          adicionarEmprestimo emprestimos
                    Nothing -> do
                      putStrLn "Identificador de livro inválido. Tente novamente."
                      putStrLn ""
                      adicionarEmprestimo emprestimos    
      Nothing -> do
        putStrLn "Identificador de emprestimo inválido. Tente novamente."
        putStrLn ""
        adicionarEmprestimo emprestimos

removerEmprestimo :: IdEmprestimo -> [EmprestimoInfo] -> IO [EmprestimoInfo]
removerEmprestimo idemprestimo emprestimos = do
    let emprestimosFiltrados = filter (\emprestimo -> getIdEmprestimo emprestimo /= idemprestimo) emprestimos
    putStrLn ""
    putStrLn "Emprestimo removido com sucesso!"
    salvarEmprestimo emprestimosFiltrados
    return emprestimosFiltrados
  where
    getIdEmprestimo (Emprestimo id _ _ _ _) = id

exibirEmprestimos :: [EmprestimoInfo] -> IO ()
exibirEmprestimos [] = putStrLn "Nenhum emprestimo adicionado."
exibirEmprestimos emprestimos = do
    let emprestimosInvertidos = reverse emprestimos
    mapM_ (putStrLn . formatEmprestimo) emprestimosInvertidos

exibirEmprestimosDev :: [EmprestimoInfo] -> IO ()
exibirEmprestimosDev [] = putStrLn "Nenhum emprestimo devolvido adicionado."
exibirEmprestimosDev empdev = do
  let empdevInvertidas = reverse empdev
  mapM_ (putStrLn . formatEmprestimo) empdevInvertidas

formatEmprestimo :: EmprestimoInfo -> String
formatEmprestimo (Emprestimo id cod idAluno dtEmp dtDev) =
  "ID Emprestimo: " ++ show id ++ ", ID Livro: " ++ show cod ++ ", ID Aluno: " ++ show idAluno ++ ", Data do Emprestimo: " ++ formatDate dtEmp ++ ", Data da Devolucao: " ++ formatDate dtDev

formatDate :: Day -> String
formatDate dt = formatTime defaultTimeLocale "%d-%m-%Y" dt

exibirAlunoEspecifico :: IdAluno -> IO ()
exibirAlunoEspecifico idaluno = do
    emprestimos <- carregarEmprestimos
    emprestimosDev <- carregarEmprestimosDev
    let emprestimosAluno = filter (\emprestimo -> getIdAlunoEmprestimo emprestimo == idaluno) emprestimos
        emprestimosDevAluno = filter (\emprestimo -> getIdAlunoEmprestimo emprestimo == idaluno) emprestimosDev

    let empAlunosInvertidos = reverse emprestimosAluno
    putStrLn "Emprestimos ativos do aluno:"
    putStrLn ""
    if null empAlunosInvertidos
      then putStrLn " -> Não tem emprestimos ativos no momento."
      else mapM_ (putStrLn . formatEmprestimo) empAlunosInvertidos

    putStrLn ""

    let empDevAlunosInvertidos = reverse emprestimosDevAluno
    putStrLn "Emprestimos devolvidos do aluno:"
    putStrLn ""
    if null empDevAlunosInvertidos
      then putStrLn "Não tem devolucoes feitas."
      else mapM_ (putStrLn . formatEmprestimo) empDevAlunosInvertidos
  where
    getIdAlunoEmprestimo (Emprestimo _ _ idAluno _ _) = idAluno

-- Função auxiliar para remover as aspas de uma string
removeAspas :: String -> String
removeAspas str = filter (/= '"') str  

-- Função para salvar a lista de emprestimos em um arquivo
salvarEmprestimo :: [EmprestimoInfo] -> IO ()
salvarEmprestimo emprestimos = do
    let nomeArquivo = "emprestimos.txt"
    withFile nomeArquivo WriteMode $ \arquivo -> do
        mapM_ (hPrint arquivo) emprestimos
    putStrLn $ "Emprestimo(s) salvo(s) com sucesso no arquivo: " ++ nomeArquivo 

-- Função pata salvar a lista de emprestimos devolvidos em uma arquivo
salvarEmprestimoDev :: [EmprestimoInfo] -> IO ()
salvarEmprestimoDev devolvidos = do
    let nomeArquivo = "emprestimosdevolvidos.txt"
    withFile nomeArquivo WriteMode $ \arquivo -> do
        mapM_ (hPrint arquivo) devolvidos
    putStrLn $ "Emprestimo(s) devolvido(s) salvo(s) com sucesso no arquivo: " ++ nomeArquivo    

removerEmprestimoDevolvido :: IdEmprestimo -> [EmprestimoInfo] -> [EmprestimoInfo] -> IO [EmprestimoInfo]
removerEmprestimoDevolvido idemprestimo emprestimos empdev = do
    let emprestimoEncontrada = acheEmprestimoId idemprestimo emprestimos
    case emprestimoEncontrada of
        Just emprestimo -> do
            let empdevAtualizados = emprestimo : empdev
            salvarEmprestimoDev empdevAtualizados
            putStrLn ""
            putStrLn "Emprestimo fechado com sucesso!"
            let emprestimosAtualizados = filter (\m -> getIdEmprestimo m /= idemprestimo) emprestimos
            salvarEmprestimo emprestimosAtualizados
            return empdevAtualizados
        Nothing -> do
            putStrLn "Emprestimo não encontrado."
            return emprestimos
  where
    acheEmprestimoId :: IdEmprestimo -> [EmprestimoInfo] -> Maybe EmprestimoInfo
    acheEmprestimoId _ [] = Nothing
    acheEmprestimoId idemprestimo (m:ms)
        | getIdEmprestimo m == idemprestimo = Just m
        | otherwise = acheEmprestimoId idemprestimo ms

    getIdEmprestimo (Emprestimo id _ _ _ _) = id

livrosDisponiveis :: [EmprestimoInfo] -> [LivroInfo] -> IO ()
livrosDisponiveis emprestimos livros = do
    let codigosEmprestimos = map getLivroCodigo emprestimos
        livrosDisponiveis = filter (\livro -> getCodigo livro `notElem` codigosEmprestimos) livros
        livrosReversos = reverse livrosDisponiveis
    if null livrosDisponiveis
      then putStrLn "Nao tem livros disponiveis."
      else mapM_ exibirLivroInfo livrosReversos
  where
    getLivroCodigo (Emprestimo _ idLivro _ _ _) = idLivro

loopPrincipal :: [EmprestimoInfo] -> [EmprestimoInfo] -> IO ()
loopPrincipal emprestimos empdevs = do
    putStrLn ""
    putStrLn "Para interagir com as funcionalidades relacionadas aos emprestimos, escolha uma das opções abaixo:"
    putStrLn "(1) - Para adicionar um ou mais emprestimos."
    putStrLn "(2) - Para remover um ou mais emprestimos."
    putStrLn "(3) - Para exibir todos os emprestimos abertos."
    putStrLn "(4) - Para fechar um emprestimo."
    putStrLn "(5) - Para exibir todas os emprestimos fechados."
    putStrLn "(6) - Para exibir os emprestimos por id de aluno."
    putStrLn "(7) - Para exibir os livros disponiveis."
    putStrLn "Sair - Qualquer tecla que não seja uma opção."
    putStrLn ""

    putStrLn "Digite:"
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            novaListaEmprestimos <- adicionarEmprestimo emprestimos 
            loopPrincipal novaListaEmprestimos empdevs
        "2" -> do
            putStrLn "Digite o identificador do emprestimo a ser removido:"
            idemprestimoStr <- getLine
            case readMaybe idemprestimoStr :: Maybe IdEmprestimo of
              Just idemprestimo -> do
                novaListaEmprestimos <- removerEmprestimo idemprestimo emprestimos
                loopPrincipal novaListaEmprestimos empdevs
              Nothing -> do
                putStrLn ""
                putStrLn "Identificador de emprestimo inválido."
                loopPrincipal emprestimos empdevs  
        "3" -> do
            putStrLn "Exibindo todos os emprestimos abertos:"
            exibirEmprestimos emprestimos
            loopPrincipal emprestimos empdevs
        "4" -> do
            putStrLn "Digite o identificador do emprestimo a ser fechado:"
            idemprestimoStr <- getLine
            case readMaybe idemprestimoStr :: Maybe IdEmprestimo of
              Just idemprestimo -> do
                novaListaEmprestimo <- removerEmprestimoDevolvido idemprestimo emprestimos empdevs
                nLE <- removerEmprestimo idemprestimo emprestimos
                loopPrincipal nLE novaListaEmprestimo
              Nothing -> do
                putStrLn ""
                putStrLn "Identificador de emprestimo inválido."
                loopPrincipal emprestimos empdevs    
        "5" -> do
            putStrLn "Exibindo todos os emprestimos devolvidos:"
            exibirEmprestimosDev empdevs
            loopPrincipal emprestimos empdevs
        "6" -> do
            putStrLn "Digite o id do aluno:"
            idalunoStr <- getLine
            putStrLn ""
            case readMaybe idalunoStr :: Maybe IdAluno of
              Just idaluno -> do
                exibirAlunoEspecifico idaluno
                loopPrincipal emprestimos empdevs  
              Nothing -> do
                putStrLn ""
                putStrLn "Identificador de aluno inválido."
                loopPrincipal emprestimos empdevs         
        "7" -> do 
            livros <- carregarLivros
            livrosDisponiveis emprestimos livros
            loopPrincipal emprestimos empdevs
        _ -> putStrLn "Encerrando às funcionalidades de emprestimos."

mainEmprestimo :: IO ()
mainEmprestimo = do
    putStrLn "Bem-vindo às funcionalidades sobre emprestimos."
    emprestimos <- carregarEmprestimos
    empdev <- carregarEmprestimosDev
    loopPrincipal emprestimos empdev
