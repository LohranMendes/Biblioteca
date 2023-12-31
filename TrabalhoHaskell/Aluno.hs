module Aluno (mainAluno, AlunoInfo, alunoTemId, carregarAlunos, exibirAlunoInfo, getIdAluno, getNome, getCurso, getNumMat) where

import Data.List.Split (splitOn)
import System.IO
import Data.List (any, intercalate)
import Data.Char (isSpace)
import Text.Read (readMaybe)

type IdAluno = Int
type NomeAluno = String
type CursoAluno = String
type NumMat = Int

data AlunoInfo = Aluno IdAluno NomeAluno CursoAluno NumMat
    deriving (Show)

criarAluno :: IdAluno -> NomeAluno -> CursoAluno -> NumMat -> AlunoInfo
criarAluno id nome curso mat = Aluno id nome curso mat

-- Função para carregar a lista de alunos de um arquivo
carregarAlunos :: IO [AlunoInfo]
carregarAlunos = do
    let nomeArquivo = "alunos.txt"
    conteudo <- readFile nomeArquivo
    if null conteudo
        then do
            putStrLn "O arquivo de alunos está vazio."
            return []  -- Retorna uma lista vazia de alunos
        else do
            let linhas = lines conteudo
            return (map parseAluno linhas)
  where
    parseAluno :: String -> AlunoInfo
    parseAluno linha =
      case splitOn ", " (trimQuotes linha) of
        [_, idStr, nome, curso, numMatStr] ->
          let id = read idStr :: IdAluno
              numMat = read (trimQuotes numMatStr) :: NumMat
          in Aluno id (removerAspas nome) (removerAspas curso) numMat
        _ -> error "Formato inválido de entrada"

removerAspas :: String -> String
removerAspas str = filter (\c -> c /= '\"' && c /= '\\') str

numMatJaExiste :: NumMat -> [AlunoInfo] -> Bool
numMatJaExiste numMat alunos = any (\(Aluno _ _ _ numMat') -> numMat' == numMat) alunos

adicionarAluno :: [AlunoInfo] -> IO [AlunoInfo]
adicionarAluno alunos = do
    putStrLn "Digite o identificador do aluno:"
    idalunoStr <- getLine
    case readMaybe idalunoStr :: Maybe IdAluno of
        Just idaluno -> do
            putStrLn ""

            -- Verificar se o idAluno já existe na lista carregada do arquivo
            let alunoExistente = any (alunoTemId idaluno) alunos
            if alunoExistente
                then do
                    putStrLn ""
                    putStrLn "O identificador do aluno já existe. Tente novamente."
                    putStrLn ""
                    adicionarAluno alunos
                else do
                    putStrLn "Digite o nome do aluno:"
                    nome <- getLine
                    putStrLn ""

                    putStrLn "Digite o curso do aluno:"
                    curso <- getLine
                    putStrLn ""

                    putStrLn "Digite o número da matrícula do aluno:"
                    numMatStr <- getLine
                    case readMaybe numMatStr :: Maybe NumMat of
                        Just numMat -> do
                            putStrLn ""
                            let numMatJaExisteNaLista = numMatJaExiste numMat alunos
                            if numMatJaExisteNaLista
                                then do
                                    putStrLn ""
                                    putStrLn "O número de matrícula já existe. Tente novamente."
                                    putStrLn ""
                                    adicionarAluno alunos
                                else do
                                    let novoAluno = criarAluno idaluno nome curso numMat
                                    putStrLn "Aluno adicionado:"
                                    print novoAluno
                                    putStrLn ""

                                    putStrLn "Deseja adicionar outro aluno? (s/n):"
                                    resposta <- getLine
                                    putStrLn ""

                                    let novaListaAlunos = novoAluno : alunos
                                    if resposta == "s"
                                        then adicionarAluno novaListaAlunos
                                        else do
                                            putStrLn "Aluno(s) adicionado(s)!"
                                            salvarAlunos novaListaAlunos
                                            return novaListaAlunos
                        Nothing -> do
                            putStrLn "Numero de matricula inválido. Por favor, tente novamente."
                            putStrLn ""
                            adicionarAluno alunos           
        Nothing -> do
            putStrLn "Identificador de aluno inválido. Por favor, tente novamente."
            putStrLn ""
            adicionarAluno alunos   

removerAluno :: IdAluno -> [AlunoInfo] -> IO [AlunoInfo]
removerAluno codigo alunos = do
    let alunosFiltrados = filter (\aluno -> getIdAluno aluno /= codigo) alunos
    putStrLn ""
    putStrLn "Aluno removido com sucesso!"
    salvarAlunos alunosFiltrados
    return alunosFiltrados
  where
    getIdAluno (Aluno id _ _ _) = id

exibirAlunos :: [AlunoInfo] -> IO ()
exibirAlunos [] = putStrLn "Nenhum aluno adicionado."
exibirAlunos alunos = do
  putStrLn "Exibindo todos os alunos:"
  let alunosInvertidos = reverse alunos
  mapM_ (putStrLn . formatAluno) alunosInvertidos

formatAluno :: AlunoInfo -> String
formatAluno (Aluno id nome curso numMat) =
  "ID Aluno: " ++ show id ++ ", Nome: " ++ nome ++ ", Curso: " ++ curso ++ ", Numero da matricula: " ++ show numMat

trimQuotes :: String -> String
trimQuotes = filter (not . isQuote)
  where isQuote c = c == '"' || c == '\\'

removeAspas :: String -> String
removeAspas str = filter (/= '"') str

alunoTemId :: IdAluno -> AlunoInfo -> Bool
alunoTemId idAluno (Aluno id _ _ _) = idAluno == id

getIdAluno :: AlunoInfo -> IdAluno
getIdAluno (Aluno idaluno _ _ _) = idaluno

getNome :: AlunoInfo -> NomeAluno
getNome (Aluno _ nome _ _) = nome

getCurso :: AlunoInfo -> CursoAluno
getCurso (Aluno _ _ curso _) = curso

getNumMat :: AlunoInfo -> NumMat
getNumMat (Aluno _ _ _ numMat) = numMat

exibirAlunoInfo :: AlunoInfo -> IO ()
exibirAlunoInfo aluno = do
  putStrLn ""  
  putStrLn "Informações do aluno:"
  putStrLn $ "ID Aluno: " ++ show (getIdAluno aluno)
  putStrLn $ "Nome: " ++ getNome aluno
  putStrLn $ "Curso: " ++ getCurso aluno
  putStrLn $ "Matricula: " ++ show (getNumMat aluno)

-- Função para salvar a lista de alunos em um arquivo
salvarAlunos :: [AlunoInfo] -> IO ()
salvarAlunos alunos = do
    let nomeArquivo = "alunos.txt"
    withFile nomeArquivo WriteMode $ \arquivo -> do
        mapM_ (hPrint arquivo . formattAluno) alunos
    putStrLn $ "Alunos salvos com sucesso no arquivo: " ++ nomeArquivo

formattAluno :: AlunoInfo -> String
formattAluno (Aluno id nome curso mat) =
    intercalate ", " ["Aluno", show id, nome, curso, show mat]    

loopPrincipal :: [AlunoInfo] -> IO ()
loopPrincipal alunos = do
    putStrLn ""
    putStrLn "Para interagir com as funcionalidades relacionadas aos alunos, escolha uma das opções abaixo:"
    putStrLn "(1) - Para adicionar um ou mais alunos."
    putStrLn "(2) - Para remover um alunos."
    putStrLn "(3) - Para exibir todos os alunos."
    putStrLn "(4) - Para salvar a lista de alunos em um arquivo."
    putStrLn "Sair - Qualquer tecla que não seja uma opção."
    putStrLn ""
    
    putStrLn "Digite:"
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            novaListaAlunos <- adicionarAluno alunos
            loopPrincipal novaListaAlunos
        "2" -> do
            putStrLn "Digite o identificador do aluno a ser removido:"
            idalunoStr <- getLine
            case readMaybe idalunoStr :: Maybe IdAluno of
                Just idaluno -> do
                    novaListaAlunos <- removerAluno idaluno alunos
                    loopPrincipal novaListaAlunos
                Nothing -> do
                    putStrLn "Identificador de aluno inválido."
                    loopPrincipal alunos     
        "3" -> do
            putStrLn "Exibindo todos os alunos:"
            exibirAlunos alunos
            loopPrincipal alunos
        "4" -> do
            salvarAlunos alunos
            loopPrincipal alunos
        _ -> putStrLn "Encerrando às funcionalidades de alunos."
    
listaAlunos :: IO [AlunoInfo]
listaAlunos = carregarAlunos

mainAluno :: IO ()
mainAluno = do
    putStrLn "Bem-vindo às funcionalidades sobre alunos."
    alunos <- carregarAlunos
    loopPrincipal alunos
