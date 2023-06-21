module Multas (mainMulta, MultaInfo, multaTemId, carregarMultas, alunosPendentes) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime, formatTime)
import Aluno (AlunoInfo, alunoTemId, carregarAlunos, exibirAlunoInfo, getIdAluno, getNome, getCurso, getNumMat)
import Data.List.Split (splitOn)
import System.IO
import Data.List (any)
import Data.Char (isSpace)

type IdMulta = Int
type IdAluno = Int
type Valor = Double

data MultaInfo = Multa IdMulta IdAluno Valor Day
    deriving (Show)

criarMulta :: IdMulta -> IdAluno -> Valor -> Day -> MultaInfo
criarMulta id idAluno valor dtPag = Multa id idAluno valor dtPag

nullDay :: Day
nullDay = fromGregorian 0 0 0

-- Função para carregar a lista de multas de um arquivo
carregarMultas :: IO [MultaInfo]
carregarMultas = do
  let nomeArquivo = "multas.txt"
  conteudo <- readFile nomeArquivo
  if null conteudo
      then do
          putStrLn "O arquivo de multas está vazio."
          return []  -- Retorna uma lista vazia de multas
      else do
          let linhas = lines conteudo
          return (map parseMulta linhas)

carregarMultasPagas :: IO [MultaInfo]
carregarMultasPagas = do
  let nomeArquivo = "multaspagas.txt"
  conteudo <- readFile nomeArquivo
  if null conteudo
      then do
          putStrLn "O arquivo de multas pagas está vazio."
          return []  -- Retorna uma lista vazia de multas
      else do
          let linhas = lines conteudo
          return (map parseMulta linhas)

parseMulta :: String -> MultaInfo
parseMulta linha =
    case splitOn " " linha of
    [_, idMultaStr, idAlunoStr, valorStr, dtPagStr] ->
        let idMulta = read idMultaStr :: IdMulta
            idAluno = read idAlunoStr :: IdAluno
            valor = read valorStr :: Valor
            dtPag = case parseDataVerificar dtPagStr of
                    Just d -> d
                    Nothing -> nullDay
        in Multa idMulta idAluno valor dtPag
    _ -> error "Formato inválido de entrada"

parseDataVerificar :: String -> Maybe Day
parseDataVerificar = parseTimeM True defaultTimeLocale "%Y-%m-%d" :: String -> Maybe Day

-- Função para fazer o parsing da data no formato "dd-mm-aaaa"
parseData :: String -> Maybe Day
parseData = parseTimeM True defaultTimeLocale "%d-%m-%Y" :: String -> Maybe Day

-- Função para verificar se um aluno existe com base no identificador
alunoJaExiste :: IdAluno -> [AlunoInfo] -> IO Bool
alunoJaExiste idAluno alunos = do
  let alunoExistente = any (alunoTemId idAluno) alunos
  if alunoExistente
    then return True
    else do
      putStrLn "O identificador do aluno não existe. Tente novamente."
      return False


adicionarMulta :: [MultaInfo] -> IO [MultaInfo]
adicionarMulta multas = do
    alunos <- carregarAlunos -- Carrega os alunos do arquivo alunos.txt
    putStrLn "Digite o identificador da multa:"
    idmultaStr <- getLine
    let idmulta = read idmultaStr :: IdMulta
    putStrLn ""

    -- Verificar se o idMulta já existe na lista carregada do arquivo
    let multaExistente = any (multaTemId idmulta) multas
    if multaExistente
        then do
            putStrLn "O identificador da multa já existe. Tente novamente."
            adicionarMulta multas
        else do
            multasPagas <- carregarMultasPagas
            let multaExistentePagas = any (multaTemId idmulta) multasPagas
            if multaExistentePagas
                then do
                    putStrLn "O identificador da multa já existe nas multas pagas. Tente novamente."
                    adicionarMulta multas
                else do
                    putStrLn "Digite o identificador do aluno:"
                    idalunoStr <- getLine
                    let idaluno = read idalunoStr :: IdAluno
                    putStrLn ""

                    alunoExiste <- alunoJaExiste idaluno alunos
                    if alunoExiste
                        then do
                            putStrLn "Digite o valor da multa:"
                            valorStr <- getLine
                            let valor = read valorStr :: Valor
                            putStrLn ""

                            putStrLn "Digite a data limite do pagamento da multa (dd-mm-aaaa):"
                            input <- getLine
                            case parseData input of
                                Just datapagamento -> do
                                    let novaMulta = criarMulta idmulta idaluno valor datapagamento
                                    putStrLn "Multa adicionada:"
                                    print novaMulta
                                    putStrLn ""

                                    putStrLn "Deseja adicionar outra multa? (s/n):"
                                    resposta <- getLine
                                    putStrLn ""

                                    let novaListaMulta = novaMulta : multas
                                    if resposta == "s"
                                        then do
                                            adicionarMulta novaListaMulta
                                        else do
                                            putStrLn "Multa(s) adicionada(s)!"
                                            salvarMultas novaListaMulta
                                            return (novaMulta : multas)
                                Nothing -> do
                                    putStrLn "Formato de data inválido. Tente novamente."
                                    adicionarMulta multas
                        else do
                            adicionarMulta multas

removerMulta :: IdMulta -> [MultaInfo] -> IO [MultaInfo]
removerMulta idmulta multas = do
    let multasFiltradas = filter (\multa -> getIdMulta multa /= idmulta) multas
    putStrLn ""
    putStrLn "Multa removida com sucesso!"
    salvarMultas multasFiltradas  -- Salva a lista completa de multas
    return multasFiltradas
  where
    getIdMulta (Multa id _ _ _) = id

exibirMultas :: [MultaInfo] -> IO ()
exibirMultas [] = putStrLn "Nenhuma multa adicionada."
exibirMultas multas = do
  let multasInvertidas = reverse multas
  mapM_ (putStrLn . formatMulta) multasInvertidas

exibirMultasPagas :: [MultaInfo] -> IO ()
exibirMultasPagas [] = putStrLn "Nenhuma multa paga adicionada."
exibirMultasPagas multaspg = do
  let multaspgInvertidas = reverse multaspg
  mapM_ (putStrLn . formatMulta) multaspgInvertidas

formatMulta :: MultaInfo -> String
formatMulta (Multa id idAluno valor dtPag) =
  "ID Multa: " ++ show id ++ ", ID Aluno: " ++ show idAluno ++ ", Valor: " ++ show valor ++ ", Data de Pagamento: " ++ formatDate dtPag

formatDate :: Day -> String
formatDate dt = formatTime defaultTimeLocale "%d-%m-%Y" dt

-- Função auxiliar para remover as aspas de uma string
removeAspas :: String -> String
removeAspas str = filter (/= '"') str

multaTemId :: IdMulta -> MultaInfo -> Bool
multaTemId idMulta (Multa id _ _ _) = idMulta == id

-- Função para salvar a lista de multas em um arquivo
salvarMultas :: [MultaInfo] -> IO ()
salvarMultas multas = do
    let nomeArquivo = "multas.txt"
    withFile nomeArquivo WriteMode $ \arquivo -> do
        mapM_ (hPrint arquivo) multas
    putStrLn $ "Multas salvas com sucesso no arquivo: " ++ nomeArquivo 

salvarMultasPagas :: [MultaInfo] -> IO ()
salvarMultasPagas pagas = do
    let nomeArquivo = "multaspagas.txt"
    withFile nomeArquivo WriteMode $ \arquivo -> do
        mapM_ (hPrint arquivo) pagas
    putStrLn $ "Multas pagas salvas com sucesso no arquivo: " ++ nomeArquivo    

removerMultaPaga :: IdMulta -> [MultaInfo] -> [MultaInfo] -> IO [MultaInfo]
removerMultaPaga idmulta multas multaspg = do
    let multaEncontrada = acheMultaId idmulta multas
    case multaEncontrada of
        Just multa -> do
            let multaspgAtualizadas = multa : multaspg
            salvarMultasPagas multaspgAtualizadas
            let multasAtualizadas = filter (\m -> getIdMulta m /= idmulta) multas
            salvarMultas multasAtualizadas
            putStrLn ""
            putStrLn "Multa fechada com sucesso!"
            return multaspgAtualizadas
        Nothing -> do
            putStrLn "Multa não encontrada."
            return multas
  where
    acheMultaId :: IdMulta -> [MultaInfo] -> Maybe MultaInfo
    acheMultaId _ [] = Nothing
    acheMultaId idmulta (m:ms)
        | getIdMulta m == idmulta = Just m
        | otherwise = acheMultaId idmulta ms

    getIdMulta (Multa id _ _ _) = id

getIdAlunoMulta :: MultaInfo -> IdAluno
getIdAlunoMulta (Multa _ idAluno _ _) = idAluno

alunosPendentes :: [MultaInfo] -> [AlunoInfo] -> IO ()
alunosPendentes multas alunos = do
    let idsAlunos = map getIdAlunoMulta multas
        alunosPendentes = filter (\aluno -> getIdAluno aluno `elem` idsAlunos) alunos
        alunosReverso = reverse alunosPendentes
    if null alunosPendentes
      then putStrLn "Nao tem alunos com multas pendentes."
      else mapM_ exibirAlunoInfo alunosReverso

loopPrincipal :: [MultaInfo] -> [MultaInfo] -> IO ()
loopPrincipal multas multaspg = do
    putStrLn ""
    putStrLn "Para interagir com as funcionalidades relacionadas às multas, escolha uma das opções abaixo:"
    putStrLn "(1) - Para adicionar uma ou mais multas."
    putStrLn "(2) - Para remover uma multa."
    putStrLn "(3) - Para exibir todas as multas abertas."
    putStrLn "(4) - Para fechar uma multa."
    putStrLn "(5) - Para exibir todas as multas fechadas."
    putStrLn "(6) - Para exibir todos os alunos com multas pendentes."
    putStrLn "Sair - Qualquer tecla que não seja uma opção."
    putStrLn ""

    putStrLn "Digite:"
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            novasMultas <- adicionarMulta multas
            loopPrincipal novasMultas multaspg
        "2" -> do
            putStrLn "Digite o identificador da multa a ser removida:"
            idmultaStr <- getLine
            let idmulta = read idmultaStr :: IdMulta
            novaListaMultas <- removerMulta idmulta multas
            loopPrincipal novaListaMultas multaspg
        "3" -> do
            putStrLn "Exibindo todas as multas abertas:"
            exibirMultas multas
            loopPrincipal multas multaspg
        "4" -> do
            putStrLn "Digite o identificador da multa a ser fechada:"
            idmultaStr <- getLine
            let idmulta = read idmultaStr :: IdMulta
            novaListaMultas <- removerMultaPaga idmulta multas multaspg
            nLM <- removerMulta idmulta multas
            loopPrincipal nLM novaListaMultas
        "5" -> do
            putStrLn "Exibindo todas as multas fechadas:"
            exibirMultasPagas multaspg
            loopPrincipal multas multaspg
        "6" -> do
            alunos <- carregarAlunos
            alunosPendentes multas alunos
            loopPrincipal multas multaspg    
        _ -> putStrLn "Encerrando as funcionalidades de multas."                           
            

listaMultas :: IO [MultaInfo]
listaMultas = carregarMultas

listaMultasPagas :: IO [MultaInfo]
listaMultasPagas = carregarMultas

mainMulta :: IO ()
mainMulta = do
    putStrLn "Bem-vindo às funcionalidades sobre multas."
    multas <- carregarMultas
    multaspg <- carregarMultasPagas
    loopPrincipal multas multaspg