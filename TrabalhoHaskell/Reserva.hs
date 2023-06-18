module Reserva (mainReserva) where

import Data.List.Split (splitOn)
import System.IO
import Data.List (any)
import Data.Char (isSpace)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)

import Aluno (AlunoInfo, alunoTemId, carregarAlunos)
import Livro (LivroInfo, livroTemCodigo, carregarLivros)

type IdReserva = Int
type IdAluno = Int
type Codigo = Int

data ReservaInfo = Reserva IdReserva Codigo IdAluno Day
    deriving (Show)

criarReserva :: IdReserva -> Codigo -> IdAluno -> Day -> ReservaInfo
criarReserva idReserva cod idAluno dtRes = Reserva idReserva cod idAluno dtRes

nullDay :: Day
nullDay = fromGregorian 0 0 0

-- Função para carregar a lista de reservas de um arquivo
carregarReservas :: IO [ReservaInfo]
carregarReservas = do
  let nomeArquivo = "reservas.txt"
  conteudo <- readFile nomeArquivo
  if null conteudo
      then do
          putStrLn "O arquivo de reservas está vazio."
          return []  -- Retorna uma lista vazia de reservas
      else do
          let linhas = lines conteudo
          return (map parseReserva linhas)

parseReserva :: String -> ReservaInfo
parseReserva linha =
    case splitOn " " linha of
    [_, idReservaStr, codigoStr, idAlunoStr, dtResStr] ->
        let idReserva = read idReservaStr :: IdReserva
            codigo = read codigoStr :: Codigo
            idAluno = read idAlunoStr :: IdAluno
            dtRes = case parseData dtResStr of
                    Just d -> d
                    Nothing -> nullDay
        in Reserva idReserva codigo idAluno dtRes
    _ -> error "Formato inválido de entrada"

-- Função para fazer o parsing da data no formato "dd/mm/aaaa"
parseData :: String -> Maybe Day
parseData = parseTimeM True defaultTimeLocale "%d/%m/%Y"

-- Função para verificar se um aluno existe com base no identificador
alunoJaExiste :: IdAluno -> [AlunoInfo] -> IO Bool
alunoJaExiste idAluno alunos = do
  let alunoExistente = any (alunoTemId idAluno) alunos
  if alunoExistente
    then return True
    else do
      putStrLn "O identificador do aluno não existe. Tente novamente."
      return False

-- Função para verificar se um livro existe com base no identificador
livroJaExiste :: Codigo -> [LivroInfo] -> IO Bool
livroJaExiste codigo livros = do
  let livroExistente = any (livroTemCodigo codigo) livros
  if livroExistente
    then return True
    else do
      putStrLn "O identificador do livro não existe. Tente novamente."
      return False      

reservaTemId :: IdReserva -> ReservaInfo -> Bool
reservaTemId idReserva (Reserva id _ _ _) = idReserva == id

adicionarReserva :: [ReservaInfo] -> IO [ReservaInfo]
adicionarReserva reservas = do
    alunos <- carregarAlunos -- Carrega os alunos do arquivo alunos.txt
    livros <- carregarLivros -- Carrega os livros do arquivo livros.txt
    putStrLn "Digite o identificador da reserva:"
    idreservaStr <- getLine
    let idreserva = read idreservaStr :: IdReserva
    putStrLn ""

    -- Verificar se o idReserva já existe na lista carregada do arquivo
    let reservaExistente = any (reservaTemId idreserva) reservas
    if reservaExistente
        then do
            putStrLn "O identificador da reserva já existe. Tente novamente."
            adicionarReserva reservas
        else do
            putStrLn "Digite o código do livro:"
            codigoStr <- getLine
            let codigo = read codigoStr :: Codigo
            putStrLn ""

            putStrLn "Digite o identificador do aluno:"
            idalunoStr <- getLine
            let idaluno = read idalunoStr :: IdAluno
            putStrLn ""

            alunoExiste <- alunoJaExiste idaluno alunos
            if alunoExiste
                then do
                    putStrLn "Digite a data da reserva (dd/mm/aaaa):"
                    input <- getLine
                    case parseData input of
                        Just datareserva -> do
                            let novaReserva = criarReserva idreserva codigo idaluno datareserva
                            putStrLn "Reserva adicionada:"
                            print novaReserva
                            putStrLn ""

                            putStrLn "Deseja adicionar outra reserva? (s/n):"
                            resposta <- getLine
                            putStrLn ""

                            let novaListaReserva = novaReserva : reservas
                            if resposta == "s"
                                then do
                                    adicionarReserva novaListaReserva
                                else do
                                    putStrLn "Multa(s) adicionada(s)!"
                                    salvarReservas novaListaReserva
                                    return (novaReserva : reservas)
                        Nothing -> do
                                    putStrLn "Formato de data inválido. Tente novamente."
                                    adicionarReserva reservas
                else do
                    adicionarReserva reservas

removerReserva :: IdReserva -> [ReservaInfo] -> IO [ReservaInfo]
removerReserva idReserva reservas = do
    let reservasFiltradas = filter (\reserva -> getIdReserva reserva /= idReserva) reservas
    putStrLn ""
    putStrLn "Reserva removida com sucesso!"
    salvarReservas reservasFiltradas  -- Salva a lista completa de reservas
    return reservasFiltradas
  where
    getIdReserva (Reserva id _ _ _) = id

exibirReservas :: [ReservaInfo] -> IO ()
exibirReservas [] = putStrLn "Nenhuma reserva adicionada."
exibirReservas reservas = do
  let reservasInvertidas = reverse reservas
  mapM_ (putStrLn . formatReserva) reservasInvertidas

formatReserva :: ReservaInfo -> String
formatReserva (Reserva idReserva codigo idAluno dtRes) =
  "ID: " ++ show idReserva ++ ", Código Livro: " ++ show codigo ++ ", ID Aluno: " ++ show idAluno ++ ", Data de Reserva: " ++ show dtRes

-- Função auxiliar para remover as aspas de uma string
removeAspas :: String -> String
removeAspas str = filter (/= '"') str

-- Função para salvar a lista de reservas em um arquivo
salvarReservas :: [ReservaInfo] -> IO ()
salvarReservas reservas = do
    let nomeArquivo = "reservas.txt"
    withFile nomeArquivo WriteMode $ \arquivo -> do
        mapM_ (hPrint arquivo) reservas
    putStrLn $ "Reservas salvas com sucesso no arquivo: " ++ nomeArquivo
 

loopPrincipal :: [ReservaInfo] -> IO ()
loopPrincipal reservas = do
    putStrLn ""
    putStrLn "Para interagir com as funcionalidades relacionadas às reservas, escolha uma das opções abaixo:"
    putStrLn "(1) - Para adicionar uma ou mais reservas."
    putStrLn "(2) - Para remover uma ou mais reservas."
    putStrLn "(3) - Para exibir todas as reservas."
    putStrLn "(4) - Para salvar a lista de reservas em um arquivo."
    putStrLn "Sair - Qualquer tecla que não seja uma opção."
    putStrLn ""

    putStrLn "Digite:"
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            novasReservas <- adicionarReserva reservas
            loopPrincipal novasReservas
        "2" -> do
            putStrLn "Digite o identificador da reserva a ser removida:"
            idreservaStr <- getLine
            let idreserva = read idreservaStr :: IdReserva
            novaListaReservas <- removerReserva idreserva reservas
            loopPrincipal novaListaReservas
        "3" -> do
            putStrLn "Exibindo todos as reservas:"
            exibirReservas reservas
            loopPrincipal reservas
        "4" -> do
            salvarReservas reservas
            loopPrincipal reservas
        _ -> putStrLn "Encerrando o programa."                           
            

listaReservas :: IO [ReservaInfo]
listaReservas = carregarReservas

mainReserva :: IO ()
mainReserva = do
    putStrLn "Bem-vindo às funcionalidades sobre reservas."
    reservas <- carregarReservas
    loopPrincipal reservas
