module Reserva (mainReserva, reservasEspecificas, carregarReservas) where

import Data.List.Split (splitOn)
import System.IO
import Data.List (any, find)
import Data.Char (isSpace)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime, formatTime)

import Aluno (AlunoInfo, alunoTemId, carregarAlunos, exibirAlunoInfo, getIdAluno, getNome, getCurso, getNumMat)
import Livro (LivroInfo, livroTemCodigo, carregarLivros, exibirLivroInfo, getCodigo, getTitulo, getAutor, getEditora, getAno)

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

carregarReservasFec :: IO [ReservaInfo]
carregarReservasFec = do
  let nomeArquivo = "reservasfechadas.txt"
  conteudo <- readFile nomeArquivo
  if null conteudo
      then do
          putStrLn ""
          putStrLn "O arquivo de reservas fechadas está vazio."
          return []  -- Retorna uma lista vazia de emprestimos
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
            dtRes = case parseDataVerificar dtResStr of
                    Just d -> d
                    Nothing -> nullDay
        in Reserva idReserva codigo idAluno dtRes
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

-- Função para verificar se um livro existe no reservas.txt com base no identificador
livroEsta :: Codigo -> ReservaInfo -> Bool
livroEsta codigo (Reserva _ cod _ _) = codigo == cod

-- Função para verificar se um idReserva existe no reservas.txt
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
            rsvfec <- carregarReservasFec
            let reservaFecExistente = any (reservaTemId idreserva) rsvfec
            if reservaFecExistente
                then do
                    putStrLn "O identificador da reserva já existe e foi fechada. Tente novamente."
                    adicionarReserva reservas
                else do
                    putStrLn "Digite o código do livro:"
                    codigoStr <- getLine
                    let codigo = read codigoStr :: Codigo
                    putStrLn ""
            
                    livroEx <- livroExiste codigo livros
                    if livroEx
                        then do
                            putStrLn "Digite o identificador do aluno:"
                            idalunoStr <- getLine
                            let idaluno = read idalunoStr :: IdAluno
                            putStrLn ""

                            alunoEx <- alunoExiste idaluno alunos
                            if alunoEx
                                then do
                                    putStrLn "Digite a data da reserva (dd-mm-aaaa):"
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
                                                    putStrLn "Reserva(s) adicionada(s)!"
                                                    salvarReservas novaListaReserva
                                                    return (novaReserva : reservas)
                                        Nothing -> do
                                                    putStrLn "Formato de data inválido. Tente novamente."
                                                    adicionarReserva reservas
                                else do
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

exibirReservasFec :: [ReservaInfo] -> IO ()
exibirReservasFec [] = putStrLn "Nenhuma reserva fechada foi adicionada."
exibirReservasFec resvfec = do
  let resvfecInvertidas = reverse resvfec
  mapM_ (putStrLn . formatReserva) resvfecInvertidas

formatReserva :: ReservaInfo -> String
formatReserva (Reserva idReserva codigo idAluno dtRes) =
  "ID: " ++ show idReserva ++ ", Código Livro: " ++ show codigo ++ ", ID Aluno: " ++ show idAluno ++ ", Data de Reserva: " ++ formatDate dtRes

formatDate :: Day -> String
formatDate dt = formatTime defaultTimeLocale "%d-%m-%Y" dt

-- Função para salvar a lista de reservas em um arquivo
salvarReservas :: [ReservaInfo] -> IO ()
salvarReservas reservas = do
    let nomeArquivo = "reservas.txt"
    withFile nomeArquivo WriteMode $ \arquivo -> do
        mapM_ (hPrint arquivo) reservas
    putStrLn $ "Reservas salvas com sucesso no arquivo: " ++ nomeArquivo
 
-- Função pata salvar a lista de emprestimos devolvidos em uma arquivo
salvarReservaFec :: [ReservaInfo] -> IO ()
salvarReservaFec fechadas = do
    let nomeArquivo = "reservasfechadas.txt"
    withFile nomeArquivo WriteMode $ \arquivo -> do
        mapM_ (hPrint arquivo) fechadas
    putStrLn $ "Reservas(s) fechadas(s) salva(s) com sucesso no arquivo: " ++ nomeArquivo    

removerReservaFechada :: IdReserva -> [ReservaInfo] -> [ReservaInfo] -> IO [ReservaInfo]
removerReservaFechada idreserva reservas rsvfec = do
    let reservaEncontrada = acheReservaId idreserva reservas
    case reservaEncontrada of
        Just reserva -> do
            let rsvfecAtualizadas = reserva : rsvfec
            salvarReservaFec rsvfecAtualizadas
            let reservasAtualizadas = filter (\m -> getIdReserva m /= idreserva) reservas
            salvarReservas reservasAtualizadas
            return rsvfecAtualizadas
        Nothing -> do
            putStrLn "Reserva não encontrada."
            return reservas
  where
    acheReservaId :: IdReserva -> [ReservaInfo] -> Maybe ReservaInfo
    acheReservaId _ [] = Nothing
    acheReservaId idreserva (m:ms)
        | getIdReserva m == idreserva = Just m
        | otherwise = acheReservaId idreserva ms

    getIdReserva (Reserva id _ _ _) = id

reservasEspecificas :: IdAluno -> [ReservaInfo] -> IO ()
reservasEspecificas idaluno reservas = do
        alunos <- carregarAlunos
        livros <- carregarLivros

        let alunosEspecifico = filter (\aluno -> getIdAluno aluno == idaluno) alunos
            livrosReservados = filter (\reserva -> getAlunoId reserva == idaluno) reservas

        if null alunosEspecifico
            then putStrLn "Tal aluno nao consta nas reservas."
            else do
                let alunoEspecifico = head alunosEspecifico
                exibirAlunoInfo alunoEspecifico

        putStrLn ""

        if null livrosReservados
            then putStrLn "O aluno nao tem livros reservados."
            else do
                let livrosReservadosPeloAluno = map (\reserva -> getReservaLivro reserva livros) livrosReservados
                    livrosReservadosInvertidos = reverse livrosReservadosPeloAluno
                mapM_ exibirLivroInfo livrosReservadosInvertidos
    where
        getAlunoId (Reserva _ _ idaluno _) = idaluno
        getReservaLivro (Reserva _ codigo _ _) livros = getLivroCodigo codigo livros

        -- Função auxiliar para obter o livro a partir do código
        getLivroCodigo codigo livros = case find (\livro -> getCodigo livro == codigo) livros of
            Just livro -> livro
            Nothing -> error "Livro nao encontrado."

loopPrincipal :: [ReservaInfo] -> [ReservaInfo] -> IO ()
loopPrincipal reservas reservasfechadas = do
    putStrLn ""
    putStrLn "Para interagir com as funcionalidades relacionadas às reservas, escolha uma das opções abaixo:"
    putStrLn "(1) - Para adicionar uma ou mais reservas."
    putStrLn "(2) - Para remover uma ou mais reservas."
    putStrLn "(3) - Para exibir todas as reservas abertas."
    putStrLn "(4) - Para fechar uma reserva."
    putStrLn "(5) - Para exibir todas as reservas fechadas."
    putStrLn "(6) - Para exibir os livros reservados por um aluno especifico."
    putStrLn "Sair - Qualquer tecla que não seja uma opção."
    putStrLn ""

    putStrLn "Digite:"
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            novasReservas <- adicionarReserva reservas
            loopPrincipal novasReservas reservasfechadas
        "2" -> do
            putStrLn "Digite o identificador da reserva a ser removida:"
            idreservaStr <- getLine
            let idreserva = read idreservaStr :: IdReserva
            novaListaReservas <- removerReserva idreserva reservas
            loopPrincipal novaListaReservas reservasfechadas
        "3" -> do
            putStrLn "Exibindo todos as reservas:"
            exibirReservas reservas
            loopPrincipal reservas reservasfechadas
        "4" -> do
            putStrLn "Digite o identificador da reserva a ser fechada:"
            idreservaStr <- getLine
            let idreserva = read idreservaStr :: IdReserva
            novaListaReserva <- removerReservaFechada idreserva reservas reservasfechadas
            nLR <- removerReserva idreserva reservas
            loopPrincipal nLR novaListaReserva
        "5" -> do
            putStrLn "Exibindo todas as reservas fechadas:"
            exibirReservasFec reservasfechadas
            loopPrincipal reservas reservasfechadas
        "6" -> do
            putStrLn "Digite o id do aluno:"
            idalunoStr <- getLine
            let idaluno = read idalunoStr :: IdAluno
            reservasEspecificas idaluno reservas
            loopPrincipal reservas reservasfechadas    
        _ -> putStrLn "Encerrando às funcionalidades de reservas."                  
            

listaReservas :: IO [ReservaInfo]
listaReservas = carregarReservas

mainReserva :: IO ()
mainReserva = do
    putStrLn "Bem-vindo às funcionalidades sobre reservas."
    reservas <- carregarReservas
    reservasfechadas <- carregarReservasFec
    loopPrincipal reservas reservasfechadas 
