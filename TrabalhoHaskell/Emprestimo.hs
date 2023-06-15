module Emprestimo (mainEmprestimo) where

import Data.List.Split
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)
import Livro (LivroInfo, livroTemCodigo)
import Aluno (AlunoInfo, alunoTemId)

type IdEmprestimo = Int
type Codigo = Int
type IdAluno = Int

data EmprestimoInfo = Emprestimo IdEmprestimo Codigo IdAluno Day Day
    deriving (Show)

criarEmprestimo :: IdEmprestimo -> Codigo -> IdAluno -> Day -> Day -> EmprestimoInfo
criarEmprestimo id cod idAluno dtEmp dtDev = Emprestimo id cod idAluno dtEmp dtDev

-- Função para fazer o parsing da data no formato "dd/mm/aaaa"
parseData :: ParseTime t => String -> Maybe t
parseData = parseTimeM True defaultTimeLocale "%d/%m/%Y"

-- Função para ler a data do teclado e convertê-la para o tipo Day
lerData :: IO Day
lerData = do
  putStrLn "Em (dd/mm/aaaa):"
  input <- getLine
  case parseData input of
    Just dataConvertida -> return dataConvertida
    Nothing -> do
      putStrLn "Formato de data inválido. Tente novamente."
      lerData

-- Função para verificar se um aluno existe com base no identificador
verificarAlunoExistente :: IdAluno -> [AlunoInfo] -> Bool
verificarAlunoExistente idAluno alunos = any (alunoTemId idAluno) alunos

-- Função para verificar se um livro existe com base no código
verificarLivroExistente :: Codigo -> [LivroInfo] -> Bool
verificarLivroExistente codigo livros = any (livroTemCodigo codigo) livros

-- Função para ler o identificador do aluno e verificar se existe
lerIdAluno :: [AlunoInfo] -> IO IdAluno
lerIdAluno alunos = do
  putStrLn "Digite o identificador do aluno:"
  idalunoStr <- getLine
  let idaluno = read idalunoStr :: IdAluno
  if verificarAlunoExistente idaluno alunos
    then return idaluno 
    else do
      putStrLn "Identificador de aluno inválido. Insira novamente."
      putStrLn ""
      lerIdAluno alunos

-- Função para ler o código do livro e verificar se existe
lerCodigoLivro :: [LivroInfo] -> IO Codigo
lerCodigoLivro livros = do
  putStrLn "Digite o código do livro:"
  codigoStr <- getLine
  putStrLn ""
  let codigo = read codigoStr :: Codigo
  if verificarLivroExistente codigo livros
    then return codigo
    else do
      putStrLn "Código de livro inválido. Insira novamente."
      putStrLn ""
      lerCodigoLivro livros      

-- Função para ler os dados do empréstimo
lerDadosEmprestimo :: [AlunoInfo] -> [LivroInfo] -> [EmprestimoInfo] -> IO (IdEmprestimo, Codigo, IdAluno, Day, Day)
lerDadosEmprestimo alunos livros emprestimos = do
    putStrLn "Digite o identificador do emprestimo:"
    idemprestimoStr <- getLine
    let idemprestimo = read idemprestimoStr :: IdEmprestimo
    putStrLn ""
    codigo <- lerCodigoLivro livros
    idaluno <- lerIdAluno alunos
    putStrLn "Digite a data do pedido do empréstimo:"
    dataEmprestimo <- lerData
    putStrLn "Digite a data prevista para a devolução do empréstimo:"
    dataDevolucao <- lerData
    return (idemprestimo, codigo, idaluno, dataEmprestimo, dataDevolucao)      

adicionarEmprestimo :: [EmprestimoInfo] -> IO [EmprestimoInfo]
adicionarEmprestimo emprestimos = do
    putStrLn "Digite o identificador do emprestimo:"
    idemprestimoStr <- getLine
    let idemprestimo = read idemprestimoStr :: IdEmprestimo
    putStrLn ""

    putStrLn "Digite o codigo do livro:"
    codigoStr <- getLine
    let codigo = read codigoStr :: Codigo
    putStrLn ""

    putStrLn "Digite o identificador do aluno:"
    idalunoStr <- getLine
    let idaluno = read idalunoStr :: IdAluno
    putStrLn ""

    putStrLn "Digite a data do pedido do emprestimo:"
    dataEmprestimo <- lerData
    putStrLn ""

    putStrLn "Digite a data prevista para a devolucao emprestimo:"
    dataDevolucao <- lerData
    putStrLn ""
    
    let novoEmprestimo = criarEmprestimo idemprestimo codigo idaluno dataEmprestimo dataDevolucao
    putStrLn "Emprestimo adicionado:"
    print novoEmprestimo
    putStrLn ""

    putStrLn "Deseja adicionar outro emprestimo? (s/n):"
    resposta <- getLine
    putStrLn ""

    if resposta == "s"
        then do
            let novaListaEmprestimo = novoEmprestimo : emprestimos
            adicionarEmprestimo novaListaEmprestimo
        else do
            putStrLn "Emprestimo(s) adicionado(s)!"
            return (novoEmprestimo : emprestimos)

removerEmprestimo :: IdEmprestimo -> [EmprestimoInfo] -> IO [EmprestimoInfo]
removerEmprestimo idemprestimo emprestimos = do
    let emprestimosFiltrados = filter (\emprestimo -> getIdEmprestimo emprestimo /= idemprestimo) emprestimos
    putStrLn ""
    putStrLn "Emprestimo removido com sucesso!"
    return emprestimosFiltrados
  where
    getIdEmprestimo (Emprestimo id _ _ _ _) = id

exibirEmprestimos :: [EmprestimoInfo] -> IO ()
exibirEmprestimos [] = putStrLn "Nenhum emprestimo adicionado."
exibirEmprestimos emprestimos = mapM_ print emprestimos

loopPrincipal :: [EmprestimoInfo] -> IO ()
loopPrincipal listaEmprestimos = do
    putStrLn ""
    putStrLn "Para interagir com as funcionalidades relacionadas aos emprestimos, escolha uma das opções abaixo:"
    putStrLn "(1) - Para adicionar um ou mais emprestimos."
    putStrLn "(2) - Para remover um ou mais emprestimos."
    putStrLn "(3) - Para exibir todos os emprestimos."
    putStrLn "Sair - Qualquer tecla que não seja uma opção."
    putStrLn ""

    putStrLn "Digite:"
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            novaListaEmprestimos <- adicionarEmprestimo listaEmprestimos
            loopPrincipal novaListaEmprestimos
        "2" -> do
            putStrLn "Digite o identificador do emprestimo a ser removido:"
            idemprestimoStr <- getLine
            let idemprestimo = read idemprestimoStr :: IdEmprestimo
            novaListaEmprestimos <- removerEmprestimo idemprestimo listaEmprestimos
            loopPrincipal novaListaEmprestimos
        "3" -> do
            putStrLn "Exibindo todos os emprestimos:"
            exibirEmprestimos listaEmprestimos
            loopPrincipal listaEmprestimos
        _ -> putStrLn "Opção inválida. Retornando às opções sobre emprestimos."

mainEmprestimo :: IO ()
mainEmprestimo = loopPrincipal []
