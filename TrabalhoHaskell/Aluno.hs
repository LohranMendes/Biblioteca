module Aluno (mainAluno, AlunoInfo, alunoTemId) where

import Data.List.Split
import Livro

type IdAluno = Int
type NomeAluno = String
type CursoAluno = String
type NumMat = Int

data AlunoInfo = Aluno IdAluno NomeAluno CursoAluno NumMat
    deriving (Show)

criarAluno :: IdAluno -> NomeAluno -> CursoAluno -> NumMat -> AlunoInfo
criarAluno id nome curso mat = Aluno id nome curso mat

adicionarAluno :: [AlunoInfo] -> IO [AlunoInfo]
adicionarAluno alunos = do
    putStrLn "Digite o identificador do aluno:"
    idalunoStr <- getLine
    let idaluno = read idalunoStr :: IdAluno
    putStrLn ""

    putStrLn "Digite o nome do aluno:"
    nome <- getLine
    putStrLn ""

    putStrLn "Digite o curso do aluno:"
    curso <- getLine
    putStrLn ""

    putStrLn "Digite o número da matrícula do aluno:"
    numMatStr <- getLine
    putStrLn ""

    let numMat = read numMatStr :: NumMat
    
    let novoAluno = criarAluno idaluno nome curso numMat
    putStrLn "Aluno adicionado:"
    print novoAluno
    putStrLn ""

    putStrLn "Deseja adicionar outro aluno? (s/n):"
    resposta <- getLine
    putStrLn ""

    if resposta == "s"
        then do
            let novaListaAlunos = novoAluno : alunos
            adicionarAluno novaListaAlunos
        else do
            putStrLn "Aluno(s) adicionado(s)!"
            return (novoAluno : alunos)

removerAluno :: IdAluno -> [AlunoInfo] -> IO [AlunoInfo]
removerAluno codigo alunos = do
    let alunosFiltrados = filter (\aluno -> getIdAluno aluno /= codigo) alunos
    putStrLn ""
    putStrLn "Aluno removido com sucesso!"
    return alunosFiltrados
  where
    getIdAluno (Aluno id _ _ _) = id

exibirAlunos :: [AlunoInfo] -> IO ()
exibirAlunos [] = putStrLn "Nenhum aluno adicionado."
exibirAlunos alunos = do
    mapM_ print alunos

alunoTemId :: IdAluno -> AlunoInfo -> Bool
alunoTemId idAluno (Aluno id _ _ _) = idAluno == id

loopPrincipal :: [AlunoInfo] -> IO ()
loopPrincipal listaAlunos = do
    putStrLn ""
    putStrLn "Para interagir com as funcionalidades relacionadas aos alunos, escolha uma das opções abaixo:"
    putStrLn "(1) - Para adicionar um ou mais alunos."
    putStrLn "(2) - Para remover um ou mais alunos."
    putStrLn "(3) - Para exibir todos os alunos."
    putStrLn "Sair - Qualquer tecla que não seja uma opção."
    putStrLn ""

    putStrLn "Digite:"
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            novaListaAlunos <- adicionarAluno listaAlunos
            loopPrincipal novaListaAlunos
        "2" -> do
            putStrLn "Digite o  do aluno a ser removido:"
            idalunoStr <- getLine
            let idaluno = read idalunoStr :: IdAluno
            novaListaAlunos <- removerAluno idaluno listaAlunos
            loopPrincipal novaListaAlunos
        "3" -> do
            putStrLn "Exibindo todos os alunos:"
            exibirAlunos listaAlunos
            loopPrincipal listaAlunos
        _ -> putStrLn "Opção inválida. Retornando às opções sobre alunos."

mainAluno :: IO ()
mainAluno = loopPrincipal []
