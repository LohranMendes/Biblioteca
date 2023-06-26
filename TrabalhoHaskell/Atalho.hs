module Atalho (primeiroRequisito, segundoRequisito, terceiroRequisito, quartoRequisito, quintoRequisito) where

import Livro (carregarLivros)
import Aluno (carregarAlunos)
import Multas (carregarMultas, alunosPendentes)
import Emprestimo (livrosDisponiveis, carregarEmprestimos, exibirAlunoEspecifico, exibirEmprestimos)
import Reserva (carregarReservas, reservasEspecificas)
import Text.Read (readMaybe)

type IdAluno = Int

primeiroRequisito :: IO ()
primeiroRequisito = do
    emprestimos <- carregarEmprestimos
    livros <- carregarLivros
    livrosDisponiveis emprestimos livros

segundoRequisito :: IO ()
segundoRequisito = do
    putStrLn "Digite o id do aluno:"
    idalunoStr <- getLine
    putStrLn ""
    case readMaybe idalunoStr :: Maybe IdAluno of
        Just idaluno -> do
            exibirAlunoEspecifico idaluno
        Nothing -> do
            putStrLn ""
            putStrLn "Identificador do aluno inválido."    

terceiroRequisito :: IO ()
terceiroRequisito = do
    emprestimos <- carregarEmprestimos
    exibirEmprestimos emprestimos

quartoRequisito :: IO ()
quartoRequisito = do
    reservas <- carregarReservas
    putStrLn "Digite o id do aluno:"
    idalunoStr <- getLine
    case readMaybe idalunoStr :: Maybe IdAluno of
        Just idaluno -> do
            reservasEspecificas idaluno reservas
        Nothing -> do
            putStrLn ""
            putStrLn "Identificador do aluno inválido."       

quintoRequisito :: IO ()
quintoRequisito = do
    alunos <- carregarAlunos
    multas <- carregarMultas
    alunosPendentes multas alunos
