module Atalho (primeiroRequisito, segundoRequisito, terceiroRequisito, quartoRequisito, quintoRequisito) where

import Livro (carregarLivros)
import Aluno (carregarAlunos)
import Multas (carregarMultas, alunosPendentes)
import Emprestimo (livrosDisponiveis, carregarEmprestimos, exibirAlunoEspecifico, exibirEmprestimos)
import Reserva (carregarReservas, reservasEspecificas)

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
    let idaluno = read idalunoStr :: IdAluno
    exibirAlunoEspecifico idaluno

terceiroRequisito :: IO ()
terceiroRequisito = do
    emprestimos <- carregarEmprestimos
    exibirEmprestimos emprestimos

quartoRequisito :: IO ()
quartoRequisito = do
    reservas <- carregarReservas
    putStrLn "Digite o id do aluno:"
    idalunoStr <- getLine
    let idaluno = read idalunoStr :: IdAluno
    reservasEspecificas idaluno reservas

quintoRequisito :: IO ()
quintoRequisito = do
    alunos <- carregarAlunos
    multas <- carregarMultas
    alunosPendentes multas alunos
