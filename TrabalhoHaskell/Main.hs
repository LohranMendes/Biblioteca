module Main where

import Livro (mainLivro)
import Emprestimo (mainEmprestimo)
import Aluno (mainAluno)
import Multas (mainMulta)
import Reserva (mainReserva)
import Atalho

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Olá."
    putStrLn "Bem-vindo ao Sistema de Gerenciamento da Biblioteca."
    putStrLn "Escolha uma das opções para interagir com as funcionalidades do sistema."
    putStrLn ""
    putStrLn "(1) - Livros."
    putStrLn "(2) - Alunos."
    putStrLn "(3) - Multas."
    putStrLn "(4) - Reservas."
    putStrLn "(5) - Emprestimos."
    putStrLn ""
    putStrLn "Atalhos!"
    putStrLn "(6) - Livros Disponíveis."
    putStrLn "(7) - Emprestimos de um aluno especifico (Por id da biblioteca)."
    putStrLn "(8) - Todos os emprestimos em abertos (não devolvidos)."
    putStrLn "(9) - Todos os livros reservados por um aluno especifico (Por id da biblioteca)."
    putStrLn "(10) - Todos os alunos com multas pendentes."

    putStrLn "Sair - Escolha qualquer tecla que não seja uma opção para encerrar o programa."
    putStrLn ""

    putStrLn "Digite: "
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            mainLivro
            main
        "2" -> do
            mainAluno
            main
        "3" -> do
            mainMulta
            main
        "4" -> do
            mainReserva
            main
        "5" -> do
            mainEmprestimo
            main 
        "6" -> do
            primeiroRequisito
            main
        "7" -> do
            segundoRequisito
            main
        "8" -> do
            terceiroRequisito
            main
        "9" -> do
            quartoRequisito
            main
        "10" -> do
            quintoRequisito
            main                   
        _ -> putStrLn "Encerrando o programa..."                  
    
