import Text.Read (readMaybe)

type Tarefa = String

main :: IO ()
main = do
    putStrLn "Bem-vindo ao Gerenciador de Tarefas!"
    menuLoop []

menuLoop :: [Tarefa] -> IO ()
menuLoop tarefas = do
    putStrLn "\nMenu:"
    putStrLn "1. Adicionar Tarefa"
    putStrLn "2. Remover Tarefa"
    putStrLn "3. Listar Tarefas"
    putStrLn "4. Sair"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            putStr "Digite a nova tarefa: "
            nova <- getLine
            putStrLn "Tarefa adicionada com sucesso!"
            menuLoop (tarefas ++ [nova])

        "2" -> do
            if null tarefas then do
                putStrLn "Não há tarefas para remover."
                menuLoop tarefas
            else do
                exibirTarefas tarefas
                putStr "Digite o número da tarefa a remover: "
                entrada <- getLine
                case readMaybe entrada :: Maybe Int of
                    Just idx ->
                        if idx >= 1 && idx <= length tarefas
                            then do
                                putStrLn "Tarefa removida com sucesso!"
                                menuLoop (removerTarefa idx tarefas)
                            else do
                                putStrLn "Número fora da faixa. Tente novamente."
                                menuLoop tarefas
                    Nothing -> do
                        putStrLn "Entrada inválida. Digite um número inteiro."
                        menuLoop tarefas

        "3" -> do
            exibirTarefas tarefas
            menuLoop tarefas

        "4" -> putStrLn "Saindo do programa. Até logo!"

        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            menuLoop tarefas

exibirTarefas :: [Tarefa] -> IO ()
exibirTarefas [] = putStrLn "Nenhuma tarefa cadastrada."
exibirTarefas ts = do
    putStrLn "\nLista de Tarefas:"
    mapM_ putStrLn [show i ++ ". " ++ t | (i, t) <- zip [1..] ts]

removerTarefa :: Int -> [Tarefa] -> [Tarefa]
removerTarefa n ts = let (antes, _:depois) = splitAt (n - 1) ts in antes ++ depois
