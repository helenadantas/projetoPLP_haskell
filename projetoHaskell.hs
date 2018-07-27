import System.IO
import System.IO.Error
import System.Process

main = do putStrLn("Menu:" ++ "\n")
          system "clear"
          putStrLn "Jogo da Memória"
          putStrLn "\nMenu:"
          putStrLn "\nComo Jogar: A linha sera a escolha da lista e a coluna a posicao do elemento nesta lista."
          putStrLn "\nA linha/coluna de escolha do nivel inciante vai de 0-3."
          putStrLn "\nNo caso do intermediario de 0-5."
          putStrLn "\nNo caso do avancado de 0-7.\n"
          putStrLn "\nEscolha um nível:"
          putStrLn "\n1. Iniciante"
          putStrLn "\n2. Intermediario"
          putStrLn "\n3. Avancado"
          putStrLn "\n4. Sair\n"
          
          number <- readLn
          menu number
 
-- o metodo ira funcionar de acordo com a escolha do nivel do usuario, ele instancia as duas matrizes
-- do sistema e do usuario, respectivamente, depois iniciar a jogada.
menu :: Int -> IO()
menu 1 = do
       let matrizIniciante = [[4,6,7,2], [1,3,5,2], [4,5,6,3], [1,8,8,7]]
       let matrizInicianteUsuario = [[0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]]
       jogada 8 0 matrizIniciante matrizInicianteUsuario
menu 2 = do 
       let matrizIntermediaria = [[18,5,8,2,4,9], [5,16,12,7,3,10], [1,17,8,6,11,18], [10,3,17,14,4,13], [7,15,2,11,16,6], [12,14,9,1,15,13]]
       let matrizIntermediariaUsuario = [[0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]]
       jogada 18 0 matrizIntermediaria matrizIntermediariaUsuario
menu 3 = do 
       let matrizAvancada = [[10,4,9,2,14,14,21,26], [23,17,29,11,20,1,30,30], [4,27,17,5,24,29,6,6], [26,28,11,8,13,16,12,31], [21,22,2,19,19,1,25,25], [3,27,10,32,24,15,7,12], [23,18,28,5,16,15,31,13], [9,22,3,18,20,32,7,8]]          
       let matrizAvancadaUsuario = [[0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0]]
       jogada 32 0 matrizAvancada matrizAvancadaUsuario
menu 4 = do 
          system "clear"
          putStrLn("\n" ++ "Jogo finalizado.")

-- O metodo faz o controller do jogo, ele ira receber as escolhas do usuario, verificar na matriz do
-- sistema se os elementos sao iguais, se sim, a modificacao da matriz do usuarario ficara permanente
-- se nao, a tela ira limpar e a matriz do usuario permanecera a anterior.
-- O metodo eh recursivo, ate que o usuario encontre todos os pares.
jogada ::Int -> Int -> [[Int]] -> [[Int]] -> IO()
jogada 0 jogadas matrizI matrizF = putStrLn "Fim de Jogo!!!"
jogada i j (s:cs) (x:xs) = do
        putStrLn "\nDigite a linha do primeiro elemento:"
        linha1 <- getLine
        let l1 = (read linha1) :: Int
        putStrLn "\nDigite a coluna do primeiro elemento:"
        coluna1 <- getLine
        let c1 = (read coluna1) :: Int
        putStrLn "\nDigite a linha do segundo elemento:"
        linha2 <- getLine
        let l2 = (read linha2) :: Int
        putStrLn "\nDigite a coluna do segundo elemento:"
        coluna2 <- getLine
        let c2 = (read coluna2) :: Int
        
        let e1 = getElemento (s:cs) l1 c1 
        let e2 = getElemento (s:cs) l2 c2
        let aux = modificaLinha ((x:xs)!!l1) e1 c1
        let matrizusuario1 = atualizaMatrizUsuario (x:xs) l1 aux                   
        let aux2 = modificaLinha (matrizusuario1!!l2) e2 c2
        let matrizUsuarioAtualizada = atualizaMatrizUsuario matrizusuario1 l2 aux2
        print matrizUsuarioAtualizada
        
        if (e1 == e2) then do jogada (i-1) (j+1) (s:cs) matrizUsuarioAtualizada 
        else do
         system "clear"
         jogada i (j+1) (s:cs) (x:xs)

-- O metodo ira modificar a linha da matriz do usuario trocando o 0 pelo respectivo elemento
--escolhido pelo usuario na matriz do sistema.                                            
modificaLinha :: [Int] -> Int -> Int -> [Int]
modificaLinha [] elemento coluna = []
modificaLinha (h:hs) elemento 0 =  (elemento:hs)
modificaLinha (h:hs) elemento coluna = (h:(modificaLinha hs elemento (coluna-1)))

-- O metodo ira modificar a matriz do usuario de acordo com a respectiva linha e coluna escolhida
--pelo usuario trocando o 0 pelo elemento
atualizaMatrizUsuario :: [[Int]] -> Int -> [Int] -> [[Int]]
atualizaMatrizUsuario [] e1 _  = []
atualizaMatrizUsuario (h:ht) 0 aux = aux:ht 
atualizaMatrizUsuario (h:ht) l1 aux = h : (atualizaMatrizUsuario ht (l1-1) aux)

-- vai retornar o elemento da matriz de acordo com a linha e coluna passadas pelo usuario                                                                        
getElemento :: [[Int]] -> Int -> Int -> Int
getElemento matriz linha coluna = matriz !! linha !! coluna


