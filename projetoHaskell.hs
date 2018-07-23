-- Funnção que exibe o Menu

menu :: IO()
menu = do
		system "clear"
		
		putStrLn "Jogo da Memória"
		putStrLn "\nMenu:"
		putStrLn "\n1. Iniciar jogo"
		putStrLn "\n2. Mostrar Ranking"
		putStrLn "\n3. Sair"
		
-- Escolha do nivel
nivel :: String -> IO()
nivel nome = do
	system "clear"
	putStrLn(" Escolha um dos niveis:\n\n" ++
			"1. Iniciante\n" ++
			"2. Intermediario\n" ++
			"3. Avancado\n\n")
	dados <- getLine
	let niveis = read(dados)
	if niveis == 1 then
		iniciaJogo niveis 4 nome
	else if niveis == 2 then
		iniciaJogo niveis 6 nome
	else if niveis == 3 then
		iniciaJogo niveis 8 nome
	else
		nivel nome
	
-- Controller
controller :: Int -> Int -> Int -> [[String]] -> [[Int]] -> IO()
controller tamanho nivel paresEncontrados matrizUsuario matrizInt tempo = do
	let tamMatriz = tamanho * tamanho
	let xD =  intToDouble x
	let g = xD / 2
	let pares = round g

	if (paresEncontrados /= pares) then do
		
		matrizPrint nivel matrizInt
		putStrLn "\nDigite a linha do primeiro elemento:"
		l1 <- getLine
		putStrLn "\nDigite a coluna do primeiro elemento:"
		c1 <- getLine
		putStrLn "\nDigite a linha do segundo elemento:"
		l2 <- getLine
		putStrLn "\nDigite a coluna do segundo elemento:"
		c2 <- getLine
		
		
		let linha1 = read(l1)
		let coluna1 = read(c1)
		let linha2 = read(l2)
		let coluna2 = read(c2)
		if (verificaPares linha1 coluna1 linha2 coluna2 matrizInt) then do
			let posicao = (linha1, coluna1)
			let elem = matrizInt !! (linha1 -1) !! (coluna1 -1)
			
			-- verificar a posicaoValida e modifica matriz
			
			
			cond <- posicaoValida posicao matrizInt
			if cond then do
				matrizM <- modificaMatriz tamanho elem posicao matrizUsuario
				let posicao2 = (linha2, coluna2)
				let elem2 = matrizInt !! (linha2 -1) !! (coluna -1)
				matrizM2 <- modificaMatriz tamanho elem2 posicao2 matrizM
				jogo tamanho nivel (paresEcontrados+1) matrizM2 matrizInt
			else do
				putStrLn("Posicao ja encontrada")
				jogo tamanho nivel paresEcontrados matrizUsuario matrizInt
		else do
				let posicao = (linha1, coluna1)
				let elem = matrizInt !! (linha1 -1) !! (coluna1 -1)
				matrizM <- modificaMatriz tamanho elem posicao matrizUsuario
				let posicao2 = (linha2, coluna2)
				let elem2 = matrizInt !! (linha2 -1) !! (coluna2 -1)
				matrizM2 <- modificaMatriz tamanho elem2 posicao2 matrizM
				matrizPrint nivel matrizM2
				jogo tamanho nivel paresEcontrados matrizUsuario matrizInt
		else do
			system "clear"
			putStrLn "As posicoes sao invalidas"
			jogo tamanho nivel paresEcontrados matrizUsuario matrizInt
	
	
	-- Nao sei se ta certo
	putStrLn "Fim de jogo"
		
		
		
