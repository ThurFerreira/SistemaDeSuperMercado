type Nome = String
type Preco = Int
type CodBar = Int
type ListaDeCodigos = [CodBar]
type Recibo = [(Nome,Preco)]

-- Criação da base de dados
data Produtos = Produto [(CodBar, Nome, Preco)]

-- Banco de dados com todos os produtos do supermercado.
listaProdutos :: Produtos
listaProdutos = Produto [(1234, "Oleo DoBom, 1l" , 195),(4756, "Chocolate Cazzeiro, 250g", 180), (3216, "Arroz DoBom, 5Kg", 213), (5823, "Balas Pedregulho, 1Kg" , 379), (4719, "Queijo Mineirim, 1Kg" , 449), (6832, "Iogurte Maravilha, 1Kg" , 499), (1112, "Rapadura QuebraDente, 1Kg", 80),(1111, "Sal Donorte, 1Kg", 221), (1113, "Cafe DoBom, 1Kg", 285), (1115, "Biscoito Bibi, 1Kg", 80), (3814, "Sorvete QGelo, 1l", 695)]

-- Função para a formatação do preço do produto antes de ser exibido na na tela;
formataCentavos :: Preco -> String
formataCentavos x = show (x `div` 100) ++ "." ++ show(x `mod` 100)

-- replica a unidade "." para que possa ser utilizada na função "formataLinha"
replica :: (Int,Char) -> String
replica (0,_) = ""
replica (x,y) = [y] ++ replica (x-1, y) 

-- formata a linha do produto com nome e preço.
formataLinha :: (Nome,Preco) -> String
formataLinha (x,y) = x ++ replica (30 - (length x + length(formataCentavos y)), '.') ++ formataCentavos y ++ "\n"

-- une todas os produtos formatados com nome e preço
formataLinhas :: [(Nome,Preco)] -> String
formataLinhas [] = []
formataLinhas ((x,y):xs) = formataLinha (x,y) ++ formataLinhas xs

-- Função responsável por procurar o código especifico e retornar o preço do produto.
-- Deve ser chamada para que possa ser feita a consulta do preço do produto por código.
achaItem :: CodBar -> (Nome, Preco)
achaItem x = acha listaProdutos x

-- Função responsável por buscar os itens na base de dados e retornar as informações do produto especícido;
acha :: Produtos -> CodBar -> (Nome, Preco)
acha (Produto []) _ = ("Item Desconhecido", 0)
acha (Produto ((x,y,z):xs)) w
 |w == x = (y,z)
 |otherwise = acha (Produto xs) w


-- faz  a soma de todos os produtos que foram escaneados
geraTotal :: Recibo -> Preco
geraTotal [] = 0
geraTotal ((_,y):xs) = y + geraTotal xs

-- formata a linha do total, acrescentando o cifrão e a unidade "."
formataTotal :: Preco -> String
formataTotal x = "TOTAL" ++ replica (20, '.') ++ "$" ++ formataCentavos x

-- Gera o recibo unindo todos os produtos passados no caixa
geraRecibo :: ListaDeCodigos -> String
geraRecibo x = formataRecibo (fazRecibo x)

-- Faz uma unica linha do recibo de acordo com o código dos itens que recebe
fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo [] = []
fazRecibo (x:xs) = achaItem x : fazRecibo xs

-- formata o recibo geral, unindo as linhas dos produtos com o total
formataRecibo :: Recibo -> String
formataRecibo [] = []
formataRecibo (x:y) = (formataLinhas (x:y)) ++ formataTotal(geraTotal(x:y))

main :: IO ()
main = putStrLn (geraRecibo [1234,4756,3216,5823,4719,6832,1112,1111,1113,1115,3814])