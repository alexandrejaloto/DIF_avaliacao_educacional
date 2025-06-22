library (mirt)

rm(list = ls())

set.seed (12345)
a1 = a2 = rlnorm (45, log(1.7), .5)

# gerar dois objetos com os valores do parâmetro b (dificuldade)
b1 = b2 = rnorm (45, 0, 1)

# alteração nos primeiro e segundo itens
b2 [1] = b2 [1] - 1.5
a2 [2] = a2 [2] / 8

d1 = -a1*b1
d2 = -a2*b2

c <- rbeta(45, 5, 17)

modelo = rep ('3PL', 45)
sim1 = simdata (a1, d1, guess = c, 1000, modelo)
sim2 = simdata (a2, d2, guess = c, 1000, modelo)
dados = data.frame(rbind (sim1, sim2))

dados$grupo <- rep (c('G1', 'G2'), c(1000, 1000))

data.table::fwrite(dados, 'dados/banco_simulado.csv')

