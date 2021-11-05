library(psych)

# se os dados da matriz forem dicotomicos
correlacao = tetrachoric(matriz)
# se os dados da matriz forem politomicos
correlacao = polychoric(matriz)    

propexp = eigen(correlacao$rho)$values/sum(eigen(correlacao$rho)$values)
propexp[1] = propexp[1]*100

if (propexp[1] >= 20){
  print("Suposição de unidimensionalidade atendida")
}else{
    print("Suposição de unidimensionalidade NÃO atendida")
}