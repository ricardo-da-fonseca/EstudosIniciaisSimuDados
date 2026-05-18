###################################################
#Função para ajustar as variâncias de uma amostra #
#autor: Ricardo da Fonseca                        #
#data: 17/05/2026                                 #
###################################################

ajVar<-function(x, ajuste = 0.05, par = c(0, 1), semente = 6){
  varAmostra<-var(x)
  limInf<-par[2]^2*(1-ajuste)
  limSup<-par[2]^2*(1+ajuste)
  
  amostra<-NULL
  contador<-0
  while(varAmostra < limInf || varAmostra > limSup){
    amostra<-rnorm(length(x), par[1], par[2])
    varAmostra<-var(amostra)
    contador<-contador+1
  }
  message("Número de tentativas realizadas: ", contador)
  message("Variância da nova amostra: ", var(amostra))
  amostra
}
