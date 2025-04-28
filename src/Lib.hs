module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje= unPersonaje{
    nombre::String,
    poderBasico:: String,
    superPoder:: String,
    superActiva:: Bool,
    vida::Int,
}

bolaEspinosa:: Personaje-> Personaje
bolaEspinosa personaje= vida personaje -1000

lluviaDeTuercas:: String->Personaje->Personaje
lluviaDeTuercas tipoDeLluvia personaje
  |tipoDeLluvia=="Sanadora"=vida personaje+800
  |tipoDeLluvia=="Dañina" =vida personaje-vida personaje/2
  |otherwise 
  
granadaDeEspinas:: Int->Personaje->Personaje
granadaDeEspinas radio personaje
  |radio>3 && vida personaje<800= nombre personaje++"Espina estuvo aquí", superActiva personaje = False, vida personaje=0
  |radio>3 = nombre personaje++"Espina estuvo aquí"
  |otherwise =bolaEspinosa personaje

torretaCurativa:: Personaje-> Personaje
torretaCurativa personaje= superActiva personaje= True vida personaje=vida personaje*2

ataqueBasico::Personaje-> Personaje -> Personaje
ataqueBasico personaje contrincante 
  |poderBasico personaje=="Bola de espinas"= bolaEspinosa contrincante
  |poderBasico personaje=="Lluvia de tuercas"= lluviaDeTuercas "Dañina" contrincante



atacarConElPoderEspecial:: Personaje-> Personaje -> Personaje
atacarConElPoderEspecial personaje contrincante 
  |superActiva personaje= ataqueBasico personaje contrincante && ataqueSuper personaje contrincante
  |otherwise 

quienesEstanEnLasUltimas::[Personaje]->[Personaje]
quienesEstanEnLasUltimas personajes= filter (<800) (map vida personaje) personajes

unPersonaje{
    nombre= "Espina",
    poderBasico="Bola de espinas" ,
    superPoder= ,
    superActiva=True ,
    vida=4800
}
unPersonaje{
    nombre= "Pamela",
    poderBasico="Bola de espinas" ,
    superPoder= ,
    superActiva=False ,
    vida=9600
}
