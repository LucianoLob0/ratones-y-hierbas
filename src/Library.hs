module Library where
import PdePreludat

-- Ejercicio 1 --
{-
●	Cerebro es un ratón con 9 años, 0.2 kg de peso y tiene brucelosis, sarampión y tuberculosis.
●	Bicenterrata es un ratón con 256 años, 0.2kg de peso, y completamente sano.
●	Huesudo es un ratón de 4 años con 10kg de peso, y alta obesidad y sinusitis.
1.	Modelar a los ratones mencionados.
-}

data Raton = UnRaton {
nombre :: String
, edad :: Number
, peso :: Number
, enfermedades :: [String]
}deriving (Show)

cerebro =  UnRaton {
nombre = "Cerebro"
, edad = 9
, peso = 0.2
, enfermedades = ["bruselosis", "sarampion", "tuberculosis"]
}

bicenterrata =  UnRaton {
nombre = "Bicenterrata"
, edad = 256
, peso = 0.2
, enfermedades = []
}

huesudo =  UnRaton {
nombre = "Huesudo"
, edad = 4
, peso = 10
, enfermedades = ["alta obesidad", "sinusitis"]
}

-- Ejercicio 2 --

--a.	hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
--Por ejemplo, si a cerebro le doy hierbaBuena, se transforma en un ratón de 3 años.

type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena = rejuvenece

rejuvenece :: Raton -> Raton
rejuvenece raton = raton {edad = sqrt (edad raton)}

--b.	hierbaVerde, elimina las enfermedades que terminen de cierta forma.
--Por ejemplo, si a cerebro le doy la hierbaVerde del tipo “sis”, queda sólo con sarampión.

type Tipo = String

hierbaVerde :: Tipo -> Hierba
hierbaVerde tipo = curarEnfermedadesDelTipo tipo 

curarEnfermedadesDelTipo :: Tipo -> Raton -> Raton
curarEnfermedadesDelTipo tipo raton = raton {enfermedades = filter (not . caracterDe tipo) (enfermedades raton)} 

type Enfermedad = String

caracterDe :: Tipo -> Enfermedad -> Bool 
caracterDe "menosDe10letras" enfermedad = length enfermedad < 10
caracterDe "obesidad" enfermedad = "alta obesidad" == enfermedad
caracterDe tipo enfermedad = reverse (take (length tipo) (reverse enfermedad)) == tipo

--c.	alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
--Por ejemplo, un raton de 3 kg queda con 2,7 kg y cerebro queda con 0.19 kg. 

alcachofa :: Hierba
alcachofa raton 
            | peso raton > 2 = raton {peso = peso raton * 0.9}
            | otherwise = raton {peso = peso raton * 0.95}

--d.	hierbaZort, hace que el ratón se transforme en Pinky, 
--perdiendo todas sus enfermedades y quedando con 0 años de edad.

hierbaZort :: Hierba
hierbaZort raton = raton {enfermedades = [] , edad = 0} 

--e.	hierbaDelDiablo, hace que el ratón pierda 0.1kg (sin disminuir de 0)
-- y elimina todas las enfermedades con menos de 10 letras.

hierbaDelDiablo :: Hierba
hierbaDelDiablo = curarEnfermedadesDelTipo "menosDe10letras" . perder100gramos

perder100gramos :: Raton -> Raton
perder100gramos raton
            | peso raton < 0.1 = raton {peso = 0}
            | otherwise = raton {peso = peso raton - 0.1}

-- Ejercicio 3 --

{-Medicamentos: Los medicamentos son la administración sucesiva de un conjunto de hierbas. 
Se pide crear los siguientes medicamentos para luego poder administrarlos en un ratón: 

a.	Hacer el pondsAntiAge, que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa. iterate
Por ejemplo, si se lo administramos al ratón Bicenterrata, queda con 2 años y 0.19 kg -}

type Medicamento = Raton -> Raton

pondsAntiAge :: Medicamento
pondsAntiAge = alcachofa . (aplicarNveces 3 hierbaBuena)

aplicarNveces :: Number -> Hierba -> Raton -> Raton
aplicarNveces n hierba raton = foldl (flip dosificarHierba) raton (replicate n hierba)

dosificarHierba :: Hierba -> Raton -> Raton
dosificarHierba hierba raton = hierba raton

{-b.	Hacer el reduceFatFast, (que viene en distintas potencias) 
y es un medicamento compuesto por una hierbaVerde de “obesidad” y tantas alcachofas como indique su potencia.
Por ejemplo administrándole a Huesudo un reduceFatFast de potencia 1 hace que huesudo pase a pesar 9 kg 
y sólo quede con sinusitis. Si en lugar de la 1 le administramos un reduceFatFast de potencia 2, 
pasa a pesar 8.1 kg y queda también solo con sinusitis.
-}

type Potencia = Number

reduceFatFast :: Potencia -> Medicamento
reduceFatFast potencia = (hierbaVerde "obesidad") . (aplicarNveces potencia alcachofa)

{-c.	Hacer la pdepCilina, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas. 
Las enfermedades infecciosas son aquellas cuyo nombre termina de alguna de estas formas (utilizar esta constante):
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]
-}

pdepCilina :: Medicamento
pdepCilina raton = foldl (flip curarEnfermedadesDelTipo) raton sufijosInfecciosas 

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

{-4.	Experimento: Los laboratorios antes de publicar un medicamento, 
lo prueban con distintos ratones para evaluar los resultados:

a.	Hacer la función que encuentra la cantidadIdeal. 
Recibe una condición y dice cuál es el primer número natural que la cumple.
> cantidadIdeal even           > cantidadIdeal (>5)
2                              6
-}

type Condicion = Number -> Bool

cantidadIdeal :: Condicion -> Number
cantidadIdeal condicion = head (filter condicion [1..])

{-b.	Saber si un medicamento lograEstabilizar una comunidad de ratones. 
Esto sucede cuando, luego de aplicarle el medicamento a todos los ratones de la comunidad, 
se elimina el sobrepeso y todos tienen menos de 3 enfermedades. Un ratón tiene sobrepeso si pesa más de 1kg.
-}

type Comunidad = [Raton]

lograEstabilizar :: Medicamento -> Comunidad -> Bool
lograEstabilizar medicamento = todosEstabilizados . (aplicoMedALaComunidad medicamento)

aplicoMedALaComunidad :: Medicamento -> Comunidad -> Comunidad
aplicoMedALaComunidad medicamento = map medicamento 

todosEstabilizados :: Comunidad -> Bool
todosEstabilizados comunidad = not (algunoConSobrepeso comunidad || algunoConMasDe3Enfermedades comunidad)

algunoConMasDe3Enfermedades :: Comunidad -> Bool
algunoConMasDe3Enfermedades comunidad = any (>=3) (cantidadesDeEnfermedades comunidad)

cantidadesDeEnfermedades :: Comunidad -> [Number]
cantidadesDeEnfermedades = map (length.enfermedades)

algunoConSobrepeso :: Comunidad -> Bool
algunoConSobrepeso comunidad = any (>1) (pesoDeRatones comunidad)

pesoDeRatones :: Comunidad -> [Number]
pesoDeRatones = map peso 

{-c.Diseñar el siguiente experimento: dado una comunidad de ratones, 
encontrar la potencia ideal del reduceFatFast necesaria para estabilizar la comunidad.
-}

potenciaIdeal :: Comunidad -> Number
potenciaIdeal comunidad = cantidadIdeal (esPotenciaIdeal comunidad)

esPotenciaIdeal :: Comunidad -> Number -> Bool
esPotenciaIdeal comunidad potencia = lograEstabilizar (reduceFatFast potencia) comunidad

-- Ejercicio 5 --

{-5.	Queremos saber si un medicamento logra estabilizar una comunidad infinita. 
¿Podemos saberlo? Responder en estos dos casos:
a.	Si todos los ratones quedan con menos de 1kg y sin enfermedades. Justificar.

En este caso, no es posible determinarlo mediante el experimento lograEstabilizar, porque
no termina de evaluar nunca la comunidad infinita, dado que debe aplicarles el medicamento
a todos los individuos para luego evaluar si estan estabilizados, por lo tanto no pasa de
ese primer paso.

b.	Si un ratón queda con 2kg y 4 enfermedades. Justificar

En este caso si se puede determinar, dado que haskell trabaja con evaluación diferida (Lazy 
Evaluation) que significa que no necesita evaluar la lista infinita de ratones para determinar 
que no se cumple con la estabilización. Con encontrar el raton que queda con 2 kg y 4 enfermedades
alcaza para saber que la comunidad no queda estabilizada. Se agrega test de ejemplo, que funciona
sin problemas.
.-}

-- Ratones y comunidad infinita modelados para usar en ejemplos y en el test

comunidadInfinita :: Comunidad
comunidadInfinita = raton1 : (repeat raton2)

raton1 =  UnRaton {
nombre = "Jerry"
, edad = 4
, peso = 2
, enfermedades = ["rinitis", "alta obesidad", "pancrealisis", "sinusitis", "estafilococos", "glucemia", "conjuntivitis"]
}

raton2 =  UnRaton {
nombre = "Stuart"
, edad = 4
, peso = 2
, enfermedades = ["alta obesidad", "sinusitis", "rinitis"]
}
