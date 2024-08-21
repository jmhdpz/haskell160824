-- Funciones auxiliares:
cuadrado :: Float -> Float
cuadrado x = x*x
semiperimetro :: Float -> Float -> Float -> Float
semiperimetro a b c = ((a+b+c)/2)

-- 1. Distancia entre dos puntos en el plano cartesiano
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x,y)(w,z) = sqrt((cuadrado (z-y))+(cuadrado (w-x)))

-- 2. Hipotenusa de un triangulo rectangulo
hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt(cuadrado b + cuadrado h)

-- 3. Pendiente de la recta que pasa por dos puntos
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x,y)(w,z) = (z-y)/(w-x)

-- 4. Raíces de una ecuación cuadrática
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = (((-b+sqrt((cuadrado b) - 4*a*c))/2*a),(((-b-sqrt((cuadrado b) - 4*a*c))/2*a))) 

-- 5. Área de un triángulo por medio de la fórmula de Herón
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt((semiperimetro a b c)*(semiperimetro a b c-a)*(semiperimetro a b c-b)*(semiperimetro a b c-c))

-- 6. Función comparador
comparador :: Int -> Int -> Int
comparador x y = if x==y
                   then 0
                 else if x>y
                      then 1
                 else -1

-- 7. Máximo entre tres numeros:
maximo :: Int -> Int -> Int -> Int
maximo x y z = if x>=y && y>=z
                  then x
               else if x>=z && z>=y
                  then x
               else if y>=x && x>=z
                  then y
               else if y>=z && z>=x
                  then y
               else if z>=x && x>=y
                  then z
               else if z>=y && y>=x
                  then z
               else 0
--  8. Números ordenados de forma descendente
esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente w x y z = if w>=x && x>=y && y>=z
                        then True
                        else False
