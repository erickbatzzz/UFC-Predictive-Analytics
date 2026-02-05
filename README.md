UFC Predictive Analytics: Machine Learning para el Análisis de combates

Este proyecto implementa un modelo de Regresión Logística Binomial desarrollado en R para predecir la probabilidad de victoria en peleas de la UFC.
El modelo se basa en el cálculo de diferenciales de rendimiento de los peleadores, permitiendo identificar ineficiencias en los momios (odds) ofrecidos por los casinos

Para garantizar la reproducibilidad de los resultados y que cualquier analista pueda validar las predicciones, se implementó una semilla aleatoria (set.seed(123)). 
Esto asegura que los diferenciales de rendimiento se procesen de manera consistente.

Casos de éxito:

Justin  Gaethje vs Paddy Pimblett en UFC 324 (24/01/2026):

Predicción del Modelo: Gaethje 72.60% | Pimblett 27.40%
Análisis: Mientras el mercado y el hype favorecían a Pimblett, el modelo detectó una superioridad técnica abrumadora en los diferenciales de golpeo y durabilidad de Gaethje.
Resultado: Victoria para Justin Gaethje ✅ 

Alexander Volkanovski vs Diego Lopes en UFC 325 (31/01/2026)

Predicción del Modelo: Volkanovski 55.46% | Lopes 44.54%
Análisis: El modelo identificó una pelea mucho más cerrada de lo que el público esperaba, pero mantuvo la ventaja para el ex-campeón basada en su volumen de golpeo y control.
Resultado: Victoria para Alexander Volkanovski ✅.


Herramientas:

R (tidyverse, pacman, janitor, pROC, broom)

Regresión Logística Binomial (GLM)


**Nota de la version**

Luego de iterar el modelo, implementar la reproductibilidad a partir del set.seed(123) y refinar la limpieza de los datos
se pudo estimar de mejor manera el peso de cada variable. Esto estabilizó de Gaethje en un 72.60%, mostrando mejor dominio estadístico