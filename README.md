---
title: Laboratorio de Funcional
author: acá van sus nombres
---
La consigna del laboratorio está en https://tinyurl.com/funcional-2024-famaf

# 1. Tareas
Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.
- [ ] Haskell instalado y testeos provistos funcionando. (En Install.md están las instrucciones para instalar.)

## 1.1. Lenguaje
- [ ] Módulo `Dibujo.hs` con el tipo `Dibujo` y combinadores. Puntos 1 a 3 de la consigna.
- [ ] Definición de funciones (esquemas) para la manipulación de dibujos.
- [ ] Módulo `Pred.hs`. Punto extra si definen predicados para transformaciones innecesarias (por ejemplo, espejar dos veces es la identidad).

## 1.2. Interpretación geométrica
- [ ] Módulo `Interp.hs`.

## 1.3. Expresión artística (Utilizar el lenguaje)
- [ ] El dibujo de `Dibujos/Feo.hs` se ve lindo.
- [ ] Módulo `Dibujos/Grilla.hs`.
- [ ] Módulo `Dibujos/Escher.hs`.
- [ ] Listado de dibujos en `Main.hs`.

## 1.4 Tests
- [ ] Tests para `Dibujo.hs`.
- [ ] Tests para `Pred.hs`.

# 2. Experiencia
Completar

# 3. Preguntas
Al responder tranformar cada pregunta en una subsección para que sea más fácil de leer.

1. ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.
2. ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?
3. ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?
4. ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?

# 4. Extras
Completar si hacen algo.