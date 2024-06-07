# Proyecto Final : Haskell + MiniSAT

| Alumnos                     | No. de Cuenta |
| --------------------------- | ------------- |
| Eulogio Sánchez Christian   | 320196872     |
| López García Luis Norberto  | 423092075     |
| Paredes Zamudio Luis Daniel | 318159926     |

# Dependencias

- Haskell 9.x> (Escrito en 9.4.8)
- Cabal 3.10.x> (Escrito en 3.10.2.1)
- MiniSAT 0.1

## Proceso de Instalación

Suponiendo un entorno Debian, Ubuntu o derivados, Haskell se instala de la siguiente manera:

```bash
sudo apt install ghc cabal-install
```

Para entornos Fedora, se realiza lo siguiente:

```bash
sudo apt install ghc cabal-install
```

Una vez instalados GHC y Cabal, la libreria se puede instalar con la siguientes líneas de comandos:

```bash
cabal update
cabal install --lib minisat-solver
```

> [!NOTE]
> Otra forma de instalación válida es usando `ghcup`. Las instrucciones de cómo instalarlo para
> sistemas basados en Unix y Windows vienen en la página oficial del programa. Se puede acceder 
> a ella haciendo [click aquí](https:www.haskell.org/ghcup/).
> Una vez instalado, se pueden verificar las instalaciones ejecutando el comando `ghcup tui`,
> el cual abrirá una interfaz de terminal para mostrar las versiones de lo que se instalo.
> Las dos palomitas verdes indican versión instalada y versión usando al momento.

Para verificar la correcta instalación de los programas y herramientas, se ejecuta el comando:

```bash
ghci GraphCLI.hs
```

Y se espera ver un resultado parecido al siguiente:

```haskell
GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help
[1 of 3] Compiling Graphs           ( Graphs.hs, interpreted )
[2 of 3] Compiling GraphColoring    ( GraphColoring.hs, interpreted )
[3 of 3] Compiling GraphCLI         ( GraphCLI.hs, interpreted )
Ok, three modules loaded.
```

Si el archivo se interpreta sin problemas, se puede proceder a la ejecución del programa.

# Ejecución del Programa

Como mencionamos antes, se debe de interpretar el archivo `GraphCLI.hs`

```bash
ghci GraphCLI.hs
```

Una vez se ejecute, se debe de escribir `main` para entrar al menu de ejecución. Una vez
en el se irán mostrando en pantalla los comandos con los cuales se puede interactuar con el programa.

Un ejemplo de la ejecución del programa es la siguiente:

```haskell
ghci> main

==== Proyecto Final A: Haskell + MiniSAT ====
 Proyecto realizado por los alumnos
 - Paredes Zamudio Luis Daniel
 - López García Luis Norberto
 - Eulogio Sánchez Christian

 ==== Elige una opción: ====
 ==== 1. Resolver Ejercicios ====
 ==== 2. Salir ====
Solo escribe el número, el punto no es necesario.

1

Creando una nueva gráfica...

Seleccione una opción:
1. Seleccionar / Cambiar Gráfica
2. Creación Manual: Agregar Vértice
3. Creación Manual: Agregar Arista
4. Mostrar Vecinos de un Vértice
5. Mostrar Grado de un Vértice
6. Iniciar Coloración de la Gráfica Actual
7. Salir

1

Selecciona la gráfica a usar
1. Gráfica de Petersen
2. Gráfica Moño
3. Gráfica K5
4. Gráfica 6-Ciclo
5. Grafica de Prueba
6. Regresar al menu Anterior
5

Gráfica actual: Grafica {vertices = [1,2,3,4], aristas = [(1,2),(2,3),(3,4),(4,1),(1,3)]}

Seleccione una opción:
1. Seleccionar / Cambiar Gráfica
2. Creación Manual: Agregar Vértice
3. Creación Manual: Agregar Arista
4. Mostrar Vecinos de un Vértice
5. Mostrar Grado de un Vértice
6. Iniciar Coloración de la Gráfica Actual
7. Salir

6
'
Ingrese el la cantidad de colores a verificar
4

El Patron resultante es el siguiente: en la primera entrada
se encuentra el número del vértice y después el color asignado a este.

[(1,2),(2,4),(3,3),(4,1)]
[(1,2),(2,4),(3,1),(4,3)]
[(1,2),(2,1),(3,4),(4,3)]
[(1,2),(2,3),(3,1),(4,4)]
[(1,2),(2,1),(3,3),(4,4)]
[(1,2),(2,3),(3,4),(4,1)]
[(1,4),(2,3),(3,1),(4,2)]
[(1,4),(2,1),(3,2),(4,3)]
[(1,1),(2,3),(3,4),(4,2)]
[(1,4),(2,1),(3,3),(4,2)]
[(1,1),(2,2),(3,4),(4,3)]
[(1,4),(2,2),(3,1),(4,3)]
[(1,1),(2,3),(3,2),(4,4)]
[(1,4),(2,3),(3,2),(4,1)]
[(1,1),(2,2),(3,3),(4,4)]
[(1,1),(2,4),(3,2),(4,3)]
[(1,4),(2,2),(3,3),(4,1)]
[(1,1),(2,4),(3,3),(4,2)]
[(1,3),(2,1),(3,4),(4,2)]
[(1,3),(2,1),(3,2),(4,4)]
[(1,3),(2,2),(3,1),(4,4)]
[(1,3),(2,2),(3,4),(4,1)]
[(1,3),(2,4),(3,1),(4,2)]
[(1,3),(2,4),(3,2),(4,1)]

Gráfica actual: Grafica {vertices = [1,2,3,4], aristas = [(1,2),(2,3),(3,4),(4,1),(1,3)]}

Seleccione una opción:
1. Seleccionar / Cambiar Gráfica
2. Creación Manual: Agregar Vértice
3. Creación Manual: Agregar Arista
4. Mostrar Vecinos de un Vértice
5. Mostrar Grado de un Vértice
6. Iniciar Coloración de la Gráfica Actual
7. Salir

7

Gráfica final:
Grafica {vertices = [1,2,3,4], aristas = [(1,2),(2,3),(3,4),(4,1),(1,3)]}
ghci>
```

Para salir del entorno de ejecución `ghci`, se ejecuta `:q`

# Aclaración sobre el Menu de Cambio de Gráficas

Las gráficas que se encuentran seleccionables en el menú `Seleccionar / Cambiar Gráfica`, se pueden
ver visualmente en la carpeta [images](images/) para un mejor entendimiento.

- [Grafica de Petersen](images/graficaPetersen.png)
- [Grafica K5](images/graficaK5.png)
- [Grafica Moño](images/graficaMono.png)
- [Grafica 6-Ciclo](images/graficaCiclo6.png)
- [Grafica de Prueba](images/graficaPrueba.png)
