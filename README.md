# DMVVM-ES

![logo del grupo](logo-grupo.png)

Ejercicios de práctica para tratar de comprender el patrón **MVVM** y quizás terminar con el desarrollo de un pequeño y útil **Framework MVVM** para Delphi.

## Otros frameworks utilizados

* Spring4D
* DUnitX

## Equipo de trabajo

Somos un pequeño grupo de programadores que nos juntamos por Telegram (grupo *Delphi MVVM en Español*) y allí compartimos nuestras ideas y dudas

## Links de interes

https://www.youtube.com/watch?v=Ci1HP8ZBJxk

https://blog.grijjy.com/2018/01/22/mvvm-starter-kit-part-1-of-3/
https://github.com/grijjy/MvvmStarterKit

https://bitbucket.org/sglienke/knockoff/src/master/
https://delphisorcery.blogspot.com/2015/06/anonymous-method-overloading.html

https://bitbucket.org/sglienke/dsharp/src/master/
https://github.com/jpluimers/Conferences/blob/master/2015/20151020-DAPUG-Denmark-Nyborg-Hotel-Hesselet/20151020-Caliburn/01-Intro-MVVM-DSharp-Caliburn/MVVM-in-Delphi-using-the-Caliburn-Micro-for-Delphi-framework.md
https://delphisorcery.blogspot.com/2011/12/putting-pieces-together-dsharp.html

https://github.com/msnts/glue

https://github.com/bogdanpolak/command-delphi

## Ideas a tener en cuenta

* Motor permite el registro de distintos tipos de estrategias/métodos de binding

***

### Prototipo 0 (finalizado)

Objetivos:
1. El objetivo no es crear unos modelo-viewmodel-vista maravillosos, por lo que puede no tener excesivo sentido el ejemplo en si mismo
2. El core MVVM debe ser **agnóstico** de la plataforma (vcl/fmx)
3. Debe haber **servicios (UI)** propios a la plataforma, que pueden ser invocados desde cualquiera de los niveles (lo normal desde el VM), se ha creado un servicio sencillo
4. Se ha hecho el ejercicio de crear unos **test unitarios** para el modelo y el viewmodel-vista
5. Se ha creado una vista específica para FMX y otra para VCL, así como una app FMX y otra VCL para probar la funcionalidad
6. En este caso concreto el viewmodel tiene unas funcionalidades similares al propio modelo, vamos que casi hace de puente, por lo que los propios test unitarios son similares
7. A un modelo puede estar linkado un viewmodel (o varios), y este viewmodel puede dar soporte a tantas vistas como se le enganchen
8. La parte del view no está trabajada

### Prototipo 1 (en curso, aún los fuentes visibles son del prototipo.0)

Objetivos:
1. Añadir Motor de Mensajes al sistema: analizar si es mejor que el patrón observer para el modelo
2. Análisis de bindings del motor de grijjy/dsharp/knockoff/Glue para ver qué ideas buenas pueden incorporarse
	a. knockoff: no hay clases especializadas ni interfaces a utilizar en cualquiera de las 3 capas, se utilizan atributos para el binding. Complejo de realizar
	b. glue: parece incompleto, sin ejemplos completos
	c. dsharp: aún no analizado
	d. grijjy: parece el más completo y sencillo de entender para aplicarlo como referencia. Se echan en falta más ejemplos
3. Integrar al core la posibilidad de distintas estrategias de binding, que se puedan registrar
4. Integrar ideas del framework de grijjy (por suerte ellos en su blog dan permiso a utilizar su framework para elaborar otros)

### Prototipo 2

Objetivos:
1. Crear cliente REST (por generalidad clientes externos a la app) y ver encaje al modelo, qué se puede automatizar, qué no, dificultades,...
2. App tethering: encaja?

