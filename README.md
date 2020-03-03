# DMVVM-ES

![logo del grupo](logo-grupo.png)

Ejercicios de práctica para tratar de comprender el patrón **MVVM** y quizás terminar con el desarrollo de un pequeño y útil **Framework MVVM** para Delphi.

## Otros frameworks utilizados

* Spring4D
* DUnitX

## Equipo de trabajo

Somos un pequeño grupo de programadores que nos juntamos por Telegram (grupo *Delphi MVVM en Español*) y allí compartimos nuestras ideas y dudas

***

### Prototipo 0

Objetivos en esta práctica:
1. El objetivo no es crear unos modelo-viewmodel-vista maravillosos, por lo que puede no tener excesivo sentido el ejemplo en si mismo
2. El core MVVM debe ser **agnóstico** de la plataforma (vcl/fmx)
3. Debe haber **servicios (UI)** propios a la plataforma, que pueden ser invocados desde cualquiera de los niveles (lo normal desde el VM), se ha creado un servicio sencillo
4. Se ha hecho el ejercicio de crear unos **test unitarios** para el modelo y el viewmodel-vista
5. Se ha creado una vista específica para FMX y otra para VCL, así como una app FMX y otra VCL para probar la funcionalidad
6. En este caso concreto el viewmodel tiene unas funcionalidades similares al propio modelo, vamos que casi hace de puente, por lo que los propios test unitarios son similares



