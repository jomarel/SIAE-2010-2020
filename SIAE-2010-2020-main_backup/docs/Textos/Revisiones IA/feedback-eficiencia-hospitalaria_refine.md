# Eficiencia hospitalaria, estructura organizativa y teoría de agencia: Un análisis de frontera estocástica del sistema hospitalario español

**Date**: 25/3/2026, 18:02:30
**Domain**: social_sciences/economics
**Taxonomy**: academic/thesis
**Filter**: Active comments

---

## Overall Feedback

A continuación presento algunas observaciones sobre el manuscrito.

**Traducción empírica de la descentralización**

El Capítulo 2 define la descentralización mediante un mecanismo teórico muy preciso, ilustrado en la Figura 2.2, donde el principal contrata solo con el hospital y este diseña con plena autonomía el contrato del médico. Sin embargo, la construcción empírica de $D^{desc}$ en las Secciones 4.6 y 4.8.3 agrupa entidades con naturalezas institucionales muy diversas, incluyendo mutuas, organizaciones religiosas, fundaciones públicas autonómicas, centros concertados y sociedades mercantiles. La propia Sección 1.4.2 identifica a las concesiones como el caso empíricamente más limpio de estructura descentralizada, lo que sugiere que buena parte del agregado utilizado en la estimación convive con dinámicas alejadas del mecanismo teórico estricto.

Además, la Observación 3.1 reconoce que la variable $pct^{SNS}$ cambia de significado según el tipo de hospital, y la Sección 5.3 admite que $ShareQ$ puede resultar una aproximación ruidosa del parámetro $\psi_{12}$. El tratamiento simultáneo de estas variables en las Secciones 1.3, 3.1 y 8.2 genera una expectativa de contrastación empírica directa del modelo de agencia. Resulta pertinente reconsiderar si el documento gana solidez al presentar el ejercicio empírico no como un test directo y definitivo del modelo, sino como una aproximación observacional estructurada por la teoría.


**Separación entre ineficiencia y heterogeneidad persistente**

La Sección 1.5.2 argumenta que la disponibilidad del panel permite aislar la heterogeneidad permanente de la ineficiencia transitoria. No obstante, las Secciones 3.7.2 y 8.4.4 reconocen que la modelización con base en Greene (2005), la cual incluye un componente hospitalario persistente, queda relegada a trabajo futuro. La especificación que verdaderamente se estima es la de Battese-Coelli con media heterogénea, carente de un parámetro $\alpha_i$ que absorba las diferencias estructurales subyacentes entre centros.

Esta decisión metodológica requiere un manejo cuidadoso en la interpretación. De acuerdo con las Tablas 4.2, 4.3 y C.1, junto con lo expuesto en la Sección 6.5.4, los hospitales descentralizados operan en entornos institucionales distintos, exhiben una concentración geográfica notable y son sistemáticamente de menor tamaño. Ante la posibilidad de que esta heterogeneidad permanente se esté filtrando en el término $\mu_{it}$, los coeficientes asociados a las predicciones P1–P4 pueden estar capturando factores estructurales no observados en lugar de diferencias puras de eficiencia técnica.


**Imputación de pesos y selección muestral**

Un aspecto estructural de la base de datos atañe a la conformación de los outputs principales. La Sección 4.1.3 detalla que los pesos GRD observados abarcan únicamente el período 2010-2015 para 246 hospitales, representando aproximadamente el 31 % del panel por año, con un marcado peso de hospitales grandes del SNS. La imputación del resto de las observaciones mediante un Random Forest entrenado sobre esta submuestra proyecta la estructura de complejidad poblacional de un segmento particular hacia el resto.

El Anexo B.6.2 indica la existencia de una mayor varianza de error para hospitales pequeños y especializados, que es exactamente donde la distribución entre centralizados y descentralizados resulta más heterogénea. Paralelamente, la Tabla 4.1 y la Sección 4.8.2 muestran que el índice $i_{diag}$ presenta un 23,4 % de valores ausentes, excluyendo preferentemente centros pequeños y generando un sesgo en el Diseño A hacia hospitales de mayor tamaño y complejidad. Un ejercicio analítico valioso consistiría en demostrar formalmente la supervivencia de los hallazgos nucleares dentro de la submuestra que cuenta con case-mix observado, aislando así el armazón empírico de posibles sesgos inducidos por la imputación y la selección.


**Interpretación de la predicción P2**

La formulación teórica de P2, presentada en la Sección 2.7 y resumida en el Cuadro 2.1, establece una predicción inequívoca sobre el signo del parámetro: la descentralización debe incrementar la ineficiencia en intensidad, exigiendo $\delta_1 > 0$. Los resultados empíricos expuestos en la Tabla 5.3 calculan un valor de $\delta_1 = -1.477$, signo negativo que se mantiene inalterado en todas las iteraciones de robustez recogidas en la Tabla 6.2.

La exposición de este hallazgo en las Secciones 5.3, 6.4 y 8.1 lo concibe como una confirmación "en sentido débil", conduciendo a la aseveración general del Capítulo 8.3 de que la evidencia resulta consistente con las predicciones del modelo. Desde una perspectiva analítica, este encaje ex post genera un quiebre en la lógica del trabajo. El manuscrito fundamenta con claridad una asimetría entre las distintas dimensiones productivas, pero catalogar un coeficiente con signo inverso al planteado como una confirmación debilita el rigor de la contribución. El documento se vería reforzado si presentara este resultado como una tensión estimulante entre la estructura predictiva y el comportamiento real del sistema hospitalario español.


**Alcance empírico de las implicaciones de política**

Los Capítulos 7 y 8 articulan recomendaciones institucionales y normativas precisas respecto al sistema sanitario. Estas disposiciones contemplan sugerencias que van desde advertencias sobre reversión de concesiones o la generalización del pago por actividad, hasta propuestas como la de vincular un margen del 5-10 % de la financiación a indicadores concretos y la introducción de auditorías específicas.

De forma paralela, las Secciones 6.6, 7.5, 7.6 y 8.4.1 constatan abiertamente la endogeneidad de la variable $D^{desc}$, detallan la ausencia de un marco de identificación causal sólido, asumen la omisión de medidas de calidad clínica en sentido estricto y proponen la necesidad futura de desarrollar diseños cuasiexperimentales. Para mantener la coherencia y proporcionalidad intelectual de la tesis, resulta imprescindible que el desarrollo normativo esté explícitamente contenido dentro del perímetro de los datos disponibles. Formular estas reflexiones como hipótesis directrices para la futura toma de decisiones mantendrá intacta la integridad analítica del diseño observacional empleado.

**Status**: [Pending]

---

## Detailed Comments (4)

### 1. Fórmula de aproximación incorrecta para el efecto marginal

**Status**: [Pending]

**Quote**:
> La relación entre la media de $u_{it}$ y la media de $TE_{it}$ es no lineal, pero una aproximación de primer orden sobre el punto de referencia sugiere que $\Delta TE \approx -\hat{\alpha}_1 \cdot \mathrm{E}\left[u_{it}\right] \cdot TE_{it}^{ref}$ para valores pequeños de $\hat{\alpha}_1$. Con $\hat{\alpha}_1=-2.530$ y los valores medios del punto de referencia, la diferencia implicada de eficiencia es de orden 4-6 puntos porcentuales

**Feedback**:
En la sección 5.7.1, se propone la aproximación $\Delta TE \approx -\hat{\alpha}_1 \cdot \mathrm{E}\left[u_{it}\right] \cdot TE_{it}^{ref}$ para calcular el efecto marginal temporal. Sin embargo, al evaluar esta fórmula con aproximaciones fieles a los propios valores indicados en el documento ($\hat{\alpha}_1 = -2.530$, $\mathrm{E}[u_{it}] \approx 0.29$, $TE^{ref} \approx 0.748$), el resultado asciende a una cifra insólita superior a los 50 puntos porcentuales. Esto entra en contradicción directa con la aseveración adyacente en esa misma frase donde se estima la diferencia real en 4-6%. Dado que el coeficiente estimado $\hat{\alpha}_1$ no es ni mucho menos asimilable a un valor pequeño para permitir expansiones lineales estables, se sugiere prescindir enteramente de la fórmula de aproximación en este apartado y sustituirla remitiendo directamente a la rigurosa estimación no lineal de eficiencias esperadas provista en la sección 6.5.1.

---

### 2. Contradicción sobre el uso de efectos fijos en el Diseño C

**Status**: [Pending]

**Quote**:
> La identificación intra-hospital es particularmente valiosa en este contexto porque cualquier característica fija del hospital (equipo directivo, cultura organizativa, capital físico) queda absorbida por el efecto fijo del hospital.

**Feedback**:
En la sección 5.5 se indica que las características invariantes del hospital quedan absorbidas por un 'efecto fijo del hospital'. Sin embargo, el Cuadro 5.5 reporta coeficientes para variables estructurales que no varían entre servicios del mismo nivel hospitalario (como `d_Priv_Conc`, `d_Priv_Merc`). En un modelo estricto de efectos fijos intra-hospitalarios, estas variables quedarían omitidas automáticamente por colinealidad perfecta. Además, en la sección 3.7.2 se argumentó explícitamente la preferencia por estimadores de efectos aleatorios sobre los de efectos fijos. Convendría corregir la redacción en la sección 5.5 para reflejar con precisión que se explota la variación intra-hospitalaria mediante los términos de interacción (efecto de la descentralización según el tipo de servicio), evitando usar el término estricto 'efecto fijo' porque induce a pensar en una abstracción econométrica que es matemáticamente incongruente con los resultados del cuadro.

---

### 3. Error tipográfico en la fórmula de D^D

**Status**: [Pending]

**Quote**:
> \begin{aligned}
B^{D} & =\frac{b\left(1+\sigma_{1}^{2} \rho \psi_{1}+\sigma_{2}^{2} \theta\left(\rho \delta \sigma_{1}^{2}+\psi_{2}\right)\right)-d \eta \sigma_{2}^{2} \psi_{12}}{1+\psi_{1} \theta \sigma_{1}^{2}+\theta \sigma_{2}^{2}\left(\theta \delta \sigma_{1}^{2}+\psi_{2}\right)} \\
D^{D} & =\frac{d\left(1+\sigma_{2}^{2} \rho \psi_{2}+\sigma_{1}^{2} \theta\left(\rho \delta \sigma_{2}^{2}+\psi_{1}\right)\right)-b \eta \sigma_{2}^{2} \psi_{12}}{1+\psi_{2} \theta \sigma_{2}^{2}+\theta \sigma_{1}^{2}\left(\theta \delta \sigma_{2}^{2}+\psi_{1}\right)}
\end{aligned}

**Feedback**:
En la subsección 2.5.1, hay un error tipográfico en el numerador del pago variable óptimo $D^D$. Por simetría con la fórmula de $B^D$ y resolviendo analíticamente el sistema, el último término en el numerador debería estar ponderado por $\sigma_1^2$ en lugar de $\sigma_2^2$, quedando como $-b \eta \sigma_1^2 \psi_{12}$. Sugiero corregir el subíndice para reflejar adecuadamente la varianza del otro output.

---

### 4. Notación del equivalente cierto

**Status**: [Pending]

**Quote**:
> \operatorname{máx}_{a, B, D} & \mathbb{E}_{\epsilon}\left[u_{h}\right]=f+(b-B)\left(a+t_{1}\right)+(d-D) t_{2}+z_{1} a-M

**Feedback**:
En la subsección 2.5.1, la función objetivo del hospital se denota matemáticamente como la utilidad esperada $\mathbb{E}_{\epsilon}[u_h]$, pero la expresión algebraica a la derecha de la igualdad es en realidad su equivalente cierto. Aunque el texto principal ya aclara correctamente la intención ('el hospital maximiza su equivalente cierto eligiendo...'), sería más riguroso formalmente denotar el lado izquierdo como $CE_h$ para que las unidades algebraicas de la ecuación coincidan por completo.

---
