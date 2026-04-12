# Instrucciones de compilación — VS Code + LaTeX Workshop

## Archivos incluidos
- `capitulo_estimacion_empirica.tex` — documento principal
- `referencias.bib` — bibliografía en formato BibTeX

## Requisitos
1. Distribución LaTeX: TeX Live 2022+ o MiKTeX 2023+
2. Extensión VS Code: LaTeX Workshop
   (identificador: `James-Yu.latex-workshop`)

## Compilación en VS Code

### Opción A — Compilación automática con LaTeX Workshop
1. Abrir `capitulo_estimacion_empirica.tex` en VS Code
2. LaTeX Workshop detecta el archivo .tex automáticamente
3. Pulsar Ctrl+Alt+B (o el botón ▶ en la barra lateral)
4. La secuencia recomendada es: pdflatex → bibtex →
   pdflatex → pdflatex

### Opción B — Configurar receta en settings.json
Añadir en `.vscode/settings.json`:
```json
{
  "latex-workshop.latex.recipes": [
    {
      "name": "pdflatex + bibtex",
      "tools": ["pdflatex", "bibtex", "pdflatex", "pdflatex"]
    }
  ],
  "latex-workshop.latex.tools": [
    {
      "name": "pdflatex",
      "command": "pdflatex",
      "args": ["-synctex=1", "-interaction=nonstopmode",
               "-file-line-error", "%DOC%"]
    },
    {
      "name": "bibtex",
      "command": "bibtex",
      "args": ["%DOCFILE%"]
    }
  ]
}
```

### Opción C — Compilación manual en terminal
```bash
pdflatex capitulo_estimacion_empirica
bibtex capitulo_estimacion_empirica
pdflatex capitulo_estimacion_empirica
pdflatex capitulo_estimacion_empirica
```

## Notas sobre el documento

### Tabla pendiente de completar
La Tabla 5.4 (Test P* razón de verosimilitud) tiene
celdas vacías que se completarán con los resultados
del test LR en ejecución. Buscar el comentario:
"Pendiente de incorporar resultados definitivos"

### Paquetes LaTeX necesarios
El documento usa los siguientes paquetes estándar
(incluidos en cualquier distribución completa):
- inputenc, fontenc, babel (español)
- amsmath, amssymb, amsthm
- booktabs, longtable, threeparttable
- array, multirow, rotating
- geometry, setspace
- natbib (bibliografía estilo APA)
- hyperref (enlaces en PDF)
- lmodern, microtype (tipografía)

### Para compilar sin errores en español
Asegurarse de que el archivo está guardado en UTF-8
(VS Code muestra la codificación en la barra inferior).

## Estructura del capítulo
1. Introducción y motivación empírica
2. Modelo teórico y predicciones (P1-P4)
3. Estrategia empírica (4 diseños SFA)
4. Datos y variables (SIAE, CNH, CMBD)
5. Resultados (Diseños D, A, B, C + Test P*)
6. Robustez y tests de especificación
7. Discusión e implicaciones
Referencias bibliográficas
