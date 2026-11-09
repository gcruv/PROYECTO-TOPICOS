// Estilo de las diapositivas (16:9)
#let azul = rgb("#1f3b70")
#let acento = rgb("#d9822b")
#let gris = rgb("#5f6670")
#let fondo = rgb("#eef2f8")

// Una diapositiva: seccion (opcional), titulo, linea y cuerpo
#let diap(titulo, seccion: none, cuerpo) = {
  pagebreak(weak: true)
  set par(justify: false, leading: 0.55em, spacing: 0.7em)
  set list(spacing: 0.75em, marker: text(fill: acento)[▪])
  if seccion != none {
    text(size: 11pt, fill: gris, weight: "bold", upper(seccion))
    v(-0.5em)
  }
  text(size: 23pt, weight: "bold", fill: azul, titulo)
  v(-0.45em)
  line(length: 100%, stroke: 1.6pt + acento)
  v(0.25em)
  cuerpo
}

// Mensaje clave al pie de la diapositiva
#let msg(cuerpo) = block(fill: fondo, inset: (x: 12pt, y: 9pt), radius: 4pt, width: 100%,
  text(fill: azul, weight: "bold", size: 15pt, cuerpo))

// Texto pequeno (notas, fuentes)
#let nota(cuerpo) = text(size: 11pt, fill: gris, cuerpo)

#set table(stroke: none, inset: (x: 7pt, y: 4.5pt))
#show table: set text(size: 13.5pt)
