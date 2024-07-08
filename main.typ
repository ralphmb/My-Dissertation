#import "./other/template.typ": getHeader

#let my_name = "Ralph Braithwaite"
#let thesis_title = "Modelling Football Data"
#let thesis_domain = "Mathematics and Statistics"
#let my_institution = "Coventry University"

#set document(author: my_name, title: thesis_title)
#set text(font: "New Computer Modern", lang: "en")
//#show math.equation: set text(weight: 300, size: 9pt)
#set heading(numbering: "1.1")
#set par(justify: true)

#v(0.25fr)
#align(center)[
  #text(2em, weight: 700, thesis_title)
]

#pad(
  top: 0.7em,
  grid(
    columns: (1fr),
    gutter: 1em,
    align(center)[
      *#my_name* \
      #thesis_domain \
      #my_institution \
    ]
  ),
)




// Table of contents.
#outline(depth: 3, indent: true)
#pagebreak()


// Main body.
#set page(numbering: "1", number-align: center)
#set page(header: getHeader(),
          margin: (x:30pt, y:50pt))
#counter(page).update(1)//

// Highlight first rows and columns in tables
#set table(
  fill: (x, y) =>
    if x == 0 or y == 0 {
      gray.lighten(40%)
    },
  align: right,
)

#show table.cell.where(x: 0): strong
#show table.cell.where(y: 0): strong

// And here's where we include all the sections.
= Introduction
#include("./sections/introduction.typ")


#pagebreak(weak: true)

#set par(first-line-indent: 20pt)
#set page(columns: 2)

= Review of Literature
#include("./sections/litreview.typ")

//#pagebreak(weak: true)
= Preliminary Steps

== Data Collection
#include("./sections/datacollection.typ")

== Exploratory Analysis
#include("./sections/exploratory.typ")

//#pagebreak(weak: true)
= Logistic Regression
#include("./sections/regression.typ")

//#pagebreak(weak: true)
= Survival Analysis
#include("./sections/survival.typ")



// Bibliography section
#set page(header: [])
#set text(8pt) // Setting smaller text sizes to save paper
#bibliography("./sections/bibliography.bib")

#pagebreak(weak: true)
#set page(columns: 1)
#set text(7pt)
#heading([Appendices], numbering:none)
#include("./sections/appendix.typ")