
---
title: "LoxMocha Language Specification"
author: "Emma Tomlinson"
date: \today
documentclass: report
toc: true
toc-depth: 3
numbersections: true
secnumdepth: 4
mainfont: Open Sans
geometry: margin=1in
header-includes:
- \usepackage{fvextra}
- \DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines,commandchars=\\\{\}}
- \usepackage{xcolor}
- \usepackage{mdframed}
- \let\oldShaded\Shaded
- \let\endoldShaded\endShaded
- |
  \renewenvironment{Shaded}{%
    \begin{mdframed}[%
      backgroundcolor=black!5,%   Set background color (5% black is a light grey)
      innerleftmargin=1.5em,%      Set left margin
      innerrightmargin=1.5em,%     Set right margin
      innertopmargin=1ex,%         Set top margin
      innerbottommargin=1ex,%      Set bottom margin
      skipabove=1.5em,
      skipbelow=1.5em,
      linewidth=1pt,%              Set the border line width to 0 (no border)
      nobreak=true%                Prevent the box from breaking across pages
    ]%
  }{%
    \end{mdframed}%
  }
---

