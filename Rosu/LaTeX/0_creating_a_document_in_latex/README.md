# Creating a document in LaTeX

## Preable of the document

```tex
\documentclass[12pt, letterpaper]{article}
\usepackage[utf8]{inputenc}

\title{First document}
\author{Razvan Rosu \thanks{thanks to Stefan Rosu}}
\date{March 2020}
```

## Title Page

```tex
% Beginning of the preamble
\documentclass[12pt, letterpaper]{article}
\usepackage[utf8]{inputenc}

\title{Comments}
\author{Razvan Rosu \thanks{thanks to the \LaTeX team}}
\date{\today}
% End of the preamble

\begin{document}

    \begin{titlepage}
        \maketitle
    \end{titlepage}

    Hello world! Here the content of the document begins.

\end{document}
```

## Comments

```tex
% Beginning of the preamble
\documentclass[12pt, letterpaper]{article}

% Packages
\usepackage[utf8]{inputenc}
\usepackage{comment}

\title{Comments}
\author{Razvan Rosu \thanks{thanks to the \LaTeX team}}
\date{\today}
% End of the preamble

\begin{document}

    \begin{titlepage}
        \maketitle
    \end{titlepage}

    This text will appear in the PDF-generated file.

    \begin{comment}
        This text will not appear in the PDF-generated file.
    \end{comment}

\end{document}
```

## Document types

Available in the ```\documentclass``` command.

| Type          | Description                                                             |
|---------------|-------------------------------------------------------------------------|
| ```article``` | For short documents and journal articles. It is the most commonly used. |
| ```report```  | For longer documents and dissertations.                                 |
| ```book```    | Useful to write books.                                                  |
| ```letter```  | For letters.                                                            |
| ```slides```  | For slides, rarely used.                                                |
| ```beamer```  | Slides in the Beamer class format.                                      |

## Reserved Characters

| Character | Function                                                                      |                                      |
|-----------|-------------------------------------------------------------------------------|--------------------------------------|
| ```#```   | Macro Parameter                                                               | ```\#```                             |
| ```$```   | Math Mode                                                                     | ```$```                              |
| ```%```   | Useful to write books.                                                        | ```\%```                             |
| ```^```   | Superscript (in math mode)                                                    | ```\^{}``` or ```\textasciicircum``` |
| ```&```   | Separate column entries in tables                                             | ```\&```                             |
| ```_```   | Subscript (in math mode)                                                      | ```\_```                             |
| ```{}```  | Processing block                                                              | ```\{\}```                           |
| ```~```   | Unbreakable space                                                             | ```\textasciitilde``` or ```\~{}```  |
| ```\```   | Starting commmands, which extend until the first non-alphanumerical character | ```\textbackslash``` or ```\```      |
