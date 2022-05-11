Hclear *
" Chapter, section etc document parts
Highlight 88 \\\(chapter\|section\|subsection\|subsubsection\|paragraph\).*
" \begin{T} and \end{T} theorem tags
Highlight 95 \\\(begin\|end\)\**{.\{-}}\(\[.\+\]\)*
" \label{T}[R] tags
Highlight 37 \\label.*
" Table references and tag.
Highlight 4 Table \d\+
Highlight 7 \(Table \d\+:\)\|\({table}\)
" Figugre references and tags.
Highlight 5 Figure \d\+
Highlight 8 Figure \d\+:
" Example references and tags.
Highlight 9 Example \d\+
Highlight 6 \(Example \d\+:\)\|\({example}\)
" And for rules summaries
Highlight 54 Rules Summary \d\+
Highlight 51 \(Rules Summary \d\+: [a-zA-Z0-9 ]\+\)\|\({rules_summary}\)
" _Underlined_ text, for paragraph titles
Highlight 2 _[a-zA-Z0-9 ]\+_
" \newpage tag
Highlight 98 \\newpage
" \charsheet[Class] tag... hey, shouldn't [Class] be {Class}?
Highlight 35 \\charsheet\[.\+\]
" Multi-line or single-line comment
Highlight 97 \(\/\*\(\_.\{-1,}\)\*\/\)\|\(^%.\+\)
" d20, d40 etc.
Highlight 22 d\d\+
" Percentile rating
Highlight 23 \d\+%
" duh
Highlight 16 fd100
