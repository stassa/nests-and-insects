:-module(configuration, [styles/2
                        ,theorem/2
                        ,toc_padding/1
                        ,toc_tabs/2
                        ,label/2
                        ,line/2
                        ]).

/** <module> Configuration options for typesetting code.

*/


%!      styles(?Part,?Styles) is semidet.
%
%       A rulebook Part and its Styles.
%
styles(chapter,[center,dunderline]).
styles(section,[center,underline]).
styles(subsection,[left_align,underline]).
styles(subsubsection,[left_align]).
styles(paragraph,[capitalise]).


%!      theorem(?Title,?Name) is semidet.
%
%       A theorem command Title and its user-friendly Name.
%
theorem(note_reader,'Note to the reader').
theorem(example,'Example').
theorem(note,'Note').
theorem(rules_summary,'Rules Summary').


%!      toc_padding(?Char) is det.
%
%       The character with which to pad ToC lines.
%
toc_padding(.).


%!      toc_tabs(?Part,?Spaces) is semidet.
%
%       Number of Spaces with which to pad ToC Parts.
%
toc_tabs(chapter,0).
toc_tabs(section,2).
toc_tabs(subsection,4).
toc_tabs(subsubsection,6).
toc_tabs(paragraph,8).


%!      label(?Type,?Name) is semidet.
%
%       A caption/ label to be counted.
%
label(table, 'Table').
label(figure, 'Figure').


%!      line(?Style,?Character) is semidet.
%
%       A line to use for underlining.
%
%       Style is the underlining style, one of "underline" or
%       "dunderline".
%
%       Character is the character to use when underlining with a line
%       of the given Style.
%
line(underline,-).
line(dunderline,=).


%!      border(?Position,?Characters) is semidet.
%
%       A portion of a border, footer, or header.
%
border(upper_left_corner,'╔►Nests & Insects◄').
border(upper_right_corner,'►Rulebook◄═╗').
border(horizontal,'═').
border(vertical,'║').
border(lower_left_corner,'╚').
border(lower_right_corner,'╝').
border(horizontal_shadow,'▀').
border(vertical_shadow,'▓').
