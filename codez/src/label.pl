:-module(label, [label_lines/4
                ]).

:-use_module(configuration).

/** <module> Label and count tables and figures.

*/

%label(table, 'Table').
%label(figure, 'Figure').

%!      label_lines(+Line,+Counts,-Format,-New_Counts) is det.
%
%       Execute a labelling command.
%
%       A labelling command is of the form \label{C}[T], where C is just
%       one line of text that captions a figure, or table, and T is the
%       type of the label, either 'label' or 'table'. If \label is
%       followed by a star ("starred") that means that no count should
%       be printed with the label.
%
%       The label command is partly caption and partly label. Its
%       primary function is to print the current count of a table or
%       figure, so that we don't have to do it by hand. Suffice to say
%       that it is not very compatible with LaTex at all.
%
%       Line is a line that may begin with '\\label', indicating, you
%       guessed it, a label.
%
%       Counts is a list of compounds c(Ti,I) where each Ti is a type of
%       theorem or label whose count must be updated; and each I is the
%       count of that type of theorem or label.
%
%       Format is the text of the label, formatted with the current
%       count of the label type, unless the \label format is starred.
%
%       New_Counts is the list Counts with the count of the current
%       label type updated. The counting is handled by the predicate
%       theorem_count/5 in module theorem.
%
%       @tbd One limitation of this predicate is that it only recognises
%       as a label text that does not break along newlines. To address
%       this though we'll probably need to implement text alignment in
%       the code, which is not an immediate priority. For the time being
%       it suffices to label the first line of a caption, since all we
%       want to do is number tables and figures.
%
label_lines(L,Cs,F,Cs_):-
        atom_concat('\\label',Lab,L)
        ,atom_chars(Lab,Chs)
        ,once(phrase(label(Leg,T,S),Chs))
        ,maplist(atomic_list_concat,[Leg,T,S],['','',''],[Leg_,T_,S_])
        ,theorem:theorem_count(Cs,T_,S,K,Cs_)
        ,configuration:label(T_,N)
        ,(   S_ = *
         ->  format(atom(F),'~w: ~w',[N,Leg_])
         ;   format(atom(F),'~w ~w: ~w',[N,K,Leg_])
         ).


label(L,T,S) --> star(S), ['{'], legend(L), ['}'], type(T).

star([*]) --> [*].
star([nil]) --> [].

legend(L) --> string(L).
type(C) --> ['['],string(C),[']'].

string([S|Ss]) --> [S], string(Ss).
string([S]) --> [S].
