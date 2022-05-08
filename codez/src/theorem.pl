:-module(theorem, [theorem_lines/7
                  ]).

:-use_module(configuration).

/** <module> Format boxed theorems.

Print unnumbered theorem:
==
?- _IP = 'input/box.txt', layout:read_lines(_IP,Ls), theorem:theorem_lines(Ls,1,82,_Fs,K,Ls_,N), reverse(_Fs,_Sf), forall(member(F,_Sf),writeln(F)).
┌[Note to the reader]────────────────────────────────────────────────────────────┐
│ This Section is a short introduction to the setting, the game and the system   │
│ of Nests & Insects. It is designed to give you a general idea of the game. It  │
│ is not designed to give you everything you need to play the game. That         │
│ information is found in the text that follows this Overview Section.           │
└────────────────────────────────────────────────────────────────────────────────┘
Ls = ['\\begin*{note_reader}', 'This Section is a short introduction to the setting, the game and the system', 'of Nests & Insects. It is designed to give you a general idea of the game. It', 'is not designed to give you everything you need to play the game. That', 'information is found in the text that follows this Overview Section.', '\\end{note_reader}'],
K = 1,
Ls_ = [],
N = 6.
==

Print numbered theorem:
==
?- _IP = 'input/box.txt', layout:read_lines(_IP,Ls), theorem:theorem_lines(Ls,1,82,_Fs,K,Ls_,N), reverse(_Fs,_Sf), forall(member(F,_Sf),writeln(F)).
┌[Note to the reader 1]──────────────────────────────────────────────────────────┐
│ This Section is a short introduction to the setting, the game and the system   │
│ of Nests & Insects. It is designed to give you a general idea of the game. It  │
│ is not designed to give you everything you need to play the game. That         │
│ information is found in the text that follows this Overview Section.           │
└────────────────────────────────────────────────────────────────────────────────┘
Ls = ['\\begin{note_reader}', 'This Section is a short introduction to the setting, the game and the system', 'of Nests & Insects. It is designed to give you a general idea of the game. It', 'is not designed to give you everything you need to play the game. That', 'information is found in the text that follows this Overview Section.', '\\end{note_reader}'],
K = 2,
Ls_ = [],
N = 6.
==
*/

%!      theorem_lines(+Lines,+Count,+Width,-Format,-Count2,-Rest,-Ln)
%
%       Format lines of a theorem command as boxed text.
%
%       This predicate wraps the lines of a theorem (a rule, an example,
%       a note, etc) into an ASCII box. The theorem starts with the
%       command \theorem{T} where T is the title of the theorem, as
%       defined in the configuration, and ends with the command \end{T}.
%       The theorem may start with the command \begin*{T} in which case
%       the theorem's name is printed without numbering.
%
%       Lines is a list of atoms representing lines of text.
%
%       Count is a list of compounds c(Ti,I) where each Ti is the title
%       of a theorem and I is the current count of theorems with that
%       title.
%
%       Width is the maximum page width in columns.
%
%       Format is a formatting atom for the lines in Lines between the
%       first \begin{T} command and first \end{T} command, where T is
%       the name of a theorem, found in configuration:theorem/2.
%
%       Count2 is the list Count with the count of the occurrences of
%       the current theorem (the one we just wrapped in a box) is
%       updated by 1. If Counts did not include a compound c(T,I) with T
%       the title of the current theorem, the compound c(T,1) is added
%       to the list.
%
%       Rest is the lines remaining in Lines after the theorem's lines
%       have been processed.
%
%       Ln is the number of lines taken up by the boxed theorem.
%
theorem_lines([C|Ls],Cs,W,Fs,Cs_,Ls_,N):-
        theorem_title(C,begin,T,S,G)
        ,!
        % Offset by theorem borders
        ,W_ is W - 2
        ,theorem_count(Cs,T,S,K,Cs_)
        ,theorem_lines(1,T,K,S,G,Ls,W_,[],Fs,Ls_,N).

%!      theorem_lines(+N,+T,+K,+S,+G,+Ls,+W,+Acc,-Acc1,-Rest,-N1)
%       is det.
%
%       Business end of theorem_lines/7
%
%       Acc1 is the accumulator of formatted theorem lines.
%
%       Rest is the list of lines remaining in Ls after the theorem
%       text.
%
%       N1 is the updated line count of the theorem.
%
%       The other args are inherited from the caller.
%
theorem_lines(N,T,_K,_S,_G,[C|Ls],W,Acc,Fs,Ls,N):-
        theorem_title(C,end,T,_,_)
        ,!
        ,W_ is W + 2
        ,format(atom(F),'~|└~`─t┘~*| ',[W_])
        ,reverse([F|Acc],Fs).
theorem_lines(1,T,K,S,G,Ls,W,Acc,Bind,Ls_Bind,N_Bind):-
        !
        ,W_ is W + 2
        ,configuration:theorem(T,T_)
        ,format_string_args(W_,T_,K,S,G,FS,As)
        ,format(atom(F),FS,As)
        ,theorem_lines(2,T,K,S,G,Ls,W,[F|Acc],Bind,Ls_Bind,N_Bind).
theorem_lines(N,T,K,S,G,[L|Ls],W,Acc,Bind,Ls_Bind,N_Bind):-
        format(atom(F),'│ ~w~` t~*| │ ',[L,W])
        ,succ(N,N_)
        ,theorem_lines(N_,T,K,S,G,Ls,W,[F|Acc],Bind,Ls_Bind,N_Bind).


%!      format_string_args(?Width,?Title,?Count,?Star,?Format,?Args)
%!       is det.
%
%       Format string and arguments for theorem header.
%
%       Width: the width of the theorem box.
%
%       Title: the title of the theorem.
%
%       Count: the theorem counter.
%
%       Star: Whether the theorem is starred and takes no count.
%
%       Format: a format/2 format string to print out the theorem
%       header.
%
%       Args: the arguments to format/2 to print out the theorem header.
%
format_string_args(W,T,_K,*,'','~|┌[~w]~`─t┐~*| ',[T,W]).
format_string_args(W,T,_K,*,G,'~|┌[~w: ~w]~`─t┐~*| ',[T,G,W]).
format_string_args(W,T,K,nil,'','~|┌[~w ~w]~`─t┐~*| ',[T,K,W]).
format_string_args(W,T,K,nil,G,'~|┌[~w ~w: ~w]~`─t┐~*| ',[T,K,G,W]).


%!      theorem_title(+Text,?StarEnd,-Title,-Star,-Legend) is det.
%
%       Extract the title field from a theorem.
%
%       Text is a line of text that may or may not be the start of a
%       theorem: \begin{Title} or \begin*{Title}.
%
%       StartEnd is one of the atoms 'begin', 'end', that mark the
%       beginning or end of the theorem's text. These are set from the
%       caller to make it easier to know where we are in the process.
%
%       Star is either the atom '*' or the atom 'nil'. If S is '*', then
%       the theorem is not to be numbered.
%
%       Legend is a legend to add to the theorem header.
%
theorem_title(C,begin,T,*,L):-
        theorem_command(C,'\\begin*',T,L)
        ,!.
theorem_title(C,begin,T,nil,L):-
        theorem_command(C,'\\begin',T,L)
        ,!.
theorem_title(C,end,T,nil,L):-
        theorem_command(C,'\\end',T,L).


%!      theorem_command(+Text,?Delimiter,?Title,?Legend) is det.
%
%       Parse a theorem command to its constituents.
%
%       Delimiter, Title and Legend are all atoms.
%
theorem_command(C,D_,T_,L_):-
        atom_chars(C,Cs)
        ,once(phrase(theorem_command(D,T,L),Cs))
        ,maplist(atomic_list_concat,[D,T,L],['','',''],[D_,T_,L_])
        ,configuration:theorem(T_,_).

%!      theorem_command// is nondet.
%
%       Theorem command parser.
theorem_command(D,T,L) --> delimiter(D), title(T), legend(L).

delimiter(['\\'|D]) --> ['\\'], string(D).
title(T) --> ['{'],string(T),['}'].
legend(L) --> ['['],string(L),[']'].
legend(['']) --> ['[',']'].
legend(['']) --> [].

string([S|Ss]) --> [S], string(Ss).
string([S]) --> [S].


%!      theorem_count(+Counts,+Title,+Star,+K,-New_counts) is det.
%
%       Update the current theorem count.
%
%       Counts is a list of compounds c(Ti,I), where each Ti is the
%       title of a theorem and each I the count of occurrences of that
%       theorem, so far in the text.
%
%       Title is the title of the current theorem, the one we're
%       processing with an ASCII box frame.
%
%       Star is either the atom * or the atom nil. If Star is *, then
%       the current theorem count does not need to be updated.
%
%       K is the current count of theorems with the same Title, updated
%       by one.
%
%       New_counts is the list Counts updated with a compound
%       c(Title,K). If Counts did not include a compound c(Title,J), for
%       some J, then the compound c(T,1) is added to Counts, to start
%       the count of the current theorem.
%
theorem_count(Cs,_T,*,0,Cs):-
        !.
theorem_count(Cs,T,_S,K,[c(T,K)|Cs_]):-
        selectchk(c(T,I),Cs,Cs_)
        ,!
        ,succ(I,K).
theorem_count(Cs,T,_S,1,[c(T,1)|Cs]).
