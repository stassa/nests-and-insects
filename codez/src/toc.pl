:-module(toc, [format_toc/4
              ]).

:-use_module(src(layout)).
:-use_module(configuration).
:-use_module(src(styles)).

/** <module> Generate a Table of Contents.

*/


%!      format_toc(+ToC,+Lines,+Width,-Formatted) is det.
%
%       Format the lines of a Table of Contents.
%
format_toc(Ts,N,W,Ls):-
        format_toc_(Ts,W,[],Fs)
        % One line of header and two lines of footer
        % Plus off-by-one offset
        ,N_ is N - 2
        ,style_part('\\chapter{Table of Contents}',W,_,[H,U])
        ,once(toc_lines(['',H,U|Fs],[1,1,N_,W],[],Ls_))
        ,reverse(Ls_,Ls).

%!      format_toc(+ToC,+Width,+Acc,-Formatted) is det.
%
%       Business end of format_toc/4.
%
%       Formats each line of the ToC with indentation, padding and page
%       numbers.
%
format_toc_([[_C,_S,_Sb,_SbSb]],_W,Fs,Fs):-
        !.
format_toc_([Cs,toc(R,T,P)|Ts],W,Acc,Bind):-
        configuration:toc_padding(C)
        ,atom_codes(C,[C_])
        ,toc_tabs(R,S)
        ,format(atom(F),'~t~*+~w~*t~w~*|',[S,T,C_,P,W])
        % Start each Chapter in a new paragraph.
        ,(   R == chapter
         ->  Acc_ = ['',F|Acc]
         ;   Acc_ = [F|Acc]
         )
        ,format_toc_([Cs|Ts],W,Acc_,Bind).



%!      toc_lines(+Lines,+Counts,+Acc,-Formatted) is det.
%
%       Layout the Lines of a Table of Contents with page borders.
%
%       Counts is the list [P,N,M,W] where P is the current page count,
%       N is the current line count, M is the maximum line count per
%       page and W is the page width.
%
%       Formatted is the list of lines in Lines with page layout
%       applied, i.e. with ASCII borders. How quaint.
%
toc_lines([],[P,N,M,W],Acc,Acc_):-
        !
        ,M_ is M - N
        ,layout:pad_toc(0,M_,W,Acc,Acc_1)
        ,layout:arabic_roman(P,R)
        ,layout:format_line(nil,last(R),W,Acc_1,Acc_).
toc_lines([L|Ls],[P,1,M,W],Acc,Bind):-
        !
        ,layout:format_line(L,1,W,Acc,Acc_)
        ,toc_lines(Ls,[P,2,M,W],Acc_,Bind).
toc_lines(Ls,[P,M,M,W],Acc,Bind):-
        !
        ,layout:arabic_roman(P,R)
        ,layout:format_line(nil,last(R),W,Acc,Acc_)
        ,succ(P,P_)
        ,toc_lines(Ls,[P_,1,M,W],Acc_,Bind).
toc_lines([L|Ls],[P,N,M,W],Acc,Bind):-
        layout:format_line(L,N,W,Acc,Acc_)
        ,succ(N,N_)
        ,toc_lines(Ls,[P,N_,M,W],Acc_,Bind).
