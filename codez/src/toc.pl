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
        % Off by one error correction
        % I DON'T KNOW!! OK?
        ,N_ is N - 2
        ,style_part('\\chapter{Table of Contents}',W,_,[H,U])
        ,once(toc_lines(['',H,U,''|Fs],[1,1,N_,W],[],Ls_))
        ,reverse(Ls_,Ls).

%!      format_toc(+ToC,+Width,+Acc,-Formatted) is det.
%
%       Business end of format_toc/4.
%
format_toc_([],_W,Fs,Fs):-
        !.
format_toc_([toc(R,T,P)|Ts],W,Acc,Bind):-
        configuration:toc_padding(C)
        ,atom_codes(C,[C_])
        ,toc_tabs(R,S)
        ,format(atom(F),'~t~*+~w~*t~w~*|',[S,T,C_,P,W])
        ,format_toc_(Ts,W,[F|Acc],Bind).


%!      toc_lines(+Lines,+Counts,+Acc,-Formatted) is det.
%
%       Layout the Lines of a Table of Contents with page borders.
%
%       Counts is the list [P,N,M,W] where P is the current page count,
%       N is the current line count, M is the maximum line count per
%       page and W is the page width.
%
%       Formatted is the list of lines in Lines with page layout
%       applied, i.e. with ASCII bordesr. How quaint.
%
toc_lines([],[P,N,M,W],Acc,Acc_):-
        !
        % Offset lines so far er plus ... one? Why?
        ,M_ is M - N + 1
        ,layout:pad_toc(1,M_,W,Acc,Acc_1)
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