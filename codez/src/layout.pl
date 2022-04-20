:-module(layout, [layout_rulebook/3
                 ,read_lines/2
                 ,format_lines/3
                 ]).

/** <module> Layout and formatting for text-based rulebooks.
*/

%!      border(?Position,?Characters) is semidet.
%
%       A portion of a border, footer, or header.
%
%       @tbd This should be in the configuration module.
%
border(upper_left_corner,'╔►Nests and Insects◄').
border(upper_right_corner,'►Rulebook◄═╗').
border(horizontal,'═').
border(vertical,'║').
border(lower_left_corner,'╚').
border(lower_right_corner,'╝').
border(horizontal_shadow,'▀').
border(vertical_shadow,'▓').


%!      layout_rulebook(+In_Path,+Page_Lines,+Out_Path) is det.
%
%       Layout a rulebook.
%
layout_rulebook(IP,N,OP):-
        read_lines(IP,Ls)
        ,format_lines(Ls,N,Fs)
        ,O = open(OP,write,S,[alias(output_file)
                             ,close_on_abort(true)
                             ,encoding(utf8)
                          ])
        ,R = forall(member(F,Fs)
                   ,writeln(S,F)
                   )
        ,C = close(S)
        ,setup_call_cleanup(O,R,C).


%!      format_lines(+Lines,+Page_Lines,-Formatted) is det.
%
%       Formats a list of text Lines for a text-based rulebook.
%
%       Page_Lines is an integer, the number of lines in one page of a
%       rulebook, complete with borders and other layout.
%
%       Formatted is the lines of text in Lines formatted with a border
%       and a header and footer.
%
format_lines(Ls,N,Fs):-
        text_width(Ls,W)
        % One line of header and two lines of footer
        % Plus off-by-one offset
        ,N_ is N - 2
        ,format_lines(Ls,1,1,N_,W,[],Fs).

%!      format_lines(+Lines,+Pages,+Count,+Page,+Acc,-Formatted) is det.
%
%       Business end of format_lines/2.
%
%       Pages: current pages count.
%
%       Count: current line count.
%
%       Page: number of lines per page.
%
format_lines([],P,_N,_M,W,Acc,Fs):-
        format_line(nil,last(P),W,Acc,Acc_)
        ,reverse(Acc_,Fs)
        ,!.
format_lines([L|Ls],P,M,M,W,Acc,Bind):-
        !
        ,format_line(nil,last(P),W,Acc,Acc_)
        ,succ(P,P_)
        ,format_lines([L|Ls],P_,1,M,W,Acc_,Bind).
format_lines([L|Ls],P,N,M,W,Acc,Bind):-
        format_line(L,N,W,Acc,Acc_)
        ,succ(N,N_)
        ,format_lines(Ls,P,N_,M,W,Acc_,Bind).


%!      format_line(+Line,+Lnum,+Width,+Acc,-New) is det.
%
%       Format one line of text with borders
%
%       Line is a line of text, pre-formatted to Width characters.
%
%       Lnum is the index of Line in the list of lines of text.
%       Alternatively, Lnum may be the term 'last(N)' which means we're
%       at the last line of the N'th page in the text.
%
%       Width is the number of columns in the line.
%
%       Acc is the accumulator of formated lines.
%
%       New is Acc with one or two lines, depending on the value of
%       Lnum.
%
%       If Lnum is 1, a header line is added to Acc, before Line.
%
%       If lnum is the term last(N), then Line is the atom 'nil' and the
%       addition to Acc is the two lines of the footer, the first of
%       which has N in the middle as the page count.
%
format_line(L,1,W,Acc,[L_,F|Acc]):-
% Process first line
        !
        ,border(upper_left_corner,ULC)
        ,border(upper_right_corner,URC)
        ,border(horizontal,HOR)
        % Width of the line plus space plus borders
        ,W_ is (W + 4)
        ,atom_codes(HOR,[C])
        ,format(atom(F),'~|~w~*t~w~*|',[ULC,C,URC,W_])
        ,format_line(L,nil,W,[],[L_]).
format_line(nil,last(P),W,Acc,[F2,F1|Acc]):-
% Process last line
        !
        ,border(lower_left_corner,ULC)
        ,border(lower_right_corner,URC)
        ,border(horizontal,HOR)
        ,border(vertical_shadow,SHA_V)
        ,border(horizontal_shadow,SHA_H)
        % Width of the line plus space plus borders
        ,W1 is (W + 4)
        % Extra space at the start of the footer shadow
        ,W2 is (W + 5)
        ,atom_codes(HOR,[C1])
        ,atom_codes(SHA_H,[C2])
        ,format(atom(F1),'~|~w~*t-~w-~*t~w~*|~w',[ULC,C1,P,C1,URC,W1,SHA_V])
        ,format(atom(F2),' ~|~*t~*|',[C2,W2]).
format_line(L,_N,W,Acc,[F|Acc]):-
% Process all other lines
        border(vertical,VER)
        ,border(vertical_shadow,SHA)
        % Width of the line plus borders, minus space
        ,W_ is W + 3
        ,format(atom(F),'~w ~w~` t~*|~w~w',[VER,L,W_,VER,SHA])
        %                  ^ Here's the space!
        .



%!      text_width(+Lines,-Width) is det.
%
%       Width is the width in column of the longest of Lines.
%
text_width(Ls,W):-
        setof(W_
             ,L^Ls^(member(L,Ls)
                   ,atom_length(L,W_)
              )
             ,Ws)
        ,reverse(Ws,[W|_]).


%!      read_lines(+File, -Lines) is det.
%
%       Read lines from a File until the end_of_file marker.
%
read_lines(F,Ls):-
        O = open(F,read,S,[alias(input_file)
                          ,close_on_abort(true)
                          ,encoding(utf8)
                      ])
        ,R = read_lines(S,[],Ls)
        ,C = close(S)
        ,setup_call_cleanup(O,R,C).

%!      read_lines(+Stream,+Acc,-Lines) is det.
%
%       Business end of read_lines/2.
%
read_lines(S,Acc,Bind):-
        read_line_to_codes(S,Cs)
        ,is_list(Cs)
        ,!
        % For developing only
        ,atom_codes(A,Cs)
        ,read_lines(S,[A|Acc],Bind).
read_lines(S,Acc,Ls):-
        read_line_to_codes(S,end_of_file)
        ,reverse(Acc,Ls).
