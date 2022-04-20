:-module(layout, [layout_rulebook/3
                 ,read_lines/2
                 ,format_lines/3
                 ,longest_line/4
                 ]).

/** <module> Layout and formatting for text-based rulebooks.
*/

%!      border(?Position,?Characters) is semidet.
%
%       A portion of a border, footer, or header.
%
%       @tbd This should be in the configuration module.
%
border(upper_left_corner,'╔►Nests & Insects◄').
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

%!      format_lines(+Text,+Pages,+Lines,+Max,+Acc,-Formatted) is det.
%
%       Business end of format_lines/2.
%
%       Pages: current pages count.
%
%       Lines: current line count.
%
%       Max: number of lines per page.
%
format_lines([],P,_N,_M,W,Acc,Fs):-
% Format the last line in the entire text.
        format_line(nil,last(P),W,Acc,Acc_)
        ,reverse(Acc_,Fs)
        ,!.
format_lines([L|Ls],P,M,M,W,Acc,Bind):-
% Format the last line in the current page.
        !
        ,format_line(nil,last(P),W,Acc,Acc_)
        ,succ(P,P_)
        ,format_lines([L|Ls],P_,1,M,W,Acc_,Bind).
format_lines(['\\newpage'|Ls],P,N,M,W,Acc,Bind):-
% Fill the rest of the page with blanks.
        !
        ,M_ is M - N
        ,findall(''
               ,between(1,M_,_K)
               ,Ss)
        ,append(Ss,Ls,Ls_)
        ,succ(P,P_)
        ,format_lines(Ls_,P_,N,M,W,Acc,Bind).
format_lines(['\\begin{nolayout}'|Ls],P,_N,M,W,Acc,Bind):-
% Skip inserted pages, already formatted.
        !
        ,noformat_lines(Ls,1,Acc,Acc_,Ls_,_)
        ,succ(P,P_)
        ,format_lines(Ls_,P_,1,M,W,Acc_,Bind).
format_lines([L|Ls],P,N,M,W,Acc,Bind):-
% Keep formatting lines
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


%!      noformat_lines(+Lines,Count,+Acc,-New,-Rest,-NewCount) is det.
%
%       Skip inserted lines, already formatted.
%
%       Lines is a list of lines directly following a
%       '\\begin{nolayout}' switch. These lines are assumed to be
%       already a formatted page that is to be inserted at the current
%       point in the text and should require no additional formatting.
%
%       Count is the counter of lines in the pre-formatted page so far.
%       This should begin at 1.
%
%       Acc is the accumulator of pre-formatted lines to be added to the
%       formatted text as a new page.
%
%       New is the binding variable, that will hold the lines of the
%       pre-formatted page.
%
%       Rest is the lines in Lines directly following the
%       '\end{nolayout}' switch, which means that the last line of the
%       pre-formatted page has been processed.
%
%       NewCount is the updated Count, incremented by the number of
%       lines in the inserted page.
%
%       @tbd This predicate is called by format_lines/7 to allow a
%       pre-formatted page to be inserted in the text. Currently this
%       is used to insert character sheets in the rules text. This
%       predicate is also called by text_width/3 to skip counting lines
%       of inserted pages. The latter use is necessary because inserted
%       pages already have borders whose width would otherwise be
%       counted when looking for the longest line in the text to
%       calculate where to put borders in all pages in the text.
%
noformat_lines(['\\end{nolayout}'|Ls],N,Acc,Acc,Ls,N):-
        !.
noformat_lines([L|Ls],N,Acc,Bind,Ls_Bind,N_Bind):-
        succ(N,N_)
        ,noformat_lines(Ls,N_,[L|Acc],Bind,Ls_Bind,N_Bind).



%!      text_width(+Lines,-Width) is det.
%
%       Width is the width in column of the longest of Lines.
%
text_width(Ls,W):-
        text_width(Ls,0,W).

%!      text_width(+Lines,+Acc,-Widest) is det.
%
%       Business end of text_width/2.
%
text_width([],W,W):-
        !.
text_width(['\\begin{nolayout}'|Ls],Wi,Bind):-
% Skip inserted pages, already formated.
% See noformat_lines/6 for exaplanation.
        !
        ,noformat_lines(Ls,1,[],_,Ls_,_)
        ,text_width(Ls_,Wi,Bind).
text_width([L|Ls],Wi,Bind):-
        atom_length(L,Wj)
        ,(   Wj > Wi
         ->  Wk = Wj
         ;   Wk = Wi
         )
        ,text_width(Ls,Wk,Bind).



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


%!      longest_line(+Lines,-Line,-Index,-Width) is det.
%
%       Find the longest Line in a list of Lines.
%
%       Lines is a list of lines of text.
%
%       Line is the line in Lines with the highest width, i.e. number of
%       characters.
%
%       Index is the (1-based) index of Line in Lines.
%
%       Width is the width of Lines, in characters.
%
longest_line(Ls,L,I,W):-
        findall(Wi-I-Li
             ,(nth1(I,Ls,Li)
              ,atom_length(Li,Wi)
              )
               ,Ws)
        ,sort(1,@>,Ws,[W-I-L|_Ws]).
