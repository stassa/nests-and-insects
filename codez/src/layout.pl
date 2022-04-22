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
format_lines([L|Ls],P,N,M,W,Acc,Bind):-
% Execute a formatting command.
        format_command(L,Ls,[P,N,M,W],Acc,Acc_,Ls_,[P_,N_,M_,W_])
        ,!
        ,format_lines(Ls_,P_,N_,M_,W_,Acc_,Bind).
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


%!      format_command(+Command,+Lines,+Counts,+Acc,-NewAcc,-NewLines,-NewCounts)
%       is det.
%
%       Execute a formatting Command on subsequent Lines.
%
%       Command is a formatting command found in the text. Those are
%       described below.
%
%       Lines is the list of lines in the input text after the Command.
%
%       Counts is a list [P,N,M,W] where each element is a number and P
%       is the current Page count, Lines is the current line count for
%       the current page, M is the maximum number of lines per page and
%       W is the maximum width of text in a page.
%
%       Acc is the accumulator of processsed lines.
%
%       NewAcc_ is Acc updated with lines processed according to
%       Command.
%
%       NewLines is the lines remaining in Lines after Command has
%       processed as many lines as it needs.
%
%       NewCounts is the list [P_,N_,M_,W_] where each element is an
%       element of Counts updated after the execution of Command.
%
%       _Formatting Commands_
%
%       * \\begin{box}[Title]: marks the beginning of a text box. Title
%       is the box title. Closing tag: \\end{box}.
%
%       * \\begin{coverpage}: marks the beginning the cover page of the
%       entire text. Closing tag: \\end{coverpage}.
%
%       * \\begin{toc}: marks the beginning of the Table of Contents.
%       Closing tag: \\end{toc}
%
%       * \\newpage: inserts a new page or completes the current page by
%       filling it with empty lines until it is M lines long. Closing
%       tag: None.
%
%       * \\begin{nolayout}: beginning of a pre-formatted page. This
%       should be exactly M lines. Closing tag: \\end{nolayout}
%
format_command(C,Ls,[P,N,M,W],Acc,Acc_,Ls_,[P,N_,M,W]):-
        atom_concat('\\begin{box}',T,C)
        ,sub_atom(T,1,_A,1,Title)
        % Offse by box borders
        ,W_ is W - 2
        ,box_lines(1,['\\begin{box}',Title|Ls],W_,Acc,Acc_,Ls_,K)
        % Offset by box header and footer.
        ,N_ is N + K - 2.
format_command('\\begin{coverpage}',Ls,[P,N,M,W],Acc,Acc_,Ls_,[P,N,M,W]):-
        skip_lines('\\end{coverpage}',Ls,1,Acc,Acc_,Ls_,_).

format_command('\\begin{toc}',Ls,[P,N,M,W],Acc,Acc_,Ls_,[P,N,M,W]):-
        toc_lines(['\\begin{toc}'|Ls],[1,N,M,W],Acc,Acc_,Ls_).


format_command('\\newpage',Ls,[P,N,M,W],Acc,Acc,Ls_,[P,N,M,W]):-
        M_ is M - N
        ,findall(''
               ,between(1,M_,_K)
               ,Ss)
        ,append(Ss,Ls,Ls_).
format_command('\\begin{nolayout}',Ls,[P,_N,M,W],Acc,Acc_,Ls_,[P_,1,M,W]):-
        skip_lines('\\end{nolayout}',Ls,1,Acc,Acc_,Ls_,_)
        ,succ(P,P_).


%!      skip_lines(+End,+Lines,+Count,+Acc,-New,-Newlines,-NewCount)
%!      is det.
%
%       Skip formatting Lines until an End marker is reached.
%
%       End is a closing tag of a formatting command, as listed in
%       format_command/7.
%
%       Lines is the list of lines after the opening task preceding End.
%
%       Count is the count of lines in the page currently processed.
%
%       Acc is the accumulator of _formatted_ lines, passed in from
%       format_line/5.
%
%       New is the list of lines in Acc updated with the lines skipped
%       by this predicate, i.e. lines left unformatted.
%
%       NewCount is Count updated with the number of lines skipped.
%
skip_lines(C,[C|Ls],N,Acc,Acc,Ls,N):-
        !.
skip_lines(C,[L|Ls],N,Acc,Bind,Ls_Bind,N_Bind):-
        succ(N,N_)
        ,skip_lines(C,Ls,N_,[L|Acc],Bind,Ls_Bind,N_Bind).


%!      box_lines(+Count,+Lines,+Width,+Acc,-New,-Rest,-NewCount)
%!      is det.
%
%       Wrap lines in a text box with a title.
%
%       Count is the count of lines to be wrapped. This is only really
%       used to find the first line and update the count of lines in
%       the current page.
%
%       Lines is a list of lines of text. All the lines in Lines up to
%       the \end{box} tag are to be placed in a box. They should be
%       already formatted to a width less than the full width of the
%       rest of the document. You'll have to find how much that is
%       empirically more or less.
%
%       Width is the width of the longest line in Lines. See above for
%       the maximum width of lines in Lines.
%
%       Acc is the accumulator of processed lines in the current page
%       (and in the document so far).
%
%       New is Acc updated with the lines in Lines placed in a box and
%       also wrapped inpage borders.
%
%       Rest is the lines remaining in Lines after the \end{box} tag.
%
%       NewCount is the final count of lines in the box, including the
%       header and footer of the box. This is used to update the count
%       of lines in the current page.
%
%       @tbd Indent lines to 86 for boxing text in 94-column text.
%
%       @tbd Make box borders configurable.
%
box_lines(N,['\\end{box}'|Ls],W,Acc,Acc_,Ls,N):-
        !
        ,W_ is W + 2
        ,format(atom(F),'~|└~`─t┘~*| ',[W_])
        ,format_line(F,nil,W,Acc,Acc_).
box_lines(1,['\\begin{box}',T|Ls],W,Acc,Bind,Ls_Bind,N_Bind):-
        !
        ,W_ is W + 2
        ,format(atom(F),'~|┌[~w]~`─t┐~*| ',[T,W_])
        ,format_line(F,nil,W,Acc,Acc_)
        ,box_lines(2,Ls,W,Acc_,Bind,Ls_Bind,N_Bind).
box_lines(N,[L|Ls],W,Acc,Bind,Ls_Bind,N_Bind):-
        format(atom(F),'│ ~w~` t~*| │ ',[L,W])
        ,format_line(F,nil,W,Acc,Acc_)
        ,succ(N,N_)
        ,box_lines(N_,Ls,W,Acc_,Bind,Ls_Bind,N_Bind).


%!      toc_lines(+Lines,+Counts,+Acc,-New,-Rest) is det.
%
%       Format Table of Content Lines.
%
%       Lines is a list of lines of text. The first line should be the
%       command '\\begin{toc}'. This predicate will add borders and
%       number pages with Roman numerals until the closing tag
%       '\\end{toc}'.
%
%       Counts is ta list [P,N,M,W] with counts of pages, lines, maximum
%       lines and width, inherited from format_command/7.
%
%       Acc is the accumulator of processed lines inherited from
%       format_lines/7.
%
%       New is Acc updated with the formatted lines of the ToC.
%
%       Rest is the list of lines remaining in Lines after the
%       '\\end{toc}' tag is encountered.
%
%       This predicate will pad the last page of the ToC up to the
%       maximum page length (M in Counts). Unfortunately this can't be
%       done easily with the \newpage command because that doesn't know
%       to stop when it encounters another command. Maybe something to
%       fix later.
%
toc_lines(['\\begin{toc}',L|Ls],[P,N,M,W],Acc,Bind,Ls_Bind):-
        !
        ,format_line(L,1,W,Acc,Acc_)
        ,succ(N,N_)
        ,toc_lines(Ls,[P,N_,M,W],Acc_,Bind,Ls_Bind).
toc_lines(['\\end{toc}'|Ls],[P,N,M,W],Acc,Acc_,Ls):-
        !
        % Offset lines so far er plus ... one? Why?
        ,M_ is M - N + 1
        ,pad_toc(1,M_,W,Acc,Acc_1)
        ,arabic_roman(P,R)
        ,format_line(nil,last(R),W,Acc_1,Acc_).
toc_lines(Ls,[P,M,M,W],Acc,Bind,Ls_Bind):-
        !
        ,arabic_roman(P,R)
        ,format_line(nil,last(R),W,Acc,Acc_)
        ,succ(P,P_)
        ,toc_lines(Ls,[P_,1,M,W],Acc_,Bind,Ls_Bind).
toc_lines([L|Ls],[P,N,M,W],Acc,Bind,Ls_Bind):-
        format_line(L,N,W,Acc,Acc_)
        ,succ(N,N_)
        ,toc_lines(Ls,[P,N_,M,W],Acc_,Bind,Ls_Bind).


%!      pad_toc(+Count,+Max,+Width,+Acc,-New) is det.
%
%       Pad the last ToC page with empty lines up to Max.
%
%       This should normally be done with \newpage, but the commands
%       don't really stop and call each other recursively so a new
%       predicate was needed to pad the toc. This might be reused to pad
%       other special pages, e.g. maybe it can be used with character
%       sheets.
%
pad_toc(M,M,_W,Ps,Ps).
pad_toc(N,M,W,Acc,Bind):-
        format_line('',nil,W,Acc,Acc_)
        ,succ(N,N_)
        ,pad_toc(N_,M,W,Acc_,Bind).


%!      arabic_roman(?Roman,?Arabic) is semidet.
%
%       Convert between an Arabic and a Roman numeral.
%
%       Simple version that converts numbers from 1 to 10, pending a
%       more complete version.
%
arabic_roman(1,i).
arabic_roman(2,ii).
arabic_roman(3,iii).
arabic_roman(4,iv).
arabic_roman(5,v).
arabic_roman(6,vi).
arabic_roman(7,vii).
arabic_roman(8,viii).
arabic_roman(9,ix).
arabic_roman(10,x).


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
%       pre-formatted page (including the cover page and the ToC pages)
%       to be inserted in the text. Currently this is used to insert
%       character sheets in the rules text. This predicate is also
%       called by text_width/3 to skip counting lines of inserted pages.
%       The latter use is necessary because inserted pages already have
%       borders whose width would otherwise be counted when looking for
%       the longest line in the text to calculate where to put borders
%       in all pages in the text.
%
%       @tbd This could be made sole responsible for deciding when to
%       skip a line, rather than having that responsibility spread out
%       over three bloody predicates as it is now.
%
%       @tbd This can be replaced with skip_lines/7 now.
%
noformat_lines([L|Ls],N,Acc,Acc,Ls,N):-
        memberchk(L,['\\end{nolayout}','\\end{coverpage}','\\end{toc}'])
        ,!.
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
%       @tbd Use skip_lines/7 rather than noformat_lines/6.
%
text_width([],W,W):-
        !.
text_width([L|Ls],Wi,Bind):-
% Skip Cover page, ToC, inserted pages, already formated.
% See noformat_lines/6 for exaplanation.
        memberchk(L,['\\begin{nolayout}','\\begin{coverpage}','\\begin{toc}'])
        ,!
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
        longest_line(Ls,['',1,0],[L,I,W]).

%!      longest_line(+Lines,+Acc,-Widest) is det.
%
%       Business end of longest_line/4.
%
%       Widest is a list [L,I,W], where L is the longest line in Lines,
%       I is the index of L in Lines and W is the width of L in
%       characters.
%
longest_line([],W,W):-
        !.
longest_line([L|Ls],Acc,Bind):-
% Skip Cover page, ToC, inserted pages, already formated.
% See noformat_lines/6 for exaplanation.
        memberchk(L,['\\begin{nolayout}','\\begin{coverpage}','\\begin{toc}'])
        ,!
        ,noformat_lines(Ls,1,[],_,Ls_,_)
        ,longest_line(Ls_,Acc,Bind).
longest_line([Lj|Ls],[Li,I,Wi],Bind):-
        succ(I,J)
        ,atom_length(Lj,Wj)
        ,(   Wj > Wi
         ->  Acc = [Lj,J,Wj]
         ;   Acc = [Li,I,Wi]
         )
        ,longest_line(Ls,Acc,Bind).
