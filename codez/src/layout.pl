:-module(layout, [layout_rulebook/3
                 ,read_lines/2
                 ,format_lines/4
                 ,longest_line/4
                 ]).

:-use_module(configuration).
:-use_module(src(tables)).
:-use_module(src(theorem)).
:-use_module(src(label)).
:-use_module(src(styles)).
:-use_module(src(toc)).
:-use_module(src(charsheet)).

/** <module> Layout and formatting for text-based rulebooks.

_Tips and tricks and bugs and quirks_

1. Tables: The width of lines of text is calcualted by text_width/2.
This doesn't know that the "&" marking table columns should not be
counted in the length of the printed line, so if a table line + &'s is
longer than you expect, the entire page will end up longer as a result.

To avoid this, take into account the &'s when formatting table lines to
their right width in the raw text.

2. Comments: The same thing as with tables happens with comments:
text_width/2 doesn't know to ignore comments, so if a comment line is
too long it will make the page longer.


*/

%!      layout_rulebook(+In_Path,+Page_Lines,+Out_Path) is det.
%
%       Layout a rulebook.
%
layout_rulebook(IP,N,OP):-
        read_lines(IP,Ls)
        ,format_lines(Ls,N,Fs,[_P,_N,W,_Cs,Ts])
        ,format_toc(Ts,N,W,Ts_F)
        ,O = open(OP,write,S,[alias(output_file)
                             ,close_on_abort(true)
                             ,encoding(utf8)
                             ])
        ,R = (write_preformatted(S,N,Fs,Fs_)
             ,write_preformatted(S,N,Fs_,Fs_1)
             % Print the ToC
             ,forall(member(T,Ts_F)
                    ,writeln(S,T))
             % Print the rest of the rulebook.
             ,forall(member(F,Fs_1)
                    ,writeln(S,F))
             )
        ,C = close(S)
        ,setup_call_cleanup(O,R,C).



%!      write_preformatted(+Stream,+Page_Lines,+Text,-Rest) is det.
%
%       Write a preformatted page at the start of the rulebook.
%
%       The reason for the existence of this predicate is that we want
%       to print the ToC immediately after printing the coverpage and
%       the credits page, but just before we start printing the text of
%       the rulebook. The ToC follows immediately from the
%       credit page which follows immediately from the coverpage so we
%       first print the coverpage, count the number of lines we print to
%       make sure we know when we're done, then we do the same for
%       the credite page, and then we'll print the ToC (in
%       layout_rulebook/3).
%
%       The coverpage starts at the first line of the document and takes
%       exactly Page_Lines lines, so we can safely enough assume that
%       after we have printed the first Page_Lines lines, we have
%       printed the coverpage. The same goes for the credits page.
%
%       Stream is the rulebook file stream.
%
%       Page_Lines is the maximum number of lines per page.
%
%       Text is the list of lines of the already formatted text of the
%       rulebook.
%
%       Rest is the list of lines in Text after the preformatted page
%       (the coverpage or the credits page).
%
%       @tbd All this is a hack to avoid having to implement proper
%       \cover, \credits and \toc commands and instead by default insert
%       the credits page after the cover page and the ToC after the
%       credits page. Which works OK, but also hurts a bit and makes a
%       bit of a mess.
%
write_preformatted(S,N,Fs,Fs_):-
        write_preformatted(S,0,N,Fs,Fs_).

write_preformatted(_S,M,M,Fs,Fs):-
        !.
write_preformatted(S,N,M,[F|Fs],Bind):-
        writeln(S,F)
        ,succ(N,N_)
        ,write_preformatted(S,N_,M,Fs,Bind).


%!      format_lines(+Lines,+Page_Lines,-Formatted,-Data) is det.
%
%       Formats a list of text Lines for a text-based rulebook.
%
%       Page_Lines is an integer, the number of lines in one page of a
%       rulebook, complete with borders and other layout.
%
%       Formatted is the lines of text in Lines formatted with a border
%       and a header and footer.
%
%       Data is a list [P,N,M,W,Cs,Ts] where:
%       * P is the page number of the last numbered page written to the
%       rulebook.
%       * N is the line number of the last line printed in the last page
%       of the rulebook.
%       * W is the width of pages written to the rulebook
%       * Cs is a list of compounds c(Ti,I) where T is a theorem type
%       and I is the count of that kind of theorem typeset in the
%       rulebook.
%       * Ts is a list of key-value pairs R-G where each key, R, is the
%       title of a document part (chapter, section, etc) and each value,
%       G, is the page in which the document part with that title was
%       typeset.
%
format_lines(Ls,N,Fs,[P,Ni,W,Cs,Ts]):-
        text_width(Ls,W)
        % One line of header and two lines of footer
        % Plus off-by-one offset
        ,N_ is N - 2
        ,format_lines(Ls,[1,1,N_,W,[],[[0,0,0,0]]],[P,Ni,_M,W,Cs,Ts],[],Fs).


%!      format_lines(+Text,+Data,-Data_Bind,+Acc,-Formatted) is det.
%
%       Business end of format_lines/2.
%
%       Data is a list [Pages,Lines,Max,Width,Counts,ToC] where:
%       * Pages: current pages count.
%       * Lines: current line count.
%       * Max: number of lines per page.
%       * Width: page width
%       * Counts: list of theorem counts.
%       * Toc: table of contents data.
%
format_lines([],[P,N,M,W,Cs,Ts],[P,N,M,W,Cs,Ts],Acc,Fs):-
% Format the last line in the entire text.
        format_line(nil,last(P),W,Acc,Acc_)
        ,reverse(Acc_,Fs)
        ,!.
format_lines([L|Ls],[P,M,M,W,Cs,Ts],Ds_Bind,Acc,Bind):-
% Format the last line in the current page.
        !
        ,format_line(nil,last(P),W,Acc,Acc_)
        ,succ(P,P_)
        ,format_lines([L|Ls],[P_,1,M,W,Cs,Ts],Ds_Bind,Acc_,Bind).
format_lines([L|Ls],[P,N,M,W,Cs,Ts],Ds_Bind,Acc,Bind):-
% Execute a formatting command.
        format_command(L,Ls,[P,N,M,W,Cs,Ts],Acc,Acc_,Ls_,[P_,N_,M_,W_,Cs_,Ts_])
        ,!
        ,format_lines(Ls_,[P_,N_,M_,W_,Cs_,Ts_],Ds_Bind,Acc_,Bind).
format_lines([L|Ls],[P,N,M,W,Cs,Ts],Ds_Bind,Acc,Bind):-
% Keep formatting lines
        format_line(L,N,W,Acc,Acc_)
        ,succ(N,N_)
        ,format_lines(Ls,[P,N_,M,W,Cs,Ts],Ds_Bind,Acc_,Bind).


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
        ,configuration:border(upper_left_corner,ULC)
        ,configuration:border(upper_right_corner,URC)
        ,configuration:border(horizontal,HOR)
        % Width of the line plus space plus borders
        ,W_ is (W + 4)
        ,atom_codes(HOR,[C])
        ,format(atom(F),'~|~w~*t~w~*|',[ULC,C,URC,W_])
        ,format_line(L,nil,W,[],[L_]).
format_line(nil,last(P),W,Acc,[F2,F1|Acc]):-
% Process last line
        !
        ,configuration:border(lower_left_corner,LLC)
        ,configuration:border(lower_right_corner,LRC)
        ,configuration:border(horizontal,HOR)
        ,configuration:border(vertical_shadow,SHA_V)
        ,configuration:border(horizontal_shadow,SHA_H)
        % Width of the line plus space plus borders
        ,W1 is (W + 4)
        % Extra space at the start of the footer shadow
        ,W2 is (W + 5)
        ,atom_codes(HOR,[C1])
        ,atom_codes(SHA_H,[C2])
        ,format(atom(F1),'~|~w~*t-~w-~*t~w~*|~w',[LLC,C1,P,C1,LRC,W1,SHA_V])
        ,format(atom(F2),' ~|~*t~*|',[C2,W2]).
format_line(L,_N,W,Acc,[F|Acc]):-
% Process all other lines
        configuration:border(vertical,VER)
        ,configuration:border(vertical_shadow,SHA)
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
%       Counts is a list [P,N,M,W,Cs,Ts] where each element P, N, M, W
%       is a number, P is the current Page count, Lines is the
%       current line count for the current page, M is the maximum number
%       of lines per page and W is the maximum width of text in a page,
%       and Cs is the list of counts of theorems and Ts the ToC data.
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
%       * \\begin{Theorem}: marks the beginning of a boxed and numbered
%       theorem. Title is the theorem's title. Closing tag:
%       \\end(Theorem).
%
%       * \\begin{coverpage}: marks the beginning of the cover page.
%       Closing tag: \\end{coverpage}.
%
%       * \\begin{credits}: marks the beginning the credits page.
%       Closing tag: \\end{credits}.
%
%       * \\newpage: inserts a new page or completes the current page by
%       filling it with empty lines until it is M lines long. Closing
%       tag: None.
%
%       * \\begin{nolayout}: beginning of a pre-formatted page. This
%       should be exactly M lines. Closing tag: \\end{nolayout}
%
%       * \\begin{table}: beginning of the rows of a table, including a
%       caption line. Closing tag: \\end{table}.
%
%       * \\chapter{T}, \\section{T}, \\subsection{T},
%       \\subsubsection{T} and \\paragraph{T} used to style document
%       parts.
%
format_command(C,Ls,[P,_N,M,W,Cs,Ts],Acc,[CS|Acc],Ls,[P_,1,M,W,Cs,Ts]):-
        atom_concat('\\charsheet',T,C)
        ,sub_atom(T,1,_A,1,Class)
        ,format_charsheet(Class,CS)
        ,succ(P,P_).
format_command(C,Ls,[P,N,M,W,Cs,Ts],Acc,Acc_,Ls_,[P,N_,M,W,Cs,Ts]):-
        atom_concat('\\begin{box}',T,C)
        ,sub_atom(T,1,_A,1,Title)
        % Offset by box borders
        ,W_ is W - 2
        ,box_lines(1,['\\begin{box}',Title|Ls],W_,Acc,Acc_,Ls_,K)
        % Offset by box lines, header and footer.
        ,N_ is N + K.
format_command(C,Ls,[P,N,M,W,Cs,Ts],Acc,Acc,Rs,[P,N,M,W,Cs_,Ts]):-
% Format theorem.
        theorem_lines([C|Ls],Cs,W,Fs,Cs_,Ls_,_K)
        ,append(Fs,Ls_,Rs).
format_command('\\begin{coverpage}',Ls,[P,N,M,W,Cs,Ts],Acc,Acc_,Ls_,[P,N,M,W,Cs,Ts]):-
        skip_lines('\\end{coverpage}',Ls,1,Acc,Acc_,Ls_,_).
format_command('\\begin{credits}',Ls,[P,N,M,W,Cs,Ts],Acc,Acc_,Ls_,[P,N,M,W,Cs,Ts]):-
        skip_lines('\\end{credits}',Ls,1,Acc,Acc_,Ls_,_).
format_command('\\newpage',Ls,[P,N,M,W,Cs,Ts],Acc,Acc,Ls_,[P,N,M,W,Cs,Ts]):-
        M_ is M - N
        ,findall(''
               ,between(1,M_,_K)
               ,Ss)
        ,append(Ss,Ls,Ls_).
format_command('\\begin{nolayout}',Ls,[P,_N,M,W,Cs,Ts],Acc,Acc_,Ls_,[P_,1,M,W,Cs,Ts]):-
        skip_lines('\\end{nolayout}',Ls,1,Acc,Acc_,Ls_,_)
        ,succ(P,P_).
format_command(C,Ls,[P,N,M,W,Cs,Ts],Acc,Acc,Ls,[P,N,M,W,Cs,Ts]):-
        atom_concat('%',_T,C).
format_command('/*',Ls,[P,N,M,W,Cs,Ts],Acc,Acc,Ls_,[P,N,M,W,Cs,Ts]):-
        skip_lines('*/',Ls,0,[],_,Ls_,_).
format_command(C,Ls,[P,N,M,W,Cs,ToCs],Acc,Acc,Rs,[P,N,M,W,Cs,ToCs]):-
        atom_concat('\\begin{table}',S,C)
        ,sub_atom(S,1,_A,1,Space)
        ,atom_number(Space,Sp)
        ,skip_lines('\\end{table}',Ls,1,[],Ts,Acc,_Acc,Ls_,_K)
        ,reverse(Ts,Ts_)
        ,tables:format_table(Ts_,Sp,─,Fs)
        ,findall(A
                ,(member(format(atom(A),FA,As),Fs)
                 ,format(atom(A),FA,As)
                 )
                ,Rows)
        ,append(Rows,Ls_,Rs).
format_command(C,Ls,[P,N,M,W,Cs,Ts],Acc,Acc_,Ls,[P,N_,M,W,Cs_,Ts]):-
% Label and caption two-in-one.
        label_lines(C,Cs,F,Cs_)
        ,format_line(F,N,W,Acc,Acc_)
        ,succ(N,N_).
format_command(C,Ls,[P,N,M,W,Cs,[Ks|Ts]],Acc,Acc,Ls_,[P,N,M,W,Cs,[Ks_,toc(R,T_,P)|Ts]]):-
% Document parts styling
% Keep the forall-write call because it outputs a nice layout
% very helpful for debugging.
        style_part(C,W,R,Ks,Ks_,Ps)
        %,forall(member(P_,Ps)
        %       ,writeln(P_))
        ,Ps = [T|_U]
        ,split_string(T,' ',' ',Ss)
        ,atomic_list_concat(Ss,' ',T_)
        ,append(Ps,Ls,Ls_).



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


%!      skip_lines(+End,+Lns,+Cnt,+Acc1,-Sks,+Acc2,-Prs,-Rest,-Newcnt)
%!      is  det.
%
%       As skip_lines/7 but also returns the lines skipped in Sks.
%
skip_lines(C,[C|Ls],N,Ss,Ss,Ps,Ps,Ls,N):-
        !.
skip_lines(C,[L|Ls],N,Ss_Acc,Ss_Bind,Ps_Acc,Ps_Bind,Ls_Bind,N_Bind):-
        succ(N,N_)
        ,skip_lines(C,Ls,N_,[L|Ss_Acc],Ss_Bind,[L|Ps_Acc],Ps_Bind,Ls_Bind,N_Bind).



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
        memberchk(L,['\\end{credits}'
                    ,'\\end{nolayout}'
                    ,'\\end{coverpage}'
                    ,'\\end{toc}'])
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
% Skip label lines because the command can make them over-long.
        atom_concat('\\label',_,L)
        ,!
        ,text_width(Ls,Wi,Bind).
text_width([L|Ls],Wi,Bind):-
% Skip Cover page, credits page, ToC, inserted pages, already formated.
% See noformat_lines/6 for exaplanation.
        memberchk(L,['\\begin{credits}'
                    ,'\\begin{nolayout}'
                    ,'\\begin{coverpage}'
                    ,'\\begin{toc}'])
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
% Skip label lines because the command can make them over-long.
        atom_concat('\\label',_,L)
        ,!
        ,longest_line(Ls,Acc,Bind).
longest_line([L|Ls],Acc,Bind):-
% Skip Cover page, credits page, ToC, inserted pages, already formated.
% See noformat_lines/6 for exaplanation.
        memberchk(L,['\\begin{credits}'
                    ,'\\begin{nolayout}'
                    ,'\\begin{coverpage}'
                    ,'\\begin{toc}'])
        ,!
        ,noformat_lines(Ls,1,[],_,Ls_,_)
        ,longest_line(Ls_,Acc,Bind).
longest_line([Lj|Ls],[Li,I,Wi],Bind):-
        succ(I,J)
        ,atom_length(Lj,Wj)
        ,(   Wj > Wi
         ->  Acc = [Lj,J,Wj]
         ;   Acc = [Li,J,Wi]
         )
        ,longest_line(Ls,Acc,Bind).
