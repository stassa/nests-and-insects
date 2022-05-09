:-module(tables, [format_table/4
                 ]).

:-use_module(library(clpfd)).

%:-encoding(utf8).

/** <module> Predicates to format table rows.

Use format_table/2 to create format/2 calls to print out table rows:

==
?- _S = 5, _Ls = ['Roll vs Modifier&        Modifier Sign&        Degree of Success','Under or Equal  &        Positive     &        Improves towards Critical Success','Over            &        Positive     &        No change','Under or Equal  &        Negative     &        Worsens towards Critical Failure','Over            &        Negative     &        No change'], tables:format_table(_Ls,_S,_Fs), nl, forall(member( format(atom(A),FA,As) ,_Fs),(writeln(format(atom(A),FA,As)) )).

format(atom(_22704),~|~w~*+~|~w~*+~w~|,[Roll vs Modifier,21,Modifier Sign,18,Degree of Success])
format(atom(_22704),~|~w~*+~|~w~*+~w~|,[Under or Equal,21,Positive,18,Improves towards Critical Success])
format(atom(_22704),~|~w~*+~|~w~*+~w~|,[Over,21,Positive,18,No change])
format(atom(_22704),~|~w~*+~|~w~*+~w~|,[Under or Equal,21,Negative,18,Worsens towards Critical Failure])
format(atom(_22704),~|~w~*+~|~w~*+~w~|,[Over,21,Negative,18,No change])
true.
==

format_table/2 composes a format atom for each table row and the
corresponding arguments lists, both to be passed to format/2. Example of
just the format atoms, and argument lists:

==
?- _S = 5, _Ls = ['Roll vs Modifier&        Modifier Sign&        Degree of Success','Under or Equal  &        Positive     &        Improves towards Critical Success','Over            &        Positive     &        No change','Under or Equal  &        Negative     &        Worsens towards Critical Failure','Over            &        Negative     &        No change'], tables:format_table(_Ls,_S,_Fs), nl, forall(member( format(atom(A),FA,As) ,_Fs),(writeln(FA-As) )).

~|~w~*+~|~w~*+~w~|-[Roll vs Modifier,21,Modifier Sign,18,Degree of Success]
~|~w~*+~|~w~*+~w~|-[Under or Equal,21,Positive,18,Improves towards Critical Success]
~|~w~*+~|~w~*+~w~|-[Over,21,Positive,18,No change]
~|~w~*+~|~w~*+~w~|-[Under or Equal,21,Negative,18,Worsens towards Critical Failure]
~|~w~*+~|~w~*+~w~|-[Over,21,Negative,18,No change]
true.
==

The entire format/2 statement can be passed to a printing predicate:

==
?- _S = 5, _Ls = ['Roll vs Modifier&        Modifier Sign&        Degree of Success','Under or Equal  &        Positive     &        Improves towards Critical Success','Over            &        Positive     &        No change','Under or Equal  &        Negative     &        Worsens towards Critical Failure','Over            &        Negative     &        No change'], tables:format_table(_Ls,_S,_Fs), nl, forall(member( format(atom(A),FA,As) ,_Fs),(format(atom(A),FA,As), write_term(A,[quoted(true),nl(true)]))).

'Roll vs Modifier     Modifier Sign     Degree of Success'
'Under or Equal       Positive          Improves towards Critical Success'
'Over                 Positive          No change'
'Under or Equal       Negative          Worsens towards Critical Failure'
'Over                 Negative          No change'
true.
==

*/


%!      format_table(+Lines,+Space,+Underline,-Formatted) is det.
%
%       Format a list of text Lines as a table.
%
%       Lines is a list of lines of text between \begin{table} and
%       \end{table} tags. Each line in Lines is a quoted atom read from
%       an input file by read_lines/2 and representing one row of the
%       table. In each row, the end of each column except for the last
%       is marked by the character '&'. All rows in the table must have
%       the same number of columns, lest the spell is broken and the
%       table comes out all mangled.
%
%       Space is the number of space characters to insert between table
%       columns.
%
%       Underline is the character to use to underline the header row
%       and the last row in the table.
%
%       Formatted is a list of format/2 calls of the form:
%
%       format(atom(Row),FormAtom,Args)
%
%       Where Row is a fully formatted table row as an atom; FormAtom is
%       a formatting atom for the row; and Args is a list of arguments
%       of the form:
%       [T1,W1, ..., Tn]
%
%       Where each Ti is the text of a column and each Wi is the width
%       of the column, including the number of spaces in Space.
%
%       See the module header for examples of use fo format_table/2.
%
format_table([L1|Ls],S,C,Fs):-
        columns_widths(L1,Cs,Ws)
        ,columns_width_args(Cs,Ws_Acc,As)
        ,format_atom(Cs,FA)
        ,F = format(atom(_F1),FA,As)
        ,row_underline(C,W,U)
        ,format_table(Ls,Ws,Ws_Acc,S,[U,F],C,W,Fs).


%!      format_table(+Lines,+Widths,+W_Acc,+Space,+Char,+UWid,+Acc,-Format)
%!      is det.
%
%       Business end of format_table/2.
%
%       Lines is a list of text lines representing a table as described
%       in format_table/2.
%
%       Widths is a list of numbers, the maximum widths of table
%       columns, except the last. Widths is updated with the maximum
%       widths of columns as format_table/8 walks over table rows.
%
%       W_Acc is a variable used to increase the maximum column widths
%       in Widths with Space spaces and then to instantiate the
%       corresponding format/2 arguments in format/2 terms constructed
%       for all rows. See columns_width_args/3 and bind_width_args/2 for
%       details.
%
%       Acc is the accumulator of format/2 terms.
%
%       Space is the number of spaces to pad columns with.
%
%       Char is the character to use in underlines.
%
%       UWid is the width of the underlines, to be instantiated at the
%       end of processing in a similar way as W_Acc.
%
%       Format is a list of format/2 terms, one for each row, as
%       described in format_table/3.
%
%       This predicate goes through the rows of a table and constructs a
%       format/2 call for each. The arguments of the format/2 call are
%       calculated by format_atom/2. These arguments include the column
%       widths, but we want to format the entire table according to the
%       maximum width of a column, which we can only do once we have
%       finished processing each row of the table. To avoid going
%       through the table twice, once to count the widths of columns and
%       once more to set the arguments of the format/2 calls with the
%       column widths found in the first iteration, we instead leave the
%       width arguments as variables shared between all the format/2
%       terms' argument lists. We bind the width arguments at the end of
%       processing when we know the maximums.
%
format_table([],Ws,Ws_,S,[F|Acc],UC,UW,Fs):-
        F = format(atom(_A),_FA,As)
        ,maplist(plus(S),Ws,Ws_)
        ,bind_width_args(As,Ws_)
        ,underline_width(Ws_,S,UW)
        ,row_underline(UC,UW,U)
        ,reverse([U,F|Acc],Fs).
format_table([L_j|Ls],Ws_i,Ws_Acc,S,Fs_Acc,UC,UW,Fs_Bind):-
        columns_widths(L_j,Cs,Ws_j)
        ,columns_width_args(Cs,Ws_Acc,As)
        ,format_atom(Cs,FA)
        ,F_j = format(atom(_F),FA,As)
        ,max_widths(Ws_i,Ws_j,Ws_k)
        ,format_table(Ls,Ws_k,Ws_Acc,S,[F_j|Fs_Acc],UC,UW,Fs_Bind).


%!      columns_widths(+Line,-Columns,-Widths) is det.
%
%       Extract Columns and their Widths from a Line of text.
%
%       Line is a line of text representing one table row, as described
%       in format_table/2.
%
%       Columns is a list of atoms each holding the text of one column
%       in the row represented by Line.
%
%       Widths is a list of numbers where each number is the width of a
%       column in a row in Line, up to the '&' column delimiter and
%       with spaces after '&' trimmed off.
%
columns_widths(L,Cs,Ws):-
        split_string(L,'&',' ',Cs)
        ,maplist(atom_length,Cs,Ws).


%!      columns_width_args(+Columns,+Width_Vars,-Args) is det.
%
%       Construct a list of Arguments for a format/2 term.
%
%       Columns is a list of atoms each atom the text of one column in
%       one row of the table we are formatting.
%
%       Width_Vars is a list of _variables_ to be bound to the widths of
%       columns in the finished table. This binding is done by
%       bind_width_args/2 and only when each row has been processed and
%       we finally know the width of the longest row in the table.
%
%       Args is a list of arguments to pass to the format/2 term
%       constructed at the end of format_table/8. The list is of the
%       form:
%
%       [T1,V1,..., Tn]
%
%       Where each Ti is the text of one column in Columns and V1 is the
%       variable in Width_Vars corresponding to that column.
%
%       Column widths bound to Width_Vars as calculated at the end of a
%       call to format_table/8 are used to pad the text of columns in
%       Columns (i.e. each of the Ti in Args) with the number of spaces
%       needed to have a well-formatted table, with equal spacing
%       between columns.
%
columns_width_args(Cs,Ws,As):-
        columns_width_args(1,Cs,Ws,[],As).

%!      columns_width_args(+Index,+Cols,+Widths,+Acc,-Args) is det.
%
%       Business end of columns_width_args/3.
%
columns_width_args(_I,[],_Ws,Acc,As):-
        !
       ,reverse(Acc,As).
columns_width_args(I,Cs,[W|Ws],Acc,Bind):-
        0 is I mod 2
        ,!
        ,succ(I,I_)
        ,columns_width_args(I_,Cs,Ws,[W|Acc],Bind).
columns_width_args(I,[C|Cs],Ws,Acc,Bind):-
        succ(I,I_)
        ,columns_width_args(I_,Cs,Ws,[C|Acc],Bind).


%!      max_widths(+Widths_1,+Widths_2,-Max_Widths) is det.
%
%       Update the mximum widths of columns in a table.
%
%       Widths_1 is the list of maximum widths of columns found so-far
%       durin the execution of format_table/8.
%
%       Widths_2 is the list of maximum widths of the column currently
%       being processed by format_table/8. Both Widths_1 and Widths_2
%       are constructed by columns_widths/3.
%
%       Max_Widths is a list of numbers where each number at index i
%       of Max_Widths is the maximum value at the same index in Widths_1
%       and Widths_2.
%
%       Example:
%       ==
%       ?- tables:max_widths([5,3,4],[5,6,2],Max).
%       Max = [5, 6, 4].
%       ==
%
%       This predicate is used during the execution of format_table/8 to
%       keep track of the maximum width of text in each column. The
%       maximum widths found that way are used at the end of
%       format_table/8 to evenly space out the columns of the table.
%
max_widths(Ws_1,Ws_2,Ws_max):-
        transpose([Ws_1,Ws_2],Ts)
        ,maplist(max_list,Ts,Ws_max).


%!      format_atom(+Row,-Atom) is det.
%
%       Construct a format Atom for one Row of a table.
%
%       Row is a list of atoms, each atom the text of one column in one
%       row of the table we are formatting.
%
%       Atom is a format atom passed as the second argument of format/2.
%       A format Atom determines the spacing between adjacent columns,
%       which may need to be padded by spaces. It consists of ~w, ~t,
%       ~| and ~+ control sequences, as described by the documentation
%       of format/2 in Swi-Prolog.
%
%       Here's a simple example:
%       ==
%       [debug]  ?- _Cs = [ row1coli, row1col2, row1col3 ]
%           , tables:format_atom(_Cs,FA).
%
%       FA = '~|~w~*+~|~w~*+~w~|'.
%       ==
%
%       In the example, '~|~w~*+~|~w~*+~w~|' is the format atom that
%       will be used to format the columns in the row _Cs. The format
%       atom constructed in the example sets a tab stop at the start
%       of the row anda the end of each column and distributes padding
%       spaces equally between them. Only the first two columns are
%       padded - we dont' want to pad the third column because we don't
%       want space to extend beyond the end of the table, duh.
%
format_atom(Cs,FA):-
        format_atom(Cs,[],Fs)
        ,atomic_list_concat(Fs,FA).

%!      format_atom(+Row,+Acc,-Format) is det.
%
%       Business end of format_atom/2.
%
format_atom([_],Acc,Fs):-
        reverse(['~w~|'|Acc],Fs)
        ,!.
format_atom([_C|Cs],Acc,Bind):-
        format_atom(Cs,['~|~w~*+'|Acc],Bind).


%!      row_underline(+Char,+Width,-Format) is det.
%
%       Construct a format/2 call for a row's underline.
%
%       Char is the character used for the underline.
%
%       Width is the width of the underline.
%
%       Format is a format/2 call used to underline a row of the table.
%
row_underline(S,W,F):-
        atom_char(S,C)
        ,F = format(atom(_A),'~*t~*|',[C,W]).


%!      underline_width(+Widths,+Space,-Width) is det.
%
%       Calculate the underline length for rows in a table.
%
%       Widths is the list of maximum column lengths in the table.
%
%       Space is the space padding length between columns.
%
%       Width is the width of an underline for the header row and the
%       last row of the table.
%
underline_width(Ws,S,W):-
        underline_width(Ws,S,0,W).

underline_width([W],S,Acc,W_):-
% Subtract S padding chars from last column.
        !
        ,W_ is Acc + W - S.
underline_width([W|Ws],S,Acc,Bind):-
        W_ is Acc + W
        ,underline_width(Ws,S,W_,Bind).


%!      bind_width_args(+Args,+Widths) is det.
%
%       Instantiate the widths of columns in a table.
%
%       Args is a list of arguments passed to format/2, as constructed
%       by columns_width_args/3. Args is a list [T1,W1 ,..., Tn] where
%       each Ti is the text of the last column in a table processed by
%       format_table/8 and each Wi is a variable that must be
%       instantiated to the width of that column (plus spaces).
%
%       Widths is the list of widths of columns calculated during
%       execution of format_table/8. These are variables shared by all
%       argument-lists of all format/2 terms constructed by
%       format_table/8 and they must now be bound with the finally
%       calculated column widths for the table.
%
bind_width_args(As,Ws):-
        bind_width_args_(As,As,Ws,As).

bind_width_args_([],As,_Ws,As):-
        !.
bind_width_args_([V_|As],Acc,[V|Ws],Bind):-
        var(V_)
        ,!
        ,V_ = V
        ,bind_width_args_(As,Acc,Ws,Bind).
bind_width_args_([_F|As],Acc,Ws,Bind):-
        bind_width_args_(As,Acc,Ws,Bind).

