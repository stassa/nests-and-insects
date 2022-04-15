:-module(display, [pp_class/1
                  ]).

:-use_module(library(clp/clpfd)).

/** <module> Pretty-printing facilities.

*/


%!      feature_categories(?Category,?Name) is det.
%
%       A Feature Category and its display Name.
%
feature_categories(attribute,'Attributes').
feature_categories(condition,'Conditions').
feature_categories(survival,'Survival').
feature_categories(specific_ability,'Specific Abilities').
feature_categories(common_ability,'Common Abilities').
feature_categories(effect,'Effects').
feature_categories(inventory_item,'Inventory').



%!      pp_class(+Id) is det.
%
%       Pretty-print a Class' name, pronouns and Features.
%
pp_class(Id):-
        nl
        ,display:pp_class_id(Id)
        ,forall(feature_categories(F,_)
               ,(nl
                ,display:pp_feature_category(Id,F)
                )
               ).



%!      pp_class_id(+Id) is det.
%
%       Pretty-print a Class name and preferred pronouns.
%
pp_class_id(Id):-
        character:character_class(Id,N)
        ,character:character_pronouns(Id,Ps)
        ,atomic_list_concat(Ps,'/',Ps_)
        ,format('Class: ~w~n', [N])
        ,format('Preferred Pronouns: ~w~n',[Ps_]).



%!      pp_feature_category(+Class,+Category) is det.
%
%       Pretty-print a Class' Features of a Category.
%
%       Class is the Class identifier.
%
%       Category is the atomic ide of the feature category to print as
%       defined in module character. One of:
%
%       [attribute, condition, survival, specific_ability,
%       common_ability, inventory_item]
%
pp_feature_category(Id,C):-
        must_be(nonvar,Id)
        ,must_be(nonvar,C)
        ,feature_categories(C,N)
        ,chargen:features(Id,C,FRs)
        ,percentalise(FRs,FRs_)
        ,pairs_keys_values(FRs_,Fs,Rs)
        ,features_names(C,Fs,Ns)
        ,transpose([Ns,Rs],Cols)
        ,columns_widths(Cols,Ws)
        ,formatting_atom(Ws,3,FA)
        ,underline(Ns,-,Us)
        ,Rows = [Ns,Us,Rs]
        ,format('~w:~n',[N])
        ,forall(member(Row,Rows)
               ,format(FA,Row)).


%!      percentalise(+Features,-Percentiles) is det.
%
%       Convert numeric Ratings to Percentiles.
%
%       Ratings is a list of key-value pairs F-N where each key F is the
%       atomic identifier of a Feature and each value N is the rating of
%       that Feature for some class.
%
%       Percentiles is a list of key-value pairs F-N% where the keys are
%       the keys in Featuers and the values are atoms 'N%' where N is
%       the rating of the associated Feature F as an atom with the
%       percentile sign '%' appended.
%
%       This is used to avoid having to record Feature ratings in data/
%       modules as percentiles, rather than numbers, to facilitate
%       performing any necessary arithmetic with reatings (e.g. adding
%       modifiers) before the point where we actually have to display
%       them.
%
percentalise(FRs,Ps):-
        findall(F-A
               ,(member(F-R,FRs)
                ,(   F = wounds
                 ->  format(atom(A),'~w',[R])
                 ;   format(atom(A),'~w%',[R])
                 )
                )
               ,Ps).


%!      features_names(+Category,+Ids,-Names) is det.
%
%       Names of Features in a Feature Category.
%
%       Category is the symbol of a predicate defined in module
%       character and representing a category of character (and
%       creature) Features. Category is one of: [attribute, condition,
%       survival, specific_ability, common_ability, effect,
%       inventory_item].
%
%       Ids is a list of identifiers of Features in the named Category.
%
%       Names is a list of player-friendly names for the Features with
%       the given Ids.
%
features_names(F,Ids,Ns):-
        findall(N
               ,(member(Id,Ids)
                ,T =.. [F,Id,N]
                ,call(character:T)
                )
               ,Ns).


%!      underline(+Row,+Character,-Underlined) is det.
%
%       Create an underline row.
%
%       Row is a list of atoms representing a table row.
%
%       Character is an underline character.
%
%       Underlined is a list of atoms such that the i'th atom in the
%       list is the same length as the i'th atom of Row and if n is the
%       length of the i'th atom in Row, the i'th atom in Underlined is
%       made up of n instances of Character.
%
underline(Ls,C,Us):-
        findall(U
               ,(member(V,Ls)
                ,atom_length(V,N)
                ,n_chars(N,C,U)
                )
               ,Us).


%!      columns_widths(+Columns:list(list),-Widths:list(number)) is det.
%
%       Widths of all table Columns.
%
%       Columns is a list-of-lists representing the columns of a table.
%
%       Widths is a list of numbers where each number is the width of a
%       column in Columns.
%
columns_widths(Cs, Ws):-
        findall(W
               ,(member(C, Cs)
                ,column_width(C,W)
                )
               ,Ws).


%!      column_width(+Column:list,-Width:number) is det.
%
%       Width of a table Column.
%
%       Column is a list of the (atomic) values of a column in a table.
%
%       Width is the width of the longest atomm in Column.
%
column_width(Ls,W):-
        findall(N
               ,(member(L,Ls)
                ,atom_length(L,N)
                )
               ,Ns)
        ,sort(0,@>,Ns,[W|_]).



%!      formatting_atom(+Widths,+Spaces,-Atom) is det.
%
%       Create an Atom to format a list of rows
%
%       Widths is a list of numbers representing column widths.
%
%       Spaces is a number denoting the inter-column spaces to add to
%       the formatting atom.
%
%       Atom is a format string passed as the first argument of
%       format/2 to table pretty-printers, e.g. pp_formatted_table/2
%       etc.
%
formatting_atom([V1|Vs],S,F):-
        n_spaces(S,Ss)
        % Formatting string of first column
        % Pads up to string length without setting a tab
        % Otherwise this column is padded up twice.
        ,atomic_list_concat(['~w~',V1,'+',Ss],'',F_1)
        % Formatting string of subsequent columns
        ,findall(F_i
               ,(member(V,Vs)
                ,V_ is V + S
                ,atomic_list_concat(['~w~t~',V_,'+',Ss],'',F_i)
                )
               ,Fs)
        ,sumlist([V1|Vs],CWs)
        ,length([V1|Vs],N)
        % Table width calculation
        ,TW is CWs + (N * S)
        % End of formatting string with tab and newline
        ,atomic_list_concat(['~t~',TW,'|~n'],'',F_n)
        ,append([F_1|Fs],[F_n],Fs_)
        ,atomic_list_concat(Fs_,'',F)
        ,debug(formatting_atom,'Formatting atom: ~w',[F]).





%!      n_spaces(+N,-Spaces) is det.
%
%       Construct an atomic string of N space characters.
%
n_spaces(N,Ss):-
        findall(' '
               ,between(1,N,_)
               ,Ss_)
        ,atomic_list_concat(Ss_,'',Ss).


n_chars(N,C,Cs):-
        findall(C
               ,between(1,N,_)
               ,Cs_)
        ,atomic_list_concat(Cs_,'',Cs).
