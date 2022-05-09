:-module(configuration, [theorem/2
                        ,label/2
                        ]).


%!      theorem(?Title,?Name) is semidet.
%
%       A theorem command Title and its user-friendly Name.
%
theorem(note_reader,'Note to the reader').
theorem(example,'Example').
theorem(note,'Note').
theorem(rules_summary,'Rules Summary').


%!      label(?Type,?Name) is semidet.
%
%       A caption/ label to be counted.
%
label(table, 'Table').
label(figure, 'Figure').
