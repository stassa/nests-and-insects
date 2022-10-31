:-module(class, [base_wounds/1
                ,class_effects/3
                ]).

:-use_module(ants).
:-use_module(beetle).
:-use_module(ladybug).
:-use_module(scorpion).
:-use_module(spider).
:-use_module(wasp).
:-use_module(template).

/** <module> Class Features.

Predicates in this module represent the ratings of Features by Class at
the start of the game, and their modifiers or component Featues used for
character generation.

*/

%!      base_wounds(?Wounds) is semidet.
%
%       The base Wounds count for all Classes.
%
base_wounds(3).


%!      class_effects(?Class, ?Effect, ?Rating) is semidet.
%
%       A Class' starting Effect Rating.
%
class_effects(ants,blind,100).
