:-module(chargen, [class_character/2
                  ,features/3
                  ,attribute/3
                  ,initiative/2
                  ,threat_rate/2
                  ,survival_rate/2
                  ,wounds/2
                  ,starting_hunger/2
                  ,starting_luck/2
                  ,specific_ability/3
                  ,common_ability/3
                  ,innate_effect/3
                  ,starting_inventory/3
                  ]).

:-use_module(data(character)).
:-use_module(data(class)).

/** <module> Character Generation for Nests and Insects

*/

%!      category_symbol(?Category,?Symbol) is det.
%
%       The Symbol of a predicate generating features of a Category.
%
category_symbol(attribute,attributes).
category_symbol(condition,conditions).
category_symbol(attacks,attacks).
category_symbol(survival,survival_features).
category_symbol(specific_ability,specific_abilities).
category_symbol(common_ability,common_abilities).
category_symbol(effect,innate_effects).
category_symbol(inventory_item,starting_inventory).


%!      class_character(+Class,-Character) is det.
%
%       Generate a Character of the given Class.
%
class_character(C,[attributes-As
                  ,conditions-Cs
                  ,attacks-Ats
                  ,survival-Ss
                  ,specific_abilities-SAs
                  ,common_abilities-CAs
                  ,innate_effects-Es
                  ,starting_inventory-Is
                  ]):-
        attributes(C,As)
        ,conditions(C,Cs)
        ,attacks(C,Ats)
        ,survival_features(C,Ss)
        ,specific_abilities(C,SAs)
        ,common_abilities(C,CAs)
        ,innate_effects(C,Es)
        ,starting_inventory(C,Is).


%!      features(+Class,+Category,-Ratings) is det.
%
%       Generate a Class' Feature Ratings of a Category.
%
%       Class is the atomic identifier of a character class.
%
%       Category is an atomic identifier of a feature category, one of
%       [attribute, condition, survival, specific_ability,
%       common_ability, effect, inventory_item].
%
%       Category is cross-referenced to category_symbol/2 to find the
%       symbol of the predicate that generates featurs of the given
%       Category, as defined in this module.
%
%       Ratings is a list of key-value pairs where each key is a feature
%       in Category and each value is its rating for the given Class.
%
features(Id,F,Fs):-
        category_symbol(F,Cat)
        ,T =.. [Cat,Id,Fs]
        ,call(T).


%!      attributes(+Class,-Attributes) is det.
%
%       Generate a list of Attributes for a Class.
%
%       Class is a class identifier.
%
%       Attributes is a list of key-value pairs, where each key is the
%       identifier of an Attribute and each value is the rating of the
%       Class at that Attribute.
%
attributes(C,As):-
        findall(A-R
               ,attribute(C,A,R)
               ,As).


%!      conditions(+Class,-Conditions) is det.
%
%       Generate a list of Conditions for a Class.
%
%       Class is a class identifier.
%
%       Condition is a list of key-value pairs where keys are in [init,
%       tr,sr,wounds], for Initiative, Threat Rate, Survival Rate and
%       Wounds, respectively, and each value is the rating of the
%       corresponding key in the given Class.
%
conditions(C,[init-Init
             ,tr-TR
             ,sr-SR
             ,wounds-Ws
             ]):-
        initiative(C,Init)
        ,threat_rate(C,TR)
        ,survival_rate(C,SR)
        ,wounds(C,Ws).


%!      attacks(+Class,-Attacks) is det.
%
%       Collect the Class' Base and Special Attacks.
%
attacks(C,[BA,SA]):-
        C:base_attack(BA)
        ,C:special_attack(SA).


%!      survival_features(+Class,-Survival) is det.
%
%       A Class' Survival Features and their starting ratings.
%
survival_features(C,[Hs,Ls]):-
        starting_hunger(C,Hs)
        ,starting_luck(C,Ls).


%!      specific_abilities(+Class,-Abilities) is det.
%
%       Generate a list of Specific Abilities for a Class.
%
%       Class is a class identifier.
%
%       Abilities is a list of key-value pairs, where each key is the
%       identifier of a Specific Ability and each value is the rating of
%       the Class at that Ability.
%
specific_abilities(C,As):-
        findall(Id-R
                ,(specific_ability(Id,_)
                 ,(   class_specific_ability(C,Id,R)
                  ->  true
                  ;   R = 0
                  )
                 )
                ,As).



%!      common_abilities(+Class,-Abilities) is det.
%
%       Generate a list of Common Abilities for a Class.
%
%       Class is a class identifier.
%
%       Abilities is a list of key-value pairs, where each key is the
%       identifier of a Common Ability and each value is the rating of
%       the Class at that Ability.
%
common_abilities(C,As):-
        findall(Id-R
                ,(common_ability(Id,_)
                 ,common_ability(C,Id,R)
                 )
                ,As).



%!      innate_effects(+Class,-Effects) is det.
%
%       Generate a list of initial Effects for a Class.
%
%       Class is a class identifier.
%
%       Effects is a list of key-value pairs, where each key is the
%       identifier of an Effect and each value is the rating of the
%       Class at that Effect _at the beginning of the game_.
%
innate_effects(C,Es):-
        findall(Id-R
               ,(effect(Id,_)
                ,(   class_effects(C,Id,R)
                 ->  true
                 ;   R = 0
                 )
                )
               ,Es).



%!      starting_inventory(+Class,-Items) is det.
%
%       Generate a list of initial inventory Items for a Class.
%
%       Class is a class identifier.
%
%       Ietms is a list of key-value pairs, where each key is the
%       identifier of an Inventory Item and each value is the rating of
%       the Class at that Item _at the beginning of the game_.
%
starting_inventory(C,Is):-
        findall(I-R
               ,class_inventory(C,I,R)
               ,Is).



%!      attribute(+Class,+Attribute,-Rating) is det.
%
%       Generate a Class' Attribute Rating.
%
attribute(C,A,R):-
        A_ =.. [attribute,A,R_]
        ,C:A_
        ,(   class:attribute_modifiers(C,A,M)
         ->  R is R_ + M
         ;   R = R_
         ).


%!      initiative(+Class,-Initiative) is det.
%
%       Calculate the Initiative rating for a Class.
%
%       The Initiative formula is:
%
%       Speed + Smarts + Passions / 3
%
%       Initiative is the result of the formula modified by any class
%       modifiers.
%
initiative(C,Init):-
        attributes_mean(C,[speed,smarts,passions],Init_)
        ,modified_condition(C,init,Init_,Init).


%!      threat_rate(+Class,-Threat_Rate) is det.
%
%       Calculate Threat Rate for a Class.
%
%       The Threat Rate formula is:
%
%       Skill + Strength + Passions / 3
%
%       Threat_Rate is the result of the formula modified by any class
%       modifiers.
%
threat_rate(C,TR):-
        attributes_mean(C,[skill,strength,passions],TR_)
        ,modified_condition(C,tr,TR_,TR).


%!      survival_rate(+Class,-Survival_Rate) is det.
%
%       Calculate the Survival Rate for a Class.
%
%       The Survival Rate formula is:
%
%       Skill + Stamina + Smarts / 3
%
%       Survival_Rate is the result of the formula modified by any class
%       modifiers.
%
survival_rate(C,SR):-
        attributes_mean(C,[skill,stamina,smarts],SR_)
        ,modified_condition(C,sr,SR_,SR).


%!      wounds(+Class,-Wounds) is det.
%
%       Calculate the Wounds count of a Class.
%
wounds(C,Ws):-
        base_wounds(Ws_)
        ,modified_condition(C,wounds,Ws_,Ws).


%!      starting_hunger(+Class,-Hunger) is det.
%
%       Starting Hunger for a Class.
%
starting_hunger(beetle,hunger-'NONE'):-
        !.
starting_hunger(_,hunger-0).


%!      starting_luck(+Class,-Luck) is det.
%
%       The starting Luck of a Class.
%
%       So far, starting Luck is 11% for all classes.
%
%       Class is a class identifier, ignored.
%
%       Luck is always the pair luck-11.
%
starting_luck(_C,luck-11).


%!      specific_ability(+Class,+Ability,-Rating) is det.
%
%       Retrieve the Rating of a Specific Ability of a Class.
%
specific_ability(C,A,R):-
        class_specific_ability(C,A,R).


%!      common_ability(+Class,+Ability,-Rating) is det.
%
%       Calculate the Rating of a Class' Common Ability.
%
common_ability(C,A,R):-
        common_ability_factors(A,Fs)
        ,attributes_mean(C,Fs,R_)
        ,(   common_ability_modifiers(C,A,M)
         ->  R is R_ + M
         ;   R = R_
        ).


%!      innate_effects(+Class,+Effect,-Rating) is semidet.
%
%       Starting Rating of an Effect for a Class.
%
%       Only really used to blind Ants for now.
%
innate_effect(C,E,R):-
        class_effects(C,E,R).


%!      starting_inventory(+Class,+Item,-Rating) is semidet.
%
%       Starting Inventory Item and its Rating for a Class.
%
starting_inventory(C,I,R):-
        class_inventory(C,I,R).


%!      modified_condition(+Class,+Feature,+Rating,-Modified) is det.
%
%       Calculate the Modified rating of a Feature.
%
modified_condition(C,F,V,V_):-
        (   condition_modifiers(C,F,M)
        ->  V_ is V + M
        ;   V_ = V
        ).



%!      attributes_mean(+Class,+Attributes,-Mean) is det.
%
%       Calculate the mean of a list of a Class' Attributes.
%
%       Class is the identifier of a class.
%
%       Attributes is a list of Attribute identifiers.
%
%       Mean is the mean of the ratings of the listed Attributes for the
%       given Class.
%
attributes_mean(C,As,M):-
        attributes_ratings(C,As,Rs)
        ,mean(Rs,M).



%!      attributes_ratings(+Class,+Attributes,-Ratings) is det.
%
%       Collect the Ratings of a list of a Class' Attributes.
%
%       Class is the identifier of a class.
%
%       Attributes is a list of Attribute identifiers.
%
%       Ratings is a list of the ratings of the listed Attributes of the
%       given Class, ordered so that the i'th element of Ratings is the
%       rating of the i'th element of Attributes for Class.
%
attributes_ratings(C,As,Rs):-
        findall(R
               ,(member(A,As)
                ,attribute(C,A,R)
                )
               ,Rs).



%!      mean(+List,-Mean) is det.
%
%       Calculate the Mean of a List of numbers.
%
%       Mean is rounded up to the nearest integer.
%
mean(Ns,Ms):-
        sumlist(Ns,Ss)
        ,length(Ns,N)
        ,Ms_ is Ss / N
        ,Ms is ceil(Ms_).
