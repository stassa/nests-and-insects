:-module(class, [base_wounds/1
                ,class_attributes/3
                ,condition_modifiers/3
                ,class_specific_ability/3
                ,common_ability_modifiers/3
                ,class_effects/3
                ,class_inventory/3
                ]).

:-use_module(scorpion).
:-use_module(spider).
:-use_module(wasp).

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


%!      class_attributes(?Class, ?Strong, ?Weak) is semidet.
%
%       The Strong and Weak Attributes of a Class.
%
% Scorpion
class_attributes(scorpion,passions,60).
class_attributes(scorpion,skill,25).
% Spider
class_attributes(spider,smarts,60).
class_attributes(spider,stamina,25).
% Wasp
class_attributes(wasp,speed,60).
class_attributes(wasp,smarts,25).


%!      condition_modifiers(?Class, ?Attribute, ?Modifier) is semidet.
%
%       Attribute Modifiers by Class.
%
%       @tbd These are modifiers for Attributes _and_ Conditions.
%
% Scorpion
condition_modifiers(scorpion,stamina,+5).
condition_modifiers(scorpion,init,+5).
condition_modifiers(scorpion,tr,+5).
condition_modifiers(scorpion,sr,+5).
condition_modifiers(scorpion,wounds,+1).
% Spider
condition_modifiers(spider,smarts,+5).
condition_modifiers(spider,init,+10).
condition_modifiers(spider,tr,-5).
condition_modifiers(spider,sr,-5).
condition_modifiers(spider,wounds,+0).
% Wasp
condition_modifiers(wasp,passions,+5).
condition_modifiers(wasp,init,+15).
condition_modifiers(wasp,tr,+10).
condition_modifiers(wasp,sr,+0).
condition_modifiers(wasp,wounds,+1).



%!      class_specific_ability(?Class, ?Ability, ?Rating) is semidet.
%
%       A Class' Rating in a Specific Ability.
%
% Scorpion
class_specific_ability(scorpion,carapace,50).
class_specific_ability(scorpion,venomous,90).
% Spider
class_specific_ability(spider,venomous,75).
class_specific_ability(spider,web_weaving,90).
% Wasp
class_specific_ability(wasp,flying,60).
class_specific_ability(wasp,swarming,25).
class_specific_ability(wasp,venomous,80).


%!      class_specific_ability(?Class, ?Ability, ?Modifier) is semidet.
%
%       Common Ability modifiers by Class.
%
% Scorpion
common_ability_modifiers(scorpion,leadership,+10).
common_ability_modifiers(scorpion,hunting,+10).
common_ability_modifiers(scorpion,signalling,+10).
% Spider
common_ability_modifiers(spider,construction,+25).
common_ability_modifiers(spider,eusociology,-30).
common_ability_modifiers(spider,foraging,-20).
common_ability_modifiers(spider,healing,+10).
common_ability_modifiers(spider,hunting,+25).
common_ability_modifiers(spider,perception,+5).
common_ability_modifiers(spider,sneaking,+5).
% Wasp
common_ability_modifiers(wasp,construction,+10).
common_ability_modifiers(wasp,eusociology,+30).
common_ability_modifiers(wasp,foraging,+35).
common_ability_modifiers(wasp,hunting,-20).
common_ability_modifiers(wasp,signalling,+30).


%!      class_effects(?Class, ?Effect, ?Rating) is semidet.
%
%       A Class' starting Effect Rating.
%
class_effects(ants,blind,100).


%!      class_inventory(?Class, ?Item, ?Rating) is semidet.
%
%       Starting Inventory Items by Class.
%
class_inventory(ants,honeydew_restoration,45).
class_inventory(beetle,nectar_shielding,45).
class_inventory(ladybug,aphids,45).
class_inventory(scorpion,crickets,45).
class_inventory(spider,firefly,45).
class_inventory(wasp,nectar_healing,45).

