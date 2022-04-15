:-module(class, [base_wounds/1
                ,class_attributes/3
                ,attribute_modifiers/3
                ,condition_modifiers/3
                ,class_specific_ability/3
                ,common_ability_modifiers/3
                ,class_effects/3
                ,class_inventory/3
                ]).

:-use_module(ants).
:-use_module(beetle).
:-use_module(ladybug).
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
% Ants
class_attributes(ants,ken,60).
class_attributes(ants,passions,25).
% Beetle
class_attributes(beetle,strength,60).
class_attributes(beetle,speed,25).
% Ladybug
class_attributes(ladybug,skill,60).
class_attributes(ladybug,charms,25).
% Scorpion
class_attributes(scorpion,passions,60).
class_attributes(scorpion,skill,25).
% Spider
class_attributes(spider,smarts,60).
class_attributes(spider,stamina,25).
% Wasp
class_attributes(wasp,speed,60).
class_attributes(wasp,smarts,25).


%!      attribute_modifiers(?Class,?Attribute,?Modifier) is semidet.
%
%       Atribute Modifiers by Class.
%
attribute_modifiers(ants,skill,+5).
attribute_modifiers(beetle,stamina,+10).
attribute_modifiers(ladybug,ken,+5).
attribute_modifiers(scorpion,stamina,+5).
attribute_modifiers(spider,smarts,+5).
attribute_modifiers(wasp,passions,+5).


%!      condition_modifiers(?Class, ?Attribute, ?Modifier) is semidet.
%
%       Condition Modifiers by Class.
%
% Ants
condition_modifiers(ants,init,+0).
condition_modifiers(ants,tr,+0).
condition_modifiers(ants,sr,+0).
condition_modifiers(ants,wounds,+7).
% Beetle
condition_modifiers(beetle,init,-10).
condition_modifiers(beetle,tr,+0).
condition_modifiers(beetle,sr,+15).
condition_modifiers(beetle,wounds,+2).
% Ladybug
condition_modifiers(ladybug,init,+5).
condition_modifiers(ladybug,tr,+5).
condition_modifiers(ladybug,sr,+5).
condition_modifiers(ladybug,wounds,+0).
% Scorpion
condition_modifiers(scorpion,init,+5).
condition_modifiers(scorpion,tr,+5).
condition_modifiers(scorpion,sr,+5).
condition_modifiers(scorpion,wounds,+1).
% Spider
condition_modifiers(spider,init,+10).
condition_modifiers(spider,tr,-5).
condition_modifiers(spider,sr,-5).
condition_modifiers(spider,wounds,+0).
% Wasp
condition_modifiers(wasp,init,+15).
condition_modifiers(wasp,tr,+10).
condition_modifiers(wasp,sr,+0).
condition_modifiers(wasp,wounds,+1).



%!      class_specific_ability(?Class, ?Ability, ?Rating) is semidet.
%
%       A Class' Rating in a Specific Ability.
%
% Ants
class_specific_ability(ants,swarming,90).
% Beetle
class_specific_ability(beetle,carapace,60).
class_specific_ability(beetle,flying,15).
% Ladybug
class_specific_ability(ladybug,carapace,45).
class_specific_ability(ladybug,flying,60).
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
% Ants
common_ability_modifiers(ants,construction,+10).
common_ability_modifiers(ants,eusociology,+10).
common_ability_modifiers(ants,foraging,+10).
% Beetle
common_ability_modifiers(beetle,construction,+10).
common_ability_modifiers(beetle,foraging,+10).
common_ability_modifiers(beetle,healing,+10).
% Ladybug
common_ability_modifiers(ladybug,exploration,+10).
common_ability_modifiers(ladybug,hunting,+10).
common_ability_modifiers(ladybug,signalling,+10).
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

