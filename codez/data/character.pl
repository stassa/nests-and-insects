:-module(character, [character_class/2
                    ,character_pronouns/2
                    ,attribute/2
                    ,condition/2
                    ,survival/2
                    ,specific_ability/2
                    ,common_ability/2
                    ,common_ability_factors/2
                    ,effect/2
                    ,inventory_item/3
                    ]).

/** <module> Features of Characters.

What makes up a character- Features, categorised as Attributes, Combat
Conditions, Survival Features, Abilities, Inventory and Effects.

This module defines predicates that represent information about the
names and descriptions of Character Features.

*/

%!      character_class(?Id,?Name) is semidet.
%
%       A Character Class.
%
character_class(ants,'Ants').
character_class(beetle,'Beetle').
character_class(ladybug,'Ladybug').
character_class(scorpion,'Scorpion').
character_class(spider,'Spider').
character_class(wasp,'Wasp').
character_class(template,'TEMPLATE').


%!      character_pronouns(?Id,?Pronouns) is semidet.
%
%       Preferred Pronouns of Characters of a Class.
%
character_pronouns(ants,[they,them,theirs]).
character_pronouns(beetle,[he,him,his]).
character_pronouns(ladybug,[she,her,hers]).
character_pronouns(scorpion,[he,him,his]).
character_pronouns(spider,[she,her,hers]).
character_pronouns(wasp,[me,'Me ME','MINE']).


%!      attribute(?Id,?Name) is semidet.
%
%       A character Attributes.
%
attribute(speed,'Speed').
attribute(skill,'Skill').
attribute(strength,'Strength').
attribute(stamina,'Stamina').
attribute(smarts,'Smarts').
attribute(charms,'Charms').
attribute(ken,'Ken').
attribute(passions,'Passions').


%!      condition(?Id,?Name) is semidet.
%
%       A character's Combat Conditions.
%
condition(init,'Initiative').
condition(tr,'Threat Rate').
condition(sr,'Survival Rate').
condition(wounds,'Wounds').


%!      survival(?Id, ?Name) is semidet.
%
%       A Character's Survival Features.
%
survival(hunger,'Hunger').
survival(luck,'Luck').


%!      specific_ability(?Id, ?Name) is semidet.
%
%       A Character's Specific Abilitis.
%
specific_ability(carapace,'Carapace').
specific_ability(flying,'Flying').
specific_ability(swarming,'Swarming').
specific_ability(venomous,'Venomous').
specific_ability(web,'Web Weaving').


%!      common_ability(?Id, ?Name) is semidet.
%
%       A Character's Common Abilities.
%
common_ability(construction,'Construction').
common_ability(eusociology,'Eusociology').
common_ability(exploration,'Exploration').
common_ability(foraging,'Foraging').
common_ability(healing,'Healing').
common_ability(hunting,'Hunting').
common_ability(leadership,'Leadership').
common_ability(perception,'Perception').
common_ability(signalling,'Signalling').
common_ability(sneaking,'Sneaking').


%!      common_ability_factors(?Ability,?Factors) is semidet.
%
%       Features factored in the calculation of a Common Ability.
%
common_ability_factors(construction,[skill,strength]).
common_ability_factors(eusociology,[smarts,ken]).
common_ability_factors(exploration,[speed,stamina]).
common_ability_factors(foraging,[ken,smarts]).
common_ability_factors(healing,[skill,ken]).
common_ability_factors(hunting,[strength,ken]).
common_ability_factors(leadership,[smarts,passions]).
common_ability_factors(perception,[speed,smarts]).
common_ability_factors(signalling,[charms,passions]).
common_ability_factors(sneaking,[skill,speed]).


%!      effect(?Id, ?Name) is semidet.
%
%       Effects a Character can be subject to.
%
effect(agony,'Agony').
effect(bleeding,'Bleeding').
effect(blind,'Blind').
effect(charmed,'Charmed').
effect(confused,'Confused').
effect(immobilised,'Immobilised').
effect(infected,'Infected').
effect(paralysed,'Paralysed').
effect(poisoned,'Poisoned').
effect(stunned,'Stunned').


%!      inventory(?Id,?Name) is semidet.
%
%       An Inventory item.
%
inventory_item(honeydew_restoration,'Honeydew of Restoration',[f]).
inventory_item(nectar_shielding,'Nectar of Shielding',[f]).
inventory_item(aphids,'Aphids',[f]).
inventory_item(crickets,'Crickets',[f]).
inventory_item(firefly,'Firefly',[f,l]).
inventory_item(nectar_healing,'Nectar of Healing',[f]).
inventory_item(empty,'',[]).

