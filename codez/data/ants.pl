:-module(ants, []).

ants:attribute(speed,35).
ants:attribute(skill,50).
ants:attribute(strength,30).
ants:attribute(stamina,30).
ants:attribute(smarts,35).
ants:attribute(charms,45).
ants:attribute(ken,60).
ants:attribute(passions,25).

ants:attribute_modifiers(skill,+5).

ants:condition_modifiers(wounds,+7).

ants:class_specific_ability(swarming,90).

ants:common_ability_modifiers(construction,+10).
ants:common_ability_modifiers(eusociology,+15).
ants:common_ability_modifiers(exploration,+10).
ants:common_ability_modifiers(foraging,+10).
ants:common_ability_modifiers(healing,-10).
ants:common_ability_modifiers(hunting,-20).
ants:common_ability_modifiers(signalling,+15).
ants:common_ability_modifiers(sneaking,-15).

ants:base_attack([name-'Bite Attack'
                     ,keywords-['Crushing','Shredding']
                     ,damage-'1 Wound per Ant'
                     ,max_range-'Close'
                     ,hit-'Target Immobilised: d40'
                     ,hit_2-['(Alt)','']
                     ,hit_3-['(Add)','']
                     ,miss_1-'Attacker Recoils'
                     ,miss_2-['(Alt)','']
                     ]
                    ).

ants:special_attack([name-'Swarming Attack'
                        ,keywords-['Crushing','Noxious','Shredding']
                        ,damage-'2 Wounds per Ant'
                        ,max_range-'Close'
                        ,hit-'Target Immobilised: d60'
                        ,hit_2-['(Alt)','']
                        ,hit_3-['(Crit)','Target Poisoned: d80']
                        ,miss_1-'Attacker Recoils'
                        ,miss_2-['(Alt)','']]
                       ).

ants:class_inventory(honeydew_restoration,45).
