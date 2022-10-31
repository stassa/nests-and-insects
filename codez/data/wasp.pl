:-module(wasp, []).

wasp:attribute(speed,60).
wasp:attribute(skill,45).
wasp:attribute(strength,50).
wasp:attribute(stamina,35).
wasp:attribute(smarts,25).
wasp:attribute(charms,30).
wasp:attribute(ken,30).
wasp:attribute(passions,40).

wasp:attribute_modifiers(passions,+5).

wasp:condition_modifiers(init,+15).
wasp:condition_modifiers(tr,+10).
wasp:condition_modifiers(wounds,+1).

wasp:class_specific_ability(flying,60).
wasp:class_specific_ability(swaming,25).
wasp:class_specific_ability(venomous,80).

wasp:common_ability_modifiers(construction,+10).
wasp:common_ability_modifiers(eusociology,+30).
wasp:common_ability_modifiers(foraging,+35).
wasp:common_ability_modifiers(hunting,-20).
wasp:common_ability_modifiers(signalling,+30).

wasp:base_attack([name-'Bite Attack'
                     ,keywords-['Shredding']
                     ,damage-'2 Wounds'
                     ,max_range-'Close'
                     ,hit-'Target Recoils'
                     ,hit_2-['(Alt)','']
                     ,hit_3-['(Add)','']
                     ,miss_1-'Attacker Recoils'
                     ,miss_2-['(Alt)','']
                     ]
                    ).

wasp:special_attack([name-'Stinger Attack'
                        ,keywords-['Piercing','Venomous','Lethal']
                        ,damage-'2 Wounds'
                        ,max_range-'Close'
                        ,hit-'Target Poisoned: d60'
                        ,hit_2-['(Crit)','Target Killed']
                        ,hit_3-['(Crit,Rem)','(Swarms take 2 Wounds)']
                        ,miss_1-'Attacker Stunned: d60'
                        ,miss_2-['(Alt)','']]
                       ).

wasp:class_inventory(nectar_healing,45).
