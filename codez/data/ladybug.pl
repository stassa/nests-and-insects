:-module(ladybug, []).

ladybug:attribute(speed,45).
ladybug:attribute(skill,60).
ladybug:attribute(strength,50).
ladybug:attribute(stamina,35).
ladybug:attribute(smarts,30).
ladybug:attribute(charms,25).
ladybug:attribute(ken,30).
ladybug:attribute(passions,35).

ladybug:attribute_modifiers(ken,+5).

ladybug:condition_modifiers(init,+5).
ladybug:condition_modifiers(tr,+5).
ladybug:condition_modifiers(sr,+5).

ladybug:class_specific_ability(carapace,45).
ladybug:class_specific_ability(flying,60).

ladybug:common_ability_modifiers(exploration,+10).
ladybug:common_ability_modifiers(hunting,+10).
ladybug:common_ability_modifiers(signalling,+10).

ladybug:base_attack([name-'Bite Attack'
                     ,keywords-['Piercing','Shredding']
                     ,damage-'1 Wound'
                     ,max_range-'Close'
                     ,hit-'Target Bleeding: d40'
                     ,hit_2-['(Alt)','']
                     ,hit_3-['(Add)','']
                     ,miss_1-'Attacker Recoils'
                     ,miss_2-['(Alt)','']
                     ]
                    ).

ladybug:special_attack([name-'Furious Bite Attack'
                        ,keywords-['Piercing','Shredding']
                        ,damage-'2 Wounds'
                        ,max_range-'Close'
                        ,hit-'Target Bleeding: d60'
                        ,hit_2-['(Alt)','']
                        ,hit_3-['(Crit)','Attack Again']
                        ,miss_1-'Attacker Stunned: d60'
                        ,miss_2-['(Alt)','']]
                       ).

ladybug:class_inventory(aphids,45).
