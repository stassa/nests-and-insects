:-module(beetle, []).

beetle:attribute(speed,25).
beetle:attribute(skill,45).
beetle:attribute(strength,50).
beetle:attribute(stamina,60).
beetle:attribute(smarts,35).
beetle:attribute(charms,30).
beetle:attribute(ken,30).
beetle:attribute(passions,35).

beetle:attribute_modifiers(strength,+5).

beetle:condition_modifiers(init,-10).
beetle:condition_modifiers(sr,+15).
beetle:condition_modifiers(wounds,+2).

beetle:class_specific_ability(carapace,60).
beetle:class_specific_ability(flying,35).

beetle:common_ability_modifiers(construction,+15).
beetle:common_ability_modifiers(exploration,-5).
beetle:common_ability_modifiers(foraging,+10).
beetle:common_ability_modifiers(healing,+10).
beetle:common_ability_modifiers(hunting,-15).

beetle:base_attack([name-'Bite Attack'
                     ,keywords-['Crushing']
                     ,damage-'2 Wounds'
                     ,max_range-'Close'
                     ,hit-'Target Bleeding: d40'
                     ,hit_2-['(Alt)','']
                     ,hit_3-['(Add)','']
                     ,miss_1-'Attacker Recoils'
                     ,miss_2-['(Alt)','']
                     ]
                    ).

beetle:special_attack([name-'Flying Horn Attack'
                        ,keywords-['Impact']
                        ,damage-'4 Wounds'
                        ,max_range-'Medium (Area)'
                        ,hit-'Targets Stunned: d60'
                        ,hit_2-['(Crit)','Damage: 6 Wounds']
                        ,hit_3-['(Crit Add)','Targets Bleeding: d60']
                        ,miss_1-'Attacker Stunned: d60'
                        ,miss_2-['(Crit)','Attacker Bleeding: d60']]
                       ).
                       
beetle:class_inventory(nectar_shielding,45).
