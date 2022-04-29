:-module(beetle, []).

beetle:attribute(speed,25).
beetle:attribute(skill,45).
beetle:attribute(strength,50).
beetle:attribute(stamina,60).
beetle:attribute(smarts,35).
beetle:attribute(charms,30).
beetle:attribute(ken,30).
beetle:attribute(passions,35).

beetle:base_attack([name-'Bite Attack'
                     ,keywords-['Crushing']
                     ,damage-'2 Wounds'
                     ,max_range-'Close'
                     ,hit-'Target Bleeding: ld40'
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
