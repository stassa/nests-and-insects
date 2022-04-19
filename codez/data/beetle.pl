:-module(beetle, []).

beetle:attribute(speed,25).
beetle:attribute(skill,45).
beetle:attribute(strength,50).
beetle:attribute(stamina,60).
beetle:attribute(smarts,35).
beetle:attribute(charms,30).
beetle:attribute(ken,30).
beetle:attribute(passions,35).

beetle:base_attack([name-'Horn Attack'
                     ,keywords-['Crushing','Impact']
                     ,wounds-1
                     ,max_range-'Close'
                     ,hit-'Target Stunned: d60'
                     ,hit_2-['(Alt)','']
                     ,hit_3-['(Add)','']
                     ,miss_1-'Attacker Recoils'
                     ,miss_2-['(Alt)','']
                     ]
                    ).

beetle:special_attack([name-'Flying Horn Attack'
                        ,keywords-['Crushing','Impact']
                        ,wounds-2
                        ,max_range-'Medium (Area)'
                        ,hit-'Targets Stunned: ld80'
                        ,hit_2-['(Add)','Targets Recoil']
                        ,hit_3-['(Crit)','Targets take +2 Wounds']
                        ,miss_1-'Attacker Stunned: ld80'
                        ,miss_2-['(Crit)','Attacker Bleeding: d60']]
                       ).
