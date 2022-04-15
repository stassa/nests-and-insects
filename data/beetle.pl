:-module(beetle, []).

beetle:attribute(speed,35).
beetle:attribute(skill,25).
beetle:attribute(strength,45).
beetle:attribute(stamina,50).
beetle:attribute(smarts,35).
beetle:attribute(charms,30).
beetle:attribute(ken,30).
beetle:attribute(passions,60).

beetle:condition(init,0).
beetle:condition(tr,0).
beetle:condition(sr,0).
beetle:condition(wounds,0).

beetle:base_attack([name-'Pincer Attack'
                     ,keywords-['Crushing','Shredding']
                     ,wounds-1
                     ,max_range-'Close'
                     ,hit-'Target Recoils'
                     ,hit_2-['(Alt)','']
                     ,hit_3-['(Add)','']
                     ,miss_1-'Attacker Recoils'
                     ,miss_2-['(Alt)','']
                     ]
                    ).

beetle:special_attack([name-'Stinger Attack'
                        ,keywords-['Piercing','Venomous','Lethal']
                        ,wounds-2
                        ,max_range-'Close'
                        ,hit-'Target in Agony: d60'
                        ,hit_2-['(Alt)','']
                        ,hit_3-['(Crit)','Target Killed']
                        ,miss_1-'Attacker Recoils'
                        ,miss_2-['(Alt)','']]
                       ).
