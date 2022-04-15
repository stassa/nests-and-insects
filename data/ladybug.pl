:-module(ladybug, []).

ladybug:attribute(speed,35).
ladybug:attribute(skill,25).
ladybug:attribute(strength,45).
ladybug:attribute(stamina,50).
ladybug:attribute(smarts,35).
ladybug:attribute(charms,30).
ladybug:attribute(ken,30).
ladybug:attribute(passions,60).

ladybug:condition(init,0).
ladybug:condition(tr,0).
ladybug:condition(sr,0).
ladybug:condition(wounds,0).

ladybug:base_attack([name-'Pincer Attack'
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

ladybug:special_attack([name-'Stinger Attack'
                        ,keywords-['Piercing','Venomous','Lethal']
                        ,wounds-2
                        ,max_range-'Close'
                        ,hit-'Target in Agony: d60'
                        ,hit_2-['(Alt)','']
                        ,hit_3-['(Crit)','Target Killed']
                        ,miss_1-'Attacker Recoils'
                        ,miss_2-['(Alt)','']]
                       ).
