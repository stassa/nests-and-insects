:-module(ants, []).

ants:attribute(speed,35).
ants:attribute(skill,25).
ants:attribute(strength,45).
ants:attribute(stamina,50).
ants:attribute(smarts,35).
ants:attribute(charms,30).
ants:attribute(ken,30).
ants:attribute(passions,60).

ants:condition(init,0).
ants:condition(tr,0).
ants:condition(sr,0).
ants:condition(wounds,0).

ants:base_attack([name-'Pincer Attack'
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

ants:special_attack([name-'Stinger Attack'
                        ,keywords-['Piercing','Venomous','Lethal']
                        ,wounds-2
                        ,max_range-'Close'
                        ,hit-'Target in Agony: d60'
                        ,hit_2-['(Alt)','']
                        ,hit_3-['(Crit)','Target Killed']
                        ,miss_1-'Attacker Recoils'
                        ,miss_2-['(Alt)','']]
                       ).
