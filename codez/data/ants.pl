:-module(ants, []).

ants:attribute(speed,35).
ants:attribute(skill,50).
ants:attribute(strength,30).
ants:attribute(stamina,30).
ants:attribute(smarts,35).
ants:attribute(charms,45).
ants:attribute(ken,60).
ants:attribute(passions,25).

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
                        ,keywords-['Crushing','Shredding']
                        ,damage-'2 Wounds per Ant'
                        ,max_range-'Close'
                        ,hit-'Target Poisoned: d60'
                        ,hit_2-['(Alt)','']
                        ,hit_3-['(Crit)','Target Immobilised: ld80']
                        ,miss_1-'Attacker Recoils'
                        ,miss_2-['(Alt)','']]
                       ).
