:-module(wasp, []).

wasp:attribute(speed,60).
wasp:attribute(skill,45).
wasp:attribute(strength,50).
wasp:attribute(stamina,35).
wasp:attribute(smarts,25).
wasp:attribute(charms,30).
wasp:attribute(ken,30).
wasp:attribute(passions,40).

wasp:base_attack([name-'Bite Attack'
                     ,keywords-['Shredding']
                     ,wounds-1
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
                        ,wounds-2
                        ,max_range-'Close'
                        ,hit-'Target Poisoned: d60'
                        ,hit_2-['(Alt)','']
                        ,hit_3-['(Crit)','Target Killed']
                        ,miss_1-'Attacker Stunned: ld80'
                        ,miss_2-['(Alt)','']]
                       ).
