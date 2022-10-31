:-module(template, []).

template:attribute(speed,0).
template:attribute(skill,0).
template:attribute(strength,0).
template:attribute(stamina,0).
template:attribute(smarts,0).
template:attribute(charms,0).
template:attribute(ken,0).
template:attribute(passions,0).

template:attribute_modifiers(_,0).

template:condition_modifiers(_,0).

template:class_specific_ability(_,0).

template:common_ability_modifiers(_,0).

template:base_attack([name-''
                     ,keywords-['']
                     ,damage-''
                     ,max_range-''
                     ,hit-''
                     ,hit_2-['(Alt)','']
                     ,hit_3-['(Add)','']
                     ,miss_1-''
                     ,miss_2-['(Alt)','']
                     ]
                    ).

template:special_attack([name-''
                        ,keywords-['']
                        ,damage-''
                        ,max_range-''
                        ,hit-''
                        ,hit_2-['(Alt)','']
                        ,hit_3-['(Crit)','']
                        ,miss_1-''
                        ,miss_2-['(Alt)','']]
                       ).

template:class_inventory(empty,'').
