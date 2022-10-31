:-module(spider, []).

spider:attribute(speed,50).
spider:attribute(skill,35).
spider:attribute(strength,35).
spider:attribute(stamina,25).
spider:attribute(smarts,65).
spider:attribute(charms,30).
spider:attribute(ken,45).
spider:attribute(passions,30).

spider:attribute_modifiers(smarts,+5).

spider:condition_modifiers(init,+10).
spider:condition_modifiers(tr,-5).
spider:condition_modifiers(sr,-5).

spider:class_specific_ability(venomous,75).
spider:class_specific_ability(web_weaving,90).

spider:common_ability_modifiers(construction,+25).
spider:common_ability_modifiers(eusociology,-30).
spider:common_ability_modifiers(foraging,-20).
spider:common_ability_modifiers(healing,+10).
spider:common_ability_modifiers(hunting,+25).
spider:common_ability_modifiers(perception,+5).
spider:common_ability_modifiers(sneaking,+5).

spider:base_attack([name-'Web Attack'
                     ,keywords-['Entangling','Healing']
                     ,damage-'N/A'
                     ,max_range-'Medium'
                     ,hit-'Enemy Immobilised: d60'
                     ,hit_2-['(Alt)','Ally Stabilised']
                     ,hit_3-['(Rem)','(Stabilised: Remove Bleeding)']
                     ,miss_1-''
                     ,miss_2-['(Alt)','']
                     ]
                    ).

spider:special_attack([name-'Bite Attack'
                        ,keywords-['Piercing','Venomous','Paralysing']
                        ,damage-'1 Wound'
                        ,max_range-'Close'
                        ,hit-'Enemy Paralysed: d80'
                        ,hit_2-['(Alt)','Ally Healed']
                        ,hit_3-['(Rem)','(Healed: Remove one Wound)']
                        ,miss_1-'Attacker Recoils'
                        ,miss_2-['(Alt)','']]
                       ).

spider:class_inventory(firefly,45).
