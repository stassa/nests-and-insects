:-module(spider, []).

spider:attribute(speed,50).
spider:attribute(skill,35).
spider:attribute(strength,35).
spider:attribute(stamina,25).
spider:attribute(smarts,65).
spider:attribute(charms,30).
spider:attribute(ken,45).
spider:attribute(passions,30).

spider:condition(init,0).
spider:condition(tr,0).
spider:condition(sr,0).
spider:condition(wounds,0).

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


spider:survival(hunger,0).
spider:survival(luck,[11]).

spider:specific_ability(carapace,0).
spider:specific_ability(flying,0).
spider:specific_ability(swarming,0).
spider:specific_ability(venomous,0).
spider:specific_ability(web,0).

spider:common_ability(construction,0).
spider:common_ability(eusociology,0).
spider:common_ability(exploration,0).
spider:common_ability(foraging,0).
spider:common_ability(healing,0).
spider:common_ability(hunting,0).
spider:common_ability(leadership,0).
spider:common_ability(perception,0).
spider:common_ability(signalling,0).
spider:common_ability(sneaking,0).

spider:effect(agony,0).
spider:effect(bleeding,0).
spider:effect(blind,0).
spider:effect(charmed,0).
spider:effect(confused,0).
spider:effect(immobilised,0).
spider:effect(infected,0).
spider:effect(paralysed,0).
spider:effect(poisoned,0).
spider:effect(stunned,0).


