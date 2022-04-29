:-module(scorpion, []).

scorpion:attribute(speed,35).
scorpion:attribute(skill,25).
scorpion:attribute(strength,45).
scorpion:attribute(stamina,50).
scorpion:attribute(smarts,35).
scorpion:attribute(charms,30).
scorpion:attribute(ken,30).
scorpion:attribute(passions,60).

scorpion:condition(init,0).
scorpion:condition(tr,0).
scorpion:condition(sr,0).
scorpion:condition(wounds,0).

scorpion:base_attack([name-'Pincer Attack'
                     ,keywords-['Crushing','Shredding']
                     ,damage-'1 Wound'
                     ,max_range-'Close'
                     ,hit-'Target Recoils'
                     ,hit_2-['(Alt)','']
                     ,hit_3-['(Add)','']
                     ,miss_1-'Attacker Recoils'
                     ,miss_2-['(Alt)','']
                     ]
                    ).

scorpion:special_attack([name-'Stinger Attack'
                        ,keywords-['Piercing','Venomous','Lethal']
                        ,damage-'2 Wounds'
                        ,max_range-'Close'
                        ,hit-'Target in Agony: d60'
                        ,hit_2-['(Alt)','']
                        ,hit_3-['(Crit)','Target Killed']
                        ,miss_1-'Attacker Recoils'
                        ,miss_2-['(Alt)','']]
                       ).


scorpion:survival(hunger,0).
scorpion:survival(luck,[11]).

scorpion:specific_ability(carapace,0).
scorpion:specific_ability(flying,0).
scorpion:specific_ability(swarming,0).
scorpion:specific_ability(venomous,0).
scorpion:specific_ability(web,0).

scorpion:common_ability(construction,0).
scorpion:common_ability(eusociology,0).
scorpion:common_ability(exploration,0).
scorpion:common_ability(foraging,0).
scorpion:common_ability(healing,0).
scorpion:common_ability(hunting,0).
scorpion:common_ability(leadership,0).
scorpion:common_ability(perception,0).
scorpion:common_ability(signalling,0).
scorpion:common_ability(sneaking,0).

scorpion:effect(agony,0).
scorpion:effect(bleeding,0).
scorpion:effect(blind,0).
scorpion:effect(charmed,0).
scorpion:effect(confused,0).
scorpion:effect(immobilised,0).
scorpion:effect(infected,0).
scorpion:effect(paralysed,0).
scorpion:effect(poisoned,0).
scorpion:effect(stunned,0).
