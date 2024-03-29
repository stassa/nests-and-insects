:-module(scorpion, []).

scorpion:attribute(speed,35).
scorpion:attribute(skill,25).
scorpion:attribute(strength,45).
scorpion:attribute(stamina,50).
scorpion:attribute(smarts,35).
scorpion:attribute(charms,30).
scorpion:attribute(ken,30).
scorpion:attribute(passions,60).

scorpion:attribute_modifiers(stamina,+5).

scorpion:condition_modifiers(init,+5).
scorpion:condition_modifiers(tr,+5).
scorpion:condition_modifiers(sr,+5).
scorpion:condition_modifiers(wounds,+1).

scorpion:class_specific_ability(carapace,50).
scorpion:class_specific_ability(venomous,90).

scorpion:common_ability_modifiers(leadership,+10).
scorpion:common_ability_modifiers(hunting,+10).
scorpion:common_ability_modifiers(signalling,+10).

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
                        ,hit_2-['(Crit)','Target Killed']
                        ,hit_3-['(Crit,Rem)','(Swarms take 2 Wounds)']
                        ,miss_1-'Attacker Recoils'
                        ,miss_2-['(Alt)','']]
                       ).

scorpion:class_inventory(crickets,45).
