" Vim syntax file for Nests & Insects rulebooks (raw and rendered)
"
" To use, copy this file into your $VIMRUNTIME/syntax/ and run the command:
"
" :syntax enable
"
" Language: Nests & Insects rulebook
" Maintainer: me Me ME!
" Last Change: 11/05/2022

:syntax clear

" Case-sensitive matching
syntax case match

" Keywords.
syntax keyword nniType Chapter Section
syntax match nniType /\(Table\|Figure\|Note\|Example\)\(:\)*\s*\(\d\+\)*/

" Special keywrods
syntax match nniSpecial /Degree\(s\)* of Success\( \(and\|or\) Failure\)*/
syntax keyword nniSpecial Outcome
syntax match nniSpecial /\(Critical \|Complete \|Partial \)*\(Success\|Failure\)/

" Constants
" A little haphazardly defined, as I found them in the text and not by very clear
" categories
syntax match nniConstant /Nests & Insects/
syntax match nniConstant /\(Game Queen\|GQ\)/
syntax keyword nniConstant Roguelike CRPG CRPGs TTRPG TTRPGs DoS fd100
syntax keyword nniConstant Intent Action Actions Effort Resistance
syntax match nniConstant /\(Static\|Dramatic\) Modifier\(s\)*/
syntax keyword nniConstant Minigame Minigames Entity Entities
syntax match nniConstant /\(Combat\|Survival and Exploration\|Hunting and Foraging\|Resting and Healing\)/
syntax match nniConstant /\(Qualification\|Quantification\|\(Contested \)*Boolean\|\) Roll/
syntax keyword nniConstant Qualification Modification Quantification
syntax keyword nniConstant Base Special Attack Attacks Hit Miss Manoeuver Manoeuvers
syntax keyword nniConstant Variable Variables Feature Features Attribute Attributes Ability Abilities Condition Effect XP
syntax keyword nniConstant Variables Features Attributes Ability Abilities Conditions Effects
syntax keyword nniConstant Retreating Recoiling Holding Advancing Charging
syntax keyword nniConstant Retreats Recoils Holds Advances Charges
syntax keyword nniConstant Retreat Recoil Hold Advance Charge
syntax match nniConstant /\(Area\(s\)*\|Class Header\|Attributes\|Combat Stats\|Survival\|Abilities\)/
syntax match nniConstant /\(Disposition\|Specific Abilities\|Common Abilities\|Effects\|Inventory\)/
syntax match nniConstant /\(Name:\|Keywords:\|Damage:\|Max. Range:\|Hit\(:\)*\|Miss\(:\)*\| (Alt):\| (Add):\| (Crit):\| (Rem)\)/
syntax match nniConstant /\cdecile \(dice\|die\)/
syntax match nniConstant /d\d\+/
syntax match nniConstant /\d\+%/
syntax match nniConstant /\d/
syntax keyword nniConstant Job Jobs Nest Level Levels Room Rooms Event Events
syntax keyword nniConstant Passage Darkness
syntax keyword nniConstant Encounter Item Items Obstacle Obstacles Infection
syntax keyword nniConstant Room Rooms Shortuct Shortcuts Resource Resources Secret Secrets Shelter
syntax keyword nniConstant Cost Costs Risk Risks Reward Rewards
syntax keyword nniConstant Combatant Combatants Round Order Turn Side Sides
syntax match nniConstant /\(Tactical\|Support\|Suppress\|Manoeuver\) Action\(s\)*/
syntax keyword nniConstant Explore Hunt Forage Food

" Feature names
syntax keyword nniIdentifier Speed Skill Strength Stamina Smarts Charms Ken Passions
syntax match nniIdentifier /\(Initiative\|Threat Rate\|Survival Rate\|Wounds\)/
syntax keyword nniIdentifier Hunger Luck Starving
syntax keyword nniIdentifier Carapace Flying Swarming Venomous
syntax match nniIdentifier /Web Weaving/
syntax keyword nniIdentifier Construction Eusociology Exploration Foraging Healing
syntax keyword nniIdentifier Hunting Leadership Perception Signalling Sneaking
syntax keyword nniIdentifier Agony Bleeding Blind Charmed Confused
syntax keyword nniIdentifier Immobilised Infected Paralysed Poisoned Stunned Edible

"Class and enemy names
syntax keyword nniSpecial Ant Ants Beetle Ladybug Scorpion Spider Wasp Termite Termites
syntax keyword nniSpecial King Queen Soldier Worker Bee Bees Termite Termites
syntax keyword nniSpecial ANTS BEETLE LADYBUG SCORPION SPIDER WASP

" Layout commands.
syntax match nniPreProc /\\\(chapter\|section\|subsection\|subsubsection\|paragraph\).*/
syntax match nniPreProc /\\\(begin\|end\)\**{.\{-}}\(\[.\+\]\)*/
syntax match nniPreProc /\\label.*/
syntax match nniPreProc /\\newpage/
syntax match nniPreProc /\\charsheet\[.\+\]/

" Comments
syntax region  nniComment start="/\*" end="\*/"
syntax match nniComment  /^%.*/

" Common errors
syntax keyword nniError Dos

" Try to ignore
syntax match nnIgnore /A Roguelike Tabletop Roleplaying Game/

" Emphasise and underline
syntax match nniUnderlined /_[a-zA-Z0-9 ]\+_/

hi def link nniType       Type
hi def link nniIdentifier Identifier
hi def link nniConstant   Constant
hi def link nniPreProc    PreProc
hi def link nniComment    Comment
hi def link nniCComment   Comment
hi def link nniUnderlined Underlined
hi def link nniSpecial    Special
hi def link nniError      Error
hi def link nniIgnore     Ignore
