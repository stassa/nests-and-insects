" Vim syntax file for Nests & Insects rulebooks (raw and rendered)
"
" To use, copy this file into your $VIMRUNTIME/syntax/ and run the command:
"
" :syntax enable
"
" Language: Nests & Insects rulebook
" Maintainer: me Me ME!
" Last Change: 22/05/2022
"
" This syntax file is a bloody mess. Because regexes and because of the way
" priority works between keywords, matches and regions, with similar elements.
" Because of priority, in particular, my feeble attempts to categorise syntax
" groups in comments above them are doomed to fail. Inevitably, a keyword
" defined in one line must be duplicated below it in a more complex expression
" to avoid the first definition taking priority and messing up the complex
" expression. As a for instance, if I want "Quantification" and "Quantification
" Die" to have different syntax highlighting, "Quantification Die" must come
" after "Quantification" because the last definition takes priority... unless
" the first one is a keyword, in which case it takes priority. It works great
" but it looks awful.

:syntax clear

" Case-sensitive matching
syntax case match

" Keywords.
syntax keyword nniType Chapter Chapters Section Sections
syntax match nniType /\(Table\|Figure\|Note\|Example\)\(:\)*\s*\(\d\+\)*/

" Constants
syntax match nniConstant /Nest\(s*\)/
syntax match nniConstant /\(Roguelike\|Job\|Room\|Level\)s*/
syntax match nniConstant /\([Mm]inigame\|Base\|Special\|Attack\|Action\|Disposition\( Track\)*\)s*/
syntax match nniSpecial /\(Advance\|Recoil\)/
syntax match nniConstant /\(Match\|Beat\|Retreating\|Holding\|Recoiling\|Advancing\|Charging\|Targets*\|Attackers*\)/
syntax match nniConstant /\(Matches\|Beats\|Retreats\|Holds\|Recoils\|Advances\|Charges\)/
syntax match nniConstant /ones\|tens/
syntax match nniConstant /\(\cdecile\|one\|ten\)s* di\(c\)*e/
syntax match nniConstant /\(Nethack\|Angband\|Moria\|Ancient Domains of Mystery\|Diablo\|Darkest Dungeon\|Dungeons & Dragons\)/
syntax match nniConstant /\(Round\|Turn\|Step\|Order\|Side\)\(s\)*/
syntax match nniConstant /\(Tactical\|Resist\|Manoeuver\|Support\)\( Action\(s\)*\)*/
syntax match nniConstant '\u\{2,}s\{0,1}'
syntax match nniConstant /\[[CFHLMS]\]/
syntax match nniConstant /\d\+\(%\)*/
syntax keyword nniConstant True False

" Feature names, decile dice and minigames.
syntax keyword nniIdentifier Speed Skill Strength Stamina Smarts Charms Ken Passions
syntax keyword nniIdentifier Carapace Flying Swarming Venomous
syntax match nniIdentifier /Web Weaving/
syntax keyword nniIdentifier Construction Eusociology Exploration Foraging Healing
syntax keyword nniIdentifier Leadership Perception Signalling Sneaking
syntax keyword nniIdentifier Agony Bleeding Blind Charmed Confused
syntax keyword nniIdentifier Immobilised Infected Paralysed Poisoned Stunned Edible
syntax match nniIdentifier /d\d\+/
syntax match nniIdentifier /Hunting/

syntax match nniConstant /\(Class\( Header\)*\|Feature\|Character Sheet\|Combat Stat\|Survival\)s*/
syntax match nniIdentifier /\(Hunger\|Luck\|Starving\)/

"Class and enemy names
syntax match nniSpecial /\(Ant\|Beetle\|Ladybug\|Scorpion\|Spider\|Wasp\|Termite\)\(s\)*/
syntax match nniSpecial /\(King\|Soldier\|Worker\|Bee\|Termite\)\(s\)*/
syntax keyword nniSpecial Minor Major Autothyte Autothytes Nasute Nasutes
syntax match nniSpecial /Queen/ 
syntax keyword nniSpecial ANTS BEETLE LADYBUG SCORPION SPIDER WASP
syntax keyword nniSpecial Cody Brody

" Constant
syntax match nniConstant /Game\(\n\|\s\)Queen/
syntax match nniConstant /\(Attribute\|Combat Condition\|Survival\( Feature\)*\|Ability\|\(Specific\|Common \)*Abilities\|Effect\|Inventory\( Item\)*\|Item\|Depleted\|Exhausted\)s*\( Area\| Header\)*/
syntax match nniConstant /Companion\|Food\|Hint\|Light\|Materials\|Message\|Inventories\|Slots/
syntax match nniConstant /\(Cost\|Hunger\|Passage\|Effects\|Darkness\|Spoilage\|Encounter\|Item (Unknown)\|Obstacle\|Infection\|Room\|Shortcut\|Level\|Item (Known)\|Resource\|Secret\)/
syntax match nniConstant /\(Event\|Cost\|Risk\|Reward\)\(s\)*\|Chance/
syntax match nniConstant /\(Environment\)s*\( Pool\)*/
syntax match nniConstant /Shelter\|Gloworm\|Carcass\|Spoiled\|Cursed/
syntax match nniConstant /\(Condition\|Attack\|Hunger\|Food\|Luck\|\(Specific\|Common\) Abilities\|Effects\|Inventory\) box\(es\)*/
syntax match nniIdentifier /\(Initiative\|Threat Rate\|Survival Rate\|Wound\(s\)*\|Max\)/
syntax match nniIdentifier /\(Combat\|Survival\(\s\|\n\)and\(\s\|\n\)Exploration\|Hunting\(\s\|\n\)and\(\s\|\n\)Foraging\|Resting\(\s\|\n\)and\(\s\|\n\)Healing\)/
syntax match nniConstant /Combatant\(s\)*/

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
" Not really a comment! But a highlighted term.
syntax region nniComment start=/"[a-zA-Z0-9 ]\+/ end=/"/

" Name of the game
syntax match nniStatement /Nests\(\s\|\n\)&\(\s\|\n\)Insects/
syntax match nniStatement /Fuzzy d100/
syntax keyword nniStatement fd100
syntax match nniStatement /Nests & Insects v\.\d\.\d\.\d/

" Special keywrods
syntax match nniSpecial /Degree\(s\)* of Success\( \(and\|or\) Failure\)*/
syntax keyword nniSpecial Effort Resistance Outcome DoS Intent Generate generated
syntax match nniSpecial /\(Qualification\|Modification\|Quantification\|Resolution\)/
syntax match nniSpecial /\(Boolean\|Qualification\|Quantification\) Roll\(s\)*/
syntax match nniConstant /Quantification Die/
syntax match nniSpecial /Roll\|Result/
syntax match nniSpecial /\(\(Critical\|Complete\|Partial\)*\(\s\|\n\)\)*\(Success\|Failure\)/
syntax match nniSpecial /\(Successful\|Failed\)/
syntax match nniSpecial /Shift\(s*\) \(Up\( \(and\|or\) Down\)*\|Down\)/
syntax match nniSpecial /\(Hit\|Miss\(es\)*\)s*/
syntax match nniConstant /\(Name\|Keywords\|Damage\|Max. Range\)\(:\)*/
syntax match nniConstant /\(Hit\|Miss\)\( (\w\{3,5})\)*:/

syntax match nniConstant /\d\+\.\d\+\.\d\+\.\d\+\s[A-Za-z -]\+/

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
hi def link nniStatement  Statement
hi def link nniTodo  Todo
