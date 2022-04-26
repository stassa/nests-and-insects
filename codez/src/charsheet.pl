:-module(charsheet, [format_charsheet/2,
                     print_charsheet/1]).

/** <module> Print character sheets in glorious ASCII.

The ASCII may be glorious but composing strings with formatting
procedures must always suck so so much.

*/

%!      pad_length(?Where,?Pad) is semidet.
%
%       A character that turns into a Pad in the full moon.
%
%       Length of lines that need to be padded with spaces or other
%       characters, smart arse.
%
pad_length(class_name, 91).


%!      format_charsheet(+Class,-Formatted) is det.
%
%       Format a Class' character sheet as an atom for printing.
%
format_charsheet(Id,CS):-
        format_class_header(Id,CH)
        ,format_attributes(Id,Atr)
        ,format_combat_stats(Id,Com)
        ,format_attacks(Id,Att)
        ,format_survival(Id,Sur)
        ,format_abilities(Id,Abl)
        ,format_effects_inventory(Id,Efs)
        ,atomic_list_concat([CH
                            ,Atr
                            ,Com
                            ,Att
                            ,Sur
                            ,Abl
                            ,Efs],'',CS).


%!      print_charsheet(+Class) is det.
%
%       Print a Class' character sheet in glorious ASCII.
%
print_charsheet(Id):-
        format_class_header(Id,CH)
        ,format(CH)
        ,format_attributes(Id,Atr)
        ,format(Atr)
        ,format_combat_stats(Id,Com)
        ,format(Com)
        ,format_attacks(Id,Att)
        ,format(Att)
        ,format_survival(Id,Sur)
        ,format(Sur)
        ,format_abilities(Id,Abl)
        ,format(Abl)
        ,format_effects_inventory(Id,Efs)
        ,format(Efs).


%!      format_class_header(+Class) is det.
%
%       Format the Class Header part of a character sheet.
%
format_class_header(Id,CH):-
        pad_length(class_name, L)
        % Length of the Class Id
        ,atom_length(Id, N)
        ,N_ is ceil(N/2)
        % Padding string offset to make space for half of Id string.
        % Because we want to print the class Id in the middle of the padded line.
        ,P is ceil(L/2) + N_ + 1
        ,upcase_atom(Id,ID)
        ,format(atom(CH1),'╔►Nests & Insects◄~|~`═t~57+►Character Sheet◄╗~n',[])
        ,format(atom(CH2),'║~`.t ~w~*| ~`.t~92|║▓~n',[ID,P])
        ,atomic_list_concat([CH1,CH2],'',CH).


%!      print_class_name(+Class) is det.
%
%       Format the Attributes Area and their ratings.
%
format_attributes(Id,Att):-
        chargen:features(Id,attribute,FRs)
        ,pairs_keys_values(FRs,_,[Sp,Sk,Str,Sta,Sm,Ch,Ke,Pa])
        ,format(atom(Att_H),'╠►Attributes◄~|~`═t~79+╣▓~n',[])
        ,format(atom(Att_1),'║ ┌~|~`─t~87+┐ ║▓~n',[])
        ,format(atom(Att_2),'║ │ □ Speed.....:[~|~`_t~w~4+%] □ Skill....:[~|~`_t~w~4+%] □ Strength....:[~|~`_t~w~4+%] □ Stamina....:[~|~`_t~w~4+%] │ ║▓~n', [Sp,Sk,Str,Sta])
        ,format(atom(Att_3),'║ │ □ Smarts....:[~|~`_t~w~4+%] □ Charms...:[~|~`_t~w~4+%] □ Ken.........:[~|~`_t~w~4+%] □ Passions...:[~|~`_t~w~4+%] │ ║▓~n',[Sm,Ch,Ke,Pa])
        ,format(atom(Att_F),'║ └<^XP>────────────────<^XP>───────────────<^XP>──────────────────<^XP>──────────────────┘ ║▓~n',[])
        ,atomic_list_concat([Att_H,Att_1,Att_2,Att_3,Att_F],'',Att).


%!      format_combat_stats(+Class) is det.
%
%       Format the Class' Combat Conditions and Disposition Track.
%
format_combat_stats(Id,Com):-
% Lots of distortion in this predicate because of missing fonts in
% Courrier. Use Deja Vu Sans Mono to display correctly in editor.
        chargen:features(Id,condition,FRs)
        ,pairs_keys_values(FRs,_,[Init,TR,SR,Ws])
        ,(   Id = ladybug
         ->  Rem = '1 Wound: Advance Disposition.'
         ;   Id = ants
         ->  Rem = '1 Wound: 1 Ant lost.'
         ;   Rem = '1 Wound = 1 Shift Down.'
         )
        ,format(atom(Com_H),'╠►Combat Stats◄~|~`═t~77+╣▓~n',[])
        ,format(atom(Com_Con),'║┌[Condition]~|~`─t~14+<Rules Reminder>~|~`─t~47+┐ ║▓~n',[])
        ,format(atom(Com_Ini),'║│ □ Initiative...:[~|~`_t~w~4+%] (Match/Beat to start Combat in Holding/Recoiling Disposition). │ ║▓~n',[Init])
        ,format(atom(Com_TR),'║│ □ Threat Rate..:[~|~`_t~w~4+%] (Match/Beat Attacker''s TR to Hit/Miss Target)................. │ ║▓~n',[TR])
        ,format(atom(Com_SR),'║│ □ Survival Rate:[~|~`_t~w~4+%] (Match/Beat Target''s SR to Hit/Miss with Base/Special Attack). │ ║▓~n',[SR])
       %,format(atom(Com_Ws),'║│ □ Wounds/Max...:[__/~|~`_t~w~2+] (1 Wound = 1 Shift Down. When Wounds ≥ Max, character dies)... │ ║▓~n',[Ws])
        ,format(atom(Com_Ws),'║│ □ Wounds/Max...:[__/~|~`_t~w~2+] (~w If Wounds ≥ Max character dies)~`.t~89| │ ║▓~n',[Ws,Rem])
       %,format(atom(Com_Sep1),'║│ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧  │ ║▓~n',[])
        ,format(atom(Com_Dis),'║├─<Disposition Track>────────────────────────────────────────────────────────────────────┤ ║▓~n',[])
	,format(atom(Com_Rem),'║│ (Advance this way -->) ......................................... (<-- Recoil this way) │ ║▓~n',[])
	,format(atom(Com_Tra),'║│ ○ Retreating...... ○ Recoiling...... ○ Holding...... ○ Advancing...... ○ Charging......│ ║▓~n',[])
	,format(atom(Com_Mod),'║└<^Cant Attack>─────<^-30%>───────────<^Doing Good>───<^+30%>───────────<^Shift Up>──────┘ ║▓~n',[])
       %,format(atom(Com_Sep2),'║ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ✧ ║▓~n',[])
	,atomic_list_concat([Com_H
                            ,Com_Con
                            ,Com_Ini
                            ,Com_TR
                            ,Com_SR
                            ,Com_Ws
                            ,Com_Dis
                            ,Com_Rem
                            ,Com_Tra
                            ,Com_Mod],'',Com).


%!      format_attacks(+Class) is det.
%
%       Format a Class' Attack descriptions.
%
%       I warn you now, before you look at the code: this is gonna hurt.
%
format_attacks(Id,Att):-
% Comments show examples of expected input to ease the pain.
% The pain... THE PAIN!
        chargen:features(Id,attacks,[BA,SA])
        ,BA = [%name-'Pincer Attack'
               name-BA_Name
              %,keywords-['Crushing','Shredding']
              ,keywords-BA_Keywords
              %,wounds-1
              ,wounds-BA_Ws
              %,max_range-'Close'
              ,max_range-BA_Max_Range
              %,hit-'Taregt Recoils'
              ,hit-BA_Hit1
              %,hit_2-['(Alt)','']
              ,hit_2-[Rem1,BA_Hit2]
              %,hit_3-['(Add)','']
              ,hit_3-[Rem2,BA_Hit3]
              %,miss_1-'Attacker Recoils'
              ,miss_1-BA_Miss1
              %,miss_2-['(Alt)','']
              ,miss_2-[Rem3,BA_Miss2]
         ]
        ,SA = [%name-'Stinger Attack'
               name-SA_Name
              %,keywords-['Piercing','Venomous','Lethal']
              ,keywords-SA_Keywords
              %,wounds-2
              ,wounds-SA_Ws
              %,max_range-'Close'
              ,max_range-SA_Max_Range
              %,hit-'Taregt in Agony: d60'
              ,hit-SA_Hit1
              %,hit_2-['(Alt)','']
              ,hit_2-[Rem4, SA_Hit2]
              %,hit_3-['(Crit)','Enemy Killed']
              ,hit_3-[Rem5,SA_Hit3]
              %,miss_1-'Attacker Recoils'
              ,miss_1-SA_Miss1
              %,miss_2-['(Alt)','']
              ,miss_2-[Rem6,SA_Miss2]
         ]
        ,maplist(atomic_list_concat,[BA_Keywords,SA_Keywords],[',',','],[BAK,SAK])
        % This is the bit that really hurts.
        % There's no way to line up these format strings without messing up formatting.
        % But the way they look I'm not sure I'll be ever capable of debugging this.
        ,format(atom(Att_H),'║┌[Base Attack]──────────────────────────────┐┌[Special Attack]───────────────────────────┐ ║▓~n',[])
        ,format(atom(Att_Nm),'║│ ~|Name:~`.t~w~41+ ││ ~|Name:~`.t~w~41+ │ ║▓~n',[BA_Name,SA_Name])
        ,format(atom(Att_Kw),'║│ ~|Keywords:~`.t~w~41+ ││ ~|Keywords:~`.t~w~41+ │ ║▓~n',[BAK,SAK])
        ,format(atom(Att_Ws),'║│ ~|Wounds:~`.t~w~41+ ││ ~|Wounds~`.t~w~41+ │ ║▓~n',[BA_Ws,SA_Ws])
        ,format(atom(Att_Rg),'║│ ~|Max. Range:~`.t~w~41+ ││ ~|Max. Range:~`.t~w~41+ │ ║▓~n',[BA_Max_Range,SA_Max_Range])
        ,format(atom(Att_H1),'║│ ~|Hit:~`.t~w~41+ ││ ~|Hit:~`.t~w~41+ │ ║▓~n',[BA_Hit1,SA_Hit1])
	,format(atom(Att_H2),'║│ ~|Hit ~w:~`.t~w~41+ ││ ~|Hit ~w:~`.t~w~41+ │ ║▓~n',[Rem1,BA_Hit2,Rem4,SA_Hit2])
        ,format(atom(Att_H3),'║│ ~|Hit ~w:~`.t~w~41+ ││ ~|Hit ~w:~`.t~w~41+ │ ║▓~n',[Rem2,BA_Hit3,Rem5,SA_Hit3])
        ,format(atom(Att_M1),'║│ ~|Miss:~`.t~w~41+ ││ ~|Miss:~`.t~w~41+ │ ║▓~n',[BA_Miss1,SA_Miss1])
        ,format(atom(Att_M2),'║│ ~|Miss ~w:~`.t~w~41+ ││ ~|Miss ~w:~`.t~w~41+ │ ║▓~n',[Rem3,BA_Miss2,Rem6,SA_Miss2])
        ,format(atom(Att_F),'║└───────────────────────────────────────────┘└───────────────────────────────────────────┘ ║▓~n',[])
        ,atomic_list_concat([Att_H
                            ,Att_Nm
                            ,Att_Kw
                            ,Att_Ws
                            ,Att_Rg
                            ,Att_H1
                            ,Att_H2
                            ,Att_H3
                            ,Att_M1
                            ,Att_M2
                            ,Att_F],'',Att).
% I warned you.


%!      format_survival(+Class) is det.
%
%       Format a Class' Survival Features and their ratings.
%
format_survival(Id,Sur):-
	chargen:survival_features(Id,[hunger-Hun,_Luck])
        ,(   Id = beetle
         ->  Pad = 5
            ,Per = ''
            ,Tic = '✗'
         ;   Pad = 4
            ,Per = '%'
            ,Tic = '○'
         )
        ,format(atom(Sur_Hed),'╠►Survival◄═════════════════════════════════════════════════════════════════════════════════╣▓~n',[])
        ,format(atom(Sur_Box),'║┌[Food]───────────────────┐┌[Luck]───────────────────────────────────────────────────────┐ ║▓~n',[])
        ,format(atom(Sur_Fea),'║│ ~w Hunger........[~|~`_t~w~*+~w] ││ [11%].[22%].[33%].[44%].[55%].[66%].[77%].[88%].[99%].[00%].│ ║▓~n',[Tic,Hun,Pad,Per])
        ,format(atom(Sur_Fot),'║└<^Starving>──────────────┘└──^──────────────────────────────────────────────────────────┘ ║▓~n',[])
        ,atomic_list_concat([Sur_Hed,Sur_Box,Sur_Fea,Sur_Fot],'',Sur).



%!      format_abilities(+Class) is det.
%
%       Format a Class' Specific and Common Abilities and their ratings.
%
format_abilities(Id,Abl):-
        maplist(chargen:features(Id),[specific_ability,common_ability],[SAs,CAs])
        ,maplist(pairs_keys_values,[SAs,CAs],[_,_],[SA_Rs%[Ca,Fl,Sw,Ven,Web]
                                                   ,[Con,Eus,Exp,For,Hea,Hun,Lea,Per,Sig,Sne]])
        ,findall(W
                ,(member(SA,SA_Rs)
                 ,(   SA == 0
                  ->  W = '_0'
                  ;   W = SA
                  )
                 )
                ,[Car,Fly,Swa,Ven,Web])
	,format(atom(Abl_H),'╠►Abilities◄════════════════════════════════════════════════════════════════════════════════╣▓~n',[])
        ,format(atom(Abl_Box),'║┌[Specific Abilities]────────┐┌[Common Abilities]────────────────────────────────────────┐ ║▓~n',[])
        ,format(atom(Abl_Ln1),'║│ Carapace..........:[~|~`_t~w~4+%] ││ □ Construction.....:[~|~`_t~w~4+%] □ Hunting...........:[~|~`_t~w~4+%] │ ║▓~n',[Car,Con,Hun])
        ,format(atom(Abl_Ln2),'║│ Flying............:[~|~`_t~w~4+%] ││ □ Eusociology......:[~|~`_t~w~4+%] □ Leadership........:[~|~`_t~w~4+%] │ ║▓~n',[Fly,Eus,Lea])
        ,format(atom(Abl_Ln3),'║│ Swarming..........:[~|~`_t~w~4+%] ││ □ Exploration......:[~|~`_t~w~4+%] □ Perception........:[~|~`_t~w~4+%] │ ║▓~n',[Swa,Exp,Per])
        ,format(atom(Abl_Ln4),'║│ Venomous..........:[~|~`_t~w~4+%] ││ □ Foraging.........:[~|~`_t~w~4+%] □ Signalling........:[~|~`_t~w~4+%] │ ║▓~n',[Ven,For,Sig])
        ,format(atom(Abl_Ln5),'║│ Web Weaving.......:[~|~`_t~w~4+%] ││ □ Healing..........:[~|~`_t~w~4+%] □ Sneaking..........:[~|~`_t~w~4+%] │ ║▓~n',[Web,Hea,Sne])
        ,format(atom(Abl_Fot),'║└────────────────────────────┘└<^XP>───────────────────────<^XP>─────────────────────────┘ ║▓~n',[])
        ,atomic_list_concat([Abl_H,
                             Abl_Box,
                             Abl_Ln1,
                             Abl_Ln2,
                             Abl_Ln3,
                             Abl_Ln4,
                             Abl_Ln5,
                             Abl_Fot],'',Abl).


%!      format_effects_inventory(+Class) is det.
%
%       Format starting Effects and Inventory Items and their ratings.
%
%       @bug Currently this can only print a single inventory item. I
%       don't think there's any class that has more than one starting
%       item so far but if one does, then this better be fixed.
%
format_effects_inventory(Id,Efs):-
        maplist(chargen:features(Id),[effect,inventory_item],[Es,Is])
        ,maplist(pairs_keys_values,[Es,Is],[_,[It]],[E_Rs ,[R] ])
        ,findall(W-Tic
                ,(member(SA,E_Rs)
                 ,(   SA == 0
                  ->  W = '' % Value of the Effect field
                      ,Tic = '○'
                  ;   W = SA
                     ,Tic = '✓'
                  )
                 )
                ,[Ago-Ago_Tic
                 ,Ble-Ble_Tic
                 ,Bli-Bli_Tic
                 ,Cha-Cha_Tic
                 ,Con-Con_Tic
                 ,Imm-Imm_Tic
                 ,Inf-Inf_Tic
                 ,Par-Par_Tic
                 ,Poi-Poi_Tic
                 ,Stu-Stu_Tic])
        ,character:inventory_item(It,Nm)
        ,atom_length(Nm, N)
        ,Pad is 30 - N
        ,format(atom(Efs_Box),'║┌[Effects]───────────────────────────────────┐┌[Inventory]───────────────────────────────┐ ║▓~n',[])
        ,format(atom(Efs_Ln1),'║│ ~w Agony....:[~|~`_t~w~4+%] ~w Immobilised.:[~|~`_t~w~4+%] ││ ✓ ~w~|~`_t~*+:[~|~`_t~w~4+%] │ ║▓~n',[Ago_Tic,Ago,Imm_Tic,Imm,Nm,Pad,R])
        ,format(atom(Efs_Ln2),'║│ ~w Bleeding.:[~|~`_t~w~4+%] ~w Infected....:[~|~`_t~w~4+%] ││ ○ ______________________________:[____%] │ ║▓~n',[Ble_Tic,Ble,Inf_Tic,Inf])
        ,format(atom(Efs_Ln3),'║│ ~w Blind....:[~|~`_t~w~4+%] ~w Paralysed...:[~|~`_t~w~4+%] ││ ○ ______________________________:[____%] │ ║▓~n',[Bli_Tic,Bli,Par_Tic,Par])
        ,format(atom(Efs_Ln4),'║│ ~w Charmed..:[~|~`_t~w~4+%] ~w Poisoned....:[~|~`_t~w~4+%] ││ ○ ______________________________:[____%] │ ║▓~n',[Cha_Tic,Cha,Poi_Tic,Poi])
        ,format(atom(Efs_Ln5),'║│ ~w Confused.:[~|~`_t~w~4+%] ~w Stunned.....:[~|~`_t~w~4+%] ││ ○ ______________________________:[____%] │ ║▓~n',[Con_Tic,Con,Stu_Tic,Stu])
        ,format(atom(Efs_Foo),'║└<^Applies>──────────<^Applies>──────────────┘└<^Edible>─────────────────────────────────┘ ║▓~n',[])
        ,format(atom(Efs_End),'╚═══════════════════════════════════════════════════════════════════════════════════════════╝▓~n',[])
        ,format(atom(Efs_Sha),' ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀~n',[])
        ,atomic_list_concat([Efs_Box,
                             Efs_Ln1,
                             Efs_Ln2,
                             Efs_Ln3,
                             Efs_Ln4,
                             Efs_Ln5,
                             Efs_Foo,
                             Efs_End,
                             Efs_Sha],'',Efs).
