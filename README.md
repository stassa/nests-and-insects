Nests & Insects
===============

This is the repository for the rulebooks and code of Nests & Insects, a Roguelike
Tabletop Roleplaying Game.

Nests & Insects is still a work in progress, in the early stages of design and
playtesting. To see how much is done and how much remains to be done, check out
the TODO file and `changelog.md`.

The Game
--------

Nests & Insects is a tabletop roleplaying game (TTRPG) for 2 to 7 players. One
player assumes the role of the Game Queen and describes the game world to the
other players. The other players control characters who explore, and interact
with, the game world.

### Characters

Players' characters are arthropods that belong to one of six classes: Spider,
Wasp, Scorpion, Ladybug, Beetle and Ants (plural). The characters are
mercenaries, assassins, and thugs, hired to invade a Nest and carry out a Job on
behalf of some arthropod client. Nests are the homes of eusocial insects: the
nests of Bees, Ants and Termites. Common Jobs are to assasinate the Queen, or
the King; steal nectar, honey, honeydew, aphids or mealybugs (farmed by ants) or
fungi (cultivated by termites); steal, or kill, larvae; deliver, or pick up a
message; or sabotage the Nest.

### The Nest

At the beginning of a new game the players' characters enter a Nest to carry out
their Job armed with their natural weapons and armour: mandibles, pincers,
stingers, carapaces, wings, venom, webs. During a Job characters must survive
combat with the soldier castes guarding the Nests. They must also hunt or forage
for food to avoid weakening and starving to death. While foraging, characters
may find food items, such as nectar and fungi, that can be consumed to provide
not only sustenance but also healing, enhanced physical and mental abilities and
other benefits.

### A tabletop roleplaying game

Nests & Insects is a tabletop roleplaying game: the players say what their
characters want to do and roll the dice to see what happens. Then the Game Queen
describes the results of the characters' actions. Nests & Insects' roleplaying
system is a percentile system where a composite, "percentile" die (d100) is
rolled to determine the outcome of actions and composite "decile dice" (d20,
d40, d60, d80, d120 and the d100 itself) are rolled to quantify the results of
actions. The system is designed to remove all mental arithmetic from "action
resolution" and to encourage the players to use their imagination to help their
characters achieve their goals. Everything characters encounter in Nests &
Insects is procedurally generated: the Nest, its guardians, items, all
challenges and rewards.

### A roguelike tabletop roleplaying game

Nests & Insects is a "Roguelike" TTRPG: it is inspired by Roguelike Computer
RPGs (CRPGs) such as Nethack, Angband, Moria, ADoM and newer games like Diablo
and Darkest Dungeon. From Roguelike CRPGs it borrows: procedural generation;
hack-and-slash, dungeon-crawling gameplay; lethal combat; hunger mechanics; and
a focus on exploration and experimentation with the game world. From TTRPGs it
borrows: weird dice; tortuous terminology with Pompous Capitalisation;
over-engineered rules; unclear motivation to learn yet another roleplaying
system; and crunch Crunch CRrRUNCH!

Reading the rulebook
--------------------

The text-based rulebook is in the following path:
```
<project root>/games/rulebook/txt/nests_and_insects.txt
```

_The text-based version is the only version of the rulebook._

To read the text-based rulebook, navigate to its directory and open it with a pager like
less or more etc.

```
cd .../games/rulebook/txt/
less nests_and_insects.txt
```

You should then be greeted by the glorious ASCII of the rulebook cover. 

Alternatively, you can open the rulebook file in your favourite text editor. On
Windows, Notepad and Notepad++ work fine. On Windows and everywhere else, vim, emacs
and friends should work as well as usual.

The cover and the rulebook are best rendered in the free font DejaVu Sans Mono. You can
download it from here:

https://dejavu-fonts.github.io/

Running the code
----------------

The ironically named /codez directory contains Prolog code used to manage game data and
generate characters, character sheets etc. Also, to lay-out the text-based rulebooks. It
is not necessary to peruse the codez directory to play the game- it is only included for
the game developer's convenience.

Reporting Errors
----------------

The text-based rulebook is formatted with the code in the Prolog module
`codez/src/layout.pl`. This, too, is a work in progress. If you find major formatting
errors, or any errors, in the rulebook, please report it by emailing the game's author
at `ep2216@ic.ac.uk`. You're welcome to send a pull request instead.

License matters
---------------

The contents of this repository come with the text of a LICENSE - the GNU
General Public License version 3 (GNU GPL v.3 for short-er). The GNU GPL v.3 is
a "copyleft license for software and other kinds of works" therefore I assume
and hereby declare my conviction that it also covers the contents of the game/
directory, which are not, strictly speaking, "software" in the commonly
understood sense of "source code writte in a programming language".

In particular, where the word "software" appears in the GNU GPL v.3 ", and for
the puproses of licensing Nests & Insects, that word should be interpreted as
saying _software and other kinds of works, including game rules text_, instead.

Should there be any doubts about the licensing of parts of the text in this
repository, assume that all text is the source code of a programming language,
except that some text may not have a compiler, or may not be designed to be
executed by a computer, but a human brain.
