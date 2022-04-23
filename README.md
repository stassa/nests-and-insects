Nests & Insects
===============

This is the repository for the rulebooks and code of Nests & Insects, a Roguelike
Tabletop Roleplaying Game.

Nests & Insects is still a work in progress, at the early stages of design and
playtesting.

Reading the rulebooks
---------------------

The text-based rulebook is in the following path:
```
<project root>/games/rulebook/txt/nests_and_insects.txt
```

_The text-based version is the main version of the rulebook._

There exists a pdf version but it is updated sporadically, or not at all. _If you are
looking for the latest changes (and additions) to the rules, you should read the
text-based version_.

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
