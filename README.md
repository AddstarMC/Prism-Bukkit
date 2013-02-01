#Prism

*An incredible plugin for rollbacks, restores, and more.*

Prism is the ultimate grief management (rollbacks, restores, etc) plugin for your Bukkit server. It's extremely fast, versatile, and incredibly powerful. It's smart rollback engine and a wide range of features put Prism in a class all it's own.

Prism not only offers the standard features you expect, but a **lot** more.


![Prism](http://dhmc.us.s3.amazonaws.com/prism.jpg)

### Users Say

"Just switched from CoreProtect to this. Totally worth it."

"The plugin is nuts. Switched over from LogBlock now and never looking back."

"This is definetly the best rollback plugin I have used"

"I've only been using it for a bit and I'm beyond impressed!"

"I love this plugin! Seemed so hard at first, then I used it."

## Official Website & Documentation

**MUST READ ;)**

[DISCOVER-PRISM.COM](http://discover-prism.com/ )


## Some Videos

[Intro & Demo Video (Pre-Beta)](http://www.youtube.com/watch?v=3b6DQQvBp30)

[Video Tutorial 1: Lookups](http://www.youtube.com/watch?v=W8DpqKiiSJs)

[Video Tutorial 2: Rollbacks](http://www.youtube.com/watch?v=IRr-4r_LS2I)

[Video Tutorial 3: More on Rollbacks](http://www.youtube.com/watch?v=oHBXYzv7vWs)

## Features Overview

- Prism tracks 51 (and growing) different player, entity, and world events.
- Multiple tools and commands let you see exactly what happened.
- Rollback or restore events directly, or with a safer preview mode first.
- Safe command defaults, very clean and fun to use chat messages.
- Smarter rollbacks/restores, and a dozen extra features.
- Extremely fast. Takes a pounding. 
- Extremely configurable.
- Dozens of extra features you don't get from other plugins.
- **Requires MySQL** for best performance. sqlite support immature, but available and stable.

## Features Detail

### Events and Lookup

- Tracks 51+ player, entity, and world events.
- Includes block changes, container access, item inserts/remove/pickups/drops, entity deaths, shearing, and a LOT more.
- Related events tracked to causing player, like blocks falling, blocks detaching, trees growing, and much more.
- Smart logging and rollback of important related data like sign text, sheep color, wolf owner, animal age, villager profession, etc
- Tracks liquid flow and items that break because of it
- Tracks world edits (Beta2-17+, requires world edit 5.4.5+)
- Fine-tuned config allows you to control tracking of every single one.
- Inspector wand allows you to find history for single blocks/spaces.
- Near command finds recent events around you.
- Lookup command is extremely powerful with our versatile parameters.
- Action names also have short-names for easier typing and can combine related actions.
- The radius command also accepts a world edit selection - for any /r/r/p/lookup actions.
- Very clean chat messages and paginated results to ease your burden.

### Rollbacks, Restore, Preview (R/R/P)

- Rollback and restore commands you know and love.
- Rollback or restore directly, or preview changes for _both_ modes first.
- Apply/cancel previews, or let them auto-cancel after a minute.
- Extremely safe default parameters make it harder for you to make mistakes, but can be overridden in-game when you absolutely need them.
- You can even rollback killed entities, items taken from containers, and more.
- Sign rollbacks will restore their text (provided we tracked what was written)
- Rollbacks of burn events also extinguish fires.
- Rollbacks of tnt/creeper explosion events also remove the laggy item drops.
- We're CONSTANTLY testing to ensure that rollbacks are smart - problem items like doors, beds, cactus, sugar cane, and more are always restored properly! No half-doors!
- Rollbacks/restores can be alerted to nearby players, and to staff members (staff see actual rollback params).
- Rollback systems moves any players in the way to a safe position on rollback
- Rollback/restore world edits (Beta2-17+, requires world edit 5.4.5+)
- Advanced, optional flags give you ultimate control over the rollback/restore action at command-time.


### Web Search Interface

- Allows you to view/search your data through a website.
- Very clean, modern design makes it an awesome experience.
- We've worked hard to increase performance of the db/queries - databases with millions of records work just fine.
- Optional user authentication support allows you to require people to login before using.

![Prism](http://dhmc.us.s3.amazonaws.com/prism-web.jpg)


### Extra Features

- Alerts for staff when players find natural ores (configurable).
- Alerts for staff when players use flint and steel, place lava, or place any item listed in the config (like tnt).
- Put out fires with the extinguish command
- Drain all liquids or just water/lava
- Profile wand shows you data about a block.

### Misc

- Consistently designed chat messages are easy to read, and never spam you.
- All commands are consistent and designed knowing a human will be using them.
- Config allows you to ignore creative mode, or a list of players, worlds, etc.

### Technical

- Multi-threaded queue system for tracking events, combined with a batch recording system makes event recording happen quickly and without impacting your server.
- Block r/r/p are queued for large changes to reduce chances of impacting the main thread.
- Extremely smart database design with a lot of time invested in structure optimization, query performance testing, and more.
- A dev server with 512M RAM, 2.7Ghz dual core processor, 6,886 blocks from a tnt explosion restored in 2.2 seconds - 2210 milliseconds from command to rollback completion. Most common rollbacks on the same machine tend to take < 200ms to restore. Similar results on relatively equal machines from shared bukkit hosting companies during initial tests.


       
## Get Help

IRC: irc.esper.net #prism

## License 

Attribution-NonCommercial-ShareAlike 3.0 United States

http://creativecommons.org/licenses/by-nc-sa/3.0/us/

## Donate

I've invested a lot of time making Prism what is, along with some contributions and testing help by our server staff. Help me out, even if it's just $1 person.

viveleroi - (PayPal; botsko@gmail.com) 
           
## Credits

Prism, designed with experience from using competing products on DHMC, and dealing with nearly 19k players in a year and a half. 

- viveleroi (*Creator, Lead Dev*)
- nasonfish (*Contributor*)
- YeaItsMe (*Release QA*)
- nasonfish, Natman93, YeaItsMe, mafoan (*Alpha Testers*)
- mafoan, randox24, tacovan, nehocbelac, Shampoo123, cxmmy14, Palczynski, drac17, ollie2000, PGKxNIGHTMARE, allies333, DocVanNostrand, drfizzman123, 00benallen, rachaelriott, PheonixTamer, YeaItsMe, Natman93, Brazter, sydney2005, rsleight, napalm1, Teh_Fishman, and plenty more from DHMC (*Live Testers on DHMC*)

- WorldEdit block logging [#1](https://github.com/LogBlock/LogBlock/tree/master/src/main/java/de/diddiz/worldedit ) based on work by [Ammaraskar](https://github.com/ammaraskar )
- Time format string parsing [#2](https://github.com/oliverw92/HawkEye/blob/master/src/main/java/uk/co/oliwali/HawkEye/SearchParser.java#L155 ) and temporary safe teleport based on work by [oliverw92](https://github.com/oliverw92 )
- [Metrics](http://mcstats.org) class Copyright 2011 Tyler Blair. All rights reserved.
- Artwork by [LegendarySoldier](http://legendary-soldier.deviantart.com/ )


## COMMENTING?

- Reporting a bug? Suggesting a feature? Do so here: [Prism Bug Tracker](https://snowy-evening.com/botsko/prism/ )
- Prism not working? Check these common problems FIRST: [Troubleshooting](http://discover-prism.com/wiki/view/troubleshooting/ )