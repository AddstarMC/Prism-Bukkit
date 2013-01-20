#Prism

*An incredible plugin for rollbacks, restores, and more.*

Prism is the ultimate grief management (rollbacks, restores, etc) plugin for your Bukkit server. It's extremely fast, versatile, and incredibly powerful. It's smart rollback engine and a wide range of features put Prism in a class all it's own.

Prism not only offers the standard features you expect, but a **lot** more.

**Check the files tab for more recent files... bukkitdev is taking 24+ hours to approve our files**

![Prism](http://dhmc.us.s3.amazonaws.com/prism.jpg)

### Users Say

"Just switched from CoreProtect to this. Totally worth it."

"After 24 hours I can tell that it seems to be the best anti-griefing plugin I know."

"This is definetly the best rollback plugin I have used"

"I've only been using it for a bit and I'm beyond impressed!"

## Official Documentation

**Everything you need to know.**

[Intro & Demo Video (Pre-Beta)](http://www.youtube.com/watch?v=3b6DQQvBp30)

[Video Tutorial 1: Lookups](http://www.youtube.com/watch?v=W8DpqKiiSJs)

[Video Tutorial 2: Rollbacks](http://www.youtube.com/watch?v=IRr-4r_LS2I)

[Video Tutorial 3: More on Rollbacks](http://www.youtube.com/watch?v=oHBXYzv7vWs)

[Official Prism Documentation](https://github.com/botskonet/Prism-Extras/wiki )

Installation, configuration, permissions, commands, tutorials, and a lot more at our official documentation wiki.

[Bug Reports/Feature Requests](https://snowy-evening.com/botsko/prism/ )


## Features Overview

- Prism tracks 46 (and growing) different player, entity, and world events.
- Multiple tools and commands let you see exactly what happened.
- Rollback or restore events directly, or with a safer preview mode first.
- Safe command defaults, very clean and fun to use chat messages.
- Smarter rollbacks/restores, and a dozen extra features.
- Extremely fast. Takes a pounding. 
- Extremely configurable.
- Dozens of extra features you don't get from other plugins.

## Features Detail

### Events and Lookup

- Tracks 46+ player, entity, and world events.
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

Coming soon.

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
- Block r/r/p are also run on separate threads and queued for large changes to reduce chances of impacting the main thread.
- Only one database table with extremely straight-forward design, as well as field types that are smart for the data we store. Records 1 million records with an average of 100MB of space consumption. A server with 200+ players per day can expect about ~2GB of data per month, depending on actions tracked and activity.
- A dev server with 512M RAM, 2.7Ghz dual core processor, 6,886 blocks from a tnt explosion restored in 2.2 seconds - 2210 milliseconds from command to rollback completion. Most common rollbacks on the same machine tend to take < 200ms to restore. Similar results on relatively equal machines from shared bukkit hosting companies during initial tests.


       
## Get Help

IRC: irc.esper.net #prism (recommended) or #dhmc_us  

## Donate

I've invested a lot of time making Prism what is, along with some contributions and testing help by our server staff. Help me out, even if it's just $1 person.

viveleroi - (PayPal; botsko@gmail.com) 
           
## Credits

This plugin was custom designed for the amazing *dhmc.us* Minecraft server.

- viveleroi (*Creator, Lead Dev*)
- nasonfish (*Contributor*)
- nasonfish, Natman93, YeaItsMe, mafoan (*Alpha Testers*)
- mafoan, randox24, tacovan, nehocbelac, Shampoo123, cxmmy14, Palczynski, drac17, ollie2000, PGKxNIGHTMARE, allies333, DocVanNostrand, drfizzman123, 00benallen, rachaelriott, PheonixTamer, YeaItsMe, Natman93, Brazter, sydney2005, rsleight, napalm1, Teh_Fishman, and more (*Live Testers on DHMC*)

* Artwork by [LegendarySoldier](http://legendary-soldier.deviantart.com/ )