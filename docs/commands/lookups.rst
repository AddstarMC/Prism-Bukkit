*******
Lookups
*******

By default, Prism will begin tracking all sorts of events that happen in your worlds. Players making block changes, killing entities, transfers of items, and much more.
Some of this data is purely informational, so you can find out who did something, and some of it is useful for rollbacks and restores.

Searching the Database
======================

The first use is to be able to find the information you need. Prism comes with three tools to find the data from in-game, each with a different focus.

Inspector (Wand)
----------------

Use ``/prism i`` to toggle what we call the "inspection" wand. Wands bind certain actions to your hand/tools. The inspection wand can be toggled with just ``/prism i`` or ``/prism wand i``. Left-click a block to see that exact block's history. Right-click on any side of the block to see the history for the space adjacent.
Excellent for finding information about a single block, like a chest or ore finds.
See the Wands page for more details.

Near
----

Use ``/prism near`` to do a quick query for all actions within a five block radius (configurable). It shows you all actions so you can quickly get a picture of what's happened.
Excellent for an area with no specific/visible issues. For example you can't tell what was griefed or you need to see if anyone was here recently.

Lookup Command
--------------

When the inspector and near commands aren't enough you have the power to query essentially any results from the database.
Use ``/prism l`` (params) to do a search of information. See Understanding Parameters By using any arrangement of the parameters you can easily filter through records and find out exactly what you need.
Lookup queries can be local to you, or even global if you so desire. Need to see only all item-pickup actions by player nasonfish within ten blocks? Or how about where player YeaItsMe last placed a chest in the entire server.
It's almost limitless.

Pagination
==========

Most times, a lookup or near query returns a lot of information. We break this into pages for easier navigating.
After any lookup or near query, use /prism page [page #] to skip to different pages of results. It simply uses the data from your last search and will expire after five minutes.
Also available are /pr page next and /pr page prev.

Teleporting
============

When you perform a lookup, the results will include an index number with every action. If using global or extended views, you'll also see the record ID.

Use ``/prism tp [index #]`` to teleport to a record's location.

or use ``/pr tp id:[id]`` to teleport to a specific database record.

Once teleported you may also use ``/pr tp (next|prev)`` to teleport to the next or previous index.

*Note: We're working on a smarter teleport system that will accurately judge the safety of the area, and attempt a nearby spot. This is in development*