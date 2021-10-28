Tracked Actions
===============

The following list of actions can be tracked and or ignored via config. To ensure they are tracked add the option to your configuration file. Any item NOT in the list will be set as false.

e.g.

.. code-block:: yaml

  prism:
    tracking:
      item-remove: true

List of Tracked Actions
-----------------------

see :ref:`action-list`

Reports
-------

Prism offers a limited set of reports on block and action data per player.

`/prism (rp|report) sum (blocks|actions) (player)`

A summary report for blocks will list the total number of block break/place actions for the player, broken down by block.

The actions summary will display a count for each action type for the specific player.

Ore Alerts
----------

This system alerts you when a player discovers a natural (non-placed) vein of ores.
It reports the ore type and the lighting levels (not always indicative of xray due to brightness differences).
The ore type messages are colored for easy recognition.

We've found this extremely effective at spotting xray. Players who show very clear patterns are very easy to identify.

Item Use Alerts
---------------

Item use alerts tell when you a player is using something that's possibly related to griefing.
Lighting fires, placing tnt, placing pistons, etc.

If you wish, you may also define a list blocks that will alert you when broken.

Vanilla Piston Xray Alert
-------------------------

There's a known exploit involving a piston that lets you see through blocks.
Prism will attempt to alert you when it seems like a player is trying to use this trick.

Ex, Drain
---------

Use `/prism ex [radius]` to extinguish all fires in the radius, or `/prism drain [radius]` to remove all liquid (water and lava) in the radius.

You may also drain specific subtypes of liquid, so `/prism drain water` or `lava`, and if providing a radius, `/prism drain lava 10`.

When performing a rollback of block-burn actions, Prism will automatically extinguish the fires as well.


Chunk Boundary View
-------------------
Use `/pr view chunk` to view a glowstone preview of the chunk edges.
Repeat the command to disable.

Delete
------

Server operators can use `/prism delete [timeframe]` to manually purge records from the database.
This isn't usually necessary, as Prism will automatically purge records (per config) every 12 hours.
See :ref:`purging` .
