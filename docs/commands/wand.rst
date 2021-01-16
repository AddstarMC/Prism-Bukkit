Wands
=====

.. tip::

  | /prism wand [rollback | restore | inspect}
  | /pr wand
  | /pr i

The wand allows rollback, restores and inspections with a click block interface - using block mode allows you to inspect an air block for changes, the hand and item mode inspect the block that is clicked.


Setting / Resetting the Wand
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. tip::
  :name: Syntax

  | /prism setmy [item | hand | block}
  | /prism resetmy

There are three wand modes: `hand`, `item`, `block`.

You can define the default mode for all Prism users in the config.
You may set the mode, which item is used, whether not to auto-equip the player with it, and more.

- "hand" is the default and works as Prism does currently.
- "item" allows you to set a default item-id that will be used as the wand.
- "block" mode allows you to set a default block id, so you can replicate the way other rollback plugins work, with the placement of a block to mimic the right-click of hand/item actions. Using the block mode is not recommend to a high number of problems introduced by other plugins, inventory update problems, etc.

Auto-Equip
^^^^^^^^^^

You may control whether or not the items are automatically given to players when the wands are activated.
If you have the item, we move it to your hand and swap out whatever you held before.

If you do not have the item, we give you one, and we will also remove it when you disable the wand!