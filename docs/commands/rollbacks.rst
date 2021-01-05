********************************
Rollbacks, Restores and Previews
********************************

Appliers
==========

Preview
-------

Previewing a rollback or restore is **the safest way**.
When you preview a rollback/restore, Prism queries the database with your given parameters, and shows those blocks changes/restorations to you only - it does not apply the rollback/restore to the world.
Players around you won't see the preview, only you will.

This allows you to see whether or not your rollback/restore will do what you intended - because if not you can cancel it without any impact on the real map.

Use `/prism preview (rollback/rb or restore/rs) (params)`.
If the results look good and you wish to apply them to the map, use `/prism preview apply`.

If not, use `/prism preview cancel`.
You may only have one preview pending at a time, and they will expire after a minute.
If you forget about them or logout, they will self-cancel.

Applying a preview doesn't require the (params) again because Prism remembers what you had queried.
When you apply a preview, it's done exactly the same way but as a real rollback/restore.

**The Prism team recommends you make a habit of always using preview.**

Note: Only block changes are supported in preview mode. Item/entity/etc rollbacks won't show in a preview, but will still be rolled back correctly when the preview is applied._

Rollback
--------

Some expert users may be comfortable enough to rollback without a preview.
Use `/prism rollback (params)`.
Your changes will be applied immediately.

Rollback Wand
^^^^^^^^^^^^^

Another wand type is a rollback wand.
Like the inspector it's for use on a single block or empty space.
Use `/prism wand rollback` and the wand will bind to your hand/tools.

Left-click a block to roll back its last action, or right-click a block face to rollback the block from that spot.
Repeat the command to disable the wand.

Useful for fixing minor griefs when you'd rather not worry about commands.

_Note: The Prism team reminds you that if you allow rollbacks, especially of items removed from chests or entity kills, be aware that staff with access to do so have the capability to do so repeatedly. There's no absolute way to ensure an item or entity hasn't already been rolled back but you shouldn't have a problem because only staff can perform rollbacks and staff are by definition trusted. However, you can disable item removal rollbacks in the config._

Restore
-------

Restoring is a way to re-apply changes that usually have been rolled back already.
Essentially a method to reverse a rollback.

Use `/prism restore (params)`.

_Note: Both rollbacks and restores of block-place actions will try their best to avoid re-applying block changes to an area with newer activity. For example if you re-apply a block-place I did, but Natman93 has already put a new block there, Prism will skip it._

What does Skipped mean?
=======================

- Entities that prism will refuse to rollback. Currently: Creepers
- Blocks that prism will refuse to rollback: fire, tnt, lava
- For some reason crops aren't rolling back onto soil (if not dirt/grass/air)
- For some reason lilypads aren't rolling back on water (water or air are ok)
- If the location that we're trying to place a block into has changed from what we expect. Example - if a player A places stone and breaks it, and player B places a gold block in that same spot, a rollback of `p:playerA a:break` will be skipped because Prism doesn't want to mess up what Player B has done. To force this, use the `-overwrite` flag during rollbacks.