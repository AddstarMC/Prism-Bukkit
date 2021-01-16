Preview
=======

.. tip::

  /prism preview [rollback | restore | apply | cancel ] <params> <flags>
  /pr pr

Previewing a rollback or restore is **the safest way**.
When you preview a rollback/restore, Prism queries the database with your given parameters, and shows those blocks changes/restorations to you only - it does not apply the rollback/restore to the world.
Players around you won't see the preview, only you will.

This allows you to see whether or not your rollback/restore will do what you intended - because if not you can cancel it without any impact on the real map.

Use `/prism preview (rollback/rb or restore/rs) (params)`.

If not, use `/prism preview cancel`.
You may only have one preview pending at a time, and they will expire after a minute.
If you forget about them or logout, they will self-cancel.

Apply a Preview
^^^^^^^^^^^^^^^

.. tip::
  :name: Syntax

  /prism preview apply

If the results look good and you wish to apply them to the map, use `/prism preview apply`.
Applying a preview doesn't require the (params) again because Prism remembers what you had queried.
When you apply a preview, it's done exactly the same way but as a real rollback/restore.

**The Prism team recommends you make a habit of always using preview.**

Note: Only block changes are supported in preview mode. Item/entity/etc rollbacks won't show in a preview, but will still be rolled back correctly when the preview is applied.